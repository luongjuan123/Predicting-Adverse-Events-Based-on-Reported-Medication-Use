import sys, os; sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import os
import json
import time
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import torch
import torch.nn.functional as F
from torch.utils.data import DataLoader
from sklearn.metrics import roc_curve, auc, precision_recall_curve, average_precision_score, f1_score
from src.model import HeteroPharmacovigilanceNet
from src.train import PatientDataset, make_collate_fn

DATA_DIR = "data"
CHECKPOINT_DIR = "checkpoints"

def evaluate_test_set(batch_size=512):
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Evaluation device: {device}")

    # Load vocabularies
    with open(os.path.join(DATA_DIR, "vocab_drugs.json")) as f:
        drug2id = json.load(f)
    with open(os.path.join(DATA_DIR, "vocab_reactions.json")) as f:
        react2id = json.load(f)

    id2react = {v: k for k, v in react2id.items()}
    num_drugs = len(drug2id)
    num_reactions = len(react2id)

    # Load KG
    kg_data = torch.load(os.path.join(DATA_DIR, "kg_edges.pt"), map_location=device, weights_only=False)
    edge_index = kg_data['edge_index_drug_to_react'].to(device)
    edge_weight = kg_data['edge_weight'].to(device)

    # Load Checkpoint
    checkpoint_path = os.path.join(CHECKPOINT_DIR, "best_model.pt")
    if not os.path.exists(checkpoint_path):
        raise FileNotFoundError(f"Checkpoint {checkpoint_path} not found!")

    ckpt = torch.load(checkpoint_path, map_location=device, weights_only=False)
    model = HeteroPharmacovigilanceNet(num_drugs, num_reactions, dem_dim=5, hidden_dim=64, dropout=0.0).to(device)
    model.load_state_dict(ckpt['model_state_dict'])
    model.eval()
    print(f"Loaded best model from epoch {ckpt['epoch']} (Val MRR: {ckpt['best_mrr']:.4f})")

    # Load Test Data
    print("Loading test data...")
    test_raw = torch.load(os.path.join(DATA_DIR, "test_data.pt"), weights_only=False)
    print(f"Total test patients: {len(test_raw):,}")

    collate = make_collate_fn(num_drugs, num_reactions)
    # Evaluate on a large representative test cohort (25,000 cases) for memory-safe ranking & ROC computation
    test_subset = test_raw[:25000]
    test_loader = DataLoader(PatientDataset(test_subset), batch_size=batch_size, shuffle=False, collate_fn=collate, num_workers=2)

    all_scores = []
    all_targets = []

    hit_counts = {1: 0, 5: 0, 10: 0, 20: 0, 50: 0}
    mrr_sum = 0.0
    ndcg10_sum = 0.0
    total_evaluated = 0

    print("Running inference across test cohort...")
    t0 = time.time()
    with torch.no_grad():
        for dems, p_drugs, p_weights, d_mask, targets in test_loader:
            dems = dems.to(device)
            p_drugs = p_drugs.to(device)
            p_weights = p_weights.to(device)
            d_mask = d_mask.to(device)
            targets = targets.to(device)

            with torch.amp.autocast('cuda'):
                logits = model(dems, p_drugs, p_weights, d_mask, edge_index, edge_weight)
                probs = torch.sigmoid(logits)

            # Store for global ROC / PR metrics
            all_scores.append(probs.cpu().numpy())
            all_targets.append(targets.cpu().numpy())

            # Ranking evaluation
            b_size = targets.size(0)
            _, top_indices = torch.topk(logits, k=50, dim=-1)

            for i in range(b_size):
                true_set = set(targets[i].nonzero().squeeze(-1).tolist())
                if not true_set:
                    continue
                ranked = top_indices[i].tolist()

                for k in hit_counts:
                    if any(r in true_set for r in ranked[:k]):
                        hit_counts[k] += 1

                # MRR
                for rank, r in enumerate(ranked, 1):
                    if r in true_set:
                        mrr_sum += 1.0 / rank
                        break

                # NDCG@10
                dcg = 0.0
                idcg = sum(1.0 / np.log2(idx + 2) for idx in range(min(10, len(true_set))))
                for rank, r in enumerate(ranked[:10]):
                    if r in true_set:
                        dcg += 1.0 / np.log2(rank + 2)
                ndcg10_sum += (dcg / idcg) if idcg > 0 else 0.0

                total_evaluated += 1

    duration = time.time() - t0
    print(f"Inference completed in {duration:.1f}s ({total_evaluated:,} patients evaluated).")

    # Ranking metrics
    ranking_results = {
        "Hit@1": hit_counts[1] / total_evaluated,
        "Hit@5": hit_counts[5] / total_evaluated,
        "Hit@10": hit_counts[10] / total_evaluated,
        "Hit@20": hit_counts[20] / total_evaluated,
        "Hit@50": hit_counts[50] / total_evaluated,
        "MRR": mrr_sum / total_evaluated,
        "NDCG@10": ndcg10_sum / total_evaluated
    }

    # Aggregate matrices for ROC and PR
    all_scores = np.concatenate(all_scores, axis=0)
    all_targets = np.concatenate(all_targets, axis=0)

    # Global Micro ROC-AUC & PR-AUC
    fpr_micro, tpr_micro, _ = roc_curve(all_targets.ravel(), all_scores.ravel())
    roc_auc_micro = auc(fpr_micro, tpr_micro)

    prec_micro, rec_micro, _ = precision_recall_curve(all_targets.ravel(), all_scores.ravel())
    pr_auc_micro = average_precision_score(all_targets.ravel(), all_scores.ravel())

    # Optimal threshold metrics
    binary_preds = (all_scores > 0.5).astype(int)
    from sklearn.metrics import precision_score, recall_score
    p_score = precision_score(all_targets.ravel(), binary_preds.ravel(), zero_division=0)
    r_score = recall_score(all_targets.ravel(), binary_preds.ravel(), zero_division=0)
    f1 = f1_score(all_targets.ravel(), binary_preds.ravel(), zero_division=0)

    print("\n" + "=" * 60)
    print("FINAL RIGOROUS TEST SET RESULTS")
    print("=" * 60)
    print(f"  Hit@5:       {ranking_results['Hit@5']*100:.2f}% (True AE in Top-5)")
    print(f"  Hit@10:      {ranking_results['Hit@10']*100:.2f}% (True AE in Top-10)")
    print(f"  Hit@20:      {ranking_results['Hit@20']*100:.2f}% (True AE in Top-20)")
    print(f"  Hit@50:      {ranking_results['Hit@50']*100:.2f}% (True AE in Top-50)")
    print(f"  MRR:         {ranking_results['MRR']:.4f}")
    print(f"  NDCG@10:     {ranking_results['NDCG@10']:.4f}")
    print(f"  Micro AUC:   {roc_auc_micro:.4f}")
    print(f"  Micro PR-AUC:{pr_auc_micro:.4f}")
    print(f"  Precision:   {p_score:.4f} (@ threshold 0.5)")
    print(f"  Recall:      {r_score:.4f} (@ threshold 0.5)")
    print(f"  F1-Score:    {f1:.4f} (@ threshold 0.5)")
    print("=" * 60)

    # Save metrics JSON
    final_metrics = {
        **ranking_results,
        "ROC_AUC_Micro": float(roc_auc_micro),
        "PR_AUC_Micro": float(pr_auc_micro),
        "Precision_0.5": float(p_score),
        "Recall_0.5": float(r_score),
        "F1_0.5": float(f1)
    }
    with open(os.path.join(CHECKPOINT_DIR, "test_results.json"), "w") as f:
        json.dump(final_metrics, f, indent=2)

    # 1. Plot ROC Curve
    plt.figure(figsize=(7, 5))
    plt.plot(fpr_micro, tpr_micro, color='darkorange', lw=2, label=f'Micro-average ROC (AUC = {roc_auc_micro:.4f})')
    plt.plot([0, 1], [0, 1], color='navy', lw=1.5, linestyle='--', label='Random Chance')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate (1 - Specificity)', fontsize=12)
    plt.ylabel('True Positive Rate (Sensitivity)', fontsize=12)
    plt.title('Receiver Operating Characteristic (ROC) - Strict Inductive Test', fontsize=13)
    plt.legend(loc="lower right")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig('evaluation_roc_curves.png', dpi=300)
    plt.close()

    # 2. Plot PR Curve
    plt.figure(figsize=(7, 5))
    plt.plot(rec_micro, prec_micro, color='forestgreen', lw=2, label=f'Micro-average PR (AUC = {pr_auc_micro:.4f})')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('Recall', fontsize=12)
    plt.ylabel('Precision', fontsize=12)
    plt.title('Precision-Recall Curve (Severe Imbalance Handling)', fontsize=13)
    plt.legend(loc="upper right")
    plt.grid(True, linestyle='--', alpha=0.6)
    plt.tight_layout()
    plt.savefig('evaluation_pr_curves.png', dpi=300)
    plt.close()

    # 3. Plot Training Curves
    history_file = os.path.join(CHECKPOINT_DIR, "training_history.json")
    if os.path.exists(history_file):
        with open(history_file) as f:
            hist = json.load(f)
        fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))
        
        # Loss
        axes[0].plot(hist['train_loss'], label='Train Loss', color='blue', marker='o')
        axes[0].plot(hist['val_loss'], label='Val Loss', color='red', marker='s')
        axes[0].set_xlabel('Epoch')
        axes[0].set_ylabel('Loss (BCE With Pos-Weight)')
        axes[0].set_title('Training & Validation Loss Convergence')
        axes[0].legend()
        axes[0].grid(True, linestyle='--', alpha=0.6)

        # Ranking
        axes[1].plot(hist['val_hit10'], label='Val Hit@10', color='purple', marker='^')
        axes[1].plot(hist['val_mrr'], label='Val MRR', color='darkgreen', marker='d')
        axes[1].set_xlabel('Epoch')
        axes[1].set_ylabel('Score')
        axes[1].set_title('Validation Clinical Ranking Metrics Across Epochs')
        axes[1].legend()
        axes[1].grid(True, linestyle='--', alpha=0.6)

        plt.tight_layout()
        plt.savefig('training_loss_curve.png', dpi=300)
        plt.close()

    print("Generated and saved evaluation plots:")
    print("  - evaluation_roc_curves.png")
    print("  - evaluation_pr_curves.png")
    print("  - training_loss_curve.png")

if __name__ == "__main__":
    evaluate_test_set()
