import sys, os; sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import os
import json
import time
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import Dataset, DataLoader
from src.model import HeteroPharmacovigilanceNet

DATA_DIR = "data"
CHECKPOINT_DIR = "checkpoints"

class PatientDataset(Dataset):
    def __init__(self, data_list):
        self.data = data_list

    def __len__(self):
        return len(self.data)

    def __getitem__(self, idx):
        return self.data[idx]

def make_collate_fn(num_drugs, num_reactions):
    def collate_fn(batch):
        batch_size = len(batch)
        
        # Demographics
        dems = torch.tensor([item['demographics'] for item in batch], dtype=torch.float32)
        
        # Padded Drugs
        max_drugs = max(len(item['drug_ids']) for item in batch)
        padded_drugs = torch.full((batch_size, max_drugs), num_drugs, dtype=torch.long)
        padded_weights = torch.ones((batch_size, max_drugs), dtype=torch.float32)
        drug_mask = torch.zeros((batch_size, max_drugs), dtype=torch.bool)
        
        # Multi-hot targets
        targets = torch.zeros((batch_size, num_reactions), dtype=torch.float32)
        
        for i, item in enumerate(batch):
            n_d = len(item['drug_ids'])
            padded_drugs[i, :n_d] = torch.tensor(item['drug_ids'], dtype=torch.long)
            padded_weights[i, :n_d] = torch.tensor(item['drug_weights'], dtype=torch.float32)
            drug_mask[i, :n_d] = True
            
            targets[i, item['reaction_ids']] = 1.0
            
        return dems, padded_drugs, padded_weights, drug_mask, targets
    return collate_fn

def compute_ranking_metrics(logits, targets, top_k_list=[5, 10, 20]):
    """
    logits: [batch_size, num_reactions]
    targets: [batch_size, num_reactions]
    """
    # Get top 20 predicted indices per patient
    _, top_indices = torch.topk(logits, k=max(top_k_list), dim=-1) # [B, 20]
    
    hits = {k: 0.0 for k in top_k_list}
    mrr_total = 0.0
    batch_size = targets.size(0)
    
    for i in range(batch_size):
        true_indices = set(targets[i].nonzero().squeeze(-1).tolist())
        if not true_indices:
            continue
            
        ranked = top_indices[i].tolist()
        
        # Hit@K
        for k in top_k_list:
            if any(r in true_indices for r in ranked[:k]):
                hits[k] += 1.0
                
        # MRR (rank of first true positive)
        for rank_pos, r in enumerate(ranked, 1):
            if r in true_indices:
                mrr_total += 1.0 / rank_pos
                break
                
    results = {f"hit@{k}": hits[k] / batch_size for k in top_k_list}
    results["mrr"] = mrr_total / batch_size
    return results

def train(epochs=10, batch_size=512, lr=1e-3, pos_weight=15.0):
    os.makedirs(CHECKPOINT_DIR, exist_ok=True)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Using device: {device}")

    # Load metadata and KG
    with open(os.path.join(DATA_DIR, "vocab_drugs.json")) as f:
        drug2id = json.load(f)
    with open(os.path.join(DATA_DIR, "vocab_reactions.json")) as f:
        react2id = json.load(f)
        
    num_drugs = len(drug2id)
    num_reactions = len(react2id)
    print(f"Entities: Drugs={num_drugs:,}, Reactions={num_reactions:,}")

    kg_data = torch.load(os.path.join(DATA_DIR, "kg_edges.pt"), map_location=device, weights_only=False)
    edge_index = kg_data['edge_index_drug_to_react'].to(device)
    edge_weight = kg_data['edge_weight'].to(device)
    print(f"Knowledge Graph edges: {edge_index.size(1):,}")

    # Load splits
    print("Loading datasets...")
    train_raw = torch.load(os.path.join(DATA_DIR, "train_data.pt"), weights_only=False)
    val_raw = torch.load(os.path.join(DATA_DIR, "val_data.pt"), weights_only=False)
    print(f"Loaded: Train={len(train_raw):,}, Val={len(val_raw):,}")

    collate = make_collate_fn(num_drugs, num_reactions)
    train_loader = DataLoader(PatientDataset(train_raw), batch_size=batch_size, shuffle=True, collate_fn=collate, num_workers=4, pin_memory=True)
    
    # Subsample validation set for faster per-epoch evaluation (15,000 cases)
    val_subset = val_raw[:15000]
    val_loader = DataLoader(PatientDataset(val_subset), batch_size=batch_size, shuffle=False, collate_fn=collate, num_workers=2, pin_memory=True)

    # Initialize model
    model = HeteroPharmacovigilanceNet(num_drugs, num_reactions, dem_dim=5, hidden_dim=64, dropout=0.2).to(device)
    
    pos_weight_tensor = torch.tensor([pos_weight], device=device)
    criterion = nn.BCEWithLogitsLoss(pos_weight=pos_weight_tensor)
    
    optimizer = torch.optim.AdamW(model.parameters(), lr=lr, weight_decay=1e-4)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(optimizer, T_max=epochs, eta_min=1e-5)
    scaler = torch.amp.GradScaler('cuda')

    best_mrr = 0.0
    history = {
        'train_loss': [],
        'val_loss': [],
        'val_mrr': [],
        'val_hit10': [],
        'val_hit20': []
    }

    print("\n" + "=" * 70)
    print("STARTING TRAINING (STRICTLY LEAKAGE-FREE CLINICAL HGNN)")
    print("=" * 70)

    for epoch in range(1, epochs + 1):
        t0 = time.time()
        model.train()
        total_train_loss = 0.0
        n_batches = 0

        for dems, p_drugs, p_weights, d_mask, targets in train_loader:
            dems = dems.to(device, non_blocking=True)
            p_drugs = p_drugs.to(device, non_blocking=True)
            p_weights = p_weights.to(device, non_blocking=True)
            d_mask = d_mask.to(device, non_blocking=True)
            targets = targets.to(device, non_blocking=True)

            optimizer.zero_grad()

            with torch.amp.autocast('cuda'):
                logits = model(dems, p_drugs, p_weights, d_mask, edge_index, edge_weight)
                loss = criterion(logits, targets)

            scaler.scale(loss).backward()
            scaler.unscale_(optimizer)
            torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=2.0)
            scaler.step(optimizer)
            scaler.update()

            total_train_loss += loss.item()
            n_batches += 1

        avg_train_loss = total_train_loss / n_batches
        scheduler.step()

        # Validation
        model.eval()
        total_val_loss = 0.0
        val_batches = 0
        all_hits = {5: 0.0, 10: 0.0, 20: 0.0}
        total_mrr = 0.0
        n_val_eval = 0

        with torch.no_grad():
            for dems, p_drugs, p_weights, d_mask, targets in val_loader:
                dems = dems.to(device, non_blocking=True)
                p_drugs = p_drugs.to(device, non_blocking=True)
                p_weights = p_weights.to(device, non_blocking=True)
                d_mask = d_mask.to(device, non_blocking=True)
                targets = targets.to(device, non_blocking=True)

                with torch.amp.autocast('cuda'):
                    logits = model(dems, p_drugs, p_weights, d_mask, edge_index, edge_weight)
                    val_loss = criterion(logits, targets)

                total_val_loss += val_loss.item()
                val_batches += 1

                # Rank metrics
                metrics = compute_ranking_metrics(logits, targets, top_k_list=[5, 10, 20])
                b_size = targets.size(0)
                all_hits[5] += metrics['hit@5'] * b_size
                all_hits[10] += metrics['hit@10'] * b_size
                all_hits[20] += metrics['hit@20'] * b_size
                total_mrr += metrics['mrr'] * b_size
                n_val_eval += b_size

        avg_val_loss = total_val_loss / val_batches
        avg_hit10 = all_hits[10] / n_val_eval
        avg_hit20 = all_hits[20] / n_val_eval
        avg_mrr = total_mrr / n_val_eval
        duration = time.time() - t0

        history['train_loss'].append(avg_train_loss)
        history['val_loss'].append(avg_val_loss)
        history['val_mrr'].append(avg_mrr)
        history['val_hit10'].append(avg_hit10)
        history['val_hit20'].append(avg_hit20)

        is_best = avg_mrr > best_mrr
        if is_best:
            best_mrr = avg_mrr
            torch.save({
                'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'best_mrr': best_mrr,
                'history': history,
                'num_drugs': num_drugs,
                'num_reactions': num_reactions
            }, os.path.join(CHECKPOINT_DIR, "best_model.pt"))
            star = " ★ [BEST MODEL SAVED]"
        else:
            star = ""

        print(f"Epoch {epoch:02d}/{epochs:02d} [{duration:.1f}s] | Train Loss: {avg_train_loss:.4f} | Val Loss: {avg_val_loss:.4f} | Val MRR: {avg_mrr:.4f} | Hit@10: {avg_hit10*100:.1f}% | Hit@20: {avg_hit20*100:.1f}%{star}")

    with open(os.path.join(CHECKPOINT_DIR, "training_history.json"), "w") as f:
        json.dump(history, f, indent=2)

    print("\nTraining completed successfully!")

if __name__ == "__main__":
    train(epochs=10, batch_size=512, lr=1e-3, pos_weight=15.0)
