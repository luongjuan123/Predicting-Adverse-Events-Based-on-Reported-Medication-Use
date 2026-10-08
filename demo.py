import sys, os; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import os
import json
import argparse
import torch
from src.model import HeteroPharmacovigilanceNet
from src.preprocess import clean_ingredient

DATA_DIR = "data"
CHECKPOINT_DIR = "checkpoints"

def load_system():
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    
    with open(os.path.join(DATA_DIR, "vocab_drugs.json")) as f:
        drug2id = json.load(f)
    with open(os.path.join(DATA_DIR, "vocab_reactions.json")) as f:
        react2id = json.load(f)
    with open(os.path.join(DATA_DIR, "demographics_scaler.json")) as f:
        scaler = json.load(f)

    id2react = {v: k for k, v in react2id.items()}
    num_drugs = len(drug2id)
    num_reactions = len(react2id)

    kg_data = torch.load(os.path.join(DATA_DIR, "kg_edges.pt"), map_location=device, weights_only=False)
    edge_index = kg_data['edge_index_drug_to_react'].to(device)
    edge_weight = kg_data['edge_weight'].to(device)

    ckpt_path = os.path.join(CHECKPOINT_DIR, "best_model.pt")
    if not os.path.exists(ckpt_path):
        raise FileNotFoundError(f"Trained model not found at {ckpt_path}. Please train first!")

    ckpt = torch.load(ckpt_path, map_location=device, weights_only=False)
    model = HeteroPharmacovigilanceNet(num_drugs, num_reactions, dem_dim=5, hidden_dim=64, dropout=0.0).to(device)
    model.load_state_dict(ckpt['model_state_dict'])
    model.eval()

    return model, drug2id, id2react, scaler, edge_index, edge_weight, device

def predict_adverse_events(age, sex, suspected_drugs, concomitant_drugs, top_k=10):
    model, drug2id, id2react, scaler, edge_index, edge_weight, device = load_system()
    num_drugs = len(drug2id)

    # 1. Demographics
    mean_age = scaler['mean_age']
    std_age = scaler['std_age']
    if age is None:
        dem = [0.0, 1.0] # age_norm=0, missing=1
    else:
        dem = [(float(age) - mean_age) / (std_age + 1e-6), 0.0]

    sex_clean = str(sex).lower().strip()
    if 'female' in sex_clean:
        dem += [1.0, 0.0, 0.0]
    elif 'male' in sex_clean:
        dem += [0.0, 1.0, 0.0]
    else:
        dem += [0.0, 0.0, 1.0]

    dem_tensor = torch.tensor([dem], dtype=torch.float32, device=device)

    # 2. Parse drugs
    drug_ids = []
    drug_weights = []
    recognized_drugs = []
    unrecognized_drugs = []

    for d in suspected_drugs:
        d_clean = clean_ingredient(d)
        if d_clean in drug2id:
            drug_ids.append(drug2id[d_clean])
            drug_weights.append(2.0) # Suspected
            recognized_drugs.append(f"{d_clean} [Suspected, weight=2.0]")
        else:
            unrecognized_drugs.append(d)

    for d in concomitant_drugs:
        d_clean = clean_ingredient(d)
        if d_clean in drug2id:
            drug_ids.append(drug2id[d_clean])
            drug_weights.append(1.0) # Concomitant
            recognized_drugs.append(f"{d_clean} [Concomitant, weight=1.0]")
        else:
            unrecognized_drugs.append(d)

    if not drug_ids:
        print("Error: None of the entered drugs matched the clinical vocabulary!")
        print("Unrecognized inputs:", unrecognized_drugs)
        return

    # 3. Batch formatting
    padded_drugs = torch.tensor([drug_ids], dtype=torch.long, device=device)
    padded_weights = torch.tensor([drug_weights], dtype=torch.float32, device=device)
    drug_mask = torch.ones((1, len(drug_ids)), dtype=torch.bool, device=device)

    # 4. Inference
    with torch.no_grad():
        with torch.amp.autocast('cuda'):
            logits = model(dem_tensor, padded_drugs, padded_weights, drug_mask, edge_index, edge_weight)
            probs = torch.sigmoid(logits)[0]

    top_probs, top_indices = torch.topk(probs, k=top_k)

    print("\n" + "=" * 65)
    print("      🏥 PERSONAL DRUG SAFETY ASSISTANT - ADVERSE EVENT PREDICTION")
    print("=" * 65)
    print(f"Patient Profile:  Age = {age if age is not None else 'Unknown'}, Sex = {sex}")
    print(f"Active Regimen:   {', '.join(recognized_drugs)}")
    if unrecognized_drugs:
        print(f"Unmatched Drugs:  {', '.join(unrecognized_drugs)}")
    print("-" * 65)
    print(f"  Rank | {'Predicted Adverse Event (MedDRA)':<32} | Likelihood Score")
    print("-" * 65)
    
    for rank, (p, idx) in enumerate(zip(top_probs.tolist(), top_indices.tolist()), 1):
        reaction_name = id2react[idx]
        pct = p * 100
        bar = "█" * int(pct / 5)
        print(f"  {rank:>4} | {reaction_name:<32} | {pct:>5.1f}%  {bar}")
    print("=" * 65 + "\n")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Personal Drug Safety Assistant")
    parser.add_argument("--age", type=float, default=58, help="Patient age in years")
    parser.add_argument("--sex", type=str, default="female", help="Patient sex (male/female/other)")
    parser.add_argument("--suspected", type=str, default="naproxen", help="Comma-separated suspected drugs")
    parser.add_argument("--concomitant", type=str, default="omeprazole", help="Comma-separated concomitant drugs")
    parser.add_argument("--top_k", type=int, default=10, help="Number of adverse events to predict")
    args = parser.parse_args()

    s_drugs = [d.strip() for d in args.suspected.split(",") if d.strip()]
    c_drugs = [d.strip() for d in args.concomitant.split(",") if d.strip()]

    predict_adverse_events(args.age, args.sex, s_drugs, c_drugs, top_k=args.top_k)
