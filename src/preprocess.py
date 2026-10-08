import os
import re
import glob
import json
import time
from collections import Counter
import numpy as np
import pandas as pd
import torch

DATA_DIR = "DAEN"
OUTPUT_DIR = "data"

def clean_ingredient(raw_str):
    """Normalize active ingredient strings."""
    s = raw_str.strip().lower()
    s = re.sub(r'\s+', ' ', s)
    return s

def clean_reaction(raw_str):
    """Normalize MedDRA reaction terms."""
    s = raw_str.replace('•', '').strip()
    s = re.sub(r'\s+', ' ', s)
    return s

def parse_record(row):
    # 1. Age
    raw_age = str(row['Age (years)']).strip()
    if raw_age in ['-', 'None', 'nan', '', 'null']:
        age = None
    elif raw_age.startswith('<'):
        age = 0.5
    else:
        try:
            age = float(raw_age)
            if age < 0 or age > 120:
                age = None
        except:
            age = None
            
    # 2. Sex: 0=Female, 1=Male, 2=Not stated/Other
    raw_sex = str(row['Sex']).strip().lower()
    if 'female' in raw_sex:
        sex = 0
    elif 'male' in raw_sex:
        sex = 1
    else:
        sex = 2

    # 3. Medicines
    raw_meds = str(row['Medicines reported as being taken'])
    drugs = {}
    for line in raw_meds.split('\n'):
        line_str = line.strip()
        if not line_str or line_str == 'nan':
            continue
        is_suspected = ('suspected' in line_str.lower() and 'not suspected' not in line_str.lower())
        weight = 2.0 if is_suspected else 1.0
        
        matches = re.findall(r'\((.*?)\)', line_str)
        for m in matches:
            for ing in re.split(r'[;,/]', m):
                ing_clean = clean_ingredient(ing)
                if ing_clean and len(ing_clean) > 1:
                    drugs[ing_clean] = max(drugs.get(ing_clean, 0.0), weight)
                    
    # 4. Reactions
    raw_reacts = str(row['MedDRA reaction terms'])
    reactions = set()
    for line in raw_reacts.split('\n'):
        r = clean_reaction(line)
        if r and r != 'nan':
            reactions.add(r)
            
    return age, sex, drugs, list(reactions)

def run_preprocessing(min_drug_freq=10, min_react_freq=50, random_seed=42):
    print("=" * 60)
    print("STARTING DAEN DATA PREPROCESSING")
    print("=" * 60)
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    excel_files = sorted(glob.glob(os.path.join(DATA_DIR, "List of Reports_*.xlsx")))
    if not excel_files:
        raise FileNotFoundError(f"No Excel files found in {DATA_DIR}")
    print(f"Found {len(excel_files)} Excel files in {DATA_DIR}:")
    for f in excel_files:
        print(f"  - {f}")

    all_cases = []
    drug_counter = Counter()
    react_counter = Counter()
    valid_ages = []

    t0 = time.time()
    for f_idx, fpath in enumerate(excel_files):
        print(f"\n[{f_idx+1}/{len(excel_files)}] Reading {fpath}...")
        df = pd.read_excel(fpath, engine='calamine')
        print(f"  Loaded {len(df):,} rows. Parsing records...")
        
        for idx in range(len(df)):
            row = df.iloc[idx]
            case_no = row['Case number']
            age, sex, drugs, reacts = parse_record(row)
            
            if not drugs or not reacts:
                continue  # Must have at least one drug and one reaction to be clinically informative
                
            if age is not None:
                valid_ages.append(age)
                
            for d in drugs.keys():
                drug_counter[d] += 1
            for r in reacts:
                react_counter[r] += 1
                
            all_cases.append({
                'case_id': int(case_no) if pd.notnull(case_no) else idx,
                'age': age,
                'sex': sex,
                'drugs': drugs,
                'reactions': reacts
            })

    print(f"\nParsing complete in {time.time() - t0:.1f}s. Valid patient cases: {len(all_cases):,}")
    
    # 1. Build Vocabularies
    mean_age = float(np.mean(valid_ages)) if valid_ages else 50.0
    std_age = float(np.std(valid_ages)) if valid_ages else 20.0
    print(f"Demographics summary: Valid ages={len(valid_ages):,}, Mean age={mean_age:.1f}, Std={std_age:.1f}")

    filtered_drugs = [d for d, c in drug_counter.items() if c >= min_drug_freq]
    filtered_reacts = [r for r, c in react_counter.items() if c >= min_react_freq]

    # Sort deterministically
    filtered_drugs.sort()
    filtered_reacts.sort()

    drug2id = {d: i for i, d in enumerate(filtered_drugs)}
    react2id = {r: i for i, r in enumerate(filtered_reacts)}
    
    print(f"Drug vocabulary size: {len(drug2id):,} (min freq >= {min_drug_freq})")
    print(f"Reaction vocabulary size: {len(react2id):,} (min freq >= {min_react_freq})")
    
    total_reaction_tokens = sum(react_counter.values())
    retained_reaction_tokens = sum(react_counter[r] for r in filtered_reacts)
    coverage = (retained_reaction_tokens / total_reaction_tokens) * 100 if total_reaction_tokens > 0 else 0
    print(f"Reaction token coverage: {coverage:.2f}% of all reported events retained!")

    with open(os.path.join(OUTPUT_DIR, "vocab_drugs.json"), "w") as f:
        json.dump(drug2id, f, indent=2)
    with open(os.path.join(OUTPUT_DIR, "vocab_reactions.json"), "w") as f:
        json.dump(react2id, f, indent=2)
    with open(os.path.join(OUTPUT_DIR, "demographics_scaler.json"), "w") as f:
        json.dump({"mean_age": mean_age, "std_age": std_age}, f, indent=2)

    # 2. Filter cases that have at least one recognized drug and reaction
    print("\nEncoding patient samples...")
    encoded_cases = []
    for c in all_cases:
        d_ids = []
        d_weights = []
        for d, w in c['drugs'].items():
            if d in drug2id:
                d_ids.append(drug2id[d])
                d_weights.append(float(w))
                
        r_ids = [react2id[r] for r in c['reactions'] if r in react2id]
        
        if not d_ids or not r_ids:
            continue
            
        age_val = c['age']
        age_missing = 1.0 if age_val is None else 0.0
        age_norm = ((age_val - mean_age) / (std_age + 1e-6)) if age_val is not None else 0.0
        
        # Sex one-hot: [is_female, is_male, is_other]
        sex_onehot = [0.0, 0.0, 0.0]
        sex_onehot[c['sex']] = 1.0
        
        demographics = [age_norm, age_missing] + sex_onehot
        
        encoded_cases.append({
            'demographics': demographics,
            'drug_ids': d_ids,
            'drug_weights': d_weights,
            'reaction_ids': r_ids
        })

    print(f"Total encoded informative patients: {len(encoded_cases):,}")

    # 3. Train / Val / Test Split (70% / 15% / 15%)
    np.random.seed(random_seed)
    indices = np.random.permutation(len(encoded_cases))
    n_train = int(0.70 * len(encoded_cases))
    n_val = int(0.15 * len(encoded_cases))
    
    train_indices = indices[:n_train]
    val_indices = indices[n_train:n_train + n_val]
    test_indices = indices[n_train + n_val:]
    
    train_cases = [encoded_cases[i] for i in train_indices]
    val_cases = [encoded_cases[i] for i in val_indices]
    test_cases = [encoded_cases[i] for i in test_indices]

    print(f"Splits: Train={len(train_cases):,}, Val={len(val_cases):,}, Test={len(test_cases):,}")

    # 4. Build Drug-Reaction Knowledge Graph EXCLUSIVELY from Train Set (Zero Leakage)
    print("\nConstructing Drug-Reaction Knowledge Graph from Train set...")
    cooccur = Counter()
    drug_train_counts = Counter()
    react_train_counts = Counter()

    for c in train_cases:
        for d_id, w in zip(c['drug_ids'], c['drug_weights']):
            drug_train_counts[d_id] += w
            for r_id in c['reaction_ids']:
                cooccur[(d_id, r_id)] += w
                react_train_counts[r_id] += 1

    src_drugs = []
    dst_reacts = []
    edge_weights = []

    for (d_id, r_id), weight in cooccur.items():
        src_drugs.append(d_id)
        dst_reacts.append(r_id)
        # Normalized weight: log-scaled co-occurrence
        norm_w = float(np.log1p(weight))
        edge_weights.append(norm_w)

    print(f"Knowledge Graph: {len(src_drugs):,} Drug-Reaction edges extracted.")
    
    kg_data = {
        'edge_index_drug_to_react': torch.tensor([src_drugs, dst_reacts], dtype=torch.long),
        'edge_weight': torch.tensor(edge_weights, dtype=torch.float),
        'num_drugs': len(drug2id),
        'num_reactions': len(react2id)
    }
    torch.save(kg_data, os.path.join(OUTPUT_DIR, "kg_edges.pt"))

    # 5. Save Splits as PyTorch serialized datasets
    print("Saving processed splits to disk...")
    torch.save(train_cases, os.path.join(OUTPUT_DIR, "train_data.pt"))
    torch.save(val_cases, os.path.join(OUTPUT_DIR, "val_data.pt"))
    torch.save(test_cases, os.path.join(OUTPUT_DIR, "test_data.pt"))

    print("\nPREPROCESSING SUCCESSFULLY COMPLETED!")
    print(f"Artifacts saved in '{OUTPUT_DIR}':")
    for fname in os.listdir(OUTPUT_DIR):
        fsize = os.path.getsize(os.path.join(OUTPUT_DIR, fname)) / (1024 * 1024)
        print(f"  - {fname}: {fsize:.2f} MB")

if __name__ == "__main__":
    run_preprocessing()
