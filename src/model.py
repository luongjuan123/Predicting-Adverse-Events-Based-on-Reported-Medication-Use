import torch
import torch.nn as nn
import torch.nn.functional as F

class DrugReactionGNN(nn.Module):
    """
    Message passing on the historical Drug-Reaction bipartite Knowledge Graph.
    Propagates clinical evidence between drugs and reactions.
    """
    def __init__(self, num_drugs, num_reactions, hidden_dim=64, dropout=0.2):
        super().__init__()
        self.num_drugs = num_drugs
        self.num_reactions = num_reactions
        self.hidden_dim = hidden_dim
        
        # Base entity embeddings (+1 for padding index)
        self.drug_emb = nn.Embedding(num_drugs + 1, hidden_dim, padding_idx=num_drugs)
        self.react_emb = nn.Embedding(num_reactions, hidden_dim)
        
        # Linear projections for message passing
        self.d2r_lin = nn.Linear(hidden_dim, hidden_dim)
        self.r2d_lin = nn.Linear(hidden_dim, hidden_dim)
        
        self.norm_drug = nn.LayerNorm(hidden_dim)
        self.norm_react = nn.LayerNorm(hidden_dim)
        self.dropout = nn.Dropout(dropout)
        
        # Initialize
        nn.init.xavier_uniform_(self.drug_emb.weight[:-1])
        nn.init.xavier_uniform_(self.react_emb.weight)

    def forward(self, edge_index_d2r, edge_weight):
        """
        edge_index_d2r: [2, num_edges] where row 0 is drug_id, row 1 is react_id
        edge_weight: [num_edges]
        """
        drug_id, react_id = edge_index_d2r[0], edge_index_d2r[1]
        
        d_emb = self.drug_emb.weight[:-1] # [num_drugs, hidden_dim]
        r_emb = self.react_emb.weight     # [num_reactions, hidden_dim]
        
        w = edge_weight.unsqueeze(-1)
        
        # 1. Drug -> Reaction messages
        msg_d2r = self.d2r_lin(d_emb[drug_id]) * w
        agg_react = torch.zeros_like(r_emb)
        agg_react.index_add_(0, react_id, msg_d2r)
        
        # 2. Reaction -> Drug messages
        msg_r2d = self.r2d_lin(r_emb[react_id]) * w
        agg_drug = torch.zeros_like(d_emb)
        agg_drug.index_add_(0, drug_id, msg_r2d)
        
        # Residual update
        out_drug = self.norm_drug(d_emb + self.dropout(F.relu(agg_drug)))
        out_react = self.norm_react(r_emb + self.dropout(F.relu(agg_react)))
        
        # Append padding vector to drug embeddings
        out_drug_with_pad = torch.cat([out_drug, self.drug_emb.weight[-1:]], dim=0)
        
        return out_drug_with_pad, out_react

class HeteroPharmacovigilanceNet(nn.Module):
    """
    Leakage-free Pharmacovigilance Network:
    - Learns drug & reaction semantics from the bipartite Knowledge Graph.
    - Encodes patient demographics (age, sex).
    - Aggregates taken drugs with causality-weighted attention (fully vectorized).
    - Predicts multi-label adverse event probabilities without target leakage.
    """
    def __init__(self, num_drugs, num_reactions, dem_dim=5, hidden_dim=64, dropout=0.2):
        super().__init__()
        self.num_drugs = num_drugs
        self.num_reactions = num_reactions
        self.hidden_dim = hidden_dim
        
        # KG GNN module
        self.kg_gnn = DrugReactionGNN(num_drugs, num_reactions, hidden_dim=hidden_dim, dropout=dropout)
        
        # Demographics MLP
        self.dem_mlp = nn.Sequential(
            nn.Linear(dem_dim, hidden_dim),
            nn.ReLU(),
            nn.LayerNorm(hidden_dim),
            nn.Dropout(dropout)
        )
        
        # Attention query for drug pooling
        self.drug_attn_query = nn.Parameter(torch.randn(hidden_dim, 1))
        
        # Fusion MLP combining Demographics and Drug Regimen
        self.fusion_mlp = nn.Sequential(
            nn.Linear(hidden_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.LayerNorm(hidden_dim),
            nn.Dropout(dropout)
        )
        
        # Reaction prior bias (baseline occurrence frequency)
        self.reaction_bias = nn.Parameter(torch.zeros(num_reactions))

    def encode_patients(self, dem_tensor, padded_drug_ids, padded_drug_weights, drug_mask, drug_embeddings):
        """
        dem_tensor: [batch_size, dem_dim]
        padded_drug_ids: [batch_size, max_len]
        padded_drug_weights: [batch_size, max_len]
        drug_mask: [batch_size, max_len] (bool)
        drug_embeddings: [num_drugs + 1, hidden_dim]
        """
        h_dem = self.dem_mlp(dem_tensor) # [batch_size, hidden_dim]
        
        # Vectorized drug embedding lookup
        d_vecs = drug_embeddings[padded_drug_ids] # [batch_size, max_len, hidden_dim]
        
        # Attentive pooling
        attn_logits = torch.matmul(d_vecs, self.drug_attn_query).squeeze(-1) # [batch_size, max_len]
        attn_logits = attn_logits.masked_fill(~drug_mask, -1e4)
        attn_logits = attn_logits + torch.log(padded_drug_weights + 1e-6)
        
        attn_scores = F.softmax(attn_logits, dim=-1) # [batch_size, max_len]
        attn_scores = attn_scores.masked_fill(~drug_mask, 0.0)
        
        # Weighted sum
        h_drugs = torch.sum(d_vecs * attn_scores.unsqueeze(-1), dim=1) # [batch_size, hidden_dim]
        
        # Fuse demographics + drug regimen
        combined = torch.cat([h_dem, h_drugs], dim=-1)
        z_patient = self.fusion_mlp(combined) # [batch_size, hidden_dim]
        
        return z_patient

    def forward(self, dem_tensor, padded_drug_ids, padded_drug_weights, drug_mask, edge_index_d2r, edge_weight):
        """
        Returns full adverse reaction logits for the batch: [batch_size, num_reactions]
        """
        # 1. Update drug and reaction embeddings via KG message passing
        h_drugs, h_reacts = self.kg_gnn(edge_index_d2r, edge_weight)
        
        # 2. Encode patients using demographics and taken drugs
        z_patient = self.encode_patients(dem_tensor, padded_drug_ids, padded_drug_weights, drug_mask, h_drugs)
        
        # 3. Compute dot-product logits across all reactions
        logits = torch.matmul(z_patient, h_reacts.t()) + self.reaction_bias.unsqueeze(0)
        
        return logits
