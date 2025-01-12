## Simulation dataset
import numpy as np
import pandas as pd

def simulate(cohorts, sample_sizes):
    
    # Define sample-sizes
    total_individuals = sum(sample_sizes)

    # Simulate Cohort and IID
    cohort_list = np.concatenate([[cohort] * size for cohort, size in zip(cohorts, sample_sizes)])
    iid_list = [f"ID_{i+1}" for i in range(total_individuals)]

    # Simulate sex
    sex = np.random.choice([0, 1], total_individuals)

    # Simulate PGS
    pgs = np.random.normal(0, 1, total_individuals)

    # Simulate binary outcome (ID_binary)
    id_binary_prob = 1 / (1 + np.exp(-0.3 * pgs))  
    ID_binary = np.random.binomial(1, id_binary_prob)

    def simulate_variant(base_prob, ID_binary, strength=0.05):
        prob = np.clip(base_prob + strength * (ID_binary - 0.5), 0, 1)
        return np.random.binomial(1, prob)

    # Rare genetic variants
    DEL = simulate_variant(0.1, ID_binary)
    DUP = simulate_variant(0.1, ID_binary)
    LOF = simulate_variant(0.05, ID_binary)
    MIS = simulate_variant(0.15, ID_binary)

    df = pd.DataFrame({
        'IID': iid_list,
        'Cohort': cohort_list,
        'sex': sex,
        'PGS': pgs,
        'DEL': DEL,
        'DUP': DUP,
        'LOF': LOF,
        'MIS': MIS,
        'ID_binary': ID_binary
    })
    return df