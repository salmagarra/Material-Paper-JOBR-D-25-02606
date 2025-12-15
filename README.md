# Materials  
**Journal of Business Research (JOBR-D-25-02606)** 
-This repository contains the data and R scripts required to replicate the empirical analyses reported in the paper:
> **Modern Slavery as Tail Risk: An Extreme Value Theory Analysis of ESG, Agrifood, and Financial Sectors**

All methodological details, model specifications, and interpretations are presented in the paper.  
The purpose of this repository is solely to **document and enable replication of the reported results**.
## Contents

### Data
- `etf_clean_returns.csv`  
  Cleaned daily log returns of ETFs used to construct ESG, Agrifood, and Financial sector portfolios.

- `conditioning_factors.csv`  
  Conditioning ETFs capturing exposure to modern slavery risk, including regional exposure and sectors with high severe labor controversies.

### Code

All codes are written in **R** and reproduce the results presented in the paper.

- `Tail_risk_code.R`  
  EVT-based tail risk estimation.

- `Systemic_risk_code.R`  
  Systemic risk (tail dependence) estimation.

- `Spillover_risk_code.R`  
  Spillover risk estimation based on joint extreme losses.

- `Rolling_EVT_tail_risk_code.R`  
  Rolling-window tail risk estimation.

- `Rolling_EVT_systemic_risk_code.R`  
  Rolling systemic risk estimation.

- `Rolling_EVT_spillover_risk_code.R`  
  Rolling spillover risk estimation.

## Replication Note

Running the scripts using the provided datasets reproduces the figures and tables reported in the paper.  
No additional data sources are required.
