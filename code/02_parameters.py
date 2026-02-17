import pandas as pd
import numpy as np
from statsmodels.tsa.ar_model import AutoReg
import json

data_quarterly = pd.read_excel("data/DataParametrization.xlsx", sheet_name="Quarterly")
for col in ['tot', 'px', 'pm', 'X', 'M', 'Y']:
    data_quarterly[col] = pd.to_numeric(data_quarterly[col], errors='coerce')

gamma_xm    = ((data_quarterly.X + data_quarterly.M)/(2*data_quarterly.Y)).mean() # Openness measure
gamma_m     = (data_quarterly.M/data_quarterly.Y).mean() # Consumption on tradables share
gamma       = gamma_m

print(f'gamma (import share): {gamma_m:.4f}')
print(f'gamma (avg trade share): {gamma_xm:.4f}')

tot        = np.log(data_quarterly.tot.astype(float))
tot_demean = tot - tot.mean()



#####################
# Update params.json
#####################

with open('code/params.json') as f:
    p = json.load(f)
p['gamma'] = round(gamma, 4)
with open('code/params.json', 'w') as f:
    json.dump(p, f, indent=4)