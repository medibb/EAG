import pandas as pd
import glob
import re

print('checkpoint')

data_path = '.\Open*.txt'
data_list = glob.glob(data_path)

for file_path in data_list:
    save_path = re.sub('.txt', '.csv', file_path)

    lines = []
    with open(file_path, 'r') as f:
        for l in f:
            lines.append(l)

    values = [l.split(', ')[1:-5] for l in lines[7:]]
    df = pd.DataFrame(values)
    df.to_csv(save_path, header=False, index=False)

