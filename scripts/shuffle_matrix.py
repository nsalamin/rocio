"""
Be careful when saving these matrix
It does not have a header!!!
"""

import pandas as pd
import numpy as np
from os import listdir
from os.path import isfile, join
import sys

folder_input = sys.argv[1]
folder_output = sys.argv[2]
msa_length = int(sys.argv[3])

msa_len = msa_length + 1
for i in listdir(folder_input):
    if 'DS' not in i:
        df = pd.read_csv(folder_input+i, index_col=0, header=None, skiprows=1)
        matrix = df.to_numpy()
        #print(matrix.shape)
        column_name = ['V'+str(i) for i in list(df.columns)]
        #column_header = (',').join(column_name)
        index_row = [i for i in range(1,msa_len)]
        #print(index_row)
        #new_matrix = matrix[:, np.random.permutation(matrix.shape[1])]  # Permutate the columns
        new_matrix = np.take(matrix,np.random.permutation(matrix.shape[0]),axis=0);
        name_output = folder_output+i[:-4]+'-shuffle.csv' # Rename with shuffle at the end
        #np.savetxt(name_output, new_matrix, header=column_header, delimiter=",")
        out_df = pd.DataFrame(data=new_matrix, index=index_row, columns=column_name)
        out_df.to_csv(name_output)


