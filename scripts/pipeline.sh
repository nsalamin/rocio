#!/bin/bash

path_coev='/Users/rramabal/Documents/PhD/Coevolution/Aminoacides/Simulated/CNN_project/DataSet_10/rawData/COEV/'
path_no_coev='/Users/rramabal/Documents/PhD/Coevolution/Aminoacides/Simulated/CNN_project/DataSet_10/rawData/NO_COEV/'
path_matrix_coev='/Users/rramabal/Documents/PhD/Coevolution/Aminoacides/Simulated/CNN_project/DataSet_10/matrixData/COEV/'
path_matrix_shuffle='/Users/rramabal/Documents/PhD/Coevolution/Aminoacides/Simulated/CNN_project/DataSet_10/matrixData/COEV_SHUFFLE/'
msa_length=20
num_species=40
tree_size=1
msa_num=500

Rscript simulate_data.r $path_coev $path_no_coev $msa_length $num_species $tree_size $msa_num 
Rscript create_ancestral_changes.r $path_coev
Rscript create_ancestral_changes.r $path_no_coev

python3 shuffle_matrix.py $path_matrix_coev $path_matrix_shuffle $msa_length

