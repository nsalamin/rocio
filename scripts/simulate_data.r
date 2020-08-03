source('/Users/rramabal/Documents/PhD/Coevolution/CNN_project/scripts/simulate_functions.r')

args <- commandArgs(trailingOnly = TRUE)
output_path_coev <- args[1]
output_path_no_coev <- args[2]
msa_length <- strtoi(args[3])
num_species <- strtoi(args[4])
tree_length <- strtoi(args[5])
num_simulations <- strtoi(args[6])

simulate_all_data(output_path_coev, output_path_no_coev, msa_length, num_species, tree_length, num_simulations)
