source('/Users/rramabal/Documents/PhD/Coevolution/CNN_project/scripts/simulate_functions.r')

output_path <- '/Users/rramabal/Documents/PhD/Coevolution/Aminoacides/Simulated/CNN_project/'
output_path_coev <- paste(output_path,'DataSet_10/rawData/COEV/',sep="")
output_path_no_coev <- paste(output_path,'DataSet_10/rawData/NO_COEV/',sep="")


simulate_all_data(output_path_coev, output_path_no_coev, 20, 40, 15, 2)
