source('/Users/rramabal/Documents/PhD/Coevolution/CNN_project/scripts/matrix_ancestral_changes.r')

args <- commandArgs(trailingOnly = TRUE)
folder_msa <- args[1]

mapping_list <- get_mapping(20);
length(mapping_list)

for ( i in list.files(folder_msa)){
    if(grepl('tree', i)){
        print(i)
        name_tree <- paste(folder_msa, i, sep="")
        aux_name <- unlist(strsplit(i,"tree"))[[1]]
        name_fasta <- gsub("tree", "fasta", name_tree)
        print(name_tree)
        print(name_fasta)
        print(aux_name)
        
        tree_coev <- read.tree(name_tree)
        #plot(tree_coev)
        align <- read.phyDat(name_fasta, format="fasta", type="AA")
        
        fit_coev <- pml(tree_coev, align,optQ=T,optEdge=F,optRate=T)
        anc.ml <- ancestral.pml(fit_coev, type = "ml") #, site.pattern=F)
        #print(anc.ml)
        #plotAnc(tree_coev, anc.ml, i=10, site.pattern=F)
        
        vec <-vector()
        count <- 0
        for(z in 1:nrow(tree_coev$edge)){

          ancestral_matrix <- anc.ml[[tree_coev$edge[z,1]]][attr(anc.ml,"index"),] #convert_to_matrix(anc.ml[tree_coev$edge[z,1]]);
          orig_matrix <- anc.ml[[tree_coev$edge[z,2]]][attr(anc.ml,"index"),] #convert_to_matrix(anc.ml[tree_coev$edge[z,2]]);
          vec <- cbind(vec, get_intermediate_values_matrix(orig_matrix, ancestral_matrix, mapping_list));
        }

        name_changes <- gsub("tree", "result_changes", name_tree)
        name_changes <- gsub("txt", "csv", name_changes)
        name_changes <- gsub("rawData", "matrixData", name_changes)
        #name_changes <- paste(name_changes, 'result_changes_NO_coev.csv',sep = '')
        print(name_changes)
        write.csv(vec, file = name_changes)
        #break
    }
}
