source('../simulator/simulator_functions.r')

#### from a tree file simulate alignment with some sites under Coev and others under LG/CAT independent model
#### input : the tree file 
### output : the fasta file, the number of substitutions and some figures showing the evolution of sites states

### need to have a folder RandTrees/ with the tree file and a folder FASTA/ for the outputs

setwd("/Users/eneveu/Documents/POSTDOC-UNIL/CoevolutionProject/PottsModelAnalysis/SimulationTrees/") ## to modify


args <- (commandArgs(trailingOnly = TRUE))
curname=args[1]  

curtreefile <- paste("RandTrees/",curname,sep="")
#curname="5randtree_1_0_.tree"
#curtreefile="/Users/eneveu/Documents/POSTDOC-UNIL/CoevolutionProject/PottsModelAnalysis/SimulationTrees/RandTrees/5randtree_1_0_.tree"


## Example of stationary freq
# Defining d/s or stationary frequency
d=100
s=1
freq <- compute_coev_pair_stationaryFreq(s=s, d=d, nStates=20)
cat(paste("Assuming : d = ", d, " | s = ", s, "\n", sep=""))
cat(paste("Stationnary frequency for 1 pair: ", freq, "\n", sep=""))
cat("-------------\n")



#### parameters of models coev and LG CAT
modelsName = c("coev","LG","CAT")
nSites = c(25,25,25) # 25 pairs coev d/s 100, 25 sites indep. - LG, 25 sites indep. CAT
alphas = c(1.0,1.0, 1.0) # More rate heterogeneity for indep sites (alpha === gammaRates)


modelsSettings = data.frame(row.names=modelsName, nSites=nSites, alphas=alphas)
show(modelsSettings)

## Example of simulation
set.seed(20)
runName <- "MY_RUN"
mainFolder <- "./FASTA/WithCoevMixture/"
dir.create(file.path(mainFolder, runName), recursive = TRUE, showWarnings = FALSE)
runFolder <- paste(mainFolder, runName, sep="/")

#nReplicas = 2

# Define the average branch length
# With this approach the tree length will on average be "treeLength[x]"

tree<-read.tree(curtreefile) ## tree from file
treeLength=node.depth.edgelength(tree)[1]
nSpecies<-length(tree$tip.label)
nBranches = nSpecies*2-3
meanBL = (1./nBranches)*treeLength


repName <- paste('SIM_AA_', curname, sep="")
dir.create(file.path(runFolder, repName), recursive = TRUE, showWarnings = FALSE)
folder <- paste(runFolder, repName, sep="/")


dir.create(file.path(folder, "figures"), recursive = TRUE, showWarnings = FALSE)
figFolder <- paste(folder, "figures", sep="/")


x <- simulateMixture(s=1, d=100, r=1, meanBL=meanBL, figFolder=figFolder, modelsSettings=modelsSettings,treefile=curtreefile )


name_fasta <- paste("FASTA/WithCoevMixture/",curname,".fasta",sep="")
write.FASTA(as.AAbin(as.matrix(x$sequences)), file=name_fasta)

name_substCnt <- paste("FASTA/WithCoevMixture/",curname, ".countCoevSubst.txt", sep="")
lapply(x$substCountCoev, write, name_substCnt, append=TRUE, ncolumns=1000)
name_substCnt <- paste("FASTA/WithCoevMixture/",curname, ".countIndepSubst.txt", sep="")
lapply(x$substCountIndep, write, name_substCnt, append=TRUE, ncolumns=1000)

