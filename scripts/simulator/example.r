source('./simulator_functions.r')

## Example of stationary freq
# Defining d/s or stationary frequency
d=7.5
s=1
freq <- compute_coev_pair_stationaryFreq(s=s, d=d, nStates=4)
cat(paste("Assuming : d = ", d, " | s = ", s, "\n", sep=""))
cat(paste("Stationnary frequency for 1 pair: ", freq, "\n", sep=""))
cat("-------------\n")

freq = 0.3
ds <- compute_coev_pair_dsRatio(stationnaryFreq=freq, nStates=4)
cat(paste("Assuming : stationaryFreq = ", 0.3,"\n", sep=""))
cat(paste("DS ratio =  ", ds, "\n", sep=""))
cat("-------------\n")


## Example of simulation
set.seed(20)
runName <- "MY_RUN"
mainFolder <- "./Simulations"
dir.create(file.path(mainFolder, runName), recursive = TRUE, showWarnings = FALSE)
runFolder <- paste(mainFolder, runName, sep="/")

nReplicas = 2

# Define the average branch length
# With this approach the tree length will on average be "treeLength[x]"
nSpecies = 100
treeLength = c(1,2)
nBranches = nSpecies*2-3
meanBL = (1./nBranches)*treeLength

for(i in 1:nReplicas) {
  repName <- paste('SIM_AA_', i, sep="")
  dir.create(file.path(runFolder, repName), recursive = TRUE, showWarnings = FALSE)
  repFolder <- paste(runFolder, repName, sep="/")

  for(iBL in 1:length(meanBL)){
    scaleName <- paste('BL_', iBL, sep="")
    dir.create(file.path(repFolder, scaleName), recursive = TRUE, showWarnings = FALSE)
    folder <- paste(repFolder, scaleName, sep="/")

    dir.create(file.path(folder, "figures"), recursive = TRUE, showWarnings = FALSE)
    figFolder <- paste(folder, "figures", sep="/")

    x<-simulateCoev(s=1, d=100, r=5, nsp=100, nCoevol=10, nNonCoev=80, meanBL=meanBL[iBL], figFolder=figFolder, gammaRate = 2.)

    name_fasta <- paste(folder, "fasta.txt", sep="/")
    name_tree <- paste(folder, "tree.txt", sep="/")
    name_substCnt <- paste(folder, "countSubst.txt", sep="/")
    write.FASTA(as.AAbin(as.matrix(x$sequences)), file=name_fasta)
    write.tree(x$tree, file=name_tree)
    lapply(x$substCountCoev, write, name_substCnt, append=TRUE, ncolumns=1000)
  }
}
