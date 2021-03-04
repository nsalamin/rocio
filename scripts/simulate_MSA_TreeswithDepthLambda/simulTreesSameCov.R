simulate.bd.tree<-function(ntips){
  simtree<-sim.bdtree(b=1,d=0,stop="taxa",n=ntips)
  
}

library(ape)
library(geiger)
#setwd("/Users/eneveu/Documents/POSTDOC-UNIL/CoevolutionProject/PottsModelAnalysis/SimulationTrees/RandTrees")


###### script to simulate nSample topologies with ntips tips/species and different scaling/lambda using classic birth death model 

### nb of topologies to simulate
nSample=10

outputtree.file="randtree"

for (s in seq(1,nSample)){
## for each topology simulate different lambdas: lambda is the parameter of the rescale function of the geiger library
  ## lambda =0  star tree lambda =1 original tree
  
  
  ##call simulation trees  
  #ntips is the number of tips/species (sequences of alignment) of the tree
  simtree<-simulate.bd.tree(ntips=500)

  ##rescale tree for lambdas
  lambdas<-c(0.0,0.3,0.6,0.8,1.0)
  rescaled.trees <-lapply( lambdas, function(x) rescaled.tree<-geiger::rescale(simtree,"lambda",x))
  layout(matrix(1:4, 2, byrow = TRUE))
  for (i in seq(1,4)){
    plot(rescaled.trees[[i]], show.tip.label = FALSE)
  }
  outputtree.files<- paste(outputtree.file,"_",s,"_",lambdas,"_.tree",sep="")
  lapply(seq(1,length(lambdas)),function(x) write.tree(rescaled.trees[[x]],file=outputtree.files[x]))
  
}

