renorm<-function(tree){
    simtree<-read.tree(tree)
    Depth<-node.depth.edgelength(simtree)[1]
                    while(Depth >0.12 | Depth<0.08){
                       if(Depth >0.12 ) simtree$edge.length<- simtree$edge.length*5/6
                       if( Depth<0.08) simtree$edge.length<- simtree$edge.length*7/6
                    
                       Depth<-node.depth.edgelength(simtree)[1]
                     }
       return(simtree)
}
computeDepth<-function(tree){
      simtree<-read.tree(tree)
      Depth<-node.depth.edgelength(simtree)[1]
     
      return(Depth)
    }

library(ape)
#setwd("/Users/eneveu/Documents/POSTDOC-UNIL/CoevolutionProject/PottsModelAnalysis/SimulationTrees/RandTrees")

###### script to compute different depths from an original tree. 
##   the script goes through the nSample topologies and different lambdas and for each create a tree with approximately Depth as defined inside the renorm function
### 
###   Now I rescale the edge lengths iteratively until the depth of the tree is approximately a given Depth. could be improved.
### 


nSample=10 ### from simulTreesSameCov.R

inputtree.file="randtree" ### from simulTreesSameCov.R

outputtree.file="norm0.1randtree" ##to define
lambdas<-c(0.0,0.3,0.6,0.8,1.0)  ### from simulTreesSameCov.R

AllDepthRand=vector(length=nSample*length(lambdas))  ### original depths
AllDepthNorm=vector(length=nSample*length(lambdas)) ### chosen depths

for (s in seq(1,nSample)){

    simtree<-(paste(inputtree.file,"_",s,"_",lambdas,"_.tree",sep=""))
  
    Depths<-unlist(lapply(seq(1,length(lambdas)),function(x) computeDepth(simtree[x])))
    ## depths of the original trees
    AllDepthRand[((s-1)*length(lambdas)+1):(s*length(lambdas))]<-Depths
    
    ## modify the depths of the tree (need to modify Depth parameter inside renorm to fix the depth as wished)
    normalised.trees <-lapply(seq(1,length(lambdas)),function(x) renorm(simtree[x]))
    
    
    # check- compute the exact depths of the new trees
    Depths<-unlist(lapply(seq(1,length(lambdas)),function(x) computeDepth(simtree[x])))
    AllDepthNorm[((s-1)*length(lambdas)+1):(s*length(lambdas))]<-Depths
    
    #output new trees
    simtree<-(paste(outputtree.file,"_",s,"_",lambdas,"_.tree",sep=""))
    lapply(seq(1,length(lambdas)),function(x) write.tree(normalised.trees[[x]],file=simtree[x]))
    

}

