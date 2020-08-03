library(phytools)

buildCoev<-function(s,d,r,nStates=4,withinProfile=FALSE) {
    
    #s/d decrease if coevolution becomes stronger cf Dib et al 2014
    
    dim=nStates*nStates
    if(nStates == 4) {
        states<-c("A","C","G","T")
    }
    else if(nStates == 20) {
        states<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
    }
    else {
        states<-as.character(1:nStates)
    }
    
    dim.names<-paste(rep(states, each=nStates), rep(states, nStates), sep="")

    #set frequencies from a Dirichlet. The two states with highest frequencies are the profile
    f<-rgamma(dim, 1, 1)
    f<-f/sum(f)
    profile<-dim.names[order(f, decreasing=TRUE)[1:2]]
    while(adist(profile[1],profile[2])<2) {
        f<-rgamma(dim, 1, 1)
        f<-f/sum(f)
        profile<-dim.names[order(f, decreasing=TRUE)[1:2]]
    }
    
    freq<-matrix(0, dim, dim)
    rownames(freq)<-colnames(freq)<-dim.names
    
    diag(freq)<-f
    #diag(freq)<-rep(f.nonprofile, dim) #assume here equal frequencies. Can be changed to bias the starting values.
    #diag(freq)[pos.profile]<-f.profile
    
    
    #set up the Q matrix and use the dim.names as row and column names
    R<-matrix(r, dim, dim)
    rownames(R)<-colnames(R)<-dim.names

    #get the position of the states in the profile and outside the profile
    pos.profile<-match(profile, dim.names)
    pos.nonprofile<-which(is.na(match(dim.names,profile))==T)
    R[pos.profile,pos.nonprofile]<-s #rate of change from coevol to non-coevol
    R[pos.nonprofile,pos.profile]<-d #rate of change from non-coevol to coevol

    #finally, check if states differ by 2 states based on their names. 
    #  If yes, set the rate to 0
    for(i in 1:(length(dim.names)-1)) {
        for(j in (i+1):length(dim.names)) {
            x<-adist(dim.names[i], dim.names[j])
            if(x==2) {
                R[i,j]<-0
                R[j,i]<-0
            }
        }
    }

    if(withinProfile) {
        R[pos.profile,pos.profile]<-max(c(s,d)) #if you're in the profile, you don't change
    }
    
    #set the diagonal to 0
    diag(R)<-0
    #and then calculate the rowSums for the total amount of change.
    
    Q<-(R %*% freq) - diag(apply(R %*% freq, 1, sum))
    scaleQ <- sum(apply(freq %*% R %*% freq, 1, sum))

    return(list(Q=Q/scaleQ, scaleQ=scaleQ, dim.names=dim.names, profile=profile))
}

buildLG<-function(r=1, nStates=20) {
    
    dim<-nStates
    
    if(nStates == 4) {
        states<-c("A","C","G","T")
    }
    else if(nStates == 20) {
        states<-c("A","R","N","D","C","Q","E","G","H","I","L","K","M","F","P","S","T","W","Y","V")
    }
    else {
        states<-as.character(1:nStates)
    }
    
    f<-rgamma(dim, 100, 1) #almost equal frequency between states
    f<-f/sum(f)
    
    freq<-matrix(0, dim, dim)
    rownames(freq)<-colnames(freq)<-states
    
    diag(freq)<-f
    
    R<-matrix(r, dim, dim)
    rownames(R)<-colnames(R)<-states
    
    diag(R)<-0
    #and then calculate the rowSums for the total amount of change.
    
    Q<-(R %*% freq) - diag(apply(R %*% freq, 1, sum))
    scaleQ <- sum(apply(freq %*% R %*% freq, 1, sum))

    return(list(Q=Q/scaleQ, scaleQ=scaleQ, dim.names=states))
}

buildLG()


#s=0.1,d=1000,r=10 -> constant with profile
simulateCoev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, nCoevol=25, nNonCoevol=500, nsp=100, meanBL=100, target=5) {
    tree<-pbtree(n=nsp, scale=1)
    tree$edge.length<-rexp(length(tree$edge.length), meanBL)
    
    sum.bl<-sum(tree$edge.length)
    tree$edge.length<-tree$edge.length*target/sum.bl
    
    logResults<-list()
    sequences<-rep("", nsp)
    treeCoev<-tree
    treeCoev$edge.length<-tree$edge.length * 2
    cat("Simulating coevolving positions: ")
    for(i in 1:nCoevol) {
        cat(".")
        model<-buildCoev(s=s,d=d,r=r,nStates=nStates,withinProfile=withinProfile)
        logResults[[i]]<-model$profile
        tt<-sim.history(treeCoev,model$Q,anc=sample(model$profile, 1),message=F)
        sequences<-paste(sequences, tt$states, sep="")
    }
    cat("\n\nSimulating non-coevolving positions: ")
    
    for(i in 1:nNonCoevol) {
        cat(".")
        model<-buildLG(r=r, nStates=nStates)
        tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
        sequences<-paste(sequences, tt$states, sep="")
    }
    cat("\n\nDone.\n")
    names(sequences)<-names(tt$states)
    return(list(sequences=sequences, coevolProfiles=logResults, treeDepth=sum(tree$edge.length), tree=tree))
}

simulate_No_Coev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, nNonCoevol=500, nsp=100, meanBL=100, target=5) {
  tree<-pbtree(n=nsp, scale=1)
  tree$edge.length<-rexp(length(tree$edge.length), meanBL)
  
  sum.bl<-sum(tree$edge.length)
  tree$edge.length<-tree$edge.length*target/sum.bl
  
  logResults<-list()
  sequences<-rep("", nsp)
  cat("\n\nSimulating non-coevolving positions: ")
  
  for(i in 1:nNonCoevol) {
    cat(".")
    model<-buildLG(r=r, nStates=nStates)
    tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
    sequences<-paste(sequences, tt$states, sep="")
  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, treeDepth=sum(tree$edge.length), tree=tree))
}

simulate_all_data <- function(output_data_coev= output_path_coev, output_data_no_coev= output_path_no_coev, sites = 20, species = 40, tree_size = 15, num_simulation = 10){
    path_data_coev <- output_data_coev
    path_data_no_coev <- output_data_no_coev
    MSA_SITES <- sites
    MSA_SPEC <- species
    TREE_SIZE <- tree_size

    MSA_NUM <- num_simulation

    ### COEV
    
    for (i in 1:MSA_NUM){
        num_coev = round(rnorm(1, mean=4, sd=1))
        while (num_coev == 0) {
          num_coev = round(rnorm(1, mean=4, sd=1))
        }
        
        #x<-simulateCoev(s=1, d=20, r=1, nsp=40, nCoevol=num_coev, nNonCoev=20-(num_coev*2), meanBL=10, target=15)
        x<-simulateCoev(s=1, d=20, r=1, nsp=MSA_SPEC, nCoevol=num_coev, nNonCoev=MSA_SITES-(num_coev*2), meanBL=10, target=TREE_SIZE)
        name_fasta <- paste('fasta_', i,'_coev_', num_coev,".txt", sep="")
        name_tree <- paste('tree_', i,'_coev_', num_coev,".txt", sep="")
        write.FASTA(as.AAbin(as.matrix(x$sequences)), file=paste(path_data_coev, name_fasta, sep=""))
        write.tree(x$tree, file=paste(path_data_coev, name_tree, sep=""))
        cat(x[["treeDepth"]])
    }
    
    
    ### NO COEV
    for (i in 1:MSA_NUM){

      x<-simulate_No_Coev(s=1, d=20, r=1, nsp=MSA_SPEC, nNonCoev=MSA_SITES, meanBL=10, target=TREE_SIZE)
      name_fasta <- paste('fasta_', i,'_no_coev.txt', sep="")
      name_tree <- paste('tree_', i,'_no_coev.txt', sep="")
      write.FASTA(as.AAbin(as.matrix(x$sequences)), file=paste(path_data_no_coev, name_fasta, sep=""))
      write.tree(x$tree, file=paste(path_data_no_coev, name_tree, sep=""))
      cat(x[["treeDepth"]])
    }
}
