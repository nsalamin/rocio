library(phytools)
library(RColorBrewer)
library(pals)
library(MASS)
library(geiger)

source('./CAT/sim_cat_functions.r')

buildCoev<-function(s,d,r,nStates=4,withinProfile=FALSE, uniformFreq=FALSE) {

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

  if(uniformFreq) {
    f<-rep(1/dim, dim)
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

plotHistory<-function(figName, startState, history, model) {

  mypalette <- brewer.set1(4)

  nullspaceQ <- Null(model$Q)
  nrm_NSQ <- nullspaceQ/sum(nullspaceQ)
  nrm_NSQ <- setNames(nrm_NSQ, model$dim.names)

  orderedPairs <- model$dim.names[order(nrm_NSQ, decreasing=TRUE)]

  cols <- setNames(rep("", length(model$dim.names)),orderedPairs)
  cols[1] <- mypalette[1]
  cols[2] <- mypalette[2]
  cols[-1:-2] <- mypalette[3]
  if(startState != orderedPairs[1] && startState != orderedPairs[2]) {
    cols[startState] <- mypalette[4]
    colorTypes <- c(orderedPairs[1], orderedPairs[2], "others", paste("Start:", startState))
    colorLeg <- setNames(mypalette, colorTypes)
  } else {
    colorTypes <- c(orderedPairs[1], orderedPairs[2], "others")
    colorLeg <- setNames(mypalette[1:3], colorTypes)
  }

  pdf(figName)

  myLayout <- t(c(1,2))
  layout(myLayout, c(0.85,0.15))

  plotSimmap(history, pts=TRUE, node.numbers=FALSE, colors=cols)

  myTitle <- sprintf("\n%s=%.1f - %s=%.1f", orderedPairs[1], nrm_NSQ[orderedPairs[1]]*100.,  orderedPairs[2], nrm_NSQ[orderedPairs[2]]*100.)
  title(main=myTitle, outer=FALSE)

  plot.new()
  legend("center", title="States", legend=colorTypes, pch=15, col=colorLeg)

  dev.off()

}

simulateCoev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, uniformFreq=FALSE,
                       nCoevol=25, nNonCoevol=500, nsp=100, meanBL=100, gammaRate=NULL,
                       figFolder=NULL, indepModelFunc="LG", treefile="") {
  
  if(treefile==""){
    tree<-pbtree(n=nsp, scale=1)
    tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)
    
  }else{
    tree<-read.tree(treefile)
    nsp=length(tree$tip.label)
  }
  

  cat("Total BL: ")
  cat(sum(tree$edge.length))
  cat("\n")
  rateScaler<-1.

  logResults<-list()
  substCountCoev<-list()
  sequences<-rep("", nsp)
  treeCoev<-tree
  treeCoev$edge.length<-tree$edge.length * 2
  cat("Simulating coevolving positions: ")
  for(i in 1:nCoevol) {
    cat(".")
    if(!is.null(gammaRate)) {
      rateScaler <- rgamma(1, gammaRate, gammaRate)
      treeCoev$edge.length <- treeCoev$edge.length * rateScaler
    }
    model<-buildCoev(s=s,d=d,r=r,nStates=nStates,withinProfile=withinProfile, uniformFreq=uniformFreq)
    logResults[[i]]<-model$profile

    startState<-sample(model$profile, 1)

    tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

    nProf1 <- sum(tt$states == model$profile[1])
    nProf2 <- sum(tt$states == model$profile[2])

    while(nProf1 < 3 || nProf2 < 3 || nProf1+nProf2 < length(tt$states)/2.) {

      nSubst <- sum(sapply(tt$maps,length)) - length(treeCoev$edge.length)
      cat(paste("Rejecting a simulation: nSubstitutionTotal= ", nSubst, " -- meanBL = ", meanBL, "\n", sep=""))
      tt<-NULL
      tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

      nProf1 <- sum(tt$states == model$profile[1])
      nProf2 <- sum(tt$states == model$profile[2])
    }

    sequences<-paste(sequences, tt$states, sep="")
    nSubst <- sum(sapply(tt$maps,length)) - length(treeCoev$edge.length)
    substCountCoev[[i]] <- nSubst
    if(!is.null(gammaRate)) {
      treeCoev$edge.length <- treeCoev$edge.length / rateScaler
    }

    if(!is.null(figFolder)) {
      #show(sum(sapply(tt$maps,length)) - length(treeCoev$edge.length))
      figName <- sprintf("%s/coev_%d.pdf", figFolder, i)
      plotHistory(figName, startState, tt, model)
    }
    tt<-NULL

  }
  cat("\n\nSimulating non-coevolving positions: ")

  if(indepModelFunc == "LG") {
    model<-buildLG(r=r, nStates=nStates)
    for(i in 1:nNonCoevol) {
      cat(".")
      if(!is.null(gammaRate)) {
        rateScaler <- rgamma(1, gammaRate, gammaRate)
        tree$edge.length <- tree$edge.length * rateScaler
      }
      tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
      sequences<-paste(sequences, tt$states, sep="")
      if(!is.null(gammaRate)) {
        tree$edge.length <- tree$edge.length / rateScaler
      }
    }
  } else if(indepModelFunc == "CAT") {
    mixtureCAT<-buildCAT(r=r, nStates=nStates, alpha0DirichletMixtureCAT=10)
    for(i in 1:nNonCoevol) {
      cat(".")

      model <- sample(mixtureCAT$models, size=1, prob=mixtureCAT$profFreq)[[1]]

      if(!is.null(gammaRate)) {
        rateScaler <- rgamma(1, gammaRate, gammaRate)
        tree$edge.length <- tree$edge.length * rateScaler
      }

      startState <- sample(model$dim.names, size=1, prob=model$profileCAT)
      tt<-sim.history(tree, model$Q, anc=startState,message=F)
      sequences<-paste(sequences, tt$states, sep="")
      if(!is.null(gammaRate)) {
        tree$edge.length <- tree$edge.length / rateScaler
      }

      if(!is.null(figFolder)) {
        figName <- sprintf("%s/CAT_%d.pdf", figFolder, i)
        plotHistory(figName, startState, tt, model)
      }
    }
  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, substCountCoev=substCountCoev,
              treeDepth=sum(tree$edge.length), tree=tree))
}


simulateOnlyCoev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, uniformFreq=FALSE,
                           nCoevol=25, nsp=100, meanBL=100, gammaRate=NULL, figFolder=NULL, treefile="") {
  
  
  if(treefile==""){
    tree<-pbtree(n=nsp, scale=1)
    tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)
    
  }else{
    tree<-read.tree(treefile)
    nsp=length(tree$tip.label)
  }
  
  rateScaler<-1.

  logResults<-list()
  sequences<-rep("", nsp)
  substCountCoev<-list()
  treeCoev<-tree
  treeCoev$edge.length<-tree$edge.length * 2
  cat("Simulating coevolving positions: ")
  for(i in 1:nCoevol) {
    cat(".")
    if(!is.null(gammaRate)) {
      rateScaler <- rgamma(1, gammaRate, gammaRate)
      treeCoev$edge.length <- treeCoev$edge.length * rateScaler
    }
    model<-buildCoev(s=s,d=d,r=r,nStates=nStates,withinProfile=withinProfile, uniformFreq=uniformFreq)
    logResults[[i]]<-model$profile

    startState<-sample(model$profile, 1)
    #nullspaceQ <- Null(model$Q)
    #nrm_NSQ <- nullspaceQ/sum(nullspaceQ)
    #nrm_NSQ <- setNames(nrm_NSQ, model$dim.names)
    #startState<-sample(model$dim.names, 1, prob=nrm_NSQ)

    tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

    nProf1 <- sum(tt$states == model$profile[1])
    nProf2 <- sum(tt$states == model$profile[2])

    while(nProf1 < 4 && nProf2 < 4) {
      tt<-NULL
      tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

      nProf1 <- sum(tt$states == model$profile[1])
      nProf2 <- sum(tt$states == model$profile[2])
    }

    sequences<-paste(sequences, tt$states, sep="")
    nSubst <- sum(sapply(tt$maps,length)) - length(treeCoev$edge.length)
    substCountCoev[[i]] <- nSubst
    if(!is.null(gammaRate)) {
      treeCoev$edge.length <- treeCoev$edge.length / rateScaler
    }

    if(!is.null(figFolder)) {
      figName <- sprintf("%s/coev_%d.pdf", figFolder, i)
      plotHistory(figName, startState, tt, model)
    }

  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, substCountCoev=substCountCoev, treeDepth=sum(tree$edge.length), tree=tree))
}


simulate_No_Coev<-function(s=1, d=10, r=1, nStates=20, nNonCoevol=500, nsp=100, meanBL=100,
                           gammaRate=NULL, figFolder=NULL, indepModel="LG", treefile="") {

  if(treefile==""){
    tree<-pbtree(n=nsp, scale=1)
    tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)
    
  }else{
    tree<-read.tree(treefile)
    nsp=length(tree$tip.label)
  }

  rateScaler<-1.
  logResults<-list()
  sequences<-rep("", nsp)
  cat("\n\nSimulating non-coevolving positions: ")
  if(indepModelFunc == "LG") {
    model<-buildLG(r=r, nStates=nStates)
    for(i in 1:nNonCoevol) {
      cat(".")
      if(!is.null(gammaRate)) {
        rateScaler <- rgamma(1, gammaRate, gammaRate)
        tree$edge.length <- tree$edge.length * rateScaler
      }
      tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
      sequences<-paste(sequences, tt$states, sep="")
      if(!is.null(gammaRate)) {
        tree$edge.length <- tree$edge.length / rateScaler
      }
    }
  } else if(indepModelFunc == "CAT") {
    mixtureCAT<-buildCAT(r=r, nStates=nStates, alpha0DirichletMixtureCAT=10)

    for(i in 1:nNonCoevol) {
      cat(".")

      model <- sample(mixtureCAT$models, size=1, prob=mixtureCAT$profFreq)[[1]]

      if(!is.null(gammaRate)) {
        rateScaler <- rgamma(1, gammaRate, gammaRate)
        tree$edge.length <- tree$edge.length * rateScaler
      }
      startState <- sample(model$dim.names, size=1, prob=model$profileCAT)
      tt<-sim.history(tree, model$Q, anc=startState,message=F)
      sequences<-paste(sequences, tt$states, sep="")
      if(!is.null(gammaRate)) {
        tree$edge.length <- tree$edge.length / rateScaler
      }

      if(!is.null(figFolder)) {
        figName <- sprintf("%s/CAT_%d.pdf", figFolder, i)
        plotHistory(figName, startState, tt, model)
      }
    }
  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, treeDepth=sum(tree$edge.length), tree=tree))
}


simulateMixture<-function(s=1, d=100, r=5, nsp=100, meanBL=meanBL[iBL], figFolder=NULL, modelsSettings=modelsSettings,
                   withinProfile=TRUE, uniformFreq=FALSE, deltaTreeModif=1.0,treefile="") {

  nStates=20
  
  if(treefile==""){
    tree<-pbtree(n=nsp, scale=1)
    tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)
  }else{
    tree<-read.tree(treefile)
    nsp=length(tree$tip.label)
    
  }

  

  if(deltaTreeModif != 1.0) {
    tree<-geiger::rescale(tree,"delta",deltaTreeModif)
    tree$edge.length<-(length(tree$edge.length)*meanBL)*(tree$edge.length/sum(tree$edge.length))
  }
  cat("Total BL: ")
  cat(sum(tree$edge.length))
  cat("\n")
  rateScaler<-1.

  logResults<-list()
  substCountCoev<-list()
  sequences<-rep("", nsp)
  
  stopifnot(any(row.names(modelsSettings) == "coev"))
  if(modelsSettings[["coev", "nSites"]] > 0) {
    treeCoev<-tree
    treeCoev$edge.length<-tree$edge.length * 2
    nCoevol=modelsSettings[["coev", "nSites"]]
    alpha <- modelsSettings[["coev", "alphas"]]
    mutationsStatesCoev<-matrix (nrow=nCoevol*2,ncol=length(tree$edge.length))
    
    cat("Simulating coevolving positions: ")
    for(i in 1:nCoevol) {
      cat(".")
      if(!is.null(alpha)) {
        rateScaler <- rgamma(1, alpha, alpha)
        treeCoev$edge.length <- treeCoev$edge.length * rateScaler
      }
      model<-buildCoev(s=s,d=d,r=r,nStates=nStates,withinProfile=withinProfile, uniformFreq=uniformFreq)
      logResults[[i]]<-model$profile

      startState<-sample(model$profile, 1)

      tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

      nProf1 <- sum(tt$states == model$profile[1])
      nProf2 <- sum(tt$states == model$profile[2])

      while(nProf1 < 3 || nProf2 < 3 || nProf1+nProf2 < length(tt$states)/2.) {

        nSubst <- sum(sapply(tt$maps,length)) - length(treeCoev$edge.length)
        #cat(paste("Rejecting a simulation: nSubstitutionTotal= ", nSubst, " -- meanBL = ", meanBL, "\n", sep=""))
        cat('r')
        tt<-NULL
        tt<-sim.history(treeCoev,model$Q,anc=startState,message=F)

        nProf1 <- sum(tt$states == model$profile[1])
        nProf2 <- sum(tt$states == model$profile[2])
      }

      sequences<-paste(sequences, tt$states, sep="")
      nSubst <- sum(sapply(tt$maps,length)) - length(treeCoev$edge.length)
      substCountCoev[[i]] <- nSubst
      
      nodeStates<-t(matrix(unlist(apply(tt$node.states,1,function(x) strsplit(x,split=""))),ncol=nrow(tt$node.states)))
      
      mutationsStatesCoev[(i-1)*2+1,]<-nodeStates[,1]!=nodeStates[,3]
      mutationsStatesCoev[(i-1)*2+2,]<-nodeStates[,2]!=nodeStates[,4]
      
      
      
      if(!is.null(alpha)) {
        treeCoev$edge.length <- treeCoev$edge.length / rateScaler
      }

      if(!is.null(figFolder)) {
        #show(sum(sapply(tt$maps,length)) - length(treeCoev$edge.length))
        figName <- sprintf("%s/coev_%d.pdf", figFolder, i)
        plotHistory(figName, startState, tt, model)
      }
      tt<-NULL
    }
  }


  cat("\n\nSimulating non-coevolving positions: ")
  nSites = 0
  substCountIndep<-list()
  indepKeys <- row.names(modelsSettings)
  indepKeys = indepKeys[indepKeys != "coev"]
  for(key in indepKeys) {
    alpha <- modelsSettings[[key, "alphas"]]
    nNonCoevol=modelsSettings[[key, "nSites"]]
    
    if(key == "LG") {
      model<-buildLG(r=r, nStates=nStates)
      mutationsStatesLG<-matrix (nrow=nNonCoevol,ncol=length(tree$edge.length))
      
      for(i in 1:nNonCoevol) {
        cat(".")
        nSites = nSites + 1 
        if(!is.null(alpha)) {
          rateScaler <- rgamma(1, alpha, alpha)
          tree$edge.length <- tree$edge.length * rateScaler
        }
        tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
        
        nSubst <- sum(sapply(tt$maps,length)) - length(tree$edge.length)
        substCountIndep[[nSites]] <- nSubst
        
        mutationsStatesLG[i,]<-tt$node.states[,1]!=tt$node.states[,2]
        
        sequences<-paste(sequences, tt$states, sep="")
        if(!is.null(alpha)) {
          tree$edge.length <- tree$edge.length / rateScaler
        }
      }
      
    } else if(key == "CAT") {
      mixtureCAT<-buildCAT(r=r, nStates=nStates, alpha0DirichletMixtureCAT=10)  #original value 10
      mutationsStatesCAT<-matrix (nrow=nNonCoevol,ncol=length(tree$edge.length))
      
      for(i in 1:nNonCoevol) {
        cat(".")
        nSites = nSites + 1 

        model <- sample(mixtureCAT$models, size=1, prob=mixtureCAT$profFreq)[[1]]

        if(!is.null(alpha)) {
          rateScaler <- rgamma(1, alpha, alpha)
          tree$edge.length <- tree$edge.length * rateScaler
        }

        startState <- sample(model$dim.names, size=1, prob=model$profileCAT)

        tt<-sim.history(tree, model$Q, anc=startState,message=F)
        
        nSubst <- sum(sapply(tt$maps,length)) - length(tree$edge.length)
        substCountIndep[[nSites]] <- nSubst
        mutationsStatesCAT[i,]<-tt$node.states[,1]!=tt$node.states[,2]
        
        sequences<-paste(sequences, tt$states, sep="")
        if(!is.null(alpha)) {
          tree$edge.length <- tree$edge.length / rateScaler
        }
        
        if(!is.null(figFolder)) {
          figName <- sprintf("%s/CAT_%d.pdf", figFolder, i)
          plotHistory(figName, startState, tt, model)
        }
      }
      
    } else {
      stopifnot(FALSE)
    }
  }
   fullmutMat<-rbind(mutationsStatesCoev,rbind(mutationsStatesLG,mutationsStatesCAT))
  cat("\n\nDone.\n")
  
  names(sequences)<-names(tt$states)
  
  return(list(sequences=sequences, coevolProfiles=logResults, substCountCoev=substCountCoev,
              substCountIndep=substCountIndep, mutationStates=fullmutMat,treeDepth=sum(tree$edge.length), tree=tree))
}




# Only valid when stationary frequencies are uniform
compute_coev_pair_stationaryFreq<-function(s=1, d=10, nStates=20, nPairInProfile=2) {
  nPairsOfStates = nStates * (nStates-1)
  sd = s/d
  denom = nPairInProfile + (nPairsOfStates-nPairInProfile)*sd
  return(1./denom)
}

# Only valid when stationary frequencies are uniform
compute_coev_pair_dsRatio<-function(stationnaryFreq=0.2, nStates=20, nPairInProfile=2) {
  nPairsOfStates = nStates * (nStates-1)

  num = (nPairsOfStates-nPairInProfile)
  denom = (1./stationnaryFreq)-nPairInProfile

  return(num/denom)
}
