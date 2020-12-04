library(phytools)
library(RColorBrewer)
library(pals)
library(MASS)

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
#buildLG()
#s=0.1,d=1000,r=10 -> constant with profile

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

simulateCoev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, uniformFreq=FALSE, nCoevol=25, nNonCoevol=500, nsp=100, meanBL=100, gammaRate=NULL, figFolder=NULL) {
  tree<-pbtree(n=nsp, scale=1)
  tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)
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

  for(i in 1:nNonCoevol) {
    cat(".")
    if(!is.null(gammaRate)) {
      rateScaler <- rgamma(1, gammaRate, gammaRate)
      tree$edge.length <- tree$edge.length * rateScaler
    }
    model<-buildLG(r=r, nStates=nStates)
    tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
    sequences<-paste(sequences, tt$states, sep="")
    if(!is.null(gammaRate)) {
      tree$edge.length <- tree$edge.length / rateScaler
    }
  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, substCountCoev=substCountCoev, treeDepth=sum(tree$edge.length), tree=tree))
}


simulateOnlyCoev<-function(s=1, d=10, r=1, nStates=20, withinProfile=TRUE, uniformFreq=FALSE, nCoevol=25, nsp=100, meanBL=100, gammaRate=NULL, figFolder=NULL) {
  tree<-pbtree(n=nsp, scale=1)
  tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)

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


simulate_No_Coev<-function(s=1, d=10, r=1, nStates=20, nNonCoevol=500, nsp=100, meanBL=100, gammaRate=NULL, figFolder=NULL) {
  tree<-pbtree(n=nsp, scale=1)
  tree$edge.length<-rexp(length(tree$edge.length), 1./meanBL)

  rateScaler<-1.

  logResults<-list()
  sequences<-rep("", nsp)
  cat("\n\nSimulating non-coevolving positions: ")
  for(i in 1:nNonCoevol) {
    cat(".")
    if(!is.null(gammaRate)) {
      rateScaler <- rgamma(1, gammaRate, gammaRate)
      tree$edge.length <- tree$edge.length * rateScaler
    }
    model<-buildLG(r=r, nStates=nStates)
    tt<-sim.history(tree, model$Q, anc=sample(model$dim.names, 1),message=F)
    sequences<-paste(sequences, tt$states, sep="")
    if(!is.null(gammaRate)) {
      tree$edge.length <- tree$edge.length / rateScaler
    }
  }
  cat("\n\nDone.\n")
  names(sequences)<-names(tt$states)
  return(list(sequences=sequences, coevolProfiles=logResults, treeDepth=sum(tree$edge.length), tree=tree))
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
