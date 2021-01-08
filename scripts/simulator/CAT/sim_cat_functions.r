library(MASS)

C20_1 <-
list(profCat = structure(list(X = 1:20, A = c(2L, 2L, 6L, 5L, 14L,
10L, 26L, 1L, 4L, 10L, 17L, 42L, 17L, 1L, 9L, 4L, 4L, 6L, 7L,
6L), C = c(1L, 1L, 0L, 1L, 0L, 2L, 5L, 1L, 1L, 1L, 1L, 3L, 0L,
1L, 0L, 0L, 1L, 1L, 2L, 0L), D = c(0L, 0L, 30L, 1L, 4L, 3L, 0L,
0L, 1L, 4L, 4L, 1L, 14L, 0L, 6L, 24L, 4L, 1L, 1L, 11L), E = c(0L,
0L, 34L, 2L, 6L, 2L, 1L, 0L, 3L, 10L, 7L, 1L, 24L, 0L, 21L, 6L,
4L, 4L, 2L, 13L), F = c(1L, 4L, 0L, 2L, 0L, 0L, 1L, 36L, 0L,
1L, 0L, 0L, 0L, 27L, 0L, 0L, 4L, 3L, 15L, 0L), G = c(0L, 0L,
3L, 1L, 3L, 3L, 3L, 0L, 2L, 2L, 9L, 20L, 4L, 0L, 2L, 14L, 3L,
1L, 2L, 4L), H = c(0L, 0L, 2L, 1L, 1L, 1L, 0L, 4L, 3L, 2L, 2L,
0L, 1L, 1L, 2L, 3L, 21L, 3L, 7L, 4L), I = c(39L, 20L, 0L, 21L,
1L, 1L, 8L, 1L, 1L, 6L, 1L, 0L, 1L, 8L, 1L, 0L, 1L, 6L, 4L, 0L
), K = c(0L, 0L, 4L, 3L, 5L, 2L, 0L, 0L, 29L, 7L, 9L, 0L, 6L,
0L, 14L, 4L, 5L, 7L, 2L, 16L), L = c(12L, 45L, 1L, 13L, 2L, 1L,
6L, 3L, 3L, 5L, 2L, 1L, 1L, 36L, 3L, 0L, 4L, 21L, 8L, 1L), M = c(2L,
12L, 0L, 4L, 1L, 1L, 3L, 1L, 1L, 2L, 1L, 0L, 0L, 7L, 1L, 0L,
1L, 8L, 2L, 0L), N = c(0L, 0L, 5L, 1L, 2L, 7L, 1L, 1L, 3L, 4L,
5L, 1L, 3L, 0L, 3L, 25L, 11L, 2L, 2L, 12L), P = c(0L, 0L, 1L,
1L, 35L, 2L, 2L, 0L, 1L, 2L, 2L, 2L, 4L, 0L, 1L, 1L, 1L, 1L,
2L, 1L), Q = c(0L, 1L, 6L, 2L, 4L, 2L, 1L, 0L, 9L, 6L, 8L, 1L,
7L, 0L, 16L, 3L, 9L, 8L, 2L, 10L), R = c(0L, 0L, 1L, 3L, 3L,
2L, 1L, 0L, 29L, 6L, 9L, 1L, 3L, 0L, 9L, 2L, 8L, 8L, 3L, 8L),
S = c(0L, 0L, 3L, 3L, 9L, 33L, 7L, 1L, 3L, 7L, 14L, 20L,
8L, 1L, 4L, 9L, 6L, 3L, 5L, 8L), T = c(2L, 1L, 3L, 10L, 5L,
26L, 11L, 0L, 4L, 13L, 7L, 4L, 5L, 1L, 5L, 3L, 3L, 5L, 4L,
6L), V = c(39L, 11L, 1L, 26L, 3L, 2L, 24L, 1L, 2L, 11L, 2L,
3L, 2L, 6L, 3L, 0L, 2L, 6L, 6L, 1L), W = c(0L, 0L, 0L, 0L,
0L, 0L, 0L, 5L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 1L, 1L, 4L,
0L), Y = c(0L, 1L, 0L, 1L, 0L, 1L, 1L, 45L, 1L, 1L, 1L, 0L,
0L, 7L, 1L, 1L, 9L, 2L, 19L, 0L)), class = "data.frame", row.names = c(NA,
-20L)))

C50_1 <-
list(profCat = structure(list(X = 1:50, A = c(2L, 3L, 0L, 5L, 8L,
6L, 11L, 8L, 1L, 5L, 33L, 2L, 16L, 12L, 1L, 2L, 42L, 2L, 15L,
5L, 2L, 3L, 9L, 4L, 4L, 12L, 22L, 7L, 11L, 9L, 7L, 13L, 3L, 15L,
9L, 19L, 13L, 5L, 0L, 36L, 1L, 21L, 7L, 30L, 8L, 5L, 8L, 11L,
2L, 17L), C = c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 3L, 0L,
0L, 0L, 0L, 0L, 2L, 1L, 0L, 1L, 1L, 1L, 1L, 2L, 1L, 4L, 0L, 0L,
0L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 1L, 0L, 1L, 0L, 7L, 0L, 21L,
0L, 0L, 0L, 0L, 0L, 0L), D = c(0L, 1L, 0L, 30L, 2L, 17L, 1L,
11L, 0L, 17L, 0L, 36L, 13L, 14L, 0L, 13L, 1L, 0L, 3L, 1L, 1L,
1L, 2L, 0L, 1L, 1L, 11L, 5L, 8L, 6L, 1L, 4L, 3L, 3L, 3L, 2L,
0L, 2L, 0L, 0L, 0L, 0L, 6L, 0L, 3L, 5L, 28L, 13L, 0L, 7L), E = c(0L,
2L, 0L, 39L, 1L, 15L, 3L, 13L, 0L, 20L, 1L, 7L, 27L, 19L, 0L,
4L, 1L, 0L, 4L, 4L, 1L, 3L, 4L, 0L, 5L, 1L, 15L, 19L, 18L, 2L,
1L, 5L, 3L, 2L, 7L, 5L, 0L, 4L, 0L, 0L, 0L, 1L, 7L, 0L, 10L,
5L, 19L, 6L, 0L, 17L), F = c(1L, 0L, 41L, 0L, 0L, 3L, 2L, 0L,
33L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 6L, 0L, 1L, 17L, 8L, 2L,
18L, 1L, 15L, 0L, 0L, 1L, 1L, 1L, 5L, 2L, 0L, 1L, 0L, 3L, 8L,
8L, 0L, 11L, 2L, 0L, 1L, 0L, 0L, 0L, 0L, 2L, 0L), G = c(0L, 2L,
0L, 2L, 3L, 3L, 2L, 3L, 0L, 3L, 3L, 5L, 3L, 3L, 0L, 48L, 9L,
0L, 3L, 1L, 1L, 1L, 3L, 0L, 0L, 4L, 8L, 2L, 2L, 9L, 1L, 7L, 2L,
56L, 2L, 8L, 1L, 1L, 1L, 52L, 0L, 3L, 28L, 5L, 2L, 4L, 14L, 16L,
0L, 3L), H = c(0L, 3L, 2L, 1L, 1L, 7L, 1L, 2L, 0L, 3L, 0L, 3L,
1L, 1L, 0L, 2L, 0L, 0L, 1L, 3L, 17L, 9L, 2L, 2L, 2L, 3L, 2L,
3L, 2L, 4L, 1L, 9L, 29L, 1L, 2L, 2L, 0L, 8L, 1L, 0L, 0L, 1L,
3L, 0L, 2L, 6L, 2L, 1L, 0L, 1L), I = c(40L, 1L, 1L, 0L, 1L, 2L,
8L, 1L, 5L, 0L, 7L, 0L, 1L, 0L, 34L, 0L, 0L, 10L, 1L, 6L, 1L,
2L, 5L, 15L, 19L, 3L, 1L, 1L, 6L, 1L, 18L, 1L, 1L, 0L, 5L, 1L,
17L, 7L, 0L, 0L, 24L, 6L, 0L, 1L, 1L, 0L, 0L, 0L, 26L, 0L), K = c(0L,
33L, 0L, 3L, 2L, 4L, 2L, 8L, 0L, 13L, 0L, 3L, 6L, 7L, 0L, 5L,
0L, 1L, 4L, 18L, 1L, 10L, 5L, 1L, 6L, 1L, 4L, 14L, 5L, 3L, 1L,
3L, 4L, 1L, 9L, 11L, 0L, 4L, 0L, 0L, 0L, 1L, 12L, 0L, 22L, 18L,
2L, 2L, 0L, 15L), L = c(8L, 2L, 3L, 1L, 1L, 4L, 12L, 1L, 39L,
1L, 3L, 0L, 1L, 1L, 37L, 0L, 1L, 50L, 2L, 10L, 4L, 13L, 20L,
15L, 15L, 9L, 2L, 3L, 7L, 1L, 9L, 3L, 3L, 0L, 3L, 2L, 21L, 12L,
2L, 1L, 36L, 11L, 1L, 2L, 1L, 1L, 0L, 0L, 37L, 1L), M = c(2L,
1L, 1L, 0L, 1L, 1L, 3L, 1L, 6L, 0L, 1L, 0L, 0L, 0L, 3L, 0L, 0L,
19L, 0L, 4L, 1L, 4L, 10L, 4L, 4L, 4L, 1L, 1L, 2L, 1L, 4L, 1L,
1L, 0L, 2L, 1L, 4L, 3L, 0L, 0L, 4L, 5L, 0L, 1L, 0L, 1L, 0L, 0L,
15L, 0L), N = c(0L, 3L, 0L, 4L, 5L, 9L, 1L, 9L, 0L, 11L, 0L,
28L, 3L, 3L, 0L, 15L, 1L, 0L, 2L, 2L, 3L, 2L, 3L, 1L, 1L, 2L,
4L, 3L, 2L, 20L, 2L, 5L, 14L, 3L, 4L, 3L, 0L, 4L, 0L, 0L, 0L,
1L, 9L, 1L, 3L, 15L, 8L, 7L, 0L, 4L), P = c(0L, 1L, 0L, 1L, 2L,
1L, 24L, 1L, 0L, 1L, 2L, 1L, 1L, 21L, 0L, 0L, 2L, 0L, 40L, 1L,
1L, 1L, 1L, 0L, 1L, 3L, 2L, 0L, 3L, 1L, 1L, 2L, 1L, 1L, 1L, 1L,
1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 5L, 1L, 3L, 7L, 0L, 1L), Q = c(0L,
8L, 0L, 6L, 1L, 6L, 3L, 7L, 0L, 10L, 0L, 2L, 10L, 5L, 0L, 3L,
1L, 1L, 3L, 9L, 2L, 7L, 9L, 1L, 5L, 1L, 6L, 19L, 8L, 3L, 1L,
5L, 11L, 1L, 6L, 8L, 0L, 5L, 0L, 0L, 0L, 1L, 7L, 0L, 11L, 9L,
4L, 2L, 0L, 10L), R = c(0L, 35L, 0L, 1L, 1L, 4L, 3L, 4L, 0L,
6L, 0L, 1L, 3L, 2L, 0L, 2L, 1L, 1L, 3L, 18L, 2L, 15L, 5L, 1L,
6L, 1L, 3L, 10L, 3L, 3L, 1L, 5L, 8L, 2L, 7L, 12L, 0L, 5L, 1L,
0L, 0L, 1L, 8L, 0L, 16L, 13L, 1L, 2L, 0L, 9L), S = c(0L, 3L,
0L, 3L, 33L, 6L, 4L, 15L, 0L, 5L, 6L, 7L, 6L, 6L, 0L, 3L, 29L,
1L, 9L, 3L, 2L, 3L, 6L, 1L, 2L, 10L, 12L, 3L, 4L, 23L, 5L, 12L,
4L, 10L, 10L, 14L, 1L, 4L, 0L, 4L, 0L, 10L, 6L, 21L, 6L, 8L,
7L, 22L, 0L, 6L), T = c(2L, 2L, 0L, 2L, 35L, 4L, 6L, 14L, 0L,
3L, 10L, 3L, 5L, 4L, 1L, 1L, 7L, 1L, 5L, 5L, 1L, 2L, 6L, 3L,
6L, 6L, 5L, 4L, 7L, 9L, 18L, 5L, 3L, 2L, 18L, 7L, 3L, 6L, 0L,
1L, 1L, 10L, 1L, 10L, 7L, 6L, 2L, 9L, 2L, 5L), V = c(44L, 1L,
1L, 1L, 2L, 3L, 13L, 2L, 3L, 0L, 28L, 0L, 2L, 1L, 22L, 0L, 3L,
5L, 2L, 7L, 1L, 2L, 5L, 15L, 21L, 5L, 2L, 2L, 11L, 1L, 25L, 2L,
1L, 0L, 9L, 2L, 30L, 8L, 1L, 2L, 17L, 15L, 0L, 6L, 2L, 1L, 0L,
0L, 14L, 1L), W = c(0L, 0L, 4L, 0L, 0L, 0L, 0L, 0L, 3L, 0L, 0L,
0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 4L, 4L, 1L, 3L, 0L, 4L, 0L,
0L, 0L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 0L, 2L, 78L, 0L, 1L, 0L,
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), Y = c(0L, 1L, 45L, 0L, 0L, 4L,
1L, 0L, 6L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 37L,
11L, 1L, 14L, 1L, 12L, 0L, 1L, 1L, 1L, 1L, 8L, 8L, 0L, 1L, 1L,
1L, 11L, 6L, 0L, 2L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L)),
class = "data.frame", row.names = c(NA,-50L)))



buildCAT <- function(r=1, nStates=20, alpha0DirichletMixtureCAT=100, profiles=C20_1) {

  stopifnot(nStates == 20)

  profilesCAT <- profiles$profCat
  profilesCAT$X <- NULL

  nProfilesCAT <- dim(profilesCAT)[1]
  stopifnot(dim(profilesCAT)[2] == 20)

  dim <- nStates
  states <- colnames(profilesCAT)

  # Draw the frequency for all CAT profiles
  profilesFreq <- rgamma(nProfilesCAT, alpha0DirichletMixtureCAT, 1)
  profilesFreq <- profilesFreq/sum(profilesFreq)

  models <- list()

  for (iModel in 1:length(profilesFreq)){

    freq <- matrix(0, dim, dim)
    rownames(freq) <- colnames(freq) <- states

    diag(freq) <- as.matrix(profilesCAT[iModel,])/sum(profilesCAT[iModel,])

    R <- matrix(r, dim, dim)
    rownames(R) <- colnames(R) <- states


    diag(R) <- 0

    #and then calculate the rowSums for the total amount of change.
    scaleQ <- sum(freq %*% R %*% freq)
    Q <- (R %*% freq) - diag(apply(R %*% freq, 1, sum))

    models[[iModel]] <- list(Q=Q/scaleQ, scaleQ=scaleQ, dim.names=states, profileCAT=as.matrix(profilesCAT[iModel,])/sum(profilesCAT[iModel,]))
  }

  return(list(profFreq=profilesFreq, models=models))

}


buildCAT20 <- function(r=1, nStates=20, alpha0DirichletMixtureCAT=100) {
  return(buildCAT(r, nStates, alpha0DirichletMixtureCAT, C20_1))
}

buildCAT50 <- function(r=1, nStates=20, alpha0DirichletMixtureCAT=100) {
  return(buildCAT(r, nStates, alpha0DirichletMixtureCAT, C50_1))
}
