#Testing functions and borders results

#Quadratic 
library(MASS)
#Dataset 
donn <- read.csv("Synth1-40.csv")
X <- donn[,1:2]
z <- donn[,3]

#Quadractic
#estimate of parameters
param <- adq.app(X,z) 

prob.ad(param, X, z, c(0.2,0.4,0.6,0.8))

#Linear
param <- adl.app(X,z) 

prob.ad(param, X, z, c(0.2,0.4,0.6,0.8))

#Bayesian
param <- nba.app(X,z) 

prob.ad(param, X, z, c(0.2,0.4,0.6,0.8))

#----Logistic regression----
#----------------------------
res <- separ1(X,z)
param <- log.app(X, z, 1, 0.00001)

prob.log(param$beta, X, z, c(0.2,0.4,0.6,0.8))
