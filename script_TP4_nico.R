#Testing functions and borders results

#Quadratic 
library(MASS)

#Dataset 
donn <- read.csv("Synth1-40.csv")
X <- donn[,1:2]
z <- donn[,3]

#Quadractic

#Estimate of parameters
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

#Linear logistic regression

param <- log.app(X, z, 1, 0.00001)
log.val(param$beta, X)$pred

prob.log(param$beta, X, z, c(0.2,0.4,0.6,0.8))

#Quadratic logistic regression

#Our individuals are defined in X = {X^1, X^2} 
#First we have to define the new set of values part of X^2 = {X^1, X^2, X^1X^2, (X^1)^2, (X^2)^2} 
Xquad <- cbind(X, X[,1]*X[,2], (X[,1])^2, (X[,2])^2)
param <- log.app(Xquad, z, 1, 0.00001)
prob.log2(param$beta, Xquad[,1:2], z, c(0.2,0.4,0.6,0.8))


