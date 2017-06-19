#-------Synth Data-------
#------------------------

#---> /!\AT FIRST : USE load-data.R /!\ <-----

#Test on simulated data

#Quadratic Discriminant Analysis 
#--------------------------

#---------Synth1-----------
#Decision borders

#Estimate parameters and visualize data with borders
paramSynth1 <- adq.app(Xapp1,zapp1) 
prob.ad(paramSynth1, X1, z1, 0.5)

#Error estimate 
synth1ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnSynth1)

#---------Synth2-----------
#Estimate parameters and visualize data with borders
paramSynth2 <- adq.app(Xapp2,zapp2) 
prob.ad(paramSynth2, X2, z2, 0.5)

#Error estimate
synth2ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnSynth2)

#---------Synth3-----------
#Estimate parameters and visualize data with borders
paramSynth3 <- adq.app(Xapp3,zapp3) 
prob.ad(paramSynth3, X3, z3, 0.5)

#Error estimate
synth3ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnSynth3)

#Linear Discriminant Analysis 
#--------------------------

#---------Synth1-----------
#Decision borders

#Estimate parameters and visualize data with borders
paramSynth1Adl <- adl.app(Xapp1,zapp1) 
prob.ad(paramSynth1Adl, X1, z1, 0.5)

#Error estimate 
synth1ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donnSynth1)

#---------Synth2-----------
paramSynth2Adl <- adl.app(Xapp2,zapp2) 
prob.ad(paramSynth2Adl, X2, z2, 0.5)

synth2ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donnSynth2)


#---------Synth3----------

paramSynth3Adl <- adl.app(Xapp3,zapp3) 
prob.ad(paramSynth3Adl, X3, z3, 0.5)

synth3ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donnSynth3)

#Naive Bayesian Analysis 
#--------------------------

#---------Synth1-----------
#Decision borders

#Estimate parameters and visualize data with borders
paramSynth1Nba <- nba.app(Xapp1,zapp1) 
prob.ad(paramSynth1Nba, X1, z1, 0.5)

#Error estimate 
synth1ErrorNba <- ErrorDisc(nba.app, ad.val, donnSynth1)

#---------Synth2-----------
#Estimate parameters and visualize data with borders
paramSynth2Nba <- nba.app(Xapp2,zapp2) 
prob.ad(paramSynth2Nba, X2, z2, 0.5)

synth2ErrorNba <- ErrorDisc(nba.app, ad.val, donnSynth2)

#---------Synth3-----------
#Estimate parameters and visualize data with borders
paramSynth3Nba <- nba.app(Xapp3,zapp3) 
prob.ad(paramSynth3Nba, X3, z3, 0.5)

synth3ErrorNba <- ErrorDisc(nba.app, ad.val, donnSynth3)

#Linear Logistic regression
#--------------------------

#---------Synth1-----------
#Decision border
paramSynth1LinReg <- log.app(Xapp1, zapp1, 1, 0.00001)
prob.log(paramSynth1LinReg$beta, X1, z1, 0.5)

synth1ErrorLinRegT <- ErrorLinReg(log.app, log.val, donnSynth1, 1)#With origin
synth1ErrorLinRegF <- ErrorLinReg(log.app, log.val, donnSynth1, 0)#Without origin

#---------Synth2-----------
paramSynth2LinReg <- log.app(Xapp2, zapp2, 1, 0.00001)
prob.log(paramSynth2LinReg$beta, X2, z2, 0.5)

synth2ErrorLinRegT <- ErrorLinReg(log.app, log.val, donnSynth2, 1)#With origin
synth2ErrorLinRegF <- ErrorLinReg(log.app, log.val, donnSynth2, 0)#Without origin

#---------Synth3-----------
paramSynth3LinReg <- log.app(Xapp3, zapp3, 1, 0.00001)
prob.log(paramSynth3LinReg$beta, X3, z3, 0.5)

synth3ErrorLinRegT <- ErrorLinReg(log.app, log.val, donnSynth3, 1)#With origin
synth3ErrorLinRegF <- ErrorLinReg(log.app, log.val, donnSynth3, 0)#Without origin

#Quadratic Logistic regression
#--------------------------

#---------Synth1-----------
#Error estimate
Xquad1 <- cbind(X1, X1[,1]*X1[,2], (X1[,1])^2, (X1[,2])^2)
paramSynth1QuadReg <- log.app(Xquad1, z1, 1, 0.00001)#with origin  
paramSynth1QuadReg <- log.app(Xquad1, z1, 0, 0.00001)#without origin
prob.log2(paramSynth1QuadReg$beta, Xquad1[,1:2], z1, 0.5)

synth1ErrorQuadRegT <- ErrorQuadReg(log.app, log.val, donnSynth1, 1)#with origin  
synth1ErrorQuadRegF <- ErrorQuadReg(log.app, log.val, donnSynth1, 0)#without origin

#---------Synth2-----------
Xquad2 <- cbind(X2, X2[,1]*X2[,2], (X2[,1])^2, (X2[,2])^2)
paramSynth2QuadReg <- log.app(Xquad2, z2, 1, 0.00001)#with origin  
paramSynth2QuadReg <- log.app(Xquad2, z2, 0, 0.00001)#without origin
prob.log2(paramSynth2QuadReg$beta, Xquad2[,1:2], z2, 0.5)

synth2ErrorQuadRegT <- ErrorQuadReg(log.app, log.val, donnSynth2, 1)#with origin  
synth2ErrorQuadRegF <- ErrorQuadReg(log.app, log.val, donnSynth2, 0)#without origin

#---------Synth3-----------
Xquad3 <- cbind(X3, X3[,1]*X3[,2], (X3[,1])^2, (X3[,2])^2)
paramSynth3QuadReg <- log.app(Xquad3, z3, 1, 0.00001)#with origin  
paramSynth3QuadReg <- log.app(Xquad3, z3, 0, 0.00001)#without origin
prob.log2(paramSynth3QuadReg$beta, Xquad3[,1:2], z3, 0.5)

synth3ErrorQuadRegT <- ErrorQuadReg(log.app, log.val, donnSynth3, 1)#with origin  
synth3ErrorQuadRegF <- ErrorQuadReg(log.app, log.val, donnSynth3, 0)#without origin
#Decision Tree
#---------------------------


#------- Pima --------------
#---------------------------

#----ACP---
#----------

#fonction princomp Pima

Xpascentrée <- data.matrix(XPim)
X = scale(Xpascentrée, center=TRUE, scale=FALSE)
ACP <- princomp(X)
plot(ACP)
biplot(ACP, xlabs=rep(".", nrow(X)))
biplot(ACP, c(1,3)) # Avec le filtre c(1,2) on choisit le premier plan factoriel, ici c'est le deuxième par exemple

XPimAfterACP <- donnPima[,2:3]
paramPima <- adl.app(XPimAfterACP,zappPim) #pas concluant
prob.ad(paramPima, XPimAfterACP, zPim, 0.5)


#Quadratic Discriminant Analysis 
#--------------------------
#Estimate parameters and visualize data with borders
paramPima <- adq.app(XPimAfterACP,zappPim) 
prob.ad(paramPima, XPimAfterACP, zPim, 0.5)


PimaErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnPima)

#Linear Discriminant Analysis 
#--------------------------

PimaErrorDiscLin <- ErrorDisc(adl.app, ad.val, donnPima)

#Bayesian
#--------
PimaErrorNba <- ErrorDisc(nba.app, ad.val, donnPima)

#Quadratic Logistic Regression Analysis 
#--------------------------------------

PimaErrorQuadRegT <- ErrorQuadReg(log.app,log.val, donnPima, 1)
PimaErrorQuadRegF <- ErrorQuadReg(log.app,log.val, donnPima, 0)

#Linear Logistic Regression Analysis 
#--------------------------

PimaErrorLinRegT <- ErrorLinReg(log.app, log.val, donnPima,1)
PimaErrorLinRegF <- ErrorLinReg(log.app, log.val, donnPima,0)

#------- Breast cancer Wisconsin --------------
#----------------------------------------------

#Quadratic Discriminant Analysis 
#-------------------------------
c
BreastErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnBreast)

#Linear Discriminant Analysis 
#--------------------------

BreastErrorDiscLin <- ErrorDisc(adl.app, ad.val, donnBreast)

#Bayesian
#--------

BreastErrorNba <- ErrorDisc(nba.app, ad.val, donnBreast)

#Linear Logistic Regression Analysis 
#--------------------------

BreastErrorLinRegT <- ErrorLinReg(log.app, log.val, donnBreast, 1)
BreastErrorLinRegF <- ErrorLinReg(log.app, log.val, donnBreast, 0)





