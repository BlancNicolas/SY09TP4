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
synth1ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn1)

#---------Synth2-----------
paramSynth2Adl <- adl.app(Xapp2,zapp2) 
prob.ad(paramSynth2Adl, X2, z2, 0.5)

synth2ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn2)

#Synth3
synth3ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn3)

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

synth2ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn2)

#Synth3
synth3ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn3)

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

synth2ErrorLinReg <- ErrorLinReg(log.app, log.val, donn2)

#Synth3
synth3ErrorLinReg <- ErrorLinReg(log.app, log.val, donn3)

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
paramSynth1QuadReg <- log.app(Xquad2, z2, 0, 0.00001)#without origin
prob.log2(paramSynth2QuadReg$beta, Xquad2[,1:2], z2, 0.5)

synth2ErrorQuadReg <- ErrorQuadReg(log.app, log.val, donn2)

#Synth3
synth3ErrorQuadReg <- ErrorQuadReg(log.app, log.val, donn3)

#Decision Tree
#---------------------------


#------- Pima --------------
#---------------------------

donnPima <- read.csv("Pima.csv", header=T)

#Quadratic Discriminant Analysis 
#--------------------------

PimaErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnPima)

#Linear Discriminant Analysis 
#--------------------------

PimaErrorDiscQuad <- ErrorDisc(adl.app, ad.val, donnPima)

#Quadratic Logistic Regression Analysis 
#--------------------------

PimaErrorQuadReg <- synthErrorQuadReg(log.app,log.val, donnPima)

#Linear Logistic Regression Analysis 
#--------------------------

PimaErrorLinReg <- ErrorLinReg(log.app, log.val, donnPima)

#------- Breast cancer Wisconsin --------------
#----------------------------------------------

donnBreast <- read.csv("bcw.csv", header=T)

#Quadratic Discriminant Analysis 
#-------------------------------

BreastErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donnBreast)

#Linear Discriminant Analysis 
#--------------------------

BreastErrorDiscQuad <- ErrorDisc(adl.app, ad.val, donnBreast)

#Quadratic Logistic Regression Analysis 
#--------------------------

BreastErrorQuadReg <- ErrorQuadReg(log.app,log.val, donnBreast)

#Linear Logistic Regression Analysis 
#--------------------------

BreastErrorLinReg <- ErrorLinReg(log.app, log.val, donnBreast)





