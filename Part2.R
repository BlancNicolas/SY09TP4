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
paramSynth1Adl <- adl.app(X1,z1) 
ad.val(paramSynth1Adl, )
prob.ad(paramSynth1Adl, X1, z1, 0.5)

#Error estimate 
synth1ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn1)

#Synth2
synth2ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn2)

#Synth3
synth3ErrorDiscLin <- ErrorDisc(adl.app, ad.val, donn3)

#Linear Logistic regression
#--------------------------

#Synth1
synth1ErrorLinReg <- synthErrorLinReg(log.app, log.val, donn1)

#Synth2
synth2ErrorLinReg <- synthErrorLinReg(log.app, log.val, donn2)

#Synth3
synth3ErrorLinReg <- synthErrorLinReg(log.app, log.val, donn3)

#Quadratic Logistic regression
#--------------------------

#Synth1
synth1ErrorQuadReg <- ErrorQuadReg(log.app, log.val, donn1)

#Synth2
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





