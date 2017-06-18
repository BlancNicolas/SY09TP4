#-------Synth Data-------
#------------------------

#Test on simulated data

#Quadratic Discriminant Analysis 
#--------------------------

#Synth1
synth1ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donn1)

#Synth2
synth2ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donn2)

#Synth3
synth3ErrorDiscQuad <- ErrorDisc(adq.app, ad.val, donn3)

#Linear Discriminant Analysis 
#--------------------------

#Synth1
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

BreastErrorQuadReg <- synthErrorQuadReg(log.app,log.val, donnBreast)

#Linear Logistic Regression Analysis 
#--------------------------

BreastErrorLinReg <- ErrorLinReg(log.app, log.val, donnBreast)





