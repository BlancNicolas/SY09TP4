#Test on simulated data

#Quadratic Discriminant Analysis 
#--------------------------

#Synth1
synth1ErrorDiscQuad <- synthErrorDisc(adq.app, ad.val, donn1)

#Synth2
synth2ErrorDiscQuad <- synthErrorDisc(adq.app, ad.val, donn2)

#Synth3
synth3ErrorDiscQuad <- synthErrorDisc(adq.app, ad.val, donn3)

#Linear Discriminant Analysis 
#--------------------------

#Synth1
synth1ErrorDiscLin <- synthErrorDisc(adl.app, ad.val, donn1)

#Synth2
synth2ErrorDiscLin <- synthErrorDisc(adl.app, ad.val, donn2)

#Synth3
synth3ErrorDiscLin <- synthErrorDisc(adl.app, ad.val, donn3)

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
synth1ErrorQuadReg <- synthErrorQuadReg(log.app, log.val, donn1)

#Synth2
synth2ErrorQuadReg <- synthErrorQuadReg(log.app, log.val, donn2)

#Synth3
synth3ErrorQuadReg <- synthErrorQuadReg(log.app, log.val, donn3)

#Decision Tree
#---------------------------





