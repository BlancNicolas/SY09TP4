#3 jeux de donnée

donn1 <- read.csv("Synth1-1000.csv")
donn2 <- read.csv("Synth2-1000.csv")
donn3 <- read.csv("Synth3-1000.csv")

#---------Synth1-----------
#Decision borders

#Parting data
donnSynth1 <- read.csv("Synth1-1000.csv")
X1 <- donnSynth1[,1:2]
z1 <- donnSynth1[,3]

donn.sep <- separ1(X1, z1) #using separ1.R function 

Xapp1 <- donn.sep$Xapp
zapp1 <- donn.sep$zapp
Xtst1 <- donn.sep$Xtst
ztst1 <- donn.sep$ztst

#---------Synth2-----------
#Decision borders

#Parting data
donnSynth2 <- read.csv("Synth2-1000.csv")
X2 <- donnSynth2[,1:2]
z2 <- donnSynth2[,3]

donn.sep <- separ1(X2, z2) #using separ1.R function 

Xapp2 <- donn.sep$Xapp
zapp2 <- donn.sep$zapp
Xtst2 <- donn.sep$Xtst
ztst2 <- donn.sep$ztst

#---------Synth3-----------
#Decision borders

#Parting data
donnSynth3 <- read.csv("Synth3-1000.csv")
X3 <- donnSynth3[,1:2]
z3 <- donnSynth3[,3]

donn.sep <- separ1(X3, z3) #using separ1.R function 

Xapp3 <- donn.sep$Xapp
zapp3 <- donn.sep$zapp
Xtst3 <- donn.sep$Xtst
ztst3 <- donn.sep$ztst

#---------Pima----------------

donnPima <- read.csv("Pima.csv", header=T)
XPim <- donnPima[,1:7]
zPim <- donnPima[,8]

donn.sep <- separ1(XPim, zPim) #using separ1.R function 

XappPim <- donn.sep$Xapp
zappPim <- donn.sep$zapp
XtstPim <- donn.sep$Xtst
ztstPim <- donn.sep$ztst

#----------Breast Cancer-------

donnBreast <- read.csv("bcw.csv", header=T)
XBreast <- donnBreast[,1:9]
zBreast <- donnBreast[,10]

donn.sep <- separ1(XBreast, zBreast) #using separ1.R function 

XappBreast <- donn.sep$Xapp
zappBreast <- donn.sep$zapp
XtstBreast <- donn.sep$Xtst
ztstBreast <- donn.sep$ztst

