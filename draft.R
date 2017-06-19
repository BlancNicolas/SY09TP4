X <- donnPima[,1:7]
z <- donnPima[,8]
N<- 100

#on part d'abord de d'ensembles de test et apprentissage classiques 

donn.sep <- separ1(X, z) #using separ1.R function 
Xapp <- donn.sep$Xapp
Xtst <- donn.sep$Xtst
Xapp2 <- Xapp
Xtst2 <- Xtst
for (p in 1:(dim(Xapp)[2]-1))
{
  for (q in (p+1):dim(Xapp)[2])
  {
    Xapp2 <- cbind(Xapp2, Xapp[,p]*Xapp[,q])
    Xtst2 <- cbind(Xtst2, Xtst[,p]*Xtst[,q])
  }
}

for (p in 1:dim(Xapp)[2])
{
  Xapp2 <- cbind(Xapp2, Xapp[,p]^2)
  Xtst2 <- cbind(Xtst2, Xtst[,p]^2)
}
Xapp <- Xapp2 #pour les utiliser dans les fonctions définies en desous
Xtst <- Xtst2