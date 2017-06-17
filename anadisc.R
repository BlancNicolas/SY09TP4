adq.app <- function(Xapp, zapp)
{
  n <- dim(Xapp)[1] #nb d'individu 
  p <- dim(Xapp)[2] #nb de variables
  g <- max(unique(zapp)) #nb de classes possible
  
  param <- NULL
  param$MCov <- array(0, c(p,p,g)) #Matrice de Cov
  param$mean <- array(0, c(g,p)) # vecteurs de moyennes
  param$prop <- rep(0, g) #proportion
  
  for (k in 1:g)
  {
    indk <- which(zapp==k)
    
    param$mean[k,] <- (1/length(indk))*colSums(Xapp[indk,]);
    param$MCov[,,k] <- cov(Xapp[indk,]);
    param$prop[k] <- length(indk)/n; 
  }
  
  param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		MCov <- MCov + length(indk)*cov(Xapp[indk,]);
		param$mean[k,] <- (1/length(indk))*sum(Xapp[indk,]);
		param$prop[k] <- length(indk)/n;
	}
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov/n;
	}

	param
}

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$mean[k,] <- (1/length(indk))*colSums(Xapp[indk,]); #meme que le quadratique 
		param$MCov[,,k] <- diag(cov(Xapp[indk,]))*diag(ncol(Xapp)); #on part des matrices quadratiques et on les diagonalisent --> Hypothèses des d'indépendancer des variables conditionnellement à Z
		param$prop[k] <- length(indk)/n; 
	}

	param
}

ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL
                                     
	prob <- matrix(0, nrow=n, ncol=g)

	for (k in 1:g)
	{
		prob[,k] <- (mvdnorm(Xtst, param$mean[k,], param$MCov[,,k]))*param$prop[k] ;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
	}
	prob <- prob/apply(prob, 1, sum);
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
