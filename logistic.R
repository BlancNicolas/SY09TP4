log.app <- function(Xapp, zapp, intr, epsi)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]

	Xapp <- as.matrix(Xapp)

	if (intr == T)
	{
		Xapp <- cbind(rep(1,n),Xapp)
		p <- p + 1
	}

	targ <- matrix(as.numeric(zapp),nrow=n)
	targ[which(targ==2),] <- 0
	tXap <- t(Xapp)

	beta <- matrix(0,nrow=p,ncol=1)

	conv <- F
	iter <- 0
	while (conv == F)
	{
		iter <- iter + 1
		bold <- beta

		prob <- as.matrix(postprob(beta, Xapp))
		MatW <- diag(apply(prob, 1, function(x) x*(1-x)))

		beta <-  bold + ginv((tXap%*%MatW%*%Xapp)) %*% tXap %*% (targ - prob)

		if (norm(beta-bold)<epsi)
		{
			conv <- T
		}
	}

	#Calcul de logL
	logL <- 0
	for (i in 1:iter){
	  logL <- logL + (targ[i]*log(prob[i]) + (1-targ[i])*log(1-prob[i]))
	}
	prob <- postprob(beta, Xapp)
	out <- NULL
	out$beta <- beta
	out$iter <- iter
	out$logL <- logL

	out
}

log.val <- function(beta, Xtst)
{
	m <- dim(Xtst)[1]
	p <- dim(beta)[1]
	pX <- dim(Xtst)[2]

	Xtst <- as.matrix(Xtst)

	if (pX == (p-1))
	{
		Xtst  <- cbind(rep(1,m),Xtst)
	}

	prob <- postprob(beta,Xtst)
	pred <- max.col(prob)

	out <- NULL
	out$prob <- prob
	out$pred <- pred

	return(out)
}

postprob <- function(beta, X)
{
	X <- as.matrix(X)

	prob <- apply(X, 1, function(x) exp(t(beta)%*%x)/(1 + exp(t(beta)%*%x)))
	as.matrix(prob)
}
