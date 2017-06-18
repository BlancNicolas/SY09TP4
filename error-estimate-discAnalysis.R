#AT FIRST USE : load-data.R
ErrorDisc <- function(appfunct, valfunct, dataset){
  #/!\-----Linear logistic regression------/!\
  #-------------------------------------
  
  #start of algorithm 
    data <- as.data.frame(dataset)
    if (ncol(dataset) == 8) #test pour savoir si c'est Pima
    {
      print('coucou')
      X <- data[,1:7]
      z <- data[,8]
      N<- 100
    }
    else if (ncol(dataset) == 10) #Test pour Breast Cancer Wisconsin
    {
      X <- Donn[,1:9]
      z <- Donn[,10]
      N <- 100
    }
    else {
      X <- data[,1:2];
      z <- data[,3];
      N<-20
    }
    
    #error matrix
    error_train <- matrix(0, length(donn_list), N);
    error_test <-  matrix(0, length(donn_list), N);
    
    for (j in 1:N ){
      
      donn.sep <- separ1(X, z) #using separ1.R function 
      
      #parting in learning and test sets 
      Xapp <- donn.sep$Xapp
      zapp <- donn.sep$zapp
      Xtst <- donn.sep$Xtst
      ztst <- donn.sep$ztst
      
      #we estimate parameters on Xapp
      param <- appfunct(Xapp, zapp);
      
      #applying values on individuals of sets 
      train_set <- valfunct(param, Xapp)$pred;
      test_set <- valfunct(param, Xtst)$pred;
      
      #error rate 
      error_train[1,j] <- dim(as.matrix(which(train_set != as.matrix(zapp))))[1] / dim(as.matrix(zapp))[1]; 
      error_test[1,j] <- dim(as.matrix(which(test_set != as.matrix(ztst))))[1] / dim(as.matrix(zapp))[1]; 
      
    }
  #applying mean on rows which represent each dataset
  average_error_rate <- matrix(0, nrow = length(donn_list), ncol = 2);
  
  average_error_rate[,1] = apply(error_train, 1, mean);
  average_error_rate[,2] = apply(error_test, 1, mean);
  
  #Confidence interval 
  
  #we consider with alpha = 0.05
  #We use : CI = [p(mean) - fract(0.975*s/sqrt(n)), p(mean) + fract(0.975*s/sqrt(n))]
  
  ptrain <- as.matrix(average_error_rate[, 1]);
  ptest <- as.matrix(average_error_rate[, 2]);
  frac = 1.96;
  
  CI_tab <- matrix(0, nrow = nrow(average_error_rate), ncol = 4); #4 because of two values for each interval 
  for (i in 1:nrow(average_error_rate)){
    CI_tab[1, 1] <- ptrain[i] - frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(dataset))); #lower bound for CI on train set
    CI_tab[1, 2] <- ptrain[i] + frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(dataset))); #upper bound for CI on train set
    CI_tab[1, 3] <- ptest[i] - frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(dataset))); #lower bound for CI on test set
    CI_tab[1, 4] <- ptest[i] + frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(dataset))); #upper bound for CI on test set
    
  }
  
  out <- NULL
  out$errortrain <- error_train
  out$errortest <- error_test
  out$averageErrorRate <- average_error_rate
  out$confInt <- CI_tab
  
  return(out)
  
}
