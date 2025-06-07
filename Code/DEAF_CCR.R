DEAF_CCR = function(X,Y,N_iteration,X_d,Y_d){
  # This function is used to calculate DEA Forest
  
  ##### Input #####
  #
  # X : Inputs, with each column representing the input data for each DMUs
  # Y : Outputs, with each column representing the output data of each DMUs
  # N_iteration : Bootstrap iterations/The number of trees
  # X_d : This is a control switch for applying RSM to X, where 1 is on and 0 is off
  # Y_d : This is a control switch for applying RSM to Y, where 1 is on and 0 is off
  #
  ##### Output #####
  #
  # theta_boot_mean : The efficiency score calculated for each DMU based on DEAF
  #
  ##################
  
  n <- dim(X)[2];      # Number of decision-making units
  m <- dim(X)[1];      # Number of Inputs
  q <- dim(Y)[1];      # Number of outputs
  
  # Basic DEA: CCR/BCC/FGL/FDH
  theta <- CCR(X,Y);
  
  ## Likehood Cross-Validation Function
  h = seq(0.01,0.5,0.001);
  N_h <- length(h);
  CV <- rep(0,N_h);
  for (i in 1:N_h){
    CV[i] <- LCV(theta,h[i]);
  }
  h <- h[which(CV==max(CV))];
  
  ## Smoothed Bootstrap
  theta_boot <- matrix(0,N_iteration,n);
  
  theta_var <- var(theta);
  for (t in 1:N_iteration){
    ## Dropout
    if (X_d == 0) {
      N_dropout_X <- 0
    } else {
      # Randomly obtain the number of drop variables
      N_dropout_X <- sample(0:(m-1), 1); 
    }
    if (Y_d == 0) {
      N_dropout_Y <- 0
    } else {
      # Randomly obtain the number of drop variables
      N_dropout_Y <- sample(0:(q-1), 1);
    }
    # Randomly drop a specified number of variables
    retain_X <- sample(1:m,m-N_dropout_X,replace=F);
    retain_Y <- sample(1:q,q-N_dropout_Y,replace=F);
    
    ## Bootstrap
    boot <- sample(1:n,n,replace=T);
    beta_star <- theta[boot];
    
    beta_bar_star <- mean(beta_star);
    epsilon <- rnorm(n);
    theta_bar_star <- beta_star + h*epsilon;
    theta_bar_star <- cbind(theta_bar_star,2-theta_bar_star);
    theta_bar_star <- apply(theta_bar_star,1,min);
    theta_star <- beta_bar_star + (1/sqrt(1+h^2/theta_var)) * (theta_bar_star - beta_bar_star);
    
    # Estimate
    Xd <- X[retain_X,,drop = FALSE];
    Yd <- Y[retain_Y,,drop = FALSE];
    XX <- matrix(0,m-N_dropout_X,n);
    for(i in 1:n){
      XX[,i] <- (theta[i]/theta_star[i])*Xd[,i];
    }
    theta_boot[t,] <- CCR_XX(Xd,XX,Yd);
  }
  ## Calculate the average efficiency
  theta_boot_mean = apply(theta_boot,2,mean);
  return(theta_boot_mean)
}