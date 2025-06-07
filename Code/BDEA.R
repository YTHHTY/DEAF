BDEA = function(X,Y,N_iteration){
  # This function is used to calculate Bootstrap DEA
  
  ##### Input #####
  #
  # X : Inputs, with each column representing the input data for each DMUs
  # Y : Outputs, with each column representing the output data of each DMUs
  # N_iteration : Bootstrap iterations
  #
  ##### Output #####
  #
  # theta_boot_mean : The efficiency score calculated for each DMU based on DEAF
  #
  ##################
  
  n <- dim(X)[2];      # Number of decision-making units
  m <- dim(X)[1];      # Number of Inputs
  
  # Basic DEA: CCR/BCC/FGL/FDH
  theta <- BCC(X,Y);
  
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
    XX <- matrix(0,m,n);
    for(i in 1:n){
      XX[,i] <- (theta[i]/theta_star[i])*X[,i]
    };
    theta_boot[t,] <- BCC_XX(X,XX,Y);
  }
  ## Calculate the average efficiency
  theta_boot_mean = apply(theta_boot,2,mean);
  return(theta_boot_mean)
}