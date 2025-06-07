Fun_Translog= function(data0,DMU,Num_X){
  # This function calculates a single output through multiple inputs using the 
  # Translog production function
  
  u <- rnorm(DMU, mean = 0, sd = 0.3);
  v <- rnorm(DMU, mean = 0, sd = 0.1);
  X <- log(data0[1:Num_X,]);
  Y <- colMeans(X)+v-u;
  for (i in 1:Num_X){
    for (j in 1:Num_X){
      beta <- if (i == j) -0.2/Num_X else 0.2/(Num_X*(Num_X-1));
      Y <- Y + 0.5*beta*X[i,]*X[j,];
    }
  }
  Y <- exp(Y);
  return(Y)
}