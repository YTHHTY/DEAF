GenerateData= function(DMU,Num_X,Num_Y,rho,fun){
  # On the premise of determining the production function, input quantity, 
  # output quantity, DMU quantity, and data correlation, generate raw data 
  # for Monte Carlo simulation.
  
  ##### Input #####
  #
  # DMU : Number of decision units
  # Num_X : Number of inputs
  # Num_Y : Number of outputs
  # rho : Data correlation
  # fun : Production function
  #
  ##### Output #####
  #
  # data0 : The generated raw data, a matrix of size (Num_X+Num_Y)*DMU
  #
  ##################
  
  data0 <- matrix(nrow = (Num_X+Num_Y), ncol = DMU, data = NA);
  # Generate the first inputs
  data0[1,] <- runif(DMU,min=10,max=20);
  # Generate the remaining inputs
  for (i in 1:(Num_X-1)){
    z = runif(DMU,min=10,max=20);
    data0[i+1,] <- rho * data0[1,] + z * sqrt(1 - rho^2);
  }
  # Generate the first output
  data0[1+Num_X,] <- fun(data0,DMU,Num_X);
  # Generate the remaining outputs
  for (i in 1:(Num_Y-1)){
    z = runif(DMU,min=10,max=20);
    data0[i+1+Num_X,] <- rho * data0[1+Num_X,] + z * sqrt(1 - rho^2);
  }
  return(data0)
}