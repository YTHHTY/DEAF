Fun_CobbDouglas= function(data0,DMU,Num_X){
  # This function calculates a single output through multiple inputs using the 
  # CobbDouglas production function
  
  u <- rnorm(DMU, mean = 0, sd = 0.3);
  v <- rnorm(DMU, mean = 0, sd = 0.1);
  Y <- data0[1:Num_X,]^(1/Num_X);
  Y <- apply(Y, 2, prod)*exp(v-u);
  return(Y)
}