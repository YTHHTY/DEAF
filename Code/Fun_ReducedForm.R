Fun_ReducedForm= function(data0,DMU,Num_X){
  # This function calculates a single output through multiple inputs using the 
  # ReducedForm production function
  
  u <- rnorm(DMU, mean = 0, sd = 0.3);
  v <- rnorm(DMU, mean = 0, sd = 0.1);
  Y = colMeans(data0[1:Num_X,]) + v - u;
  return(Y)
}