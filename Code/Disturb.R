Disturb = function(data0,ED,ER){
  # This function is used to obtain perturbation datasets under 
  # fixed perturbation probability (ER) and perturbation degree (ED)
  
  # data0: Datasets that require perturbation
  
  data <- data0;
  x <- dim(data)[1];
  y <- dim(data)[2];
  Rand <- array(runif(x*y), dim=dim(data));
  for (i in 1:x){
    for (j in 1:y){
      if (Rand[i,j]<ER){
        TR <- rnorm(1,data[i,j],ED*data[i,j]);
        while(TR<=0){
          TR <- rnorm(1,data[i,j],ED*data[i,j]);
        }
        data[i,j] <- TR;
      }
    }
  }
  return(data)
}
