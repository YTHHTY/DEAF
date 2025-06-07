LCV = function(x,h){
  # The Likehood cross validation function is used to automatically calculate 
  # the most suitable bandwidth h in Bootstrap DEA/DEA Forest
  n <- length(x);
  CV <- rep(0,n);
  for (i in 1:n){
    for (j in 1:n){
      if (i != j){
        CV[i] <- CV[i] + ( dnorm((x[i]-x[j])/h) + dnorm((x[i]-2+x[j])/h) ) / ((n-1)*h);
      }
    }
  }
  CV = sum(log(CV))/n;
  return(CV)
}