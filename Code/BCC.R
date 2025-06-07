BCC = function(X,Y){
  # This function is used to calculate the classical DEA model - BCC
  # X:  Inputs  m*n
  # Y:  Outputs q*n
  n <- dim(X)[2];      # Number of decision-making units
  m <- dim(X)[1];      # Number of Inputs
  q <- dim(Y)[1];      # Number of outputs
  
  Result <- rep(0,n);
  
  for (i in 1:n){
    # Define objective function
    f.obj <- c(rep(0,n),1);
    
    # Set inequality constraints
    A1 <- as.matrix(cbind(X,-X[,i]));
    A2 <- as.matrix(cbind(Y,rep(0,q)));
    # Specify lower bound
    A3 <- matrix(0,n,n+1);
    for(j in 1:n){A3[j,j]=1}
    # Specify restrictions
    A4 <- matrix(1,1,n+1);
    A4[n+1] <- 0;
    
    f.con=rbind(A1,A2,A3,A4);
    f.dir=c(rep("<=",m),rep(">=",q),rep(">=",n),rep("=",1));
    f.rhs=c(rep(0,m),Y[,i],rep(0,n),1);
    
    # Solve the model
    reu = lp("min",f.obj,f.con,f.dir,f.rhs)
    Result[i] <- reu$solution[n+1]
  }
  return(Result)
}