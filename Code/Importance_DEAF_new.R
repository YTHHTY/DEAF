rm(list = ls());
gc();
setwd("D:/BaiduSyncdisk/R/GDEA");
library(lpSolve);
source('BCC.R');
source('BCC_XX.R');
source('Disturb.R');
source('LCV.R');
source('DEAF_BCC.R');

data0 <- read.csv(paste('Data/Data.csv', sep = ""),header = T);

Time_start <- Sys.time();    # Record time
N_iteration <- 500;  # Bootstrap iterations/The number of trees
Num_X0 <- 17;        # Number of Inputs
Num_Y0 <- 9;         # Number of Outputs

# Cache
Num_X <- Num_X0;
Num_Y <- Num_Y0;
N_char <- Num_X+Num_Y;  
Importance <- matrix(0, Num_X0+Num_Y0 , 1);

Drop_X <- 0;
Drop_Y <- 0;

data <- data0;
Drop_list = c();

set.seed(12345);
for (j in 1:N_char-4){
  # Inputs, with each column representing the input data for each DMUs
  X= data[1:Num_X,]; 
  # Outputs, with each column representing the output data of each DMUs
  Y= data[(Num_X+1):(Num_X+Num_Y),]; 
  # Original efficiency score
  Effi0 <- DEAF_BCC(X,Y,N_iteration,1,1);
  
  Index <- c();
  for (i in 1:(Num_X+Num_Y)){
    X= data[1:Num_X,];
    Y= data[(Num_X+1):(Num_X+Num_Y),];
    if (i<=Num_X) { 
      X[i,]<- sample(X[i,])
    } else {
      Y[i-Num_X,]<- sample(Y[i-Num_X,])
    };
    # Efficiency score after shuffling feature
    Effi <- DEAF_BCC(X,Y,N_iteration,1,1);
    # Relevance to reflect importance
    Index <- c(Index, cor(Effi0, Effi, method = "spearman")); # kendall   spearman
  }
  # Delete feature with the highest relevance (lowest importance)
  Drop_index <- which.max(Index);
  data <- data[,-Drop_index];
  Effi0 <- Effi0[-Drop_index];
  
  # Process cache variables after deleting feature
  Drop_list <- sort(Drop_list, decreasing = F); 
  Drop_num <- length(Drop_list);
  
  if (Drop_num>0){
    for (i in Drop_list){
      Index <- append(Index, 0, after = (i-1));
    }
  }
  Drop_index <- which.max(Index);
  Drop_list <- c(Drop_list, Drop_index);
  
  if (max(Index)==0){
    Importance <- Importance + Index;
    break;
  }
  
  if (Drop_index<=Num_X0){
    if (Num_X<3){
      Importance <- Importance + Index;
      break;
    }
    Importance[Drop_index,] <- Index[Drop_index];
    Drop_X <- Drop_X+1;
    Num_X <- Num_X-1;
  } else {
    if (Num_Y<3){
      Importance <- Importance + Index;
      break;
    }
    Importance[Drop_index,] <- Index[Drop_index];
    Drop_Y <- Drop_Y+1;
    Num_Y <- Num_Y-1;
  }
  
}
write.table(Importance, file=paste0('Result/DEAF_Importance_new_SP.csv'), sep=',', row.names = FALSE, col.names = FALSE);
#write.table(Importance, file=paste0('Result/DEAF_Importance_new_KE.csv'), sep=',', row.names = FALSE, col.names = FALSE);

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")