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
Num_X <- 17;         # Number of Inputs
Num_Y <- 9;          # Number of Outputs
N_char<- Num_X+Num_Y;

# Inputs, with each column representing the input data for each DMUs
X= data0[1:Num_X,];  
# Outputs, with each column representing the output data of each DMUs
Y= data0[(Num_X+1):(Num_X+Num_Y),];
# Original efficiency score
Effi0 <- DEAF_BCC(X,Y,N_iteration,1,1);

SP <- matrix(0, N_char , 1);
KE <- matrix(0, N_char , 1);

set.seed(12345);
for (i in 1:N_char){
  data = data0;
  X= data[1:Num_X,];
  Y= data[(Num_X+1):(Num_X+Num_Y),]; 
  if (i<(Num_X+1)) { 
    X<- X[-i,]
  } else {
    Y<- Y[-(i-17),]
  };
  # Efficiency score after shuffling feature
  Effi = DEAF_BCC(X,Y,N_iteration,1,1);
  # Relevance to reflect importance
  SP[i,] <- cor(Effi0, Effi, method = "spearman");
  KE[i,] <- cor(Effi0, Effi, method = "kendall");
}

write.table(SP, file=paste0('Result/DEAF_Importance_old_SP.csv'), sep=',', row.names = FALSE, col.names = FALSE);
write.table(KE, file=paste0('Result/DEAF_Importance_old_KE.csv'), sep=',', row.names = FALSE, col.names = FALSE);

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")