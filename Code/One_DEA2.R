rm(list = ls());
gc();
setwd("D:/BaiduSyncdisk/R/GDEA");
library(lpSolve);
source('BCC.R');

data0 <- read.csv(paste('Data/Data_In_5_9.csv', sep = ""),header = T);

Time_start <- Sys.time();    # Record time
N_test <- 1;         # Number of tests
N_iteration <- 500;  # Bootstrap iterations
Num_X <- 9;         # Number of Inputs
Num_Y <- 5;          # Number of Outputs

Effi <- matrix(0,N_test,dim(data0)[2]);
set.seed(12345);
for (i in 1:N_test){
  data = data0;
  # Inputs, with each column representing the input data for each DMUs
  X= data[1:Num_X,];  
  # Outputs, with each column representing the output data of each DMUs
  Y= data[(Num_X+1):(Num_X+Num_Y),];
  Effi[i,] = BCC(X,Y);
}
write.table(Effi, file=paste0('Result/实证/DEA_In_5_9.csv'), sep=',', row.names = FALSE, col.names = FALSE);

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")