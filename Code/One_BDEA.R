rm(list = ls());
gc();
setwd("D:/BaiduSyncdisk/R/GDEA");
library(lpSolve);
source('BCC.R');
source('BCC_XX.R');
source('Disturb.R');
source('LCV.R');
source('BDEA_BCC.R');

data0 <- read.csv(paste('Data/Data.csv', sep = ""),header = T);

Time_start <- Sys.time();    # Record time
N_test <- 1;         # Number of tests
N_iteration <- 500;  # Bootstrap iterations
Num_X <- 17;         # Number of Inputs
Num_Y <- 9;          # Number of Outputs

Effi <- matrix(0,N_test,dim(data0)[2]);
set.seed(12345);
for (i in 1:N_test){
  data = data0;
  # Inputs, with each column representing the input data for each DMUs
  X= data[1:Num_X,];  
  # Outputs, with each column representing the output data of each DMUs
  Y= data[(Num_X+1):(Num_X+Num_Y),];
  Effi[i,] = BDEA_BCC(X,Y,N_iteration);
}
write.table(Effi, file=paste0('Result/BDEA_all.csv'), sep=',', row.names = FALSE, col.names = FALSE);

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")