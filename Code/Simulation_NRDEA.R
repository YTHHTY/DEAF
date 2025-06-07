rm(list = ls());
gc();
setwd("D:/BaiduSyncdisk/R/GDEA");
library(lpSolve);
library(deaR);
source('Disturb.R');
source('LCV.R');
source('GenerateData.R');
source('Fun_ReducedForm.R'); # CobbDouglas, ReducedForm, Translog

Time_start <- Sys.time();    # Record time
N_test <- 500;               # Number of tests

DMU = 50;          # Number of DMUs
Num_X = 4;         # Number of Inputs: 4, 6, 8, 10, 30
Num_Y = Num_X/2;   # Number of Outputs

rho = 0.2;         # Data correlation for DGP: 0.2, 0.5, 0.85
# Degree of data disturbance
ED_list <- c(0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.20); 

dmunames <- paste0("DMU", seq(1, DMU));
inputnames <- paste0("X", seq(1, Num_X));
outputnames <- paste0("Y", seq(1, Num_Y));

for (ED in ED_list){
  Effi0 <- matrix(0,N_test,DMU);
  Effi <- matrix(0,N_test,DMU);
  for (i in 1:N_test){
    ## DGP
    # Fixed random seed
    set.seed(i);
    # Generate raw data
    data0 <- GenerateData(DMU,Num_X,Num_Y,rho,Fun_ReducedForm);
    # Generate observation data
    data <- Disturb(data0,ED,1);
    
    ## Obtain True Efficiency on Raw Data
    X= data0[1:Num_X,];  
    Y= data0[(Num_X+1):(Num_X+Num_Y),]; 
    inputs <- matrix(X, nrow = Num_X, ncol = DMU, dimnames = list(inputnames, dmunames));
    outputs <- matrix(Y, nrow = Num_Y, ncol = DMU, dimnames = list(outputnames, dmunames));
    data_example <- make_deadata(inputs = inputs, outputs = outputs);
    result <- model_nonradial(data_example,
                              orientation = "io",
                              rts = "vrs");
    Effi0[i,] = unname(efficiencies(result)[,'mean_eff']);
    
    ## Obtaining Pseudo Efficiency on Observational Data
    X= data[1:Num_X,];
    Y= data[(Num_X+1):(Num_X+Num_Y),]; 
    inputs <- matrix(X, nrow = Num_X, ncol = DMU, dimnames = list(inputnames, dmunames));
    outputs <- matrix(Y, nrow = Num_Y, ncol = DMU, dimnames = list(outputnames, dmunames));
    data_example <- make_deadata(inputs = inputs, outputs = outputs);
    result <- model_nonradial(data_example,
                              orientation = "io",
                              rts = "vrs");
    Effi[i,] = unname(efficiencies(result)[,'mean_eff']);
  }
  write.table(Effi0, file=paste0('Result/NRDEA0_ReducedForm_500_', Num_X, '_', rho, '.csv'), sep=',', append = TRUE, row.names = FALSE, col.names = FALSE);
  write.table(Effi, file=paste0('Result/NRDEA_ReducedForm_500_', Num_X, '_', rho, '.csv'), sep=',', append = TRUE, row.names = FALSE, col.names = FALSE);
}

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")