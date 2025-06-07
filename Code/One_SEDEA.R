rm(list = ls());
gc();
setwd("D:/BaiduSyncdisk/R/GDEA");
library(lpSolve);
library(deaR);

data_o <- read.csv(paste('Data/Data.csv', sep = ""),header = T);

data0 <- data_o;
for (i in 1:26) {
  # 获取当前行的最大值
  max_value <- max(data_o[i, ])
  # 将当前行的每个元素除以最大值
  data0[i, ] <- data_o[i, ] / max_value
}

#data0 <- data0 + 0.001;
data0[data0 < 0.001] <- 0.001;

Time_start <- Sys.time();    # Record time
N_test <- 1;         # Number of tests
Num_X <- 17;         # Number of Inputs
Num_Y <- 9;          # Number of Outputs

# Determine DMUs, inputs and outputs
dmunames <- c('BJ','TJ','HeB','SX','NMG','LN','JL','HLJ',
              'SH','JC','ZJ','AH','FJ','JX','SD','HeN','HuB',
              'HuN','GD','GX','HaiN','CQ','SC','GZ','YN','SX',
              'GS','QH','NX','XJ');
if (F){
dmunames <- c('BeiJing', 'TianJin', 'HeBei', 'ShanXi', 'NeiMengGu', 'LiaoNing', 'JiLin', 'HeiLongJiang',
              'ShangHai', 'JiangSu', 'ZheJiang', 'AnHui', 'FuJian', 'JiangXi', 'ShanDong', 'HeNan', 'HuBei',
              'HuNan', 'GuangDong', 'GuangXi', 'HaiNan', 'ChongQing', 'SiChuan', 'GuiZhou', 'YunNan', 'ShanXi',
              'GanSu', 'QingHai', 'NingXia', 'XinJiang');
}
inputnames <- paste0("X", seq(1, Num_X));
outputnames <- paste0("Y", seq(1, Num_Y));

# Determine the number of DMUs, inputs, and outputs
DMU <- length(dmunames) # Number of DMUs
Num_X <- length(inputnames) # Number of Inputs
Num_Y <- length(outputnames) # Number of Outputs

# Build the data format that deaR package needs
X= as.matrix(data0[1:Num_X,]);  
Y= as.matrix(data0[(Num_X+1):(Num_X+Num_Y),]); 
inputs <- matrix(X, nrow = Num_X, ncol = DMU, dimnames = list(inputnames, dmunames));
outputs <- matrix(Y, nrow = Num_Y, ncol = DMU, dimnames = list(outputnames, dmunames));
data_example <- make_deadata(inputs = inputs, outputs = outputs);

# Estimate
set.seed(12345);
result <- model_supereff(data_example,
                         orientation = "io",
                         rts = "crs");
Effi = unname(efficiencies(result));

write.table(Effi, file=paste0('Result/SEDEA_all.csv'), sep=',', row.names = FALSE, col.names = FALSE);

Time_end <- Sys.time();      # Record time
# Representing computation time in minutes
difftime(Time_end, Time_start, units = "min")