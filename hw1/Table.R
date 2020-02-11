## This code is used to report outputs of autoSim.R in a form of table

## count output files of autoSim in folder
wd = getwd()
fileName <- dir(wd,pattern='n*.txt')
fileNum <- length(fileName)
rowNum <- fileNum/3

## initiate arguments and vectors
distTypes = c("gaussian", "t1", "t5")
N <- c()
N_ <- c()
data_1 <- c()
data_2 <- c()
data_3 <- c()
datalist_1 <- c()
datalist_2 <- c()
datalist_3 <- c()
MSE <- c()

## store MSE data of different distTypes and sample sizes separately
for (n in 1:10000){
  for (d in distTypes){
    Fname <- paste("n_",n,"_dist_",d,".txt",sep="")
    if (file.exists(Fname)==TRUE){
      MSE <- tryCatch(read.table(Fname,header=FALSE))
      if (d=="gaussian"){
        data_1 <- t(MSE)
        datalist_1 <- c(datalist_1,data_1)
      }else if (d=="t1"){
        data_2 <- t(MSE)
        datalist_2 <- c(datalist_2,data_2)
      }else if (d=="t5"){
        data_3 <- t(MSE)
        datalist_3 <- c(datalist_3,data_3)
      }
      # store sample sizes
      N <- c(N, rep(n,2))
    }
  }
}

# delete repeating N numbers
for (r in 1:(fileNum*2)){
  if (floor(r/3)==r/3){
    N_[r/3] = N[r]
  }
}

# set 2nd column by replication
M <- rep(c('PrimeAvg','SampAvg'),rowNum)

# stitch the table and present
table <- data.frame(n = N_, Method = M, Gaussian = datalist_1, t1 = datalist_2,
                    t5 = datalist_3)
# kable(table)



