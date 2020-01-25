## The code is used for automatic simulation based on given seed and number of replicates
## Arguments: nVals (sample size), disTypes (distribution)
##            seed (random seed by input) and rep (number of replicates by input)
## Output: report the average mean squared error (MSE)
##         of primed-indexed average estimator  and the classical sample average estimator

## generate sample sizes and distribution types automatically
nVals <- seq(100, 500, by=100)
distTypes = c("gaussian", "t1", "t5")
seed <- 203
rep <- 50

## simulate with runSim.R
for (dist in distTypes){
  for (n in nVals) {
    arg <- paste("n=", n, " dist=", shQuote(shQuote(dist))," seed=", seed, " rep=", rep, sep="")
    oFile <- paste("n_", n,"_dist_", dist, ".txt", sep="")
    sysCall <- paste("nohup Rscript runSim.R ", arg, " > ", oFile, sep="")
    system(sysCall, wait = FALSE)
   print(paste("sysCall=", sysCall, sep=""))
  }
}
