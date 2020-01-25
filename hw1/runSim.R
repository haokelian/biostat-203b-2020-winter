## The code is used for automatic simulation based on given seed and number of replicates
## Arguments: nVals (sample size), disTypes (distribution)
##            seed (random seed by input) and rep (number of replicates by input)
## Output: report the average mean squared error (MSE)
##         of primed-indexed average estimator  and the classical sample average estimator

## parsing command arguments
for (arg in commandArgs(TRUE)){
  eval(parse(text=arg))
}

## check if a given integer is prime
isPrime = function(n){
  if (n<=3){
    return (TRUE)
  }
  if (any((n %% 2: floor(sqrt(n)))==0)){
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indicies
estMeanPrimes = function(x){
  n = length(x)
  ind = sapply(1:n,isPrime)
  return (mean(x[ind]))
}

## generate seed
set.seed (seed)

## calculate MSE
mseSampAvg = 0
msePrimeAvg = 0

for (r in 1:rep){
  # simulate data
  if (dist == "gaussian"){
    x = rnorm(n)
  } else if (dist == "t1"){
    x = rt(n,df=1)
  } else if (dist =="t5"){
    x = rt(n,df=5)
  } else{
    stop("unrecohnized distribution")
  }
  # two methods
  mseSampAvg = mseSampAvg + mean(x)^2
  msePrimeAvg = msePrimeAvg + estMeanPrimes(x)^2
}

cat(msePrimeAvg/rep,"\t",mseSampAvg/rep,"\n")
