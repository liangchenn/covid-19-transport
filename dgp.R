library(data.table)
library(magrittr)
# pandemic version --------------------------------------------------------
# no variation for`post`
obs <- 5000


xj <- c(1, 2)
beta <- .5

alpha <- 1
Pj <- c(3.5, 4.3)

XIj <- c(3.5, 4.35)

gamma <- 0.5

rgev <- function(n=1){
  return(-log(-log(runif(n))))
}

u <- function(j, et1, et2, Post, covid_risk){
  ((xj[j] + et1)*beta - alpha*(Pj[j] + et2) + XIj[j]) - Post*gamma*covid_risk
}


decision <- function(et1, et2, Post, covid_risk){
  
  u0 <- rgev()
  u1 <- u(1, et1, et2, Post, covid_risk) + rgev()
  u2 <- u(2, et1, et2, Post, covid_risk) + rgev()
  
  res <- which.max(c(u0, u1, u2)) - 1
  
  return(res)
}

decision <- Vectorize(decision)

all_data <- data.table()
T_ <- 1000
ptm <- proc.time()
pb <- txtProgressBar(min = 0, max = T_, style = 3)
for (t in 1:T_) {
  
  et1 <- rnorm(1, 0, 1)
  et2 <- rnorm(1, 0, 1)
  covid_risk <- rpois(1, lambda = 5)
  
  tmp_data <- data.table(id=1:obs, et1=et1, et2=et2, covid_risk=covid_risk)
  
  tmp_data[, decision := decision(et1, et2, t>(T_/2), covid_risk)][, period := t]
  
  all_data <- rbind(all_data, tmp_data)
  
  setTxtProgressBar(pb, t)
  
}
proc.time() - ptm


agg_data <- all_data[,{
  n = .N
  .SD[, .(share = .N / sum(n)), by=decision]
}, by=.(period, et1, et2, covid_risk)][order(period, decision)]





# Parallel Version --------------------------------------------------------

# Utilize R's foreach and doParallel package to accelerate the data genrating process.

# redefine decision to un-vectorized version,
# since vectorized function could yeild error in foreach loop.
decision <- function(et1, et2, Post, covid_risk){
  
  u0 <- -log(-log(runif(1)))
  u1 <- u(1, et1, et2, Post, covid_risk) + -log(-log(runif(1)))
  u2 <- u(2, et1, et2, Post, covid_risk) + -log(-log(runif(1)))
  
  res <- which.max(c(u0, u1, u2)) - 1
  
  return(res)
}

# number of periods
T_ <- 1000

# use multiple cores
cl = makeCluster(4)
registerDoParallel(cl)


ptm <- proc.time()
res = foreach(t = 1:1000, .packages = "data.table") %dopar% {
  
  et1 <- rnorm(1, 0, 1)
  et2 <- rnorm(1, 0, 1)
  covid_risk <- rpois(1, lambda = 5)
  tmp_data <- data.table(id=1:obs, et1=et1, et2=et2, covid_risk=covid_risk)
  
  tmp_data[, decision := mapply(decision, et1=et1, et2=et2, Post=t>(T_/2), covid_risk=covid_risk)]
  
  tmp_data
  
  
}
stopCluster(cl)
proc.time() - ptm


# compare of time :

# original version   : 179.4 sec
# paralleled version :  44.4 sec
