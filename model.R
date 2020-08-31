# model.R simulates the choices of people on transportation decisions
# during the COVID-19 pandemic.

library(data.table)
library(magrittr)

# ====================================== SETTINGS ======================================

# CONSTANTS ---------------------------------------------------------------

VMTr = c(164, 307)# distance (km)
Nr   = c(1881000/1e5, 2817000/1e5) # destination populations (100,000 ppl)

Nj     = c(1, 20)# number of contacts
PRICEj = c(3.5, 4.3)# Price per kilometer
Xj     = c(1, 3)# utility obtained from choosing j

BETAjr  = matrix(c(.5, 1, 1.5, 2), nrow = 2) + 4# utility of getting somewhere


# PARAMETERS --------------------------------------------------------------
alpha = 1
beta  = .5

# TODO risk may change along time
alphaRj = c(.0005, .001)# risk coefficients 
BETA_Rj = c(.05, .05)# risk coefficients
 


# Analytic Form of Market Share -------------------------------------------
Analytic <- function(j, ROUTE, POST = T){
  
  e0 = exp(0)
  e1 = exp(( Xj[1]*beta - alpha*PRICEj[1] - POST*alphaRj[1]*Nj[1] )*VMTr[ROUTE]*.01 + BETAjr[1, ROUTE] - POST*BETA_Rj[1]*Nr[ROUTE])
  e2 = exp(( Xj[2]*beta - alpha*PRICEj[2] - POST*alphaRj[2]*Nj[2] )*VMTr[ROUTE]*.01 + BETAjr[2, ROUTE] - POST*BETA_Rj[2]*Nr[ROUTE])
  
  e  = c(e0, e1, e2)
  
  res = e[(j+1)] / (e0 + e1 + e2)
  
  return(res)
}


# UTILITY -----------------------------------------------------------------

# Utility Function
utility <- function(yi, ROUTE, POST = 1){
  
  
  # j = 1
  j = 1
  uj1 = alpha* .01 * yi + ( Xj[j]*beta - alpha*PRICEj[j] - POST*alphaRj[j]*Nj[j] )*VMTr[ROUTE]*.01 + BETAjr[j, ROUTE] - POST*BETA_Rj[j]*Nr[ROUTE] + -log(-log(runif(1)))
  
  # j = 2
  j = 2
  uj2 = alpha* .01 * yi + ( Xj[j]*beta - alpha*PRICEj[j] - POST*alphaRj[j]*Nj[j] )*VMTr[ROUTE]*.01 + BETAjr[j, ROUTE] - POST*BETA_Rj[j]*Nr[ROUTE] + -log(-log(runif(1)))
  
  # j = 0
  uj0 = alpha* .01 * yi + -log(-log(runif(1)))
  
  
  res = which.max(c(uj1, uj2, uj0))
  res = ifelse(res == 3, 0, res)
  
  # sprintf("[%d] %.3f\n", 0:2, c(uj0, uj1, uj2)) %>% cat
  return(res)
}

# Vectorized Utility Function
Utility <- Vectorize(Utility)



# ====================================== DATA GENERATING PROCESS ======================================

# Here we first consider 4 scenarios : (Pre, Post-pandemic) x (Route 1, Route 2).

# Number of observation in each scenario
obs = 1e4

# (1) Pre Pandemic -----------------------------------------------------------
# (1-1) MARKET 1  ROUTE = 1, POST = 0

yi  = rnorm(obs, 1e4, 100)

data = data.table(id = 1:obs, y = yi, route = 1)

data[, decision := Utility(y, route, POST = F)]

# empirical market share
prop.table(table(data$decision))

# analytic form
Analytic(j=0, 1, F)
Analytic(j=1, 1, F)
Analytic(j=2, 1, F)


# (1-2) MARKET 2 ROUTE = 2, POST = 0

yi  = rnorm(obs, 1e4, 100)

data = data.table(id = 1:obs, y = yi, route = 2)

data[, decision := Utility(y, route, POST = F)]

# empirical market share
prop.table(table(data$decision))

# analytic form
Analytic(j=0, 2, F)
Analytic(j=1, 2, F)
Analytic(j=2, 2, F)




# (2) Post Pandemic -----------------------------------------------------------


# (2-1) MARKET 1  ROUTE = 1, POST = 1

yi  = rnorm(obs, 1e4, 100)

data = data.table(id = 1:obs, y = yi, route = 1)

data[, decision := Utility(y, route)]

# empirical market share
prop.table(table(data$decision))

# analytic form
Analytic(j=0, 1)
Analytic(j=1, 1)
Analytic(j=2, 1)


# (2-2) MARKET 2 ROUTE = 2, POST = 1

yi  = rnorm(obs, 1e4, 100)

data = data.table(id = 1:obs, y = yi, route = 2)

data[, decision := Utility(y, route)]

# empirical market share
prop.table(table(data$decision))

# analytic form
Analytic(j=0, 2)
Analytic(j=1, 2)
Analytic(j=2, 2)

