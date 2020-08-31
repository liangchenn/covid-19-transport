library(magrittr)
library(data.table)
library(dplyr)

# ============================== T-period model ==============================



# PARAMETERS
alpha = 1
beta  = .5

# TODO risk may change along time
alphaRj = c(.0005, .001)# risk coefficients 
BETA_Rj = c(.05, .05)# risk coefficients


# CONSTANTS
VMTr = c(164, 307)# distance (km)
Nr   = c(1881000/1e5, 2817000/1e5) # destination populations (100,000 ppl)

Nj     = c(1, 20)# number of contacts
PRICEj = c(3.5, 4.3)# Price per kilometer
Xj     = c(1, 3)# utility obtained from choosing j

BETAjr  = matrix(c(.5, 1, 1.5, 2), nrow = 2) + 4# utility of getting somewhere


# utility with time noise
utility <- function(yi, ROUTE, POST = 1, et1, et2, et3, et4){
  
  
  # j = 1
  j = 1
  uj1 = alpha* .01 * yi + ( (Xj[j] + et1)*beta - alpha*(PRICEj[j] + et2) - POST*alphaRj[j]*(Nj[j] + et3) )*VMTr[ROUTE]*.01 + BETAjr[j, ROUTE] - POST*BETA_Rj[j]*(Nr[ROUTE] + et4) + -log(-log(runif(1)))
  
  # j = 2
  j = 2
  uj2 = alpha* .01 * yi + ( (Xj[j] + et1)*beta - alpha*(PRICEj[j] + et2) - POST*alphaRj[j]*(Nj[j] + et3) )*VMTr[ROUTE]*.01 + BETAjr[j, ROUTE] - POST*BETA_Rj[j]*(Nr[ROUTE] + et4) + -log(-log(runif(1)))
  
  # j = 0
  uj0 = alpha* .01 * yi + -log(-log(runif(1)))
  
  
  res = which.max(c(uj1, uj2, uj0))
  res = ifelse(res == 3, 0, res)
  
  # sprintf("[%d] %.3f\n", 0:2, c(uj0, uj1, uj2)) %>% cat
  return(res)
}

# # In every period
# obs = 1e3
# # 假設每一期人的 Xj 會變動, (隨時間以及個人)
# data <- data.table(id=1:obs, y = rnorm(obs, 1e4, 100), t = 1, et = rnorm(1))
# 
# 
FOO <- Vectorize(utility)
# data[, decision := FOO(y, ROUTE = 1, POST = t > 50, et)]
# 


route1_list <- vector('list', length = 100)
for (i in 1:100) {
  obs <- 1000
  dt <- data.table(id = 1:obs, y = rnorm(obs, 1e4, 100), t = i, 
                   et1 = rnorm(1), et2 = rnorm(1), et3 = rnorm(1), et4 = rnorm(1))
  
  route1_list[[i]] <- dt
  
}
route1_dt <- do.call(rbind, route1_list)
rm(route1_list)
route1_dt[, decision := FOO(y, ROUTE = 1, POST = t>50, et1, et2, et3, et4)]


et_table <- route1_dt[, unique(.SD[, .(t, et1, et2, et3, et4)])]

dt <- route1_dt %>%
  as.data.frame() %>%
  group_by(t, decision) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  setDT()

dj0 <- dt[decision==0]
dj1 <- dt[decision==1]
dj2 <- dt[decision==2]

dj1$share1 <- dj1$share / dj0$share
dj2$share2 <- dj2$share / dj0$share


data <- et_table[dj2, on = .(t)]

data[, `:=`(Xj = 1.64*(Xj[2]+et1), Pj = 1.64*(PRICEj[2] + et2), Nj = 1.64*(Nj[2] + et3), Nrj = Nr[1] + et4, POST = ifelse(t > 50, 1, 0))]


m <- data %$% lm(log(share2) ~ Xj - Pj - POST*Nj + POST*Nrj)
summary(m)
# 可以估計出 beta 部分

