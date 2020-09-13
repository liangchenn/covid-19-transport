# 100-period data generating process

library(data.table)
library(magrittr)
library(dplyr)
library(dtplyr)


# Model -------------------------------------------------------------------

# u_ij = (xj*b + a*Pj - r*Post - alpha_riskj*Post + XIj) * Ri + epsilon

# Functions ---------------------------------------------------------------

# Gumbel Extreme Value distribution
rgev <- function(n=1){
  return(-log(-log(runif(n))))
}

# utility function
u <- function(j, Ri, Post_t){
  (xj[j]*beta - alpha*Pj[j] - gamma*Post_t - Post_t*alpha_risk_j[j] + XIj[j])*Ri
}


# decision function

decision <- function(Ri, Post_t=1){
  
  u0 <- rgev()
  u1 <- u(1, Ri, Post_t) + rgev()
  u2 <- u(2, Ri, Post_t) + rgev()
  
  res <- which.max(c(u0, u1, u2)) - 1
  
  return(res)
}

# Vectorized decision function

Decision <- Vectorize(decision)


# Parameters ---------------------------------------------------------------

xj <- c(1, 2.2)-.5
beta <- .5

alpha <- 1
Pj <- c(3.5, 4.3)

gamma <- .1

alpha_risk_j <- c(.1, .25)

XIj <- c(3.5, 3.5)



# Data --------------------------------------------------------------------

# 假設有 100 期，每一期有1000名消費者存在市場。
obs <- 1000
Routes <- c(7, 11)
grids <- expand.grid(id=1:obs, t=1:100)
data <- setDT(grids)
rm(grids)

# 每一名消費者從 2 種路線中隨意抽一路線
data[, Ri := sample(Routes, size = .N, replace = T)]

# 疫情爆發於 t=51
data[, Post_t := ifelse(t>50, 1, 0)]

# 決定搭乘 {j=0 : 不出門, j=1: 汽車, j=2: 高鐵ㄋ}
data[, decision := Decision(Ri, Post_t)]


# 統計每一期、每一種路線選擇市佔率

dt <- data %>%
  lazy_dt() %>%
  group_by(t, Ri, decision) %>%
  summarise(n = n()) %>%
  mutate(share = n / sum(n)) %>%
  as.data.table()


