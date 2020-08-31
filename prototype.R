# simple logistic case

x <- rnorm(1000)

u <- 5*x + -log(-log(runif(1000)))


y <- ifelse(u > -log(-log(runif(1000))), 1, 0)

yhat <- exp(x) / (1+exp(x))

m <- glm(y ~ x, family = binomial('logit'))


# empirical share
mean(y)

# analytic form
mean(yhat)


summary(m)



# simple multinomial choice case ------------------------------------------
obs <- 10000
x1 <- rnorm(obs)
x2 <- rnorm(obs)
beta <- 2

u0 <- -log(-log(runif(obs)))
u1 <- x1*beta + -log(-log(runif(obs)))
u2 <- x2*beta + -log(-log(runif(obs)))

y <- case_when(
  
  u1 > u2 & u1 > u0 ~ 1,
  
  u2 > u1 & u2 > u0 ~ 2,
  
  u0 >= u1 & u0 >= u2 ~ 0
)

  
# empirical market share
table(y) %>% prop.table

# analytic share
yhat_1 <- exp(x1*beta) / ( exp(0) + exp(x1*beta) + exp(x2*beta) )
yhat_2 <- exp(x2*beta) / ( exp(0) + exp(x1*beta) + exp(x2*beta) )
yhat_0 <- exp(0) / ( exp(0) + exp(x1*beta) + exp(x2*beta) )

mean(yhat_0)
mean(yhat_1)
mean(yhat_2)
