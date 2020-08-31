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
