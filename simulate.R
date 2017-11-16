x <- 1:20
beta0 <- 3
beta1 <- 0.5
y <- beta0 + beta1*x
y1 <- beta0 + beta1 * x + rnorm(20, sd=1)
data.frame(x,y,y1)
plot(y~x)
plot(y1~x)
Y <- matrix(NA_real_, nrow=20, ncol=1000)
for(i in 1:1000) Y[,i] <- beta0 + beta1 * x + rnorm(20)
Y[,1:3]
par(mfrow=c(3,3))
for(i in 1:9) plot(Y[,i]~x, main=i)
estimator <- function(y)
{
  beta0 <- coef(lm(y~x))[[1]]
  beta0
}
Beta0 <- numeric(1000)
for(i in 1:1000) Beta0[i] <- estimator(Y[,i])
