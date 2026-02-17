library(quantmod)
library(tseries)

getSymbols("^JKSE", src = "yahoo")
stocks <- JKSE
log_stocks <- na.omit(log(JKSE$JKSE.Close))
r <- na.omit(diff(log(JKSE$JKSE.Close)))  
r
r_array <- data.matrix(r)

ar1 <- arma(r, order=c(1,0))
sr <- sharpe(r_array)
sr

ou_base <- sim_ou(x0= a=ar1$coef[1], b=mean(r), )

#ou : x_t = a(b - x_{t-1})dt + sigma*sqrt(dt)
sim_ou <- function(x0,a,b,sigma, dt, n){
  paths <- numeric(n)
  paths[1] <- x0
  for(t in 2:n){
    paths[t] <- a(b - paths[t-1]) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  return(paths)
}

