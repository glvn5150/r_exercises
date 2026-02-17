library(quantmod)
library(bpsr)
library(plotly)

getSymbols("^TYX", src = "yahoo")
data <- na.omit(TYX)
yields <- data$TYX.Close
yields
yields_arr <- data.matrix(yields)
summary(yields)
yields_norm <- yields_arr / yields_arr[1,]
yields_norm


par(mfrow=c(2,1))
plot(yields)

ou_sim <- function(r0, kappa, theta, sigma, dt, n){
  r_path <- numeric(n)
  r_path[1] <- r0
  for(t in 2:n){
    r_path[t] <- kappa * (theta - r_path[t-1]) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  return(r_path)
}



ar1 <- tseries::arma(x=yields_norm, order=c(1,0))
ar1$coef[1]

ou_raw <- ou_sim(r0=yields_norm[1], kappa=ar1$coef[1], theta=mean(yields_norm), sigma=sd(yields_norm), dt=1/252, length(yields_norm))
ou_raw_ts <- ts(ou_raw)

acf(yields_norm)
acf(ou_raw)

yield_ts <- ts(yields_norm)
plot(yield_ts)
plot(ou_raw_ts)

par(mfrow=c(2,1))
plot(yield_ts, col='gray')
lines(ou_raw_ts, col='red')



