library(quantmod)
library(vars)
library(tseries)
library(zoo)
library(gmm)
#----create multivariate system----
getSymbols("^JKSE", src="yahoo")
price <- Ad(JKSE)
ret <- diff(log(price))
ret <- na.omit(ret)
acf(ret)

#---- roll day volatility for multivariate ----
vol <- rollapply(ret, width=20, sd, align="right")
vol <- na.omit(vol)

data_var <- na.omit(merge(ret, vol))
colnames(data_var) <- c("ret","vol")

par(mfrow=c(2,1))
plot(ret, main="JKSE Log Returns", col='gray')
lines(vol, col='red')

#----estimate VAR----
lagselect <- VARselect(data_var, lag.max=10, type='const')
lagselect$selection
var_model <- VAR(data_var, p=2, type='const')
summary(var_model)

#----impulse response function----
irf_res <- irf(var_model, impulse='ret', response='vol', n.ahead=20)
plot(ret, main="JKSE Log Returns")
acf(ret)
par(mfrow=c(2,1))
plot(ret, main="JKSE Log Returns")
acf(ret)
plot(ret, col='lightblue')
lines(vol, col='red')
plot(irf_res)

#----variance decomp----
fevd_res <- fevd(var_model, n.ahead=20)
fevd_res

#----spectral decomp----
spec_res <- spectrum(ret, log="no")
plot(spec_res)
pred <- predict(var_model, n.ahead=10)
forecast_ret <- pred$fcst$ret
forecast_vol <- pred$fcst$vol
plot(ts(forecast_ret), col='lightblue')
plot(ts(forecast_vol), col='red')

gmm_moment <- function(theta, data){
  y <- data[-1]
  x <- data[-length(data)]
  eps <- y - theta*x
  return(eps * x)
}

gmm_fit <- gmm(gmm_moment, x=as.numeric(ret), t0=0.1)
summary(gmm_fit)

#---value at risk---
VaR_95 <- quantile(ret, 0.05)
VaR_99 <- quantile(ret, 0.01)
VaR_param <- mean(ret) + sd(ret) * qnorm(0.05)

#roll_var
roll_VaR <- rollapply(ret, 250, function(x) quantile(x,0.05), align='right')
roll_VaR

plot(na.omit(roll_VaR))

#monte carlo simulation
mc_r <- rnorm(n=10000, mean=mean(ret), sd=sd(ret))
hist(mc_r)
sim_VaR <- quantile(mc_r[,1], 0.05)
length(mc_r)
sim_VaR <- quantile(mc_r, 0.05)
