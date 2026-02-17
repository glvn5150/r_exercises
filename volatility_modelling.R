library(quantmod)
library(tseries)

#----import data set----
getSymbols("^JKSE", src = "yahoo")
stocks <- JKSE
head(JKSE)
summary(JKSE)
df_s <- data.frame(stocks)
df_s

returns <- diff(log(data.matrix(df_s["JKSE.Close"])))
plot(returns, type='l')
hist(returns)
hist(returns^2)
r <- na.omit(returns)
is.na(r)
length(r)

#--compute mean vs variance dependence-----
acf(r)
acf(r^2)

#----compare abs(r) with r^2------
abs_r <- abs(r)

plot(abs_r, type='l')
plot(r^2, type='l')
summary(abs_r)
summary(r^2)
#absolute values explodes faster

#----AR(1) dosen't fix volatility : test an AR(1) with the Box Test------
ar1_r <- tseries::arma(r, order = c(1,0))
eps <- resid(ar1_r)
hist(eps)
Box.test(eps, lag=20, type="Ljung-Box")
Box.test(eps^2, lag=20, type="Ljung-Box")
#p-value is small, so we reject the null hypothesis that there is no volatility clustering
#eps^2 pval < 2.2e16, so we reject the null hypothesis that there is no volatility clustering
#eps pval = 0.3599, so we fail to reject the null hypothesis that there is no autocorrelation in residuals

#----why does volatility clustering matter------
plot.ts(r)
plot.ts(r^2)

#------run lag sensitivity via box.test-------
Box.test(r^2, lag=5, type="Ljung-Box")
Box.test(r^2, lag=20, type="Ljung-Box")
#lag 5 has smaller Xsquare(1284.7) than lag 20(2742.9), this means the deviations are bigger

#------IID counterfactual-------
z_r <- rnorm(length(r))
acf(z_r^2)
acf(r^2)
#z_r^2 has no autocorrelation, while r^2 has significant autocorrelation, this means that the returns are not IID and there is volatility clustering in the data


