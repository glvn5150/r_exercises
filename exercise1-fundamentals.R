library(quantmod)
library(tseries)

#---get stock data------
getSymbols("BMRI.JK", src='yahoo', from='2025-01-01')
stocks <- BMRI.JK
head(stocks)
df_s <- data.frame(stocks)
head(df_s)

#---get stock data summary------
close_stocks <- df_s["BMRI.JK.Close"]
s_mat <- data.matrix(df_s["BMRI.JK.Close"])
summary(close_stocks)
hist(data.matrix(close_stocks))


#---compute log returns and simple returns of stock data------
log_returns <- diff(log(s_mat))
simple_returns <- diff(s_mat)/ s_mat[-length(price)]

print(log_returns)
print(simple_returns)

plot(log_returns, type='l')
plot(simple_returns, type='l')

#----try indexing negative returns, last 20 prices, and dropping the first ----
neg_returns <- log_returns[log_returns < 0]
last20 <- log_returns[length(log_returns) - 19 : length(log_returns)]
drop_first <- log_returns[-1]

neg_returns
last20
drop_first

#---drop nan values of stock data------
is.na(log_returns)
na.omit(is.na(log_returns))

#---compute z_t = (r_t - rbar_t)**2 ------
rt <- ts(log_returns)
rbar <- mean(rt)
z <- (rt - rbar)**2
plot(rt)
plot(z)

rt_1 <- diff(rt)
plot(rt_1)

#----compute an ar_1 model----
ar1 <- tseries::arma(rt, order = c(1,0))
ar_1_summary <- summary(ar1)
resid(ar1)
hist(resid(ar1))
acf(rt)
pacf(rt)
pacf(log_returns)
pacf(close_stocks)

#---synthetic equation---
R <- lm(rt ~ lag(rt, 1) + I(lag(rt,1)^2))
R_summary <- summary(R)

R_summary
ar_1_summary
plot(resid(R))
plot(resid(ar1))

#----bonus:use a ljung-box test : H0 = rho1 =....=rho_20 =0
Box.test(rt, lag=20, type="Ljung-Box")
#at alpha=5%, dont reject. at alpha=10%, reject :D



