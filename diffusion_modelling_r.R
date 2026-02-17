library(quantmod)

#----import data set----
getSymbols("^JKSE", src = "yahoo")
stocks <- JKSE
log_stocks <- na.omit(log(JKSE$JKSE.Close))
r <- na.omit(diff(log(JKSE$JKSE.Close)))  
r
#plot to see
par(mfrow=c(2,1))
plot(r, col='red')
plot(log_stocks, col='blue')

#change into array
log_stocks_arr <- data.matrix(log_stocks)
r_arr <- data.matrix(r)
r_arr
log_stocks_arr


#----log-likelihood for GBM----
ll_gbm <- function(params, r, dt){
  mu <- params[1]
  sigma <- params[2]
  m <- (mu -  0.5 * sigma^2)*dt
  s <- sigma * sqrt(dt)
  -sum(dnorm(r, mean=m, sd=s, log=TRUE))
}

#do parameters
init_params_gbm <- c(mean(log_stocks_arr), sd(log_stocks_arr))
init_params_gbm

#run optimization via 'optim(theta0, ll(x;theta), r=r, dt=1/T)'
ll_res_gbm <- optim(init_params_gbm, ll_gbm, r=log_stocks_arr, dt=1/252)


#----log-likelihood for OU----
ll_ou <- function(params, x, dt){
  kappa <- params[1]
  theta <- params[2]
  sigma <- params[3]
  n <- length(x)
  x_lag1 <- x[-n]
  x_now <- x[-1]
  m <- theta + (x_lag1 - theta)*exp(-kappa * dt)
  s2 <- (sigma**2)/(2 * kappa) *(1 - exp(-2 * kappa * dt))
  -sum(dnorm(x, mean =m, sd=sqrt(s2), log = TRUE))
}
#change into matrix

init_params_ou <- c(0.5, mean(r_arr), sd(r_arr))
init_params_ou 
ll_res_ou <- optim(init_params_ou, ll_ou, x=r, dt=1/252)

par_ll_gbm <- ll_res_gbm$par
par_ll_ou <- ll_res_ou$par

mu_hat <- par_ll_gbm[1]
sigma_hat <- par_ll_gbm[2]

kappa_hat <- par_ll_ou[1]
theta_hat <- par_ll_ou[2]
sigma_hat <- par_ll_ou[3]

#----simulate ll----
sim_gbm <- function(log_s0, mu, sigma, dt, n){
  l_path <- numeric(n)
  l_path[1] <- log_s0
  for(t in 2:n){
    l_path[t] <- l_path[t-1] + (mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  return(l_path)
}

sim_ou <- function(r0, kappa, theta, sigma, dt, n){
  path <- numeric(n)
  path[1] <- r0
  for(t in 2:n){
    path[t] <- path[t-1] + kappa * (theta - path[t-1]) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  return(path)
}

#simulate diffusion
n_sim <- length(r)
dt <- 1/252
r0 <- as.numeric(tail(r_arr,1))
s0 <- as.numeric(tail(log_stocks_arr, 1))

gbm_raw <- sim_gbm(s0, init_params_gbm[1], init_params_gbm[2], dt, n_sim)
gbm_opt <- sim_gbm(s0, mu_hat, sigma_hat, dt, n_sim)

ou_raw <- sim_ou(r0, init_params_ou[1], init_params_ou[2], init_params_ou[3], dt, n_sim)
ou_opt <- sim_ou(r0, kappa_hat, theta_hat, sigma_hat, dt, n_sim)

#----TSA and diffusion----
par(mfrow=c(2,1))
plot(as.numeric(r),col='gray')
lines(ou_opt, col='blue', lty=1)
lines(ou_raw, col='red', lty=1)

plot(as.numeric(log_stocks),col='gray')
lines(gbm_opt, col='blue', lty=1)
lines(gbm_raw, col='red', lty=1)





     