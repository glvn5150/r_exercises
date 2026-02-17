library(ggplot2)
library(plotly)
library(reshape2)

#---- simulate data ----
x <- rnorm(mean=0.1,sd=0.05,n=1000)*1000
x
hist(x)

#---- def likelihood ----
ll <- function(x0, params, mean, sd, n){
  mean <- params[1]
  sd <- params[2]
  n <- length(x)
  -sum(dnorm(x=x0, mean=mean, sd=sd, log=TRUE))
}

initial_params <- c(mean(x), sd(x))
res_opt <- optim(initial_params, ll, x=x)
mean_hat <- res_opt$par[1]
sd_hat <- res_opt$par[2]
mean_hat
sd_hat

ll_plot <- function(m,s,data){
  sum(dnorm(x=data, mean=m, sd=s, log=TRUE))
}

ll_vec <- Vectorize(ll_plot, vectorize.args=c('m', 's'))
m_vals<- seq(mean_hat - 10, mean_hat + 10, length.out =50)
s_vals<- seq(sd_hat - 5, sd_hat + 5, length.out =50)
grid<- expand.grid(mean=m_vals, sd=s_vals)
grid$loglik <- ll_vec(grid$mean, grid$sd, data=x)

#2d
ggplot(grid, aes(x = mean, y = sd, z = loglik)) + 
  geom_contour_filled() + 
  geom_point(aes(x = mean_hat, y = sd_hat), color = 'red', size = 3) +
  labs(title = 'Log-Likelihood Surface', subtitle = 'Red dot indicates MLE point', x = 'Mean (mu)', y = 'Standard Deviation (sigma)', fill = 'Log-Likelihood') +
  theme_minimal()
       
#3d
z <- matrix(grid$loglik, nrow=length(m_vals), ncol=length(s_vals))
persp(m_vals, s_vals, z, phi=30,theta=30,shade=0.5,col='lightblue', xlab='mean', ylab='sd', zlab='ll')

#3d-intr
z_matrix <- acast(grid, sd~mean, value.var='loglik')
plot_ly(x = m_vals, y = s_vals, z = ~z_matrix) %>% 
  add_surface() %>%
  layout(
    title = "3D Log-Likelihood Surface",
    scene = list(
      xaxis = list(title = "Mean"),
      yaxis = list(title = "SD"),
      zaxis = list(title = "Log-Lik")
    )
  )


