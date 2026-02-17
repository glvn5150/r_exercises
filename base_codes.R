#simulating data

set.seed(123)
n <- 100
x <- rnorm(n, 10, 2)
error <- rnorm(n, 0,1)
y <- 5 + 2 * x + error
df <- data.frame(x,y)

#--------------properties of arrays---------------
#-vectors : single variables
#-matrix : numeric linear algebra
#- data.frame : regression ready
#- ts : time series with frequency
#- list : model outputs

class(y)
class(x)
class(df) 
dim(df)
length(df)

#--------indexes : remove negative indices, logical indices filter, $is for---------
x[1] #first object
x[2:10] #2nd to 10
x[x>0] #all more than 0
df$y
df[,"y"]


#--------remove NaN datas---------
z <- is.na(df)
na.omit(z) #previous data is synthetic so no NaN's existence
x_square <- x^2
print(x_square)


#--------plotting---------
plot(y)
plot(x)
plot(x,y)
ts.plot(y)
ts.plot(x)
acf(y)
pacf(y)

#--------let's make equations---------
r ~ x1 + x2
r ~ .
r ~ x
r ~ x + I(x_square)
print(r)

#--------regressions : summary(m), resid(m), fitted(m)---------
x_matrix <- data.matrix(x)
y_matrix <- data.matrix(y)
lm.fit(x_matrix, y_matrix)
a <- 0.01
b <- 0.05
ols <- lm(y ~ x, df)

summary(ols)
resid(ols)
fitted(ols)
plot(resid(ols))
hist(resid(ols))

#--------TSA : ts(), diff(), acf(), pacf()---------
ts(x)
diff(x)
acf(x)
pacf(x)
names(ols)
coef(ols)
vcov(ols)

ar1 <- ar.ols(ts(x))
length(ar1)
summary(ar1)

