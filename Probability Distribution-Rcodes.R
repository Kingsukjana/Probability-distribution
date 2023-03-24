# Probability Density Function.
# f(X)>=0 for all x and integral of f(x) over the range of x is equal 1.

# Support: The set of values at which the PDF of PMF has positive value.
# X ~ Binomial(n,p), Support(X) = {0,1,2,3..n}
# X ~ Poisson(lambda), Support(X) = {0,1,2....} 
# X ~ Normal(mu, sigma^2), Support(X) = {-Inf, +Inf}
# X ~ Exponential(lambda), Support(X)= {0, +Inf}


## Exponential distribution.

1. # How can we check whether a given exponential function is pdf

lamda = 2
Exponential_fun = function(x){
   2*exp( -2*x )
}
print(Exponential_fun)
# In this case the support is {0,+Inf}. We need to integrate the function over  
# the range "0" to "Inf" and show that the integral is one. 

out = integrate(Exponential_fun, lower = 0, upper = Inf) 
print(out)
value = out$value
print(value)
# Since the value is 1, so the density is a pdf.

curve(Exponential_fun(x), col ="blue", lwd=2, main="Exp(lamda=2)", 0, 10)
# We introduced two parameters "lwd' and "col", 
# "lwd" stands for line width and "col" stands for line colur. 

# How can we find the population mean(E(X)) and population variance(var(X)) of a 
#random variable x?
# E(X)= Integral x*f(x)
# Var(X)=E(X-E(X))^2= E(x^2)-(E(x))^2

## Mean and variance of exponential distribution

# a. Let's calculate the population mean= E(X), It is also called first order row moment.

f1 = function(x){
    x*2*exp(-2*x)}

print(mean1_f1)
mean = integrate(mean1_f1, 0, Inf)$value
print(mean)

# Locate the mean in the density curve.
abline(v = mean, col = "red", lwd = 3, lty= 1)

# b. Now we calculate the E(x^2), it is called second order row moment.

mean2_f1 = function(x){
  x^2*mean1_f1(x)
}
print(mean2_f1)
mean2 = integrate(mean2_f1, 0, Inf)$value
print(mean2)

#c. Population variance V(x) of the random variable X.

var = mean2 - (mean1)^2  # where, var=var(x), mean1= E(x), mean2=E(x^2)
print(var)
sqrt(var)  # Standard deviation.

## Normal distribution
mu = 3
sigma = 1

# Probability density function

fun_normal = function(x){
    (1/(sqrt(2*pi)*sigma))*exp((-(x - mu)^2/2*sigma^2))
}
print(fun_normal)
out_put = integrate(fun_normal, -Inf, +Inf)$value
print(out_put)
curve(fun_normal(x), -10, 10, col = "red", lwd=2, main = "Normal(3,1)") 
# Plot the normal density function over the range -10 to 10.
abline(v = mu, col = "blue", lwd = 2 ) 

##Mean and variance of normal distribution:

#a. Population mean=E(X), it is also called first order row moment.

fun_mean1_normal = function(x){
   x*fun_normal(x)
}
print(fun_mean1_normal)
mean1_normal = integrate(fun_mean1_normal, -Inf, +Inf)$value
print(mean1_normal)

#b.  E(X^2), second order row moment.

fun_mean2_normal = function(x){
   x^2*fun_normal(x)
}
print(fun_mean2_normal)
mean2_normal = integrate(fun_mean2_normal, -Inf, +Inf)$value
print(mean2_normal)

#c. Population variance of X,  Var(x)=E(x^2)-(E(x))^2.

var=mean2_normal- (mean1_normal)^2
print(var)
sqrt(var)  # Standard deviation.

## Discrete distribution

# a. Binomial distribution

# Binomial distribution has two parameters(n, p).
par(mfrow = c(2, 2))
n = 10
p = 0.5
Binom_fun = function(x){
  choose(10, x)*p^(x)*(1 - p)^(10 - x) }
print(Binom_fun)

x=1:n
Binom_prob = Binom_fun(x)
Binom_prob
plot(x,Binom_prob, type="h", col="blue", main = "Bin(10,0.5)", lwd=2)
points(x,Binom_prob, pch=19, cex=1.5, col="blue")

# b. Poisson distribution

# Poisson distribution has one parameter(lambda)

Poisson_lambda = n * p
Poisson_fun = function(x){
  exp( - Poisson_lambda )*( Poisson_lambda )^x/factorial(x)
}
Poisson_prob=Poisson_fun(x)
print(Poisson_prob)
plot(x,Poisson_prob, type="h",col="green", main = "Poisson(lambda)", lwd=2)
points(x,Poisson_prob,col = "green", pch=1.9,lwd=2 )
#legend("topright", c("Binomial","Poisson"),col=c("blue","green"),
#lwd=c(2,2),bty="n")

## Lets use some inbuilt functions for deal with probability distribution.

# The four important symbols  are 'r', 'd', 'p', 'p'.

x = round(rnorm(n = 30, mean=0, sd = 1), 2)
print(x)
hist(x, main = "Normal(mu = 0, sd = 1)", probability = TRUE)
curve(dnorm(x), add = TRUE, col="red", lwd = 2)
pnorm(2, mean = 0, sd = 1, lower.tail = TRUE)
qnorm(0.9772, mean = 0, sd = 1)
#  "rnorm" function is used to simulate random sample from normal distribution with 
# mu = 0 and standard deviation = 1.
# "dnorm" function for probability density function.
# "pnorm" for cumulative distribution function.
# "qnorm" quantile function. 

# Sample to population 
# Understanding distribution of data (histogram) and its connection
# to the underlying population density function.As the sample size 
# increases the histograms are getting closer to the population density function.
# More and more you are collecting  data from a particular population
# more and more accurate your histogram will be to the population density function.

par(mfrow = c(2, 2))
# The function "Par(mfrow)" is used to arranging the multiple plot in one plotting space.

## Exponential distribution:

# Histogram of sample of size(n) =100 

x = rexp(n = 100, rate = 1)  # Draw sample from the exponential distribution, size=100
print(x)
hist(x, probability = TRUE, main = "n = 100")
curve(dexp(x, rate = 1), add = TRUE, col ="red", lwd = 2)

# Histogram of sample of size (n)=500

x = rexp(n = 500, rate = 1)   # Draw sample from the exponential distribution, size=500
print(x)
hist(x, probability = TRUE, main = "n=500")
curve(dexp(x, rate = 1),add = TRUE, col="red", lwd = 2)

#Histogram of sample of size (n) = 1000

x= rexp(n = 1000, rate = 1) # Draw sample from the exponential distribution, size=1000
print(x)
hist(x, probability = TRUE, main = "n=1000")
curve(dexp(x, rate = 1), add = TRUE, col="red", lwd=2)

#Histogram of sample of size (n)=10000

x = rexp(n = 10000, rate = 1) # Draw sample from the exponential distribution, size=10000
print(x)
hist(x, probability = TRUE, main = "n=10000")
curve(dexp(x, rate = 1), add = TRUE, col = "red", lwd = 2)

# From above four histograms we can conclude that as the sample size increases the histogram are getting closer to the 
# probability density curve of exponential distribution.


##Normal distribution(Standard normal distribution)

par(mfrow = c(2, 2))

# Histogram of sample of size(n)=100

x = rnorm(n = 100, mean = 0, sd = 1)   # Draw sample from the normal distribution, size=100
hist(x, probability = TRUE, main= "n=100")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2, -5, 5)



# Histogram of sample of size(n)=500

x = rnorm(n = 500, mean = 0, sd = 1)   # Draw sample from the normal distribution, size=200
hist(x, probability = TRUE, main = "n=500")
curve(dnorm(x, mean= 0, sd = 1), add = TRUE, col = "red", lwd = 2, -5 ,5 )

#Histogram of sample of size(n)=1000

x = rnorm(n = 1000, mean = 0, sd = 1)   # Draw sample from the normal distribution, size=500
hist(x, probability = TRUE, main = "n=1000")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2, -5, 5)

# Histogram of sample of size(n)=10000

x = rnorm(n = 10000, mean = 0, sd = 1)  # Draw sample from the normal distribution, size=10000
hist(x, probability = TRUE, main = "n=10000")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2, -5, 5)

# From the above four histogram we can conclude as the sample size increases the histogram getting closer to the density function of 
#the normal distribution.











