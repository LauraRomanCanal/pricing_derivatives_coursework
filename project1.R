rm(list=ls())
library(ggplot2)

##################
### QUESTION 1 ###
##################

r <- 0.04
u <- 1.06
d <- 0.98
(1+r-d)/(u-d) 

##################
### QUESTION 2 ###
##################

# Part 1

Sn <- function(n,T = 5){
  # Create function that produces random walk with step size sqrt(T/n)
  stepsize <- sqrt(T/n)
  Z <- rbinom(n,1,0.5)
  Z[Z==1] <- stepsize
  Z[Z==0] <- -stepsize
  X <- rep(0,n)
  X[1] <- Z[1]
  for ( i in 2:n) { 
    X[i] = X[i-1] + Z[i] }
  return(X)
}

# Now plot the process Sn(t) for n = 10,50,100,1000
y <- Sn(10)
x <- seq(0,5,length.out = length(y))
plot(y=y,x=x,type = 'l')

y <- Sn(50)
x <- seq(0,5,length.out = length(y))
plot(y=y,x=x,type = 'l')

y <- Sn(100)
x <- seq(0,5,length.out = length(y))
plot(y=y,x=x,type = 'l')

y <- Sn(10000)
x <- seq(0,5,length.out = length(y))
plot(y=y,x=x,type = 'l')

######################################
# PART 2
######################################

geometric.brownian <- function(n,T,sigma,mu){
  B <- Sn(n,T)
  t <- seq(0,T,length.out = n)
  X <- sigma * B + mu * t
  return(exp(X))
}

brownian.drift <- function(n,T,sigma,mu){
  B <- Sn(n,T)
  t <- seq(0,T,length.out = n)
  X <- sigma * B + mu * t
  return(X)
}

brownian.bridge <- function(n,T){
  B <- Sn(n,T)
  t <- seq(0,1,length.out = n)
  X <- B - t * B[n]
}

martingale <- function(n,T) {
  B <- Sn(n,T)
  t <- seq(0,T,length.out = n)
  return(B^2 - t)
}

n <- 1000
T <- 1
t <- seq(0,T,length.out = n)

simulations <- as.data.frame( 
  cbind(
    t,
    geometric.brownian(n,T,sigma = 1, mu = -0.5),
    geometric.brownian(n,T,sigma = 1, mu = 0.5),
    brownian.drift(n,T,sigma = 0.1, mu = 1),
    brownian.drift(n,T,sigma = 1, mu = 0.1),
    brownian.bridge(n,T),
    martingale(n,T)
  )
)

ggplot(data = simulations,aes(x=t,y=V2)) + 
  labs(x='t', y='Value of process', title = 'Processes related to Brownian motion') +
  geom_line(aes(x=t,y=V2,colour='Geometric mu = -.5, sigma=1')) + 
  geom_line(aes(x=t,y=V3,colour='Geometric mu = .5, sigma=1')) + 
  geom_line(aes(x=t,y=V4,colour='Drift mu = 1, sigma = 0.1')) + 
  geom_line(aes(x=t,y=V5,colour='Drift mu = 0.1, sigma = 1')) +
  geom_line(aes(x=t,y=V6,colour='Brownian Bridge')) +
  geom_line(aes(x=t,y=V7,colour='Martingale'))

