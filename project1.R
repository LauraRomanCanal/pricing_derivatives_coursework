rm(list=ls())
library(ggplot2)

##################
### QUESTION 1 ###
##################

# Exercise 1
r <- 0.04
u <- 1.06
d <- 0.98
(1+r-d)/(u-d) 

# Exercise 2
S0 <- c(20)
S1 <- c(21.2,  19.6)
S2 <- c(22.472,  20.776,  19.208)
S3 <- c(23.82032,  22.02256, 20.36048, 18.82384)
S4 <- c(25.24954,   23.34391,  21.58211, 19.95327, 18.44736)

# Exercise 3
p <- c(0.75, 0.25)
V4 <- c((25.24953-20), (23.34391-20), (21.58211-20), 0, 0)
V3 <- c(p%*%V4[1:2], p%*%V4[2:3], p%*%V4[3:4], p%*%V4[4:5])/(1+r)
V2 <- c(p%*%V3[1:2], p%*%V3[2:3], p%*%V3[3:4])/(1+r)
V1 <- c(p%*%V2[1:2], p%*%V2[2:3])/(1+r)
V0 <- c(p%*%V1[1:2])/(1+r)

# Exercise 4
premFee <- (p[1]^4*V4[1] + p[1]^3*p[2]*4*V4[2] + p[1]^2*p[2]^2*6*V4[3] +
  p[1]*p[2]^3*4*V4[4] + p[2]^4*V4[5])*(1+r)^(-4)

# Exercise 5
a4 <- (u*V4[3] - d*V4[2]) / ((1+r)*(u-d))
b4 <- (V4[2] - V4[3])/(S3[2]*(u-d))

a3 <- (u*V3[3] - d*V3[2]) / ((1+r)*(u-d))
b3 <- (V3[2] - V3[3])/(S2[2]*(u-d))

a2 <- (u*V2[2] - d*V2[1]) / ((1+r)*(u-d))
b2 <- (V2[1] - V2[2])/(S1[1]*(u-d))

a1 <- (u*V1[2] - d*V1[1]) / ((1+r)*(u-d))
b1 <- (V1[1] - V1[2])/(S0[1]*(u-d))


##################
### QUESTION 2 ###
##################

# Part 1

Sn <- function(n,T = 5){
  # Create function that produces random walk with step size sqrt(T/n)
  stepsize <- sqrt(T/n)
  Z <- rbinom(n+1,1,0.5)
  Z[Z==1] <- stepsize
  Z[Z==0] <- -stepsize
  X <- rep(0,n+1)
  for ( i in 1:n+1) { 
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
  t <- seq(0,T,length.out = n+1)
  X <- sigma * B + mu * t
  return(exp(X))
}

brownian.drift <- function(n,T,sigma,mu){
  B <- Sn(n,T)
  t <- seq(0,T,length.out = n+1)
  X <- sigma * B + mu * t
  return(X)
}

brownian.bridge <- function(n,T){
  B <- Sn(n,T)
  t <- seq(0,1,length.out = n+1)
  X <- B - t * B[n]
}

martingale <- function(n,T) {
  B <- Sn(n,T)
  t <- seq(0,T,length.out = n+1)
  return(B^2 - t)
}

n <- 1000
T <- 1
t <- seq(0,T,length.out = n+1)

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

