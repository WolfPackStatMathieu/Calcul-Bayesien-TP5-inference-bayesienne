# Exercice 2
#Question 1
theta <- 1
a <-2
n<-100
X <- rnorm(n, theta)
Xobs <-X
Xobs[Xobs >=a]<-a
Xobs

#data
set.seed(1234)
x <- rnorm(n, theta, 1)
xobs <- x;
xobs[x>a] <-a


zobs <- (x>a)#Question 2 # les donnees censurees
sum(zobs)

## EMV (donnees non censurees
mean(x)
# 0.8432383

#Q2 EMV (donnees observees)
mean(xobs[!zobs])
#0.5476244

## Q3 - EM
#init
theta_old <- 0
iter <- 0
#convergence
tol <- 0.01
rule <- TRUE
maxiter <- 1000
while(rule){
  xnew <- xobs; #(x1, x2, ... xm, y1, y2, ... , y(n-m))
  xnew[zobs] <- theta_old + dnorm(a -theta_old)/(1 - pnorm(a - theta_old))
  tnew <- mean(xnew)
  iter <- iter +1
  rule <- (abs(tnew - theta_old) > tol) & (iter < maxiter)
  theta_old <- tnew
}


#comparaison
tnew ## EM 0.8196956




#Question 3

#Question 4
theta_old <- 0
N <- 1000
ech <- as.data.frame()
for (i in 1:N) {
  u <- runif(1, min = 0, max = 1)  
  q_a = pnorm(a - theta_old) + u * (1 - pnorm(a - theta_old))
  
  
}



#Exercice 3
n = 100
set.seed(123)
p<-runif(1)
tau <- rgamma(1, shape = 1, scale = 1/1)
m <- c(rnorm(1, sd = sqrt(1/tau)), rnorm(1, sd = sqrt(1/tau)))
z <- rbinom(n, size = 1, prob = p)
x <- rnorm(n, mean = z*m[2] + (1-z)*m[1], sd = 1) # z * m1 + (1-z) * m0

plot(density(x))





