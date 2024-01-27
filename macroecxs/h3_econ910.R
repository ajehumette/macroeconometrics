###################################
#H3: Bayesian Inference, Gibbs Sampling...
###################################

library(tstools)
set.seed(123)

#Jehu Mette
#HOMEWORK 3 - Econ 910
rm(list = ls())
#Q2

cpi.us.raw <- read.csv("CPIAUCSL.csv", header=TRUE)
dcpi.us <- diff(log(cpi.us.raw$CPIAUCSL))
dcpi.us <- ts(dcpi.us, start=c(1947,1), frequency=12)

#Bring in Canadian CPI 
cpi.can.raw <- read.csv("CPALCY01CAM661N.csv", header=TRUE)
dcpi.can <- diff(log(cpi.can.raw$CPALCY01CAM661N))
dcpi.can <- ts(dcpi.can, start=c(1961,1), frequency=12)

ar1.can <- tsreg(dcpi.can, lags(dcpi.can,1))
ar1.can

#Q3: Gibbs Sampling Algorithm

b.can <- coefficients(ar1.can)

s2.can <- (summary(ar1.can)$sigma)^2 #the s2 for Canada regression

nu.can<- length(dcpi.can) # degrees of freedom for Can regression.

N<-length(dcpi.us) # number of U.S. observations.

nu.top <- N + nu.can 

ar1.us <- tsreg(dcpi.us, lags(dcpi.us,1))
ar1.us
names(ar1.us)

sse = sum((fitted(ar1.us) - mean(dcpi.us))^2)
sse

ar1.us$residuals

s2.top <-( sse + nu.can*s2.can )/nu.top

#Conditionnal on a draw of h

###Dimension checking###
xx <- crossprod(as.matrix(dcpi.can))
head(xx)
dim(xx)
class(xx)

x.prime.y <- crossprod((dcpi.can), as.matrix(lags(dcpi.can,1)))
dim(x.prime.y)
class(x.prime.y)


xx.inv <- solve(crossprod(xx))
xx.inv
dim(xx.inv)


class(xx.inv)
class(b.can)

g <- xx.inv %*% b.can
class(g)
dim(g)

v.top <- (1/(xx.inv + h.draw*xx))

set.seed(123)

h.draw <- rgamma(1, 1/(s2.top)^2, nu.top)
class(h.draw)

p<-h.draw %*% x.prime.y
class(p)
dim(p)

m<-xx.inv + h.draw*xx
class(m)
dim(m)

v.top <- solve((xx.inv + h.draw*xx))
dim((v.top %*% ( xx.inv *  0.227 + h.draw %*% x.prime.y)))

####end of dimension checking###

# my delta is one

#b.can= 0.227

as.numeric((v.top %*% (xx.inv* 0.227+ h.draw %*% x.prime.y)))

rm(h.draw)

x.prime.y <- crossprod((dcpi.can), as.matrix(lags(dcpi.can,1)))

library(mvtnorm)
draws <- replicate(100000, {
  h.draw <- rgamma(1, 1/(s2.top)^2, nu.top)
  rmvnorm(1, as.numeric(solve((xx.inv + h.draw*xx)) %*% ( xx.inv *0.227 + h.draw %*% x.prime.y)) ,solve((xx.inv + h.draw*xx)) )
}, simplify="matrix")

####################


head(draws)
draws[7]
mean(draws)
sd(draws)
plot(density(draws), main="Posterior Distribution of US's Beta")

# Q4 :Check

ar1.us <- tsreg(dcpi.us, lags(dcpi.us,1))
ar1.us
#The OLS coefficient is 0.576 for US

ar1.can <- tsreg(dcpi.can, lags(dcpi.can,1))
ar1.can

#The OLS coefficient is 0.227 for Canada

#My posterior for beta.US is not between the two so there is a problem with my estimation.
#Dear Dr. Bachmeier, I have struggled a lot with the last two questions. I know that my output
#is not yet correct, but I do not know how to specify the following part.

library(mvtnorm)
draws <- replicate(100000, {
  h.draw <- rgamma(1, 1/(s2.top)^2, nu.top)
  rmvnorm(1, as.numeric(solve((xx.inv + h.draw*xx)) %*% ( xx.inv *0.227 + h.draw %*% x.prime.y)) ,solve((xx.inv + h.draw*xx)) )
}, simplify="matrix")


#I know its the whole point of the homework so I didn't how to ask for help without asking
#for the solution. I expect to have to revise it. Sincerely. Jehu.

########################################################################################

# REVISION OF HOMEWORK II #Fixing the recursion for the AR3

########################################################################################

#What I had before and was wrong
ar3sim <- function(result, element) {
  return(ahat + b1hat*result + b2hat*result + b3hat*result + rnorm(1,sd=sdhat))
}
#I understand tha tthis one returns a scalr and is wrong.


#What it should have been.
ar3sim <- function(result, element) {
  print(result)
  result.new <- c(ahat + b1hat*result + b2hat*result + b3hat*result + rnorm(1,sd=sdhat))
  return(result.new)
}

#I refrained from rewriting the whole code for space's sake.

#####################################################################################################
#END OF REVISION OF HOMEWORK II
####################################################################################################

