library(tstools)
set.seed(123)

#Jehu Mette
#HOMEWORK 2 - Econ 910
rm(list = ls())
#Q1 Estimate AR3 ,compute half life and 95% C.I  using bootstrap.
cpi.raw <- read.csv("CPIAUCSL.csv", header=TRUE)
cpi <- ts(cpi.raw[,2], start=c(1947,1), frequency=12)
tr<- make.trend(cpi)
ar3 <- tsreg(cpi, lags(cpi, 1:3))
summary(ar3)

#Half life

ar3pred <-function(result, element) {
  print(result)
  result.new <- c(1.578*result[1] + (-0.734)*result[2] + 0.156*result[3], result[1], result[2])
  return(result.new)
}

start=c(1.0, 0.0, 0.0)
fcsts <- Reduce(ar3pred, 1:30, init=start,accumulate=TRUE)


sapply(fcsts, function(element) { element[1] })
irf <- sapply(fcsts, function(element) { element[1] })
plot(ts(irf) , main="IRF Half Life of CPI")
irf < 0.5
min(which(irf < 0.5))

#The shock to CPI does not die even after 30 months 

# Bootstrap and 95% CI

ar3 <- tsreg(cpi, lags(cpi,1:3))
ar3
ahat <- 0.0748
b1hat <- 1.578
b2hat <- -0.734
b3hat <- 0.156

sdhat <- sd(ar3$resids)
# This is not correct. You have an AR(3) model. This function returns a scalar and
# then it uses the same scalar for the t-1, t-2, and t-3 terms on the right side
# of the equation. The function has to return a vector and then you have to pull out
# the right elements to do the calculation.
ar3sim <- function(result, element) {
  return(ahat + b1hat*result + b2hat*result + b3hat*result + rnorm(1,sd=sdhat))
}



simoutput <- replicate(1000, {
  cpisim <- ts(Reduce(ar3sim, 1:200, init=0.0, accumulate=TRUE))
  fit <- tsreg(cpisim, lags(cpisim,1:3))
 
  b1hat<- coefficients(fit)[2]
  b2hat<- coefficients(fit)[3]
  b3hat<- coefficients(fit)[4]
  ar3fos <- function(result, element) {
    result.n <- c(b1hat*result[1] - b2hat*result[2] + b3hat*result[3], result[1], result[2])
    return(result.n)
  }
  
  irf <- sapply(fcsts, function(element) {element[1]})
  half.life <- which(irf < 0.5)
  hl<- if(length(half.life)==1){which.min(half.life)} else {irf}
}, simplify="array")

quantile(simoutput, prob=c(0.025, 0.975))

# My lower bound is 1 and my upper bound is 1.76 for the IRF

########Question 2
length(cpi)
#Investigating unkown break date using rule of thumb 15% in each regime
floor(0.15*length(cpi))
t1 <- time(cpi)[132]
t1 #the time period associated with the first regime is september 1957
floor(0.85*length(cpi))
t2 <- time(cpi)[751]
t2 # the time period associated with the second regime is April 2009

# We now set our possible break somewhere in between this window of 1957-2010
possible.breaks <- dates(t1, t2, 12) #including the two extremes and the frequency
possible.breaks

#The logic is now to do back to back around 637 structural break 
#tests for each of these dates


#Now we need an efficient procedure to return the f-stat associated with each test.

f.procedure <- function(break.date) {
  d <- time.dummy(cpi, break.date, "end") 
  rhs <- ts.intersect(lags(cpi, 1:3), d*lags(cpi, 1:3))
  rhs1 <- rhs[,1:3]
  fit.large <- tsreg(cpi, rhs)
  fit.small <- tsreg(cpi, rhs1)
  test <- waldtest(fit.large, fit.small)
  return(test[2,3]) # F-stat is at this location in the matrix
}

# Using the procedure on all possible structural break dates
fstats <- sapply(possible.breaks, f.procedure)
fstats
plot(ts(fstats))

# most likely date of structural break
which.max(fstats)
possible.breaks[396]

#The US CPI most likely experienced a structural break in August 1990.

#Note that what we obtain cannot be considered like an f-stat distributio we need
#to go further.

library(strucchange)
dataset <- ts.combine(cpi, lags(cpi,1:3))
colnames(dataset) <- c("cpi", "cpi1", "cpi2","cpi3")
model <- cpi ~ cpi1 + cpi2 + cpi3
fs <- Fstats(model, from=c(1957,9),
             to=c(2010,10), data=dataset)
names(fs)
plot(fs, aveF=TRUE)
sctest(model, type="expF", from=c(1957,9),
       to=c(2010,10), data=dataset)

#here we can reject the null hypothesis and conclude that it is likely that the 
# the 1990 structural break is genuine.

#Q3:The case of two structural breaks at unknown dates

#With a total of T=884 observations

#T1=396 for best single break

#T2=132+396=528 first possible second break
#T2=523 second possible second break
#T2=524 third possible second break
#... so on and so forth
#T2=751

# Then I select the highest F-stat from these possibilities.

#Model 1 T1 = 396 T2 = 523 (Two dummy variables)
#Model 2 T1 = 396 T2 = 524 (Two dummy variables)
#Model 3 T1 = 396 T2 = 525 (Two dummy variables)
#...
#Model n T1 = 396 T2 = 751 (Two dummy variables)


########################################################################################

# REVISION OF HOMEWORK I

########################################################################################
  
#Comment: I am still unsure what an .R extension is for a file. I looked on the web but could not find something that made sense.
#I will contact you to discuss it.

library(mFilter)
library(tstools)
set.seed(123)

#Jehu Mette
#HOMEWORK 1 (Revised) - Econ 910

rm(list = ls())

#73 years of monthly CPI from FRED

#####PLOT AND COMMENTS
cpi.raw <- read.csv("CPIAUCSL.csv", header=TRUE)
names(cpi.raw)[1] <- "date"
names(cpi.raw)[2] <- "cpi"
cpi <- ts(cpi.raw$cpi, start=c(1947,1), frequency=12)
tr<- make.trend(cpi)
tsreg(cpi, tr)
plot(cpi,main="U.S Consumer Price Index 1947-2020, Monthly",
     xlab="Date", ylab="CPI")


#RANDOW WALK??
tsreg(cpi,lags(cpi,1))
#Yes as cpi_t = 0.129 + 1.001*cpi_(t-1)

#Estimation with time trend

logcpi<-log(cpi)
dcpi <- diff(logcpi)

plot(dcpi,main="Log differenced U.S Consumer Price Index 1947-2020",
     xlab="Date", ylab="Differenced CPI")

cpi.w.trend<-tsreg(cpi, ts.combine(lags(cpi,1), tr))
cpi.w.trend

dcpi.ar1<- tsreg(dcpi, lags(dcpi,1))
dcpi.ar1

#Detrended serie
fit<- lm(cpi~ I(1:length(cpi)))
cpi.detrend <- residuals(fit)
cpi.detrend<-ts(cpi.detrend, start=c(1947,1), frequency=12)


#####HP filter decomposition

#CPI= trend + cycle

hcpi <-hpfilter(cpi,freq=100)
plot(hcpi$trend)
plot(hcpi$cycle)
plot(cpi.detrend)

#Comparing them, the cyclical component and the trend hardly look similar.

######Hamilton's alternative

plot(dcpi)
fit <- tsreg(cpi, lags(cpi, 8:11))
trend <- fit$fitted
cycle <- fit$resids
plot(cycle)
ct <- ts.combine(cycle, hcpi$cycle)
plot(ct, plot.type="single", main="Hamilton Cyclical and HP Filter Cyclical (Purple)",
     lty=c(1,1), col=c("black","purple"), ylab="")
abline(h=0,lty=3,col="black",lwd=2)
par(new="FALSE")

#These are two different cyclical series the two approaches. 

#####################################################################################################
#END OF REVISION OF HOMEWORK I
####################################################################################################

