## General comments
##
## You need to revise this. You don't seem to understand the HP filtering discussion
## at all.
##
## You need to provide a program that runs. This one doesn't. You defined your
## cpi variable out of order. I'm not sure how you did the homework if the
## program doesn't run.
##
## Give your files a .R extension. If you have no extension, you won't know the file
## type. Some tools don't even work without a proper file extension.

## You shouldn't need the vars package - this is purely a univariate exercise.
# library(vars)
library(tstools)
library(mFilter)

## Shouldn't need this because we didn't generate any random numbers on this homework
# set.seed(123)

#Jehu Mette
#HOMEWORK 1 - Econ 910

rm(list = ls())

#73 years of monthly CPI from FRED

#PLOT AND COMMENTS
cpi.raw <- read.csv("CPIAUCSL.csv", header=TRUE)
names(cpi.raw)[1] <- "date"
names(cpi.raw)[2] <- "cpi"

## What you had was clumsy. Just refer to the column while creating a ts.
cpi <- ts(cpi.raw[,2], start=c(1947,1), frequency=12)

## Are you creating a trend variable? Just do this
#cpi.raw$time <-I(1:length(cpi.raw[,2]) )
tr <- make.trend(cpi)

## A couple questions
## 1. What happens if you return to this later? Will you remember which variable is in which column?
## 2. What if you make a change in your program above? Will you always remember to change this
## code? There's a high probability that you won't make all the necessary changes. Best case
## scenario is that your program won't run.
## Either refer to the variables by name or form the trend outside of your data.
#tsreg(cpi[,2], cpi[,3])
tsreg(cpi, tr)

## Note that cpi.raw is a data frame, not a ts object
## That means you can lose any ts properties of the variables
cpi.raw$cpi_detrend <-cpi.raw$cpi -0.3*cpi.raw$time
plot(cpi[,2],main="U.S Consumer Price Index 1947-2020, Monthly",
     xlab="Date", ylab="CPI")

## I don't think this is going to work as expected because the coefficient is greater than one
## tsreg doesn't have that problem
#RANDOW WALK??
#cpi_ar1<-arima(cpi, order=c(1,0,0))
#cpi_ar1
tsreg(cpi, lags(cpi,1))
#Estimation with time trend

## Note that in practice you'd normally take the log diff to get a percentage change
dcpi <- diff(cpi)
plot(dcpi,main="Differenced U.S Consumer Price Index 1947-2020",
     xlab="Date", ylab="Differenced CPI")

## The use of numerical indexes makes this hard to follow
#cpi_w_trend<-tsreg(cpi[,2], ts.combine(lags(cpi[,2],1), cpi[,3]))
cpi_w_trend <- tsreg(cpi, ts.combine(lags(cpi,1), tr))
cpi_w_trend

## This is a confusing name. It looks like the difference of the CPI
##cpi_diff<-arima(dcpi[,2], order=c(1,0,0))
##cpi_diff
dcpi.ar1 <- arima(dcpi, order=c(1,0,0))
dcpi.ar1

#HP filter decomposition
## This is not what we did in class
## freq is not the frequency of the data
## freq corresponds to lambda, which is usually at least 100
hcpi <-hpfilter(cpi,freq=12)
head(hcpi$cycle)

par(mfrow=c(3,1))
plot( cpi[,4] ,xlab="", ylab="",main="CPI Detrended", lwd=2)
abline(h=0,lty=3,col="blue",lwd=2)
par(new="FALSE")
plot(hcpi$cycle,xlab="", ylab="",main="CPI Cycle", lwd=2)
abline(h=0,lty=3,col="blue",lwd=2)
par(new="FALSE")
plot(hcpi$trend ,xlab="", ylab="",main="CPI Trend", lwd=1 )
par(mfrow=c(1,1))

#Hamilton's alternative

## This is nothing but an AR(11) model. You better review the HP filtering
## lectures and Hamilton's paper and redo this.
fit <- tsreg(cpi, lags(cpi, 1:11))
trend <- fit$fitted
cycle <- fit$resids
plot(trend)
plot(cycle)

ct <- ts.combine(cycle, hcpi$cycle)
plot(ct, plot.type="single", main="Hamilton Cyclical and HP Filter Cyclical (Purple)",
     lty=c(1,1), col=c("black","purple"), ylab="")
abline(h=0,lty=3,col="black",lwd=2)
par(new="FALSE")

