#------------------------------------------------------------------------------
# File:         chapter_01
# Description:  Exercises from Chapter 1
# Author:       Ervin Mazlagic
# Date:         2018-02-25
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Exercise 1.2.1 - R Installation
#------------------------------------------------------------------------------

# installed R and RStudio

#------------------------------------------------------------------------------
# Exercise 1.2.2 - R Beginner
#------------------------------------------------------------------------------

# skipped R-Intro by CRAN

#------------------------------------------------------------------------------
# Exercise 1.2.3 - Furst Steps with R
#------------------------------------------------------------------------------

# define absolute root path for Applied Statistics Module
path_abs_app_stat <- "/home/ninux/data/studium/master/FTP/FTP_AppStat"

# define relative path to data within Applied Statistics Module
path_rel_app_stat_data <- "/Zusatzmaterial/Datasets"

# set working directory
setwd(paste(path_abs_app_stat, path_rel_app_stat_data, sep=""))

# read data file
fn <- "sample.dat"
dat <- read.table(fn, header=TRUE)

# 1.2.3.a - check structure of the data
str(dat)

# 1.2.3.b - check summary of the data
summary(dat)

# 1.2.3.c - create a plot of the data
# matplot(dat,
#         type=c("b"), 
#         col=1:3,
#         pch=1,
#         main="Temperature Measurements", 
#         xlab="Day", 
#         ylab="Temperature [°C]")
plot(dat[,1],
     col="red",
     type=c("b"),
     main="Temperature Measurements",
     xlab="Day",
     ylab="Temperature [°C]",
     xlim=c(1,20),
     ylim=c(min(dat)-1,max(dat)+1))

lines(dat[,2], type="b", col="blue")
lines(dat[,3], type="b", col="green")

# legend( "topleft",
#         legend=c("x1","x2","x3"), 
#         col=1:3, 
#         pch=1)

# 1.2.3.d - add mean, median, variance, and standard deviation to the plot
dat.mean <- apply(X=dat, MARGIN=1, FUN=mean)
dat.med <- apply(X=dat, MARGIN=1, FUN=median)
dat.var <- apply(X=dat, MARGIN=1, FUN=var)
dat.sd <- apply(X=dat, MARGIN=1, FUN=sd)

lines(dat.mean, type="s", col="black")
legend( "bottomleft",
        legend=c("x1","x2","x3","mean day"), 
        col=1:3, 
        pch=1)

# 1.2.3.e - generate histogram and a boxplot of the mean values
attach(dat)
par(mfrow=c(1,2))
hist(dat.mean, main="Mean Temperature", xlab="Temperature [°C]", ylab="Frequency")
boxplot(dat.mean, main="Mean Temperature", ylab="Temperature [°C]")
detach(dat)

# 1.2.3.f - check x1 and x2 for significant difference
attach(dat)
par(mfrow=c(1,2))
qqnorm(dat[,1])
qqline(dat[,1])
qqnorm(dat[,2])
qqline(dat[,2])
detach(dat)

# the plot show a somewhat normal distribution of x1 and x2, therefore the
# t.test can be applied

# QUESTION: I HAVE NO IDEA HOW TO PROCEED

#------------------------------------------------------------------------------
# Exercise 1.2.4
#------------------------------------------------------------------------------

# 1.2.4.a - percentage of failures for max 8.10 mm thickness
pnorm(q=8.1E-3, mean=8E-3, sd=0.05E-3, lower.tail=FALSE)

# 1.2.4.b - percentage of failure for min 7.92 mm and 8.08 mm
below <- pnorm(q=7.92E-3, mean=8E-3, sd=0.05E-3)
above <- 1-pnorm(q=8.08E-3, mean=8E-3, sd=0.05E-3)
total <- below + above

# QUEATION: WHAT ABOUT THE LOWER.TAIL?

# 1.2.4.c - max allowed error by max 5% failures
8E-3 - qnorm(p=0.025, mean=8E-3, sd=0.05E-3)

#------------------------------------------------------------------------------
# Exercise 1.2.5
#------------------------------------------------------------------------------

# 1.2.5.a - random runs
m <- 500
n <- 5
rdat <- matrix(rnorm(n=5*500, mean=0, sd=1), m, n)

# 1.2.5.b - histogram of mean values
rdat.mean <- apply(X=rdat, MARGIN=1, FUN=mean)
hist(rdat.mean, freq=FALSE)
curve(dnorm(x, mean=0, sd=1/sqrt(n)), from=-4, to=4, add=TRUE, col="red")
qqnorm(rdat.mean)
qqline(rdat.mean, col="red")

# 1.2.5.c - repeat with n=2,10,100
m <- 2
n <- 5
rdat <- matrix(rnorm(n=5*500, mean=0, sd=1), m, n)
rdat.mean <- apply(X=rdat, MARGIN=1, FUN=mean)
hist(rdat.mean, freq=FALSE)
curve(dnorm(x, mean=0, sd=1/sqrt(n)), from=-4, to=4, add=TRUE, col="red")
qqnorm(rdat.mean)
qqline(rdat.mean, col="red")

m <- 10
n <- 5
rdat <- matrix(rnorm(n=5*500, mean=0, sd=1), m, n)
rdat.mean <- apply(X=rdat, MARGIN=1, FUN=mean)
hist(rdat.mean, freq=FALSE)
curve(dnorm(x, mean=0, sd=1/sqrt(n)), from=-4, to=4, add=TRUE, col="red")
qqnorm(rdat.mean)
qqline(rdat.mean, col="red")

m <- 100
n <- 5
rdat <- matrix(rnorm(n=5*500, mean=0, sd=1), m, n)
rdat.mean <- apply(X=rdat, MARGIN=1, FUN=mean)
hist(rdat.mean, freq=FALSE)
curve(dnorm(x, mean=0, sd=1/sqrt(n)), from=-4, to=4, add=TRUE, col="red")
qqnorm(rdat.mean)
qqline(rdat.mean, col="red")
