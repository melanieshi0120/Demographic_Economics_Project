# Stony Brook University
# Graduate Department of Economics
# Demographic Economics
# 2016 Fall Semester
# Hua Shi
# SBU ID#110635839
# 
#=================================================================================================================
#=================================================================================================================
rm(list=ls()) # clear console - control + L
help(logbin)
library(logging)
library(plm)
library(foreign)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(car)
library(lmtest)
library(stargazer)
require(foreign)
require(MASS)
library(gplots)
require(urca)
library(ggplot2)
library(lattice)
library(reshape)
library(splm)
#=================================================================================================================

# input data 
mydata<-read.csv("/Users/yingyuxuan/Desktop/Demo/average+demo+paneldata.csv",header = TRUE)
attach(mydata)

pdata<-plm.data(mydata,index=c("country","year"))
m<-pdata[c(5:24)]
n<-m[c(3,15)]

stargazer(m)
stargazer(log(n))
#=================================================================================================================
#=================================================================================================================
#Pooled OLS estimator
#basic specification
pdata<-plm.data(mydata,index=c("ID","year"))
pooling<-plm(gy ~ y+log(K)+H+CTP+OTP+GRCTP+GROTP,data=pdata,model="pooling")
summary(pooling)
vif(pooling)
stargazer(pooling,title="Pooled OLS Model", align=TRUE)
confint(pooling) #Computes confidence intervals for  parameters in a fitted model.
#=================================================================================================================
#=================================================================================================================
#Pooled OLS estimator
#population growth specification
pdata<-plm.data(mydata,index=c("ID","year"))
pooling<-plm(gy ~ y+log(K)+H+PG+CTP+OTP+GRCTP+GROTP,data=pdata,model="pooling")
summary(pooling)
vif(pooling)
stargazer(pooling,title="Pooled OLS Model", align=TRUE)
confint(pooling) #Computes confidence intervals for  parameters in a fitted model.
#=================================================================================================================
#=================================================================================================================
#Pooled OLS estimator
#Demographic specification
pooling<-plm(gy ~ y+log(K)+H+PG+CTP+OTP+GRCTP+GROTP+FR+log(LifeExpectancy),data=pdata,model="pooling")
summary(pooling)
vif(pooling)
stargazer(pooling,title="Pooled OLS Model", align=TRUE)
confint(pooling) #Computes confidence intervals for  parameters in a fitted model.

#=================================================================================================================
#=================================================================================================================
#Pooled OLS estimator
#Geograpgic specification
pooling<-plm(gy ~ y+log(K)+H+CTP+OTP+GRCTP+GROTP+0+SouthAsiaDummy+SouthEastAsiaDummy+EastAsiaDummy+WestAsiaDummy,data=pdata,model="pooling")
summary(pooling)
vif(pooling)
stargazer(pooling,title="Pooled OLS Model", align=TRUE)
confint(pooling) #Computes confidence intervals for  parameters in a fitted model.
#=================================================================================================================
#=================================================================================================================
#Pooled OLS estimator
#Geograpgic specification
pooling<-plm(gy ~ y+log(K)+H+PG+FR+log(LifeExpectancy)+CTP+OTP+GRCTP+GROTP+0+SouthAsiaDummy+SouthEastAsiaDummy+EastAsiaDummy+WestAsiaDummy,data=pdata,model="pooling")
summary(pooling)
vif(pooling)
stargazer(pooling,title="Pooled OLS Model", align=TRUE)
confint(pooling) #Computes confidence intervals for  parameters in a fitted model.





# regional level
regiondata<-read.csv("/Users/yingyuxuan/Desktop/Demo/Region_Level_paneldata.csv",header = TRUE)

scatterplot(GDPPC~year|region, boxplots=FALSE, smooth=TRUE, reg.line=TRUE,xlab="Year",ylab = "Growth Rate of GDP per capita ",data=regiondata)
scatterplot(K~year|region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "Capital Stock",data=regiondata)
scatterplot(H~year|region, boxplots=FALSE, smooth=TRUE, reg.line=FALSE,xlab="Year",ylab = "Human Capital",data=regiondata)
scatterplot(FR~year|region, boxplots=FALSE, smooth=FALSE, reg.line=TRUE,xlab="Year",ylab = "Fertility Rate ",data=regiondata)
scatterplot(LifeExpectancy~year|region, boxplots=FALSE, smooth=FALSE, reg.line=FALSE,xlab="Year",ylab = "Life Expactency",regiondata)
scatterplot(PG~year|region, boxplots=FALSE, smooth=FALSE, reg.line=FALSE,xlab="Year",ylab = "Population Growth",data=regiondata)
scatterplot(OTP~year|region, boxplots=FALSE, smooth=FALSE, reg.line=TRUE,xlab="Year",ylab = "Old-age share ",data=regiondata)
scatterplot(CTP~year|region, boxplots=FALSE, smooth=FALSE, reg.line=FALSE,xlab="Year",ylab = "Youth-age share",data=regiondata)
scatterplot(GROTP~year|region, boxplots=FALSE, smooth=FALSE, reg.line=FALSE,xlab="Year",ylab = "Change of old-age share",data=regiondata)
scatterplot(GRCTP~year|region, boxplots=FALSE, smooth=FALSE, reg.line=TRUE,xlab="Year",ylab = "Change of youth-age share ",data=regiondata)


# Asian level
Asianadata<-read.csv("/Users/yingyuxuan/Desktop/Demo/Asian_level_paneldata.csv",header = TRUE)
plot(GDPPC~year,xlab="Year",ylab="GDP per capita",  type="l",data=Asianadata)
plot(K~year,xlab="Year",ylab="Capital stock", type="l",data=Asianadata)

plot(H~year,xlab="Year",ylab="Huaman capital", type="l",data=Asianadata)
plot(PG~year,xlab="Year",ylab="Population growth",  type="l",data=Asianadata)

plot(FR~year,xlab="Year",ylab="Fertility Rate", type="l",data=Asianadata)
plot(LifeExpectancy~year,xlab="Year",ylab="Life expectancy", type="l",data=Asianadata)

plot(OTP~year,xlab="Year",ylab="Old-age share",  type="l",data=Asianadata)
plot(CTP~year,xlab="Year",ylab="Youth-age share",  type="l",data=Asianadata)

plot(GROTP~year,xlab="Year",ylab="Change of old-age share",  type="l",data=Asianadata)
plot(GRCTP~year,xlab="Year",ylab="Change of youth-age share",  type="l",data=Asianadata)
plot(GrowthofGDPPercapita~year,xlab="Year",ylab="Growth rate of GDP per capita",  type="l",data=Asianadata)

Adata<-read.csv("/Users/yingyuxuan/Desktop/Demo/GOGDPPC.csv",header = TRUE)
plot(gogdppc~year,xlab="Year",ylab="Growth of GDP per capita",  type="l",data=Adata)

Rdata<-read.csv("/Users/yingyuxuan/Desktop/Demo/regional_level_GOGDPPC.csv",header = TRUE)
plot(gogdppc~year,xlab="Year",ylab="Growth of GDP per capita",  type="l",data=Adata)
