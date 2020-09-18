---
  title: "Lab 4"
author: "Emily Aaron"
date: "`r Sys.Date()`"
output: 
  html_document:
  toc: yes
toc_float: yes
---
# Tasks

## Task 1

getwd() #Gets the working directory

## Task 2
spruce.df= read.csv("SPRUCE.csv") #Reads in the data and gives that top 6 lines
tail(spruce.df)

## Task 3

library(s20x)
trendscatter(Height~BHDiameter,f=0.5,data=spruce.df)
spruce.lm=with(spruce.df, lm(Height~BHDiameter))
height.res=residuals(spruce.lm)
height.fit=fitted(spruce.lm)
plot(height.fit,height.res)
trendscatter( height.fit,height.res)

plot(spruce.lm, which =1)
normcheck(spruce.lm,shapiro.wilk = TRUE)

## Task 4
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
summary(quad.lm)
add1(spruce.lm,.~.+I(BHDiameter^2))
anova(spruce.lm)
anova(quad.lm)
anova(spruce.lm,quad.lm)
cubic.lm=lm(Height~BHDiameter + I(BHDiameter^2)+I(BHDiameter^3),data=spruce.df)
anova(cubic.lm)
add1(quad.lm,.~.+I(BHDiameter^3))

plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

## Task 5
summary(quad.lm)

predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))

anova(quad.lm)
anova(spruce.lm)

height.qfit=fitted(quad.lm)
RSS=with(spruce.df, sum((Height-height.qfit)^2))
RSS
MSS = with(spruce.df, sum((height.qfit-mean(Height))^2))
MSS
TSS = with(spruce.df, sum((Height-mean(Height))^2))
TSS
MSS/TSS

## Task 6

cooks20x(quad.lm)

quad2.lm=lm(Height~BHDiameter + I(BHDiameter^2) , data=spruce.df[-24,])
summary(quad2.lm)
summary(quad.lm)

## Task 7

sp2.df=within(spruce.df, X<-(BHDiameter-18)*(BHDiameter>18))
sp2.df

lmp=lm(Height~BHDiameter + X,data=sp2.df)
tmp=summary(lmp)
names(tmp)
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
plot(Height~BHDiameter,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Piecewise Regression",data=spruce.df)
myf(0, coef=tmp$coefficients[,"Estimate"])
curve(myf(x,coef=tmp$coefficients[,"Estimate"] ),add=TRUE, lwd=2,col="Blue")
abline(v=18)
text(18,16,paste("R sq.=",round(tmp$r.squared,4) ))

## Task 8


