t---
  title: "Lab3"
author: "Emily Aaron"
date: "`r Sys.Date()`"
output: 
  html_document:
  toc: yes
toc_float: yes
---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Tasks

## Task 1
```{r}
getwd()
```

## Task 2

```{r}
spruce.df=read.csv("SPRUCE.csv")
head(spruce.df)
```

## Task 3

Scatter Plot
```{r}
plot(Height~BHDiameter, data = spruce.df, pch=21, bg="Blue", cex=1.2, xlim=c(0,1.1*max(spruce.df$BHDiameter)), ylim=c(0,1.1*max(spruce.df$Height)))
```

Based on tbhe graph, it shows a straight line relationship between the height and BHDiameter. As the diameter is increasing, the height is increasing as well.

Lowess Smooth Scatter
```{r}
library(s20x)
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE) )
trendscatter(Height~BHDiameter,f=0.5, data=spruce.df, )
trendscatter(Height~BHDiameter,f=0.6, data=spruce.df)
trendscatter(Height~BHDiameter,f=0.7, data=spruce.df)
```

Least squares regression
```{r}
spruce.lm = lm(Height~BHDiameter, data = spruce.df)
plot(Height~BHDiameter, data = spruce.df, pch=21, bg="Blue", cex=1.2, xlim=c(0,1.1*max(spruce.df$BHDiameter)), ylim=c(0,1.1*max(spruce.df$Height)))
abline(spruce.lm)
```

A straight line would not be appropriate because there is a curve happening from the first point to the last.

## Task 4

```{r}
layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))
spruce.lm = lm(Height~BHDiameter, data = spruce.df)
plot(Height~BHDiameter, data = spruce.df, pch=21, bg="Blue", cex=1.2, xlim=c(0,1.1*max(spruce.df$BHDiameter)), ylim=c(0,1.1*max(spruce.df$Height)))
abline(spruce.lm)
plot(Height~BHDiameter, data = spruce.df, pch=21, bg="Blue", cex=1.2, xlim=c(0,1.1*max(spruce.df$BHDiameter)), ylim=c(0,1.1*max(spruce.df$Height)))
abline(spruce.lm)
yhat=fitted(spruce.lm)
with(spruce.df,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})
plot(Height~BHDiameter, data = spruce.df, pch=21, bg="Blue", cex=1.2, xlim=c(0,1.1*max(spruce.df$BHDiameter)), ylim=c(0,1.1*max(spruce.df$Height)))
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)
with(spruce.df, 
     plot(Height~BHDiameter,bg="Blue",pch=21,ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), main="Emily Aaron's Plot")
)
with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
```

TSS, MSS, RSS Calculations
```{r}
RSS=with(spruce.df,sum((Height-yhat)^2))
RSS
MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS
TSS=with(spruce.df,sum((Height-mean(Height))^2))
TSS
```

MSS/TSS Calculations
```{r}
MSS/TSS
```
The MSS/TSS shows how well the data set fits the least squares regression line. With a value so low, we can assume that the data is not linear.

TSS=MSS+RSS
```{r}
MSS+RSS
TSS
```
Yes

## Task 5

```{r}
summary(spruce.lm)
```

(a) m = 0.48147
(b) b = 9.14684
(c) y= 0.48147x+ 9.14684

```{r}
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))
```

## Task 6

Height vs Diameter
```{r}
library(ggplot2)
g=ggplot(spruce.df, aes(x=BHDiameter,y=Height,colour=BHDiameter))
g=g+geom_point() + geom_line()+ geom_smooth(method="lm")
g+ggtitle("Height vs BHDiameter")
```

## Task 7

<center>
  !["name of image"](one.png){ width=70% }
</center>
  
  <center>
  !["name of image"](two.png){ width=70% }
</center>
  
  <center>
  !["name of image"](three.png){ width=70% }
</center>