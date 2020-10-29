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

```{r}
getwd()
```

## Task 2

```{r}
spruce.df= read.csv("SPRUCE.csv")
tail(spruce.df)
```

## Task 3

```{r}
library(s20x)
trendscatter(Height~BHDiameter,f=0.5,data=spruce.df)
spruce.lm=with(spruce.df, lm(Height~BHDiameter))
height.res=residuals(spruce.lm)
height.fit=fitted(spruce.lm)
plot(height.fit,height.res)
trendscatter( height.fit,height.res)
```

The graph resembles a triangular shape. The first graph has a slight curve towards the top as the height and diameter increase. Whereas, the second graph shows a peak and then a decrease. When compared to the first trendscatter, the second graph has non-linearity.

```{r}
plot(spruce.lm, which =1)
normcheck(spruce.lm,shapiro.wilk = TRUE)
```

The P-value for the Shapiro-Wilk test is 0.29. The NULL hypothesis is that the error in the data has a normal distribution.

```{r}
round(mean(height.res),4)
```

Since the residual graph shows a U shape and the first graph has a slight curve, using a linear model for this data would not be the best fit. 

## Task 4

### Using quad.lm
```{r}
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
summary(quad.lm)
```

```{r}
coef(quad.lm)
```

### New Scatter of Height vs BHDiameter
```{r}
plot(spruce.df)
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
} 
curve(myplot, lwd=2, col="steelblue",add=TRUE)
```

### Quad.fit
```{r}
quad.fit=fitted(quad.lm)
```

### Residuals vs Fitted Values
```{r}
plot(quad.lm, which = 1)
```


```{r}
normcheck(quad.lm,shapiro.wilk = TRUE)
```

The P-Value is 0.684. 

## Task 5

### Determing Beta values
```{r}
summary(quad.lm)
```

\begin{equation}
\hat\beta_0 = 0.860896
\hat\beta_1 = 1.1469592
\hat\beta_2 = -0.027457
\end{equation}

### Interval Estimates
```{r}
ciReg(quad.lm)
```

### Equation for the fitted line
Height=0.860896+1.469592x-0.027457X^2

### Predict the Height
```{r}
predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))
```

### Comparison
```{r}
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))
```

### R squared Comparison
```{r}
summary(quad.lm)$r.squared
summary(spruce.lm)$r.squared
```
The adjusted r squared value shows how well the data fits the model used. This value can change with the addition of new date.In this case, quad.lm has a better fit than spruce.lm.

### Multiple R Squared
A multiple r squared value tells us how well this specific model works with the given data. From here, we can conclude that a quadratic fit works best.

### Height Variability
```{r}
summary(quad.lm)$r.squared
summary(spruce.lm)$r.squared
summary(quad.lm)$adj.r.squared
summary(spruce.lm)$adj.r.squared
```
Based on on the r squared and adjusted r squared values, quad.lm has the greater variability.

### Anova comparison
```{r}
anova(quad.lm,spruce.lm)
```
Quad.lm is still the better fit because it has a lower RSS number than spruce.lm.

### TSS, MSS, RSS
```{r}
height.qfit=fitted(quad.lm)
RSS=with(spruce.df, sum((Height-height.qfit)^2))
RSS
MSS = with(spruce.df, sum((height.qfit-mean(Height))^2))
MSS
TSS = with(spruce.df, sum((Height-mean(Height))^2))
TSS
MSS/TSS
```


## Task 6
### Cooks Plot
```{r}
cooks20x(quad.lm)
```

### Cook's Distance
cook's Distance tells use how much one point would change the regression for the data. The larger the distance on the graph, the bigger the chance of it affecting the analysis.

### Cook's Plot for quad2.lm
```{r}
quad2.lm=lm(Height~BHDiameter + I(BHDiameter^2) , data=spruce.df[-24,])
summary(quad2.lm)
```
Quad2.lm removes the number that has the highest impact on the analysis. By removing that one value, the R squared value increased to 0.8159 and the adjusted R squared is 0.8044.

### Comparison to quad.lm
```{r}
summary(quad.lm)
```
The R squared values are lower than the quad2.lm show it has a better fit.

### Conclusion
By removing the data for 24, it did increase the fit of the model and proving the impact of that point.

## Task 7

### Proof

\begin{equation}
eq1: y-\beta_0+\beta_1x
\end{equation}
\begin{equation}
eq2: y-\beta_0+\delta+(\beta_1+\beta_2)x
\end{equation}
\begin{equation}
y_k=\beta_0+\beta_1x_k=\beta_0+\delta+(\beta_1+\beta_2)x_k
\end{equation}
\begin{equation}
\beta_0+\beta_1x_k-\beta_0+\delta+\beta_1x_k+\beta_2x_k
\end{equation}
\begin{equation}
\delta=-\beta_2x_k
\end{equation}
\begin{equation}
y=\beta_0-\beta_2x_k+(\beta_1+\beta_2)x
\end{equation}
\begin{equation}
y=\beta_0-\beta_2x_k+\beta_1x+\beta_2x
\end{equation}
\begin{equation}
y=\beta_0+\beta_1x+\beta_2(x-x_k)
\end{equation}
\begin{equation}
y=\beta_0+\beta_1x+\beta_2(x-x_k)I(x>x_k)
\end{equation}

### Piecewise Regression Plot
```{r}
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
```

## Task 8

```{r}
library(roxygen2)
myf(15,summary(quad.lm)$coefficients)
```
This function returns the y value when give X and the coefficients for quadratic fitting data when x>xk.
