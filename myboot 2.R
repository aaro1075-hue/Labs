#'myboot2
#'
#'@param iter is the number of iterations
#'@param x is the data
#'@param fun is the mean of the data
#'@param alpha is the confidence
#'
#'@example
#'myboot(1000,x=sam, fun="mean",alpha=0.2) #sam is a set of data points
#'
#'
myboot<-function(iter,x,fun="mean",alpha,...){
n=length(x)
y=sample(x,n*iter,replace=TRUE)
rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
xstat=apply(rs.mat,2,fun)
ci=quantile(xstat,c(alpha/2,1-alpha/2))
para=hist(xstat,freq=FALSE,las=1,main="Histogram of Bootstrap sample statistics",...)
mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
pte=apply(mat,2,fun)
abline(v=pte,lwd=3,col="Black")
segments(ci[1],0,ci[2],0,lwd=4)
text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)
text(pte,max(para$density)/2,round(pte,2),cex=3)
return(list(ci=ci,fun=fun,x=x))
}
