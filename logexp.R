#' Max Likelihood
#'
#'@param x is the data
#'@param lfun is a log for probability distribution functions
#'@param param is parameter values
#'
#'@example
#'mymaxlik(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)
#'
mymaxlik=function(lfun,x,param,...){
  np=length(param)
  z=outer(x,param,lfun)
  y=apply(z,2,sum)
  plot(param,y,col="Blue",type="l",lwd=2,...)
  i=max(which(y==max(y))) # B
  abline(v=param[i],lwd=2,col="Red")
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
