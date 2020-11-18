#' Central Limit Theorem Function
#'
#' @param n is the sample size
#' @param iter is the number of iterations
#' @example
#' myclt(10,5)
#'
#'
myclt=function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
  sm}
