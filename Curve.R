#' Curve plot
#'
#'@param mu is the mean
#'@param sigma is the standard deviation
#'@param a is the probability area
#'@return Curve for the sample
#'@example
#'myncurve(mu=10, sigma=5, a=6)
#'
#'
myncurve = function(mu, sigma, a){curve(dnorm(x,mean = mu, sd = sigma),xlim = c(mu-3*sigma, mu +3*sigma), col="blue")}
