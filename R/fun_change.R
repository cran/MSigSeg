#' @title fun_change.
#' @description Generate matrix based on specified change points.
#' @details Generate matrix with common change points, based on specific probability of occurrence.
#' @param M A matrix users aimed to add change points.
#' @param p Probability of occurrence of change points.
#' @return A list containing the matrix with specified change point and the location of change points.
#' @importFrom MASS ginv
#' @importFrom stats rbinom rnorm sd

fun_change <- function(M,p=0.01){
  m <- dim(M)[1]
  n <- dim(M)[2]
  cp <- rbinom(m-1,1,p)
  cp <- as.data.frame(cp)
  cp$l <- 1:(m-1)
  loc <- cp[cp$cp==1,]$l
  h <- rnorm(length(loc))
  N <- M
  for (i in 1:(length(loc)-1)){
    N[(loc[i]+1):loc[i+1],] = N[(loc[i]+1):loc[i+1],] + h[i]
  }
  return(list(N,loc))
}
