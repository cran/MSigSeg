#' @title Detecting common change points with designated number.
#' @description Calculates the optimal positioning of common change points with designated number.
#' @details This function is based on the fun_segmentation_PELT() function. Number of change points are defined by users and lambda is calculated by algorithm automatically.
#' @param Y An data.frame/matrix containing the data within which you wish to find common change points. Each column is considered a separate signal.
#' @param K Number of change points users want to detect.
#' @return An object of S4 class "MSigSeg".
#'
#' @importFrom methods new
#' @importFrom stats rbinom rnorm sd
#'
#' @examples
#' data(PELT_test)
#' fun_lambda_estimator(PELT_test,5)
#'
#' @export

fun_lambda_estimator <- function(Y,K){
  fun_var <- function(x){
    sum((x-mean(x))^2)/(length(x)-1)
  }

  if(!(is.matrix(Y))){
    Y <- as.matrix(Y)
  }

  if(K>=dim(Y)[1]){
    stop("K must not greater than the signal length.")
  }

  if(K<=0){
    stop("K must be a positive integer.")
  }

  n <- dim(Y)[1]
  vec_lambda <- c(min(diff(Y,1,1)^2),sum(apply(Y,2,fun_var))*n+1e-2) + 1e-3
  k_brkps <- c(n-1,0)
  idx <- 1L

  while ((!(sum(k_brkps==K,na.rm = T))) && (vec_lambda[idx+1]-vec_lambda[idx]>1e-3)) {
    idx_all <- which(k_brkps>K)
    idx <- idx_all[length(idx_all)]
    lambda <- 10^mean(log10(vec_lambda[idx:(idx+1)]))
    vec_lambda <- c(vec_lambda[1:idx],lambda,vec_lambda[(idx+1):length(vec_lambda)])
    brkps <- fun_segmentation_PELT(Y,lambda,smooth_signals = FALSE)@brkps
    k_brkps=c(k_brkps[1:idx],sum(brkps!=0,na.rm = TRUE),k_brkps[(idx+1):length(k_brkps)])
  }
  a <- fun_segmentation_PELT(Y,lambda)
  ans <- new("MSigSeg",data.input=Y, data.output=a@data.output, fmin=a@fmin, brkps=brkps, lambda=lambda)
  return(ans)
}
