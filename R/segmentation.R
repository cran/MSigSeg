#' @title Detecting common change points for multiple signals.
#' @description Calculates the optimal positioning and number of common breakpoints for multiple signals.
#' @details This function uses modified PELT method to find optimal common change points for multiple signals.
#' @param Y An data.frame/matrix containing the data to be segmented. Each column stores a signal.
#' @param lambda A penalty term, small value leads to large number of breakpoints, and vice versa.
#' @param flag Logical. If True then use th PELT method. If False then use the OP method.
#' @param return_smooth_signals Logical. If True then smoothed signals are returned.
#' @return An object of S4 class "MSigSeg"
#'
#' @importFrom methods new
#' @importFrom stats rbinom rnorm sd
#'
#' @examples
#' data(data_test)
#' segmentation(data_test,100)
#'
#' @export

segmentation <- function(Y,lambda,flag=TRUE,return_smooth_signals=TRUE){
  if(lambda<0){
    stop("lambda must be a positive number.")
  }

  if(!(is.matrix(Y))){
    Y <- as.matrix(Y)
  }

  if(dim(Y)[1]<dim(Y)[2]){
    warning("The number of signals is greater than the signal length.")
  }

  #if(dim(Y)[1]<dim(Y)[2]){
  #  Y <- t(Y)
  #}

  size <- dim(Y)
  n <- size[1] ; m<- size[2]
  f <- matrix(0,1,n+1)
  brkps <- matrix(0,n,n+1) #brkps <- list(); brkps[[1]] <- 0L
  R <- 1
  E <- 0
  M <- as.matrix(Y[1,])

  for (i in 1:n) {
    vec <- f[R] + E
    val <- min(vec)
    idx1 <- which.min(vec)
    f[i+1] <- val + lambda
    loc <- as.integer(R[idx1])
    idx0 <- brkps[,loc]!=0
    brkps[idx0,i+1]=brkps[idx0,loc]
    brkps[sum(idx0)+1,i+1] <- loc

    if (flag){
      idx2 <- vec<f[i+1]
    }
    else{
      idx2 <- 1:length(vec)
    }

    if(i < n){
      y_new <- as.matrix(Y[i+1,])
      R <- R[idx2]
      lens <- as.matrix(i-R+1)
      L <- matrix(1,m,1) %*% t(lens)
      B <- y_new %*% matrix(1,1,length(lens))
      MI <- M[,idx2]
      #E <- E[idx2] + lens/(lens+1)*(fun_colsum((B-MI)^2))
      E <- E[idx2] + lens/(lens+1)*(colSums((B-MI)^2))
      E <- c(E,0)
      M <- (MI*L+B)/(L+1)
      M <- cbind(M,y_new)
      R <- c(R,i+1)
    }
  }
  brkps <- brkps[,n+1]
  brkps <- brkps[brkps!=0]
  brkps <- brkps[2:length(brkps)]-1
  fmin <- f[length(f)]

  if(return_smooth_signals==TRUE){
    X <- matrix(0,n,m)
    if(is.na(brkps[1])){
      X <- matrix(1,n,1) %*% fun_colmean(Y)
    }
    else{
      if(m==1){
        X[1:brkps[1],] <- mean(Y[1:brkps[1],])
        X[(brkps[length(brkps)]+1):n,] <- mean(Y[(brkps[length(brkps)]+1):n,])
        if(length(brkps)>1){
          for (i in 1:(length(brkps)-1)) {
            X[(brkps[i]+1):brkps[i+1],] <- mean(Y[(brkps[i]+1):brkps[i+1],])
          }
        }
      }
      else{
      X[1:brkps[1],] <- matrix(1,brkps[1],1) %*% fun_colmean(Y[1:brkps[1],])
      X[(brkps[length(brkps)]+1):n,] <- matrix(1,n-brkps[length(brkps)],1) %*% fun_colmean(Y[(brkps[length(brkps)]+1):n,])
      if(length(brkps)>1){
        for (i in 1:(length(brkps)-1)) {
          X[(brkps[i]+1):brkps[i+1],] <- matrix(1,brkps[i+1]-brkps[i],1) %*% fun_colmean(Y[(brkps[i]+1):brkps[i+1],])
        }
      }
      }
    }
    ans <- new("MSigSeg",data.input=Y, data.output=X, fmin=fmin, brkps=brkps, lambda=lambda)
    return(ans)
  }
  else{
    ans <- new("MSigSeg",data.input=Y, data.output=matrix(0), fmin=fmin, brkps=brkps, lambda=lambda)
    return(ans)
  }
}
