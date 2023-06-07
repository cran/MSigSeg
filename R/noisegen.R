#' @title Noisegen.
#' @description Generate matrix based on signal-to-noise ratio.
#' @param X A matrix users aimed to add signal-to-noise ratio.
#' @param SNR Signal-to-noise ratio.
#' @return A matrix with specified signal-to-noise ratio.
#' @importFrom MASS ginv
#' @importFrom stats rbinom rnorm sd

noisegen <- function(X,SNR){
  NOISE=matrix(rnorm(dim(X)[1]*dim(X)[2]),nrow=dim(X)[1])
  NOISE=NOISE-colMeans(NOISE)
  signal_power = (1/max(dim(X)[1],dim(X)[2]))%*%colSums(X*X)
  noise_variance = signal_power/(10^(SNR/10))
  NOISE=as.numeric(sqrt(noise_variance) %*% ginv(t(apply(NOISE,2,sd))))*NOISE
  Y=X+NOISE
  return(Y)
}
