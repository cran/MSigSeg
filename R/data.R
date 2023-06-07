#' A data set used for testing.
#'
#' @name PELT_test
#' @docType data
#' @format A matrix with 1000 rows and 20 columns.
#' @importFrom MASS ginv
#' @importFrom stats rbinom rnorm sd

PELT_test <- matrix(0,1000,20)

M <- PELT_test
p <- 0.01
m <- dim(M)[1]
n <- dim(M)[2]
cp <- rbinom(m-1,1,0.01)
cp <- as.data.frame(cp)
cp$l <- 1:(m-1)
loc <- cp[cp$cp==1,]$l
h <- rnorm(length(loc))
N <- M
for (i in 1:(length(loc)-1)){
  N[(loc[i]+1):loc[i+1],] = N[(loc[i]+1):loc[i+1],] + h[i]
}

SNR <- 5
X <- N
NOISE=matrix(rnorm(dim(X)[1]*dim(X)[2]),nrow=dim(X)[1])
NOISE=NOISE-colMeans(NOISE)
signal_power = (1/max(dim(X)[1],dim(X)[2]))%*%colSums(X*X)
noise_variance = signal_power/(10^(SNR/10))
NOISE=as.numeric(sqrt(noise_variance) %*% ginv(t(apply(NOISE,2,sd))))*NOISE
Y=X+NOISE

PELT_test <- Y
