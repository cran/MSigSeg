#' A simulated data set used for testing.
#'
#' @name data_test
#' @docType data
#' @format A matrix with 1000 rows and 20 columns.
#' @importFrom MASS ginv
#' @importFrom stats rbinom rnorm sd
#' @examples
#' data("data_test",package = "MSigSeg")

data_test <- matrix(0,1000,20)

M <- data_test
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

data_test <- Y


#' influenza  data set from CDC used as an example.
#'
#' @name NCHSData
#' @docType data
#' @format A matrix with 52 rows and 10 columns.
#' @examples
#' data("NCHSData",package = "MSigSeg")
"NCHSData"


#' A stock data set used as an example.
#'
#' @name stock
#' @docType data
#' @format A data.frame with 757 rows and 488 columns.
#' @examples
#' data("stock",package = "MSigSeg")
"stock"


#' A chromosome sequencing data set used as an example.
#'
#' @name T16P
#' @docType data
#' @format A data.frame with 2928 rows and 16 columns.
#' @references Navin N, Kendall J, Troge J, et al. Tumour evolution inferred by single-cell sequencing. Nature. 2011;472(7341):90-94. doi:10.1038/nature09807
#' @examples
#' data("T16P",package = "MSigSeg")
"T16P"


#' A chromosome sequencing data set used as an example.
#'
#' @name T16M
#' @docType data
#' @format A data.frame with 2928 rows and 22 columns.
#' @references Navin N, Kendall J, Troge J, et al. Tumour evolution inferred by single-cell sequencing. Nature. 2011;472(7341):90-94. doi:10.1038/nature09807
#' @examples
#' data("T16M",package = "MSigSeg")
"T16M"

