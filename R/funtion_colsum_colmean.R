fun_colmean <- function (x) {
  if (length(dim(x)) < 2L){
    x
  }
  else{
    colMeans(x)
  }
}

#fun_colsum <- function (x, na.rm = FALSE, dims = 1L)
#{
#  if (!is.array(x) || length(dn <- dim(x)) < 2L)
#    stop("'x' must be an array of at least two dimensions")
#  if (dims < 1L || dims > length(dn) - 1L)
#    stop("invalid 'dims'")
#  n <- prod(dn[id <- seq_len(dims)])
#  dn <- dn[-id]
#  z <- if (is.complex(x))
#    .Internal(colSums(Re(x), n, prod(dn), na.rm)) + (0+1i) *
#    .Internal(colSums(Im(x), n, prod(dn), na.rm))
#  else .Internal(colSums(x, n, prod(dn), na.rm))
#  if (length(dn) > 1L) {
#    dim(z) <- dn
#    dimnames(z) <- dimnames(x)[-id]
#  }
#  else names(z) <- dimnames(x)[[dims + 1L]]
#  z
#}
