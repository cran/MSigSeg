#' @title Generic Function-lambda.
#' @description This function returns the lambda slot of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It retrieves lambda slot, which contains penalty coefficient to prevent over fitting.
#' @param object A MSigSeg object.
#' @return The lambda slot of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' lambda(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name lambda
#' @rdname lambda-methods
#' @aliases lambda,MSigSeg-method
setMethod("lambda","MSigSeg",function(object) object@lambda)
