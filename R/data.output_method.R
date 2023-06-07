#' @title Generic Function-data.output.
#' @description This function returns the data.output slot of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It retrieves data.output slot, which contains the input data which has been smoothed..
#' @param object A MSigSeg object.
#' @return The data.output slot of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' data.output(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name data.output
#' @rdname data.output-methods
#' @aliases data.output,MSigSeg-method
setMethod("data.output","MSigSeg",function(object) object@data.output)
