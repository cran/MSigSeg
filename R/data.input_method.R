#' @title Generic Function-data.input.
#' @description This function returns the data.input slot of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It retrieves data.input slot, which contains the data users input.
#' @param object A MSigSeg object.
#' @return The data.input slot of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' data.input(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name data.input
#' @rdname data.input-methods
#' @aliases data.input,MSigSeg-method
setMethod("data.input","MSigSeg",function(object) object@data.input)
