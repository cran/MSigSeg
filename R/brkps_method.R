#' @title Generic Function-brkps.
#' @description This function returns the brkps slot of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It retrieves brkps slot, which contains the locations of break points.
#' @param object A 'MSigSeg' object.
#' @return The brkps slot of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' brkps(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name brkps
#' @rdname brkps-methods
#' @aliases brkps,MSigSeg-method
setMethod("brkps","MSigSeg",function(object) object@brkps)
