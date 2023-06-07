#' @title Generic Function-seg.len.
#' @description This function returns the length of segmentation.
#' @details This function is a S4 method for MSigSeg object. It calculates the distance between each change points.
#' @param object A MSigSeg object.
#' @return A vector contains length of segmentation.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' seg.len(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name seg.len
#' @rdname seg.len-methods
#' @aliases seg.len,MSigSeg-method
setMethod("seg.len","MSigSeg",function(object){
  object@brkps[1]
  if(length(object@brkps)>=1){
    object@brkps[-1]-object@brkps[-length(object@brkps)]
    }
  })
