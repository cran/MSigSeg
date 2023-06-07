#' @title Generic Function-print.
#' @description This function print the basic information of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It prints class, slots, created  date and summary of MSigSeg object.
#' @param object A MSigSeg object.
#' @return The the basic information of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' print(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name print
#' @rdname print-methods
#' @aliases print,MSigSeg-method
setMethod("print","MSigSeg",function(object){
  cat("Class 'MSigSeg' : MSigSeg Object\n")
  cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
  cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
  cat("Created on  :", object@date, "\n\n")
  cat("summary(.)  :\n----------\n")
  summary(object)
})
