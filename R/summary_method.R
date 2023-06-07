#' @title Generic Function-summary.
#' @description This function summarize the information of MSigSeg object.
#' @details This function is a S4 method for MSigSeg object. It summarizes the number of signals, length of signals, number of change points and fmin.
#' @param object A MSigSeg object.
#' @return A summary of MSigSeg object.
#'
#' @examples
#' x=new("MSigSeg") # Creating a new MSigSeg object.
#' summary(x)
#'
#' @docType methods
#' @include MSigSeg_Class.R
#' @name summary
#' @rdname summary-methods
#' @aliases summary,MSigSeg-method
setMethod("summary","MSigSeg",function(object){
  cat("Number of signals     :",dim(data.input(object))[2],'\n')
  cat("Length of signals     :",dim(data.input(object))[1],'\n')
  cat("fmin                  :", object@fmin,"\n")
  if(length(brkps(object))<=20){cat("Changepoint Locations :",brkps(object),"\n")}
  cat("Number of changepoints:", length(brkps(object)),"\n")
})
