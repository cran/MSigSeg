#' An S4 class to encapsulation the result of breakpoints analysis.
#'
#' @slot data.input An data.frame/matrix containing the data to be segmented. Each column stores a signal.
#' @slot data.output A matrix containing the input data which has been smoothed.
#' @slot lambda A penalty term, small value leads to large number of breakpoints, and vice versa.
#' @slot brkps A vector containing the locations of common breakpoints.
#' @slot fmin A numeric containing the optimal numerical value calculated.
#' @slot date Character string containing date information.

setClass("MSigSeg",
         slots = list(data.input="matrix",data.output="matrix",lambda="numeric",brkps="numeric",fmin="numeric",date="character"),
         prototype = list(date=date()))

