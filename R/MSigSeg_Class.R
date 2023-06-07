#' An S4 class to encapsulation the result of changepoint analysis.
#'
#' @slot data.input An data.frame/matrix containing the data within which you wish to find common change points. Each column is considered a separate signal.
#' @slot data.output A matrix containing the input data which has been smoothed.
#' @slot lambda A numeric containing penalty coefficient to prevent over fitting.
#' @slot brkps A vector containing the locations of common change points.
#' @slot fmin A numeric containing the optimal numerical value calculated.
#' @slot date Character string containing date information.

setClass("MSigSeg",
         slots = list(data.input="matrix",data.output="matrix",lambda="numeric",brkps="numeric",fmin="numeric",date="character"),
         prototype = list(date=date()))

