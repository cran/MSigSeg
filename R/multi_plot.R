#' @title Plot function of MSigSeg package.
#' @description Graph signals and breakpoints based on ggplot2 and ggarange packages.
#' @param m An object of S4 class "MSigSeg".
#' @param ncol Column numbers of signals arrangement in the graph.
#' @param nrow Row numbers of signals arrangement in the graph
#' @return A list, first item in the list is a graphic objects with all signals drawn and second is a list with individual signals.
#'
#' @importFrom methods new
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom stats rbinom rnorm sd
#'
#' @examples
#' data(data_test)
#' m <- segmentation(data_test,100)
#' p <- multi_plot(m,4,5)
#'
#' @export

multi_plot <- function(m,ncol,nrow){
  x <- as.data.frame(m@data.input)
  y <- as.data.frame(m@data.output)
  cp <- m@brkps

  plot_list <- list()
  for (i in 1:ncol(x)) {
    p <- ggplot() +
      geom_point(data = x,aes_string(x=1:nrow(x),y=x[,i])) +
      geom_line(data = y,aes_string(x=1:nrow(y),y=y[,i])) +
      geom_vline(aes(xintercept=cp),linetype="dotted")
    plot_list[[i]] <- p
  }
  p_final <- ggarrange(plotlist = plot_list, ncol = ncol, nrow = nrow)
  return(list(p_final,plot_list))
}
