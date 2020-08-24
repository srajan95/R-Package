#' Plot climr output
#'
#' @param x Output from the \code{\link{gp_fit}} function
#' @param time_grid An optional time grid over which to produce fitted values of the model
#' @param ... Other arguments to plot (not currently implemented)
#' 
#' @return Nothing: just a nice plot
#' @seealso \code{\link{load_clim}}, \code{\link{gp_fit}}
#' @export
#' @import ggplot2
#' @importFrom stats "predict"
#' @importFrom tibble "tibble"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' data1 = load_clim('NH')
#' data2 = gp_fit(data1)
#' plot(data2)
plot.climr_gp_fit <- function(x, time_grid = pretty(x$data$year, n = 100), ...) {
  
  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = year = NULL
  
  # Create a nice plot from the output of fit.climr
  
  # Get the data set out
  df <- x$data
  #Get the predicted data out
  preddata<-x$pred[,1]
  #stored in a data frame having time_grid and predicted data
  pred<-data.frame(time_grid,preddata)
  
  
 
  # # Finally create the plot
  ggplot(data=df,aes(year,temp))+geom_point(aes(colour = temp)) +
    theme_bw() +
    xlab('Year') +
    ylab('Temperature anomaly')  +
    scale_color_viridis()+
    geom_line(pred,mapping=aes(x=time_grid,y=preddata,colour=df$temp))+
    theme(legend.position = "none")
  
}
