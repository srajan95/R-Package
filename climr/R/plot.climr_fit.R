#' Plot climr output
#'
#' @param x Output from the \code{\link{fit}} function
#' @param time_grid An optional time grid over which to produce fitted values of the model
#' @param ... Other arguments to plot (not currently implemented)
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{load_clim}}, \code{\link{fit}}
#' @export
#' @import ggplot2
#' @importFrom stats "predict"
#' @importFrom tibble "tibble"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = fit(ans1, fit_type = 'smooth.spline')
#' plot(ans2)
plot.climr_fit <- function(x, time_grid = pretty(x$data$year, n = 100), ...) {
  
  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = year = NULL
  
  # Create a nice plot from the output of fit.climr
  
  # Get the data set out
  df <- x$data
  
  # Get some predicted values based on the time_grid
  if(x$fit_type == 'lm') {
    fits <- tibble(time_grid, pred = predict(x$model, newdata = tibble(x = time_grid)))
  } else if(x$fit_type == 'loess') {
    fits <- tibble(time_grid, pred = predict(x$model, newdata = tibble(x = time_grid))) %>%
      na.omit()
  } else if(x$fit_type == 'smooth.spline') {
    fits <- tibble(time_grid, pred = predict(x$model, tibble(time_grid))$y[,1])
  }
  
  # Finally create the plot
  ggplot(df, aes(year, temp)) +
    geom_point(aes(colour = temp)) +
    theme_bw() +
    xlab('Year') +
    ylab('Temperature anomaly') +
    geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
    theme(legend.position = 'None') +
    scale_color_viridis()
  
}
