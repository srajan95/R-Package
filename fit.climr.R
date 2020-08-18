#' Fit basic statistical models to climate data
#'
#' @param obj An object of class \code{climr} from \code{\link{load_clim}}
#' @param fit_type The type of model required, either linear regression (\code{lm}), loess, or smoothing spline (\code{smooth.spline})
#'
#' @return Returns a list of class \code{climr_fit} which includes the model details as well as the data set and fit type used
#' @seealso \code{\link{load_clim}}, \code{\link{plot.climr_fit}}
#' @export
#' @importFrom magrittr "%$%"
#' @importFrom stats "lm" "loess" "smooth.spline"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = fit(ans1, 'lm')
fit <- function(obj, fit_type = c('lm', 'loess', 'smooth.spline')) {
  UseMethod('fit')
}

fit.climr <- function(obj, fit_type = c('lm', 'loess', 'smooth.spline')) {

  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = x = year = NULL

  # Find what type of fitting method
  fit_arg <- match.arg(fit_type)

  # Fit some models
  if(fit_arg == 'lm') {
    mod <- obj$clim_year %$% lm(temp ~ year)
  } else if(fit_arg == 'loess') {
    mod <- obj$clim_year %$% loess(temp ~ year)
  } else if(fit_arg == 'smooth.spline') {
    mod <- obj$clim_year %$% smooth.spline(year, temp)
  }
  print(mod)

  # Output so it can be plotted
  out <- list(model = mod,
             data = obj$clim_year,
             fit_type = fit_arg)
  class(out) <- 'climr_fit'

  invisible(out)

}


