#' Load in climate data from NASA
#'
#' @param type Either \code{"GLB"}, \code{"NH"} or \code{"SH"} for global, northern or southern hemisphere temperature anomalies respectively
#'
#' @return A \code{\link[tibble]{tibble}} which contains the yearly values, as well as which series was obtained (i.e. GLB, NH or SH)
#' @export
#' @importFrom stats "na.omit"
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate" "select" "arrange"
#' @importFrom readr "read_csv"
#'
#' @seealso \code{\link{fit}}, \code{\link{plot.climr_fit}}
#' @examples
#' data = load_clim(type='SH')
load_clim <- function(type = c('GLB', 'NH', 'SH')) {

  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = x = year = NULL

  # Find out which type
  arg <- match.arg(type)

  # Get the URL of the data set
  url <- paste0('http://data.giss.nasa.gov/gistemp/tabledata_v3/',arg,'.Ts+dSST.csv')

  # Read in the data (read_cv comes from the readr package)
  out <- read_csv(url,
                 skip = 1,
                 na = '***',
                 col_types = paste(c('i', rep('d', 18)), collapse = ''),
                 progress = FALSE)

  # Sort out yearly data
  out_year <- out %>% na.omit() %>%
    mutate(year = Year,
           temp = `J-D`) %>%
    select(year, temp) %>%
    arrange(year)

  # Put it all in a list and return
  out_list <- list(clim_year = out_year, type = arg)
  class(out_list) <- 'climr'

  return(out_list)

}
