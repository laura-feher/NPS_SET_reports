#' Scale SLR or linear model rates to work with date x-axis ggplots
#'
#' @description This function converts a slope and intercept from a linear model
#'   to the proper scale for use in `ggplot2::geom_abline()` when the x-axis of
#'   the plot is formatted as dates. Thus, this function can be used for either:
#'   1) to scale rates of SLR to work with ggplots with dates on the x-asis, or
#'   2) to scale linear rates of SET/MH change to work with ggplots with dates
#'   on the x-axis.
#'
#'
#' @param rate numeric. The slope from a linear model.
#' @param int numeric. The intercept from a linear model. Note that if you are
#'   scaling an SLR rate, 'int' needs to be the intercept of the SET/MH model.
#' @param first_date string. The first date of the SET/MH data. Needs to be
#'   formatted as "YYYY-MM-DD".
#'
#' @returns a list of two objects, "scaled_slope" and "scaled_int".
#'
#' @export
#'
#' @examples
#' slr_rate <- 3.13 # (rate of SLR at Woods Hole NOAA gauge as of June 2025)
#' set_int <- 9.80 # (imaginary intercept from a linear model of SET data)
#' set_first_date <- "2019-07-29"
#'
#' slr_line <- scale_slr_line(rate = slr_rate, int = set_int, first_date = set_first_date)
#'
scale_slr_line <- function(rate, int, first_date){

  # 'int' needs to be the intercept of the SET/MH linear model
  # first_date needs to be the first date of the SET/MH data

  scaled_slope <- rate*4/(4*365+1) # converts slope to mm/day and accounts for leap years
  scaled_int <- int - scaled_slope * as.integer(as.Date(first_date))

  return(list("scaled_slope" = scaled_slope, "scaled_int" = scaled_int))

}
