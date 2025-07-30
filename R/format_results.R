#' Functions for consistent formatting of linear regression results.
#'
#' @param x number. Either a linear rate, p-value, or r-squared value.
#' @param output_type string. One of:
#'  * `"text"`: returns the formatted value without italics.
#'  * `"plot"`: returns the formatted value prefixed with either the italicized p-value symbol or r2 symbol.
#'  * `"subtitle"`: for p-values only; returns the formatted p-value and symbol without italics.
#'
#' @description `r format_result_vals()` is used for consistently formatting
#' values to 2-decimal places + 2 significant digits. `r format_pval()` is used
#' for taking a model p-value and formatting it as one of the commonly used
#' p-value threshold symbols (e.g., p < 0.05, p < 0.01, etc.). `r format_r2()`
#' is used for consistently formatting a model r2 value (using format_result
#' vals) and optionally returning the italicized "r2" symbol with the
#' formatted r2 value.
#' 
#' @name format_results
#' @import dplyr
#' 
#' @examples
#' 
#' @rdname format_results
#' @export
format_result_vals <- function(x) {
  format(round(x, 2), nsmall = 2, digits = 2)
}

#' @rdname format_results
#' @export
format_pval <- function(x, output_type = "text") {
  
  if (output_type == "plot") {
    case_when(x > 0.05 ~ "italic(ns)",
              x <= 0.05 & x > 0.01 ~ "italic(p) < 0.05",
              x <= 0.01 & x > 0.001 ~ "italic(p) < 0.01",
              x <= 0.001 ~ "italic(p) < 0.001")
  } else if (output_type == "text") {
    case_when(x > 0.05 ~ "ns",
              x <= 0.05 & x > 0.01 ~ "p < 0.05",
              x <= 0.01 & x > 0.001 ~ "p < 0.01",
              x <= 0.001 ~ "p < 0.001")
  } else if (output_type == "subtitle") {
    case_when(x > 0.05 ~ "",
              x <= 0.05 & x > 0.01 ~ "< 0.05",
              x <= 0.01 & x > 0.001 ~ "< 0.01",
              x <= 0.001 ~ "< 0.001")
  } else if(output_type == "stars1") {
    case_when(x > 0.05 ~ "·",
              x <= 0.05 & x > 0.01 ~ "*",
              x <= 0.01 & x > 0.001 ~ "**",
              x <= 0.001 ~ "***")
  } else if(output_type == "stars2") {
    if_else(x > 0.05, "·", "*")
  }
}

#' @rdname format_results
#' @export
format_r2 <- function(x, output_type = "text") {
  
  if (output_type == "plot") { 
  paste("italic(r^2) == ", format_result_vals(x))
  } else if (output_type == "text") {
    format_result_vals(x)
  }
}
