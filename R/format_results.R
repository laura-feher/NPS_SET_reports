#' Functions for consistent formatting of linear regression results.
#'
#' @param x number. Either a linear rate, p-value, or r-squared value.
#' @param output_type string. One of:
#'  * `"text"`: returns the formatted value without italics.
#'  * `"plot"`: returns the formatted value prefixed with either the italicized p-value symbol or r2 symbol.
#'  * `"subtitle"`: for p-values only; returns the formatted p-value and symbol without italics.
#' @param park_code string. The 4-letter park unit code.
#' @param long_slr_rate_df dataframe. The long-term slr dataframe.
#' @param recent_slr_rate_df dataframe. The recent slr dataframe.
#' @param future_slr_rate_df dataframe. The future slr rate dataframe.
#' @param site_set_rates_df dataframe. A dataframe of site-level surface
#'   elevation change rates.
#'
#' @description Functions for consistent formatting of linear regression
#'   results. 
#'   `r format_result_vals()` is used for consistently formatting values to 
#'   2-decimal places + 2 significant digits.
#'   `r format_pval()` is used for taking a model p-value and formatting it as 
#'   one of the commonly used  p-value threshold symbols (e.g., p < 0.05, p < 
#'   0.01, etc.). 
#'   `r format_r2()`is used for consistently formatting a model r2 value (using 
#'   format_result_vals) and optionally returning the italicized "r2" symbol 
#'   with the formatted r2 value. 
#'   `r format_slr_rate_comps()` is used to create a dataframe comparing 
#'   site-level rates of surface elevation to long-term, recent, and future 
#'   predicted SLR.
#'
#' @name format_results
#' @import dplyr
#' @import tidyr
#'
#' @rdname format_results
#' @export
format_result_vals <- function(x, decimals = 2) {
  if(decimals > 0) {
    format(round(x, decimals), nsmall = decimals, digits = decimals)
  } else if (decimals == 0) {
    format(round(x, decimals), nsmall = decimals)
  }
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
    if_else(x > 0.05, "", "*")
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

#' @rdname format_results
#' @export
format_slr_rate_comps <- function(park_code, long_slr_rate_df, recent_slr_rate_df, future_slr_rate_df, site_set_rates_df) {
  
  slr_rate_comps <- long_slr_rate_df %>% 
    select(
      "slr_rate" = MSL.Trends.mm.yr., 
      "rate_type" = object_type
    ) %>%
    bind_rows(., recent_slr_rate_df %>% 
                filter(term == "yr") %>%
                select("slr_rate" = estimate, "rate_type" = object_type)) %>%
    bind_rows(., future_slr_rate_df %>%
                select("slr_rate" = future_slr_rate, "rate_type" = scenario_name)) %>%
    pivot_wider(., 
                names_from = rate_type, 
                values_from = slr_rate) %>%
    select(
      "long_rate" = `long term slr rate`, 
      "recent_rate" = `recent slr rate`, 
      "future_low_rate"  = low, 
      "future_int_low_rate" = int_low, 
      "future_int_rate" = "int", 
      "future_int_high_rate" = int_high, 
      "future_high_rate" = high
    ) %>%
    {if (park_code != "NACE")
      bind_cols(., site_set_rates_df %>%
                              select(
                                park_code,
                                site_name,
                                rate,
                                rate_se
                              ))
      else if (park_code == "NACE")
        bind_cols(., site_set_rates_df %>%
                                select(
                                  park_code,
                                  "site_name" = station_name,
                                  rate,
                                  rate_se
                                ))
      } %>%
    mutate(rate_plus_se = rate + rate_se) %>%
    {if (park_code == "CACO")
      mutate(.,
             long_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), long_slr_rate_nauset$MSL.Trends.mm.yr., long_rate),
             recent_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), recent_slr_rate_nauset$estimate[2], recent_rate),
             future_low_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), future_slr_rate_nauset$future_slr_rate[future_slr_rate_nauset$scenario_name == "low"], future_low_rate),
             future_int_low_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), future_slr_rate_nauset$future_slr_rate[future_slr_rate_nauset$scenario_name == "int_low"], future_int_low_rate),
             future_int_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), future_slr_rate_nauset$future_slr_rate[future_slr_rate_nauset$scenario_name == "int"], future_int_rate),
             future_int_high_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), future_slr_rate_nauset$future_slr_rate[future_slr_rate_nauset$scenario_name == "int_high"], future_int_high_rate),
             future_high_rate = if_else(site_name %in% c("Nauset Marsh new", "Nauset Marsh old"), future_slr_rate_nauset$future_slr_rate[future_slr_rate_nauset$scenario_name == "high"], future_high_rate))
      else .} %>%
    bind_cols(., long_slr_rate_df %>%
                select(long_rate_se)) %>%
    bind_cols(., recent_slr_rate_df %>%
                filter(term == "yr") %>%
                select("recent_rate_se" = std.error)) %>%
    mutate(long_rate_minus_se = long_rate - long_rate_se,
           recent_rate_minus_se = recent_rate - recent_rate_se) %>%
    mutate(., across(c(long_rate_minus_se, recent_rate_minus_se, future_low_rate, future_int_low_rate, future_int_rate, future_int_high_rate, future_high_rate), ~if_else(rate_plus_se >= .x, TRUE, FALSE), .names = "{.col}_comp")) %>%
    # across(c(long_rate:future_high_rate), ~if_else(rate >= .x, TRUE, FALSE), .names = "{.col}_comp"))
    rename(long_rate_comp = long_rate_minus_se_comp, recent_rate_comp = recent_rate_minus_se_comp)
}