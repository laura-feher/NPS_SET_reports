#' Functions for consistent formatting of kable tables.
#'
#' @param data_df dataframe. A dataframe of SET pin_heights.
#' @param dom_veg_df dataframe. A dataframe of the dominant vegetation at each
#'   site.
#' @param park_name string. The full official park unit name.
#' @param site_order vector. A character vector defining the site order.
#' @param site_set_rates_df dataframe. A dataframe of site-level surface
#'   elevation change rates.
#' @param slr_rate_comps_df dataframe. A dataframe of site-level surface
#'   elevation change rates compared to rates of SLR.
#' @param site_hydro_df dataframe. A dataframe of site-level hydrology metrics,
#'   tidal datums, marsh elevations, and elevation capital.
#' @param long_slr_rate_df dataframe. The long-term slr dataframe.
#'
#' @description Functions for consistent formatting of kable tables.
#'  `r site_info_table()` creates table 1 showing site-specific details. 
#'  `r site_elev_rates_table()` creates table 2 showing the site-level rates of
#'   surface elevation change. 
#'   `r summary_tbl()` creates table 3 showing the summary of results for each
#'    site.
#'
#' @name format_tables
#' @import dplyr
#' @import kable
#' @import kableExtra
#'
#' @rdname format_tables
#' @export
site_info_tbl <- function(data_df, dom_veg_df, park_name, site_order){
  
  park <- data_df %>%
    distinct(park_code) %>%
    pull(park_code)
  
  network <- data_df %>%
    mutate(network_code = if_else(park_code %in% c("ACAD", "BOHA"), "NCBN", network_code)) %>%
    distinct(network_code) %>%
    pull(network_code)
  
  if (!park %in% c("GWMP", "NACE")){
    data_df %>%
      mutate(site_name = forcats::fct_relevel(site_name, site_order)) %>%
      group_by(network_code, park_code, park_name, site_name) %>%
      summarise(station_count = n_distinct(station_code),
                first_year = lubridate::year(min(event_date_UTC)),
                last_year = lubridate::year(max(event_date_UTC)),
                site_status = unique(site_status)) %>%
      ungroup() %>%
      left_join(., dom_veg_df) %>%
      select("Site" = site_name, "SET station count" = station_count, "First reading" = first_year, "Most recent reading" = last_year, "Status" = site_status, "Dominant vegetation" = dom_veg) %>%
      kable(., booktabs = T, label = NA, align = "c", escape = FALSE, caption = paste0("Table 1. Surface elevation table sites monitored by ", network, " within ", park_name, ".")) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) 
  } else {
    data_df %>%
      mutate(station_name = forcats::fct_relevel(station_name, station_order)) %>%
      group_by(network_code, park_code, park_name, site_name, station_name) %>%
      summarise(first_year = lubridate::year(min(event_date_UTC)),
                last_year = lubridate::year(max(event_date_UTC)),
                station_status = unique(station_status)) %>%
      ungroup() %>%
      left_join(., dom_veg_df) %>%
      select("Station" = station_name, "First reading" = first_year, "Most recent reading" = last_year, "Status" = station_status, "Dominant vegetation" = dom_veg) %>%
      kable(., booktabs = T, label = NA, align = "c", escape = FALSE, caption = paste0("Table 1. Surface elevation table stations monitored by NCRN within ", park_name, ".")) %>%
      kable_styling(bootstrap_options = c("striped", "hover")) 
  }
}
#' 
#' @rdname format_tables
#' @export
site_elev_rates_tbl <- function(site_set_rates_df) {
  
  park <- site_set_rates_df %>%
    distinct(park_code) %>%
    pull(park_code)
  
  site_set_rates_df %>%
    mutate(site_set_rate_text_tbl = paste0(format_result_vals(rate), " ± ", format_result_vals(rate_se), format_pval(rate_p, output_type = "stars2")),
           adj_r2 = format_result_vals(rate_r2),
           first_year = map_chr(data, function(x) as.character(lubridate::year(min(x$event_date_UTC)))),
           last_year = map_chr(data, function(x) as.character(lubridate::year(max(x$event_date_UTC)))),
           date_range = paste0(first_year, "-", last_year)) %>%
    ungroup() %>%
    # select(site_name, site_set_rate_text_tbl, adj_r2, date_range) %>%
    {if (!park %in% c("GWMP", "NACE")) 
      select(., site_name, site_set_rate_text_tbl, adj_r2, date_range) %>%
      kable(., booktabs = T, label = NA, align = "c", caption = paste0("Table 2. Site-level rates of surface elevation change within ", params$park_name, ". Rates of surface elevation change labeled with * are significantly different from 0. Note that 𝑟² values represent the proportion of variation in the data explained by the site-level rates of surface elevation change."), col.names = c("Site", "Rate of surface<br>elevation change (mm/yr)", "Adjusted 𝑟²", "Years measured"), escape = FALSE) 
      else 
        {if (park == "NACE")
          select(., station_name, site_set_rate_text_tbl, adj_r2, date_range)
          else select(., site_name, site_set_rate_text_tbl, adj_r2, date_range)} %>%
            kable(., booktabs = T, label = NA, align = "c", caption = paste0("Table 2. Station-level rates of surface elevation change within ", params$park_name, ". Rates of surface elevation change labeled with * are significantly different from 0. Note that 𝑟² values represent the proportion of variation in the data explained by the station-level rates of surface elevation change."), col.names = c("Station", "Rate of surface<br>elevation change (mm/yr)", "Adjusted 𝑟²", "Years measured"), escape = FALSE)
    } %>%
    kable_styling(., bootstrap_options = c("striped", "hover"))
}
#' 
#' @rdname format_tables
#' @export
summary_tbl <- function(slr_rate_comps_df, site_hydro_df, park_name, long_slr_rate_df) {
  
  park <- slr_rate_comps_df %>%
    ungroup() %>%
    distinct(park_code) %>%
    pull(park_code)
  
  site_station <- if_else(!park %in% c("GWMP", "NACE"), "Site", "Station")
  
  green_text <- "green"
  red_text <- "#AD0000"
  
  if (!park %in% c("VIIS", "SARI", "BISC", "CANA", "CAHA", "CUIS", "FOFR", "FOPU", "TIMU", "FOMA", "CALO")) {
    slr_rate_comps_df %>%
      {if (park %in% c("GWMP", "NACE"))
           left_join(., site_hydro_df, by = c("site_name" = "station_name"), keep = TRUE) 
           else
             left_join(., site_hydro_df, by = "site_name") 
             } %>%
      select(., site_name, park_code, rate, rate_se, elev_navd88, MHW, MSL, percent_time_flooded, elev_capital, long_rate_comp, recent_rate_comp) %>%
      mutate(.,
             rate_max = as.numeric(format(round(max(rate), 2), nsmall = 2)),
             rate_min = as.numeric(format(round(min(rate), 2), nsmall = 2)),
             across(c(elev_navd88, MHW, MSL), ~as.numeric(format(round(max(.x), 3), nsmall = 3)), .names = "{.col}_max"),
             across(c(elev_navd88, MHW, MSL), ~as.numeric(format(round(min(.x), 3), nsmall = 3)), .names = "{.col}_min"),
             percent_time_flooded_max = as.numeric(format(round(max(percent_time_flooded), 0), nsmall = 0)),
             percent_time_flooded_min = as.numeric(format(round(min(percent_time_flooded), 0), nsmall = 0)),
             across(c(rate, rate_se), ~format_result_vals(.x, decimals = 2), .names = "{.col}_text"),
             across(c(elev_navd88, MHW, MSL), ~format_result_vals(.x, decimals = 3), .names = "{.col}_text"),
             percent_time_flooded_text = format_result_vals(percent_time_flooded, decimals = 0),
             elev_capital_text = format_result_vals(elev_capital, 2),
             rate_plus_se_text = paste0(rate_text, " ± ", rate_se_text),
             long_rate_comp_text = if_else(long_rate_comp, "close or greater", "lower"),
             recent_rate_comp_text = if_else(recent_rate_comp, "close or greater", "lower")) %>%
      mutate(
        rate_plus_se_text = cell_spec(
          rate_plus_se_text,
          format = "html",
          color = case_when(
            as.numeric(rate_text) == rate_max ~ green_text,
            as.numeric(rate_text) == rate_min ~ red_text,
            .default = "black"
          )
        ),
        elev_navd88_text = cell_spec(
          elev_navd88_text,
          format = "html",
          color = case_when(
            as.numeric(elev_navd88_text) == elev_navd88_max ~ green_text,
            as.numeric(elev_navd88_text) == elev_navd88_min ~ red_text,
            .default = "black"
          )
        ),
        MHW_text = cell_spec(
          MHW_text,
          format = "html",
          color = case_when(
            as.numeric(MHW_text) == MHW_min & park_code == "BOHA" ~ "black",
            as.numeric(MHW_text) == MHW_min ~ green_text,
            as.numeric(MHW_text) == MHW_max ~ red_text,
            .default = "black"
          )
        ),
        
        MSL_text = cell_spec(
          MSL_text,
          format = "html",
          color = case_when(
            as.numeric(MSL_text) == MSL_min & park_code == "BOHA" ~ "black",
            as.numeric(MSL_text) == MSL_min ~ green_text,
            as.numeric(MSL_text) == MSL_max ~ red_text,
            .default = "black"
          )
        ),
        percent_time_flooded_text = cell_spec(
          percent_time_flooded_text,
          format = "html",
          color = case_when(
            as.numeric(percent_time_flooded_text) == percent_time_flooded_min ~ green_text,
            as.numeric(percent_time_flooded_text) == percent_time_flooded_max ~ red_text,
            .default = "black"
          )
        ),
        elev_capital_text = cell_spec(
          elev_capital_text,
          format = "html",
          color = if_else(
            as.numeric(elev_capital_text) >= 0.5,
            green_text,
            red_text
          )
        ),
        long_rate_comp_text = cell_spec(
          long_rate_comp_text,
          format = "html",
          color = if_else(
            long_rate_comp_text == "lower",
            red_text,
            green_text
          )
        ),
        recent_rate_comp_text = cell_spec(
          recent_rate_comp_text,
          format = "html",
          color = if_else(
            recent_rate_comp_text == "lower",
            red_text,
            green_text
          )
        )
      ) %>%
      { if (!park %in% c("GWMP", "NACE"))
        select(., site_name, rate_plus_se_text, elev_navd88_text, MHW_text, MSL_text, percent_time_flooded_text, elev_capital_text, long_rate_comp_text, recent_rate_comp_text) %>%
          kable(., booktabs = T, label = NA, escape = FALSE, align = "c",
                caption = paste0("Table 3. ", park_name, " data summary (", years, "). Note that green/red text represent high/low values for each column."),
                col.names = c(site_station, "SEC rate (mm/yr)", "Marsh elevation (m NAVD88)", "MHW (m NAVD88)", "MSL (m NAVD88)", "Time flooded (%)", "Elevation capital", paste0("SEC vs. SLR-long", footnote_marker_number(1)), paste0("SEC vs. SLR-recent", footnote_marker_number(2)))) 
        else 
          select(., site_name, rate_plus_se_text, elev_navd88_text, percent_time_flooded_text, elev_capital_text, long_rate_comp_text, recent_rate_comp_text) %>%
          kable(., booktabs = T, label = NA, escape = FALSE, align = "c",
                caption = paste0("Table 3. ", park_name, " data summary (", years, "). Note that green/red text represent high/low values for each column."),
                col.names = c(site_station, "SEC rate (mm/yr)", "Marsh elevation (m NAVD88)", "Time flooded (%)", "Elevation capital", paste0("SEC vs. SLR-long", footnote_marker_number(1)), paste0("SEC vs. SLR-recent", footnote_marker_number(2))))
      } %>%
      footnote(., number = c(paste0("Long-term SLR - rate of surface elevation change compared to the long-term rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (", long_slr_rate_df$First.Year, "-", long_slr_rate_df$Last.Year, ")."), paste0("Recent SLR - rate of surface elevation change compared to the recent rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (2001-2019)."))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>%
      scroll_box(width = "100%")
  } else if (park == "FOFR") {
    slr_rate_comps_df %>%
      select(site_name, rate, rate_se, long_rate_comp, recent_rate_comp) %>%
      mutate(
        rate_max = as.numeric(format(round(max(rate), 2), nsmall = 2)),
        rate_min = as.numeric(format(round(min(rate), 2), nsmall = 2)),
        across(c(rate, rate_se), ~format_result_vals(.x, decimals = 2), .names = "{.col}_text"),
        rate_plus_se_text = paste0(rate_text, " ± ", rate_se_text),
        long_rate_comp_text = if_else(long_rate_comp, "close or greater", "lower"),
        recent_rate_comp_text = if_else(recent_rate_comp, "close or greater", "lower")
      ) %>%
      mutate(
        rate_plus_se_text = cell_spec(
          rate_plus_se_text,
          format = "html",
          color = case_when(
            as.numeric(rate_text) == rate_max ~ green_text,
            as.numeric(rate_text) == rate_min ~ red_text,
            .default = "black"
          )
        ),
        long_rate_comp_text = cell_spec(
          long_rate_comp_text,
          format = "html",
          color = if_else(
            long_rate_comp_text == "lower",
            red_text,
            green_text
          )
        ),
        recent_rate_comp_text = cell_spec(
          recent_rate_comp_text,
          format = "html",
          color = if_else(
            recent_rate_comp_text == "lower",
            red_text,
            green_text
          )
        )
      ) %>%
      select(., site_name, rate_plus_se_text, long_rate_comp_text, recent_rate_comp_text) %>%
      kable(booktabs = T, label = NA, escape = FALSE, align = "c",
            caption = paste0("Table 3. ", park_name, " data summary (", years, ")."),
            col.names = c("Site", "SEC rate (mm/yr)", paste0("SEC vs. SLR-long", footnote_marker_number(1)), paste0("SEC vs. SLR-recent", footnote_marker_number(2)))) %>%
      footnote(number = c(paste0("Long-term SLR - rate of surface elevation change compared to the long-term rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (", long_slr_rate_df$First.Year, "-", long_slr_rate_df$Last.Year, ")."), paste0("Recent SLR - rate of surface elevation change compared to the recent rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (2001-2019)."))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>%
      scroll_box(width = "100%")
    
  } else {
    slr_rate_comps_df %>%
      { if (park %in% c("CANA", "CAHA", "CUIS", "FOPU", "TIMU", "FOMA", "CALO"))
        bind_cols(., site_hydro_df %>%
                    ungroup() %>%
                    select(-c(site_name)))
        else
          left_join(., site_hydro_df, by = "site_name")} %>%
      select(., site_name, rate, rate_se, MHW, MSL, long_rate_comp, recent_rate_comp) %>%
      mutate(
        rate_max = as.numeric(format(round(max(rate), 2), nsmall = 2)),
        rate_min = as.numeric(format(round(min(rate), 2), nsmall = 2)),
        MHW_max = as.numeric(format(round(max(MHW), 3), nsmall = 3)),
        MHW_min = as.numeric(format(round(min(MHW), 3), nsmall = 3)),
        MSL_max = max(MSL),
        MSL_min = min(MSL),
        across(c(rate, rate_se), ~format_result_vals(.x, decimals = 2), .names = "{.col}_text"),
        MHW_text = format_result_vals(MHW, decimals = 3),
        MSL_text = if_else(round(MSL_max, 3) == round(MSL_min, 3) & !park %in% c("CANA", "CAHA", "CUIS", "FOPU", "TIMU", "FOMA", "CALO"), format_result_vals(MSL, decimals = 4), format_result_vals(MSL, decimals = 3)),
        rate_plus_se_text = paste0(rate_text, " ± ", rate_se_text),
        long_rate_comp_text = if_else(long_rate_comp, "close or greater", "lower"),
        recent_rate_comp_text = if_else(recent_rate_comp, "close or greater", "lower")) %>%
      mutate(
        rate_plus_se_text = cell_spec(
          rate_plus_se_text,
          format = "html",
          color = case_when(
            park %in% c("SARI", "FOMA", "CALO") ~ "black",
            as.numeric(rate_text) == rate_max ~ green_text,
            as.numeric(rate_text) == rate_min ~ red_text,
            .default = "black"
          )
        ),
        MHW_text = cell_spec(
          MHW_text,
          format = "html",
          color = case_when(
            park %in% c("SARI", "CANA", "CAHA", "CUIS", "FOPU", "TIMU", "FOMA", "CALO") ~ "black",
            as.numeric(MHW_text) == MHW_min ~ green_text,
            as.numeric(MHW_text) == MHW_max ~ red_text,
            .default = "black"
          )
        ),
        
        MSL_text = cell_spec(
          MSL_text,
          format = "html",
          color = case_when(
            park %in% c("SARI", "CANA", "CAHA", "CUIS", "FOPU", "TIMU", "FOMA", "CALO") ~ "black",
            MSL == MSL_min ~ green_text,
            MSL == MSL_max ~ red_text,
            .default = "black"
          )
        ),
        long_rate_comp_text = cell_spec(
          long_rate_comp_text,
          format = "html",
          color = if_else(
            long_rate_comp_text == "lower",
            red_text,
            green_text
          )
        ),
        recent_rate_comp_text = cell_spec(
          recent_rate_comp_text,
          format = "html",
          color = if_else(
            recent_rate_comp_text == "lower",
            red_text,
            green_text
          )
        )
      ) %>%
      select(., site_name, rate_plus_se_text, MHW_text, MSL_text, long_rate_comp_text, recent_rate_comp_text) %>%
      kable(booktabs = T, label = NA, escape = FALSE, align = "c",
            caption = paste0("Table 3. ", park_name, " data summary (", years, ")."),
            col.names = c("Site", "SEC rate (mm/yr)", "MHW (m)", "MSL (m)", paste0("SEC vs. SLR-long", footnote_marker_number(1)), paste0("SEC vs. SLR-recent", footnote_marker_number(2)))) %>%
      footnote(number = c(paste0("Long-term SLR - rate of surface elevation change compared to the long-term rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (", long_slr_rate_df$First.Year, "-", long_slr_rate_df$Last.Year, ")."), paste0("Recent SLR - rate of surface elevation change compared to the recent rate of SLR from the NOAA gauge at ", long_slr_rate_df$Station.Name, " (2001-2019)."))) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>%
      scroll_box(width = "100%")
  } 
}