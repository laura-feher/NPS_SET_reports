#' Functions for consistent formatting of inline text results.
#'
#' @param park_code string. The 4-letter park unit code.
#' @param long_slr_rate_nau dataframe. Optional, dataframe of long-term slr for
#'   Nauset.
#' @param recent_slr_rate_nau dataframe. Optional, dataframe of recent slr for
#'   Nauset.
#' @param future_slr_rate_nau dataframe. Optional, dataframe of future predicted
#'   slr for Nauset.
#' @param park_name string. The full official park unit name.
#' @param data_df dataframe. A dataframe of SET pin_heights.
#' @param site_hydro_df dataframe. A dataframe of site-level hydrology metrics,
#'   tidal datums, marsh elevations, and elevation capital.
#' @param slr_rate_comps_df dataframe. A dataframe of site-level surface
#'   elevation change rates compared to rates of SLR.
#' @param rate_type string. One of:
#'  *"relative long-term"
#'  *"relative recent"
#' @param dp_year string. The year that the data package was published on
#'   datastore.
#'
#' @description Functions for consistent formatting of inline text results. 
#'   `r total_measurements_text()` prints the sentence describing the number of
#'   measurements taken on each sampling event in the 'Protocol' methods
#'   section. 
#'   `r extra_sample_size_text()` prints any extra details about
#'   site-specific info in the 'Study Site & Sample Size' methods section. 
#'   `r extra_hydro_text()` prints a sentence about which water level loggers were
#'   used for each site. 
#'   `r extra_slr_methods_text()` prints info about the
#'   separate gauge that was used for the Nauset site at CACO. 
#'   `r elev_capital_comp_text()` prints a sentence describing the site-level
#'   elevation capital values. 
#'   `r extra_slr_results_text()` returns sentences
#'   about the other long-term and recent SLR for Nauset and site-level
#'   comparisons to future SLR.
#'   `r slr_current_rate_comp_text()` prints a sentence
#'   describing the site-level comparisons to long-term or recent SLR. 
#'   `r summary_paragraph()` returns sentences summarizing the results for the annual
#'   report.
#'
#' @name format_text
#' @import dplyr
#' @import knitr
#' @import english
#'
#' @rdname format_text
#' @export
total_measurements_text <- function(park_code) {
  
  if (park_code == "CACO") {
    text <- paste("Note that each site has between 3 to 9 stations, each station (an individual SET) has four arm positions, and each arm has nine pins - thus a total of 108 to 324 nested measurements are taken at each site on each sampling event.")
  } else if (park_code %in% c("ASIS", "COLO", "FIIS", "GATE", "ACAD")) {
    text <- paste("Note that each site has 3 stations, each station (an individual SET) has four arm positions, and each arm has nine pins - thus a total of 108 nested measurements are taken at each site on each sampling event.")
  } else if (park_code == "BOHA") {
    text <- paste("Note that each site has between 3 to 4 stations, each station (an individual SET) has four arm positions, and each arm has nine pins - thus a total of 108 to 144 nested measurements are taken at each site on each sampling event.")
  } else if (park_code %in% c("GWMP", "NACE")) {
    text <- paste("Note that each station has a single SET, each individual SET has four arm positions, and each arm has nine pins - thus a total of 36 nested measurements are taken at each station on each sampling event.")
  } else {
    text <- ""
  }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
extra_sample_size_text <- function(park_code) {
  
  if (park_code == "COLO") {
    text <- paste("Note that at Marsh 19, station 2 was retired in 2023 due to erosion and replaced with station 4 in 2024.")
  } else if (park_code == "CACO") {
    text <- paste("Note that 4 stations at Nauset Marsh that used the original-style SET arm were retired in 2013 and replaced with 3 new stations that use the newer deep rod-style SET arm. Similarly, 3 stations at Gut were retired in 2015 for the same reason and replaced with 3 new stations. Finally, the 6 stations at Hatches Harbor restricted and 3 stations at Hatches Harbor unrestricted were retired in 2013 for the same reason and replaced with 6 new stations. At these sites (Hatches Harbor restricted, Hatches Harbor unrestricted, Gut, and Nauset Marsh), data for the older original-style SETs has been combined with the data from the newer deep rod-style SETs by standardizing the pin height measurements to soil elevation referenced to the vertical datum NAVD88 using the formula outlined in the 'Marsh Surface Elevation Data' section below.")
  } else if (park_code == "GATE") {
    text <- paste("One of the sites - Big Egg Spray-Restored - was restored in 2003 using dregded sediment to raise the elevation of the marsh platform.")
  } else if (park_code == "ACAD") {
    text <- paste("Note that at station 1 at Bass Harbor, the benchmark was bent by ice in 2016 and was subsequently replaced with a newly installed benchmark in 2017.")
  } else if (park_code == "GWMP") {
    text <- paste("Note that the Creek Bank, Interior, and River stations were retired in 2019 due to severe erosion and are no longer actively monitored. The retired stations were replaced with the 12 Dyke SET stations in 2024 but data from these stations have not yet been analyzed since they are only a year old. Thus, only data from the ten retired stations are presented in this report.")
  } else if (park_code == "NACE") {
    text <- paste("Note that the stations Kenilworth 1 to 5 and Kingman 1 to 5 were retired in 2019 due to severe erosion and are no longer actively monitored. The retired stations were replaced with the 6 KenSET stations in 2022.")
  } else {
    text <- ""
    }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
sfcn_water_level_text <- function(park_code) {
  
  if (park_code == "SARI") {
    text <- paste0("Mean low water (MLW) was ", format_result_vals(site_hydro$MLW, decimals = 3), " m, mean high water was ", format_result_vals(site_hydro$MHW, decimals = 3), " m, and mean sea level (MSL) was ", format_result_vals(site_hydro$MSL, decimals = 3), " m.")
  } else if (park_code %in% c("VIIS", "BISC")) {
    text <- paste0("Mean low water (MLW) at the ", as.character(english::english(active_site_count)), " sites varied by ", format_result_vals((max(site_hydro$MLW)-min(site_hydro$MLW)), decimals = 3), " m, with ", site_hydro %>% ungroup() %>% slice_min(as.numeric(format_result_vals(MLW, decimals = 3)), n = 1) %>% pull(site_name) %>% knitr::combine_words(), " having the lowest MLW value (", format_result_vals(min(site_hydro$MLW), decimals = 3), " m) and ", site_hydro %>% ungroup() %>% slice_max(as.numeric(format_result_vals(MLW, decimals = 3)), n = 1) %>% pull(site_name) %>% knitr::combine_words(), " having the highest MLW value (", format_result_vals(max(site_hydro$MLW), decimals = 3), " m). Similarly, mean high water (MHW) varied by ", format_result_vals((max(site_hydro$MHW)-min(site_hydro$MHW)), decimals = 3), " m, with ", site_hydro %>% ungroup() %>% slice_min(as.numeric(format_result_vals(MHW, decimals = 3)), n = 1) %>% pull(site_name) %>% knitr::combine_words(), " having the lowest MHW value (", format_result_vals(min(site_hydro$MHW), decimals = 3), " m) and ", site_hydro %>% ungroup() %>% slice_max(as.numeric(format_result_vals(MHW, decimals = 3)), n = 1) %>% pull(site_name) %>% knitr::combine_words(), " having the highest MHW value (", format_result_vals(max(site_hydro$MHW), decimals = 3), " m). Finally, mean sea level varied by ", if_else(format_result_vals((max(site_hydro$MSL)-min(site_hydro$MSL)), decimals = 3) == "0.000", format(round((max(site_hydro$MSL)-min(site_hydro$MSL)), 4), nsmall = 4, scientific = FALSE), format_result_vals((max(site_hydro$MSL)-min(site_hydro$MSL)), decimals = 3)),  ", with ", site_hydro %>% ungroup() %>% slice_min(as.numeric(format_result_vals(MSL, decimals = 4)), n = 1) %>% pull(site_name) %>% knitr::combine_words(),
                   " having the lowest MSL value (", if_else(format_result_vals(max(site_hydro$MSL), decimals = 3) == format_result_vals(min(site_hydro$MSL), decimals = 3), format(round(min(site_hydro$MSL), 4), scientific = FALSE), format_result_vals(min(site_hydro$MSL), decimals = 3)),
                   " m) and ", site_hydro %>% ungroup() %>% slice_max(as.numeric(format_result_vals(MSL, decimals = 4)), n = 1) %>% pull(site_name) %>% knitr::combine_words(),
                   " having the highest MSL value (", if_else(format_result_vals(max(site_hydro$MSL), decimals = 3) == format_result_vals(min(site_hydro$MSL), decimals = 3), format(round(max(site_hydro$MSL), 4), scientific = FALSE), format_result_vals(max(site_hydro$MSL), decimals = 3)),
                   " m). The park-wide average values for MLW, MHW, and MSL were ", format_result_vals(park_datums$MLW, decimals = 3),
                   ", ", format_result_vals(park_datums$MHW, decimals = 3), ", and ", format_result_vals(park_datums$MSL, decimals = 3),
                   " m, respectively.")
  } else {
    text <- ""
  }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
extra_hydro_text <- function(park_code) {
  
  if (park_code == "ASIS") {
    text <- paste("Note that the data from the water logger at Marsh 6 was also applied to Marsh 11 given their close proximity.")
  } else if (park_code == "COLO") {
    text <- paste("Note that the data from the water logger at Marsh 19 was applied to Marsh 5 and the data from the water logger at Marsh 30 was applied to Marsh 13 given their close proximity.")
  } else if (park_code == "GATE") {
    text <- paste("Note that due to their proximity, the data from the water logger at Black Bank was applied to Elders East and Elders East NF, the data from the water logger at Big Egg Control was applied to Big Egg Spray-Restored, and the data from the water logger at JOCO was applied to JOCO REF.") 
  } else if (park_code == "CACO") {
    text <- paste("Note that the data from the water logger at Blackfish Creek was also applied to High Toss, Phrag, and Gut given their close proximity.")
  } else if (park_code == "GWMP") {
    text <- paste("Note that the data from the single water logger was applied to all stations at Dyke Marsh given their close proximity.")
  } else if (park_code == "NACE") {
    text <- paste("Note that the data from the single water logger was applied to all stations at Kenilworth Marsh and Kingman Lake given their close proximity.")
  } else {
    text <- ""
    }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
extra_slr_methods_text <- function(park_code) {
  
  if (park_code == "CACO") {
    text <- list(
      extra_slr_methods_text1 = paste0("Additionally, a separate long-term relative rate of sea-level rise was calculated for the site at Nauset Marsh due to its location on the Atlantic side of the Cape. This rate was obtained from the NOAA tides and currents website for the nearby tide gauge at Nantucket Island, MA (Station ID: 8449130) and is based on the full record of water level observations collected between 1955 up to ", year(Sys.Date())-1,"."),
      extra_slr_methods_text2 = paste0("Additionally, a separate recent rate of relative sea-level rise was calculated for Nauset Marsh using the data from the Nantucket Island, MA gauge."),
      extra_slr_methods_text3 = paste0("Similarly, future predicted sea-level rise rates were estimated for Nauset Marsh using the same method for the Nantucket Island, MA gauge.")
    )
  } else {
    text <- list(
      extra_slr_methods_text1 = "",
      extra_slr_methods_text2 = "",
      extra_slr_methods_text3 = ""
    )
  }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
elev_capital_comp_text <- function(park_name, park_code, data_df, site_hydro_df) {
  
  data <- data_df
  num_sites <- if_else(!park_code %in% c("GWMP", "NACE"), n_distinct(data$site_name), n_distinct(droplevels(site_hydro_df$station_name)))
  
  elev_cap_df <- site_hydro_df %>%
    mutate(., elev_cap_comp = if_else(elev_capital >= 0.5, TRUE, FALSE))
  
  num_greater_sites <- sum(elev_cap_df$elev_cap_comp)
  num_lesser_sites <- sum(!elev_cap_df$elev_cap_comp)
  
  site_name_text_lesser <- elev_cap_df %>%
    filter(!elev_cap_comp) %>%
    { if (!park_code %in% c("GWMP", "NACE"))
      pull(., site_name)
      else if (park_code %in% c("GWMP", "NACE"))
        pull(., station_name)
      } %>%
    knitr::combine_words(.)
  
  site_name_text_greater <- elev_cap_df %>%
    filter(elev_cap_comp) %>%
    { if (!park_code %in% c("GWMP", "NACE"))
      pull(., site_name)
      else if (park_code %in% c("GWMP", "NACE"))
        pull(., station_name)
      } %>%
    knitr::combine_words(.)
  
  sites_stations <- if_else(park_code %in% c("GWMP", "NACE"), " stations ", " sites ")
  
  all_sites_greater_sentence <- function(park_name, num_sites) {
    paste0("All ", english::english(num_sites), sites_stations, "at ", park_name, " had elevation capital values above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  all_sites_lesser_sentence <- function(park_name, num_sites) {
    paste0("All ", english::english(num_sites), sites_stations, "at ", park_name, " had elevation capital values below 0.5, indicating a higher vulnerability to sea-level rise.")
  }
  
  one_site_greater_sentence <- function(park_name, num_sites, site_name_text_greater) {
    paste0(site_name_text_greater, " had an elevation capital value above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  one_site_lesser_sentence <- function(park_name, num_sites, site_name_text_lesser) {
    paste0(site_name_text_lesser, " had an elevation capital value below 0.5, indicating a higher vulnerability to sea-level rise.")
  }
  
  any_sites_greater_sentence <- function(park_name, num_sites, num_greater_sites, site_name_text_greater) {
    paste0(english::Words(num_greater_sites), sites_stations, "- ", site_name_text_greater, " - had elevation capital values above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  any_sites_lesser_sentence <- function(park_name, num_sites, num_lesser_sites, site_name_text_lesser) {
    paste0("Out of the ", english::english(num_sites), sites_stations, "at ", park_name, ", ", english::english(num_lesser_sites), sites_stations, "- ", site_name_text_lesser, " - had elevation capital values below 0.5, indicating a higher vulnerability to sea-level rise.")
  }
  
  if(num_greater_sites == num_sites & num_sites > 1) {
    text <- all_sites_greater_sentence(park_name = park_name, num_sites = num_sites)
  } else if (num_greater_sites == num_sites & num_sites == 1) {
    text <- one_site_greater_sentence(park_name = park_name, num_sites = num_sites, site_name_text_greater = site_name_text_greater)
  } else if (num_greater_sites > 1 & num_lesser_sites > 1 & num_sites > 1) {
    text <- paste0(any_sites_lesser_sentence(park_name = park_name, num_sites = num_sites, num_lesser_sites = num_lesser_sites, site_name_text_lesser = site_name_text_lesser), " ", any_sites_greater_sentence(park_name = park_name, num_sites = num_sites, num_greater_sites = num_greater_sites, site_name_text_greater = site_name_text_greater))
  } else if (num_greater_sites > 1 & num_lesser_sites == 1 & num_sites > 1) {
    text <- paste0(one_site_lesser_sentence(park_name = park_name, num_sites = num_sites, site_name_text_lesser = site_name_text_lesser), " ", any_sites_greater_sentence(park_name = park_name, num_sites = num_sites, num_greater_sites = num_greater_sites, site_name_text_greater = site_name_text_greater))
  } else if (num_greater_sites == 1 & num_lesser_sites > 1 & num_sites > 1){
    text <- paste0(any_sites_lesser_sentence(park_name = park_name, num_sites = num_sites, num_lesser_sites = num_lesser_sites, site_name_text_lesser = site_name_text_lesser), " ", one_site_greater_sentence(park_name = park_name, num_sites = num_sites, site_name_text_greater = site_name_text_greater))
  } else if (num_greater_sites < 1 & num_lesser_sites == num_sites) {
    text <- all_sites_lesser_sentence(park_name = park_name, num_sites = num_sites)
  }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
extra_slr_results_text <- function(park_code, long_slr_rate_nau = NA, recent_slr_rate_nau = NA, future_slr_rate_nau = NA) {
  
  if (park_code == "CACO") {
    long_slr_rate_nm <- long_slr_rate_nau
    recent_slr_rate_nm <- recent_slr_rate_nau
    future_slr_rate_nm <- future_slr_rate_nau
    
    text <- list(
      extra_long_slr_text1 = paste0("For the Nauset Marsh site, the current long-term rate of relative sea-level rise from the nearest NOAA tide gauge at ", long_slr_rate_nm$Station.Name, " was ", long_slr_rate_nm$MSL.Trends.mm.yr., " mm/yr (± ", long_slr_rate_nm$Perc.95.CI.mm.yr., ") for the period between ", long_slr_rate_nm$First.Year, " to ", long_slr_rate_nm$Last.Year, " (", paste0("https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=", long_slr_rate_nm$Station.ID), "). "),
      extra_recent_slr_text1 = paste0("For the Nauset Marsh site, we calculated a recent rate of relative sea-level rise of ", recent_slr_rate_nm %>% filter(term == "yr") %>% mutate(estimate = format_result_vals(estimate)) %>% pull(estimate), " mm/yr (± ", recent_slr_rate_nm %>% filter(term == "yr") %>% mutate(std.error = format_result_vals(std.error)) %>% pull(std.error), ") over the most recent tidal epoch (2001-2019). "),
      extra_future_slr_text1 = paste0("For the Nauset Marsh site, the future predicted rates of relative sea-level rise by 2100 were ", future_slr_rate_nm$future_slr_rate[future_slr_rate_nm$scenario_name == "low"], ", ", future_slr_rate_nm$future_slr_rate[future_slr_rate_nm$scenario_name == "int_low"], ", ", future_slr_rate_nm$future_slr_rate[future_slr_rate_nm$scenario_name == "int"], ", ", future_slr_rate_nm$future_slr_rate[future_slr_rate_nm$scenario_name == "int_high"], ", and ", future_slr_rate_nm$future_slr_rate[future_slr_rate_nm$scenario_name == "high"], " mm/yr for the low, intermediate-low, intermediate, intermediate-high, and high future sea-level rise scenarios, respectively. "),
      extra_future_slr_text2 = paste0("Only one site - Phrag - gained elevation at a rate close to or greater than the future predicted low rate of sea-level rise. None of the sites gained elevation at a rate close to or greater than the future predicted intermediate-low, intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code == "GATE") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Two sites - JOCO REF and Elders East NF - gained elevation at a rate close to or greater than the future predicted low rate of sea-level. Additionally, one site - Elders East NF - gained elevation at a rate close to or greater than the future predicted intermediate-low rate of sea-level rise. None of the sites gained elevation at a rate close to or greater than the future predicted intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code %in% c("ASIS", "COLO", "FIIS", "ACAD", "VIIS", "BISC")) {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Notably, none of the sites gained elevation at a rate close to or greater than the future predicted low, intermediate-low, intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code == "BOHA") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Two sites - Peddocks Island and Thompson Island gained elevation at a rate close to or greater than the future predicted low rate of sea-level rise. Additionally, one site - Thompson Island - gained elevation at a rate close to or greater than the future predicted intermediate-low rate of sea-level rise. None of the sites gained elevation at a rate close to or greater than the future predicted intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code == "SARI") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Notably, SARI 1 gained elevation at a rate close to or greater than the future predicted low rate of sea-level rise although it is unlikely that the site will be able to keep pace with the future predicted intermediate-low, intermediate, intermediate high, or high rates of sea-level rise")
      )
    } else if (park_code == "GWMP") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Six stations gained elevation at a rate close to greater than the future predicted low rate of sea-level rise, four stations gained elevation at a rate close to greater than the future predicted intermediate-low rate, and one station - River 3 - gained elevation at a rate close to or greater than the intermediate rate. None of the stations gained elevation at a rate close to or greater than the future predicted intermediate-high or high rates of sea-level rise")
      )
    } else if (park_code == "NACE") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Eleven stations gained elevation at a rate close to greater than the future predicted low rate of sea-level rise, five stations gained elevation at a rate close to or greater than the future predicted intermediate-low rate, and two stations gained elevation at a rate close to greater than the future predicted intermediate rate (KenSET05 and KenSET06). None of the stations gained elevation at a rate close to or greater than the future predicted intermediate-high or high rates of sea-level rise")
      )
    } else {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = ""
      )
    }
  
  return(text)
}
#'
#' @rdname format_text
#' @export
slr_current_rate_comp_text <- function(park_name, slr_rate_comps_df, data_df, rate_type) {
  
  comps_data <- slr_rate_comps_df
  num_sites <- if_else(!park_name %in% c("George Washington Memorial Parkway", "National Capital Parks-East"), n_distinct(data_df$site_name), n_distinct(comps_data$site_name))
  
  
  if (rate_type == "relative long-term") {
    col_name <- "long_rate_comp"
  } else if (rate_type == "relative recent") {
    col_name <- "recent_rate_comp"
  }
  
  num_greater_sites <- sum(comps_data[[col_name]]) 
  
  site_name_text <- comps_data %>%
    filter(!!sym(col_name)) %>%
    pull(., site_name) %>%
    knitr::combine_words(.)
  
  sites_stations <- if_else(park_name %in% c("George Washington Memorial Parkway", "National Capital Parks-East"), " station", " site")
  
  all_sites_sentence <- function(park_name, num_sites, rate_type) {
    
    if (num_sites == 1) {
      paste0("The", sites_stations, " at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise.")
    } else {
      paste0("All ", english::english(num_sites), sites_stations, "s at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise.")
    }
  }
  
  any_sites_sentence <- function(park_name, num_sites, num_greater_sites, site_name_text, rate_type) {
    
    if (num_sites == 1) {
      paste0("The", sites_stations, " at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise")
    } else {
      paste0("Out of the ", english::english(num_sites), sites_stations, "s at ", park_name, ", ", english::english(num_greater_sites), sites_stations, "s - ", site_name_text, " - gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise") 
    }
  }
  
  no_sites_sentence <- function(park_name, rate_type) {
    
    if (num_sites == 1) {
      paste0("The", sites_stations, " at ", park_name, " did not gain elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise")
    } else {
      paste0("None of the", sites_stations, "s at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise")
    }
  }
  
  if (all(comps_data[[col_name]])) {
    text <- all_sites_sentence(park_name = park_name, num_sites = num_sites, rate_type = rate_type)
  } else if (any(comps_data[[col_name]]) & !all(comps_data[[col_name]])) {
    text <- any_sites_sentence(park_name = park_name, num_sites = num_sites, num_greater_sites = num_greater_sites, site_name_text = site_name_text, rate_type = rate_type) 
  } else if (!all(comps_data[[col_name]])) {
    text <- no_sites_sentence(park_name = park_name, rate_type = rate_type)
  }
  
  return(text)
  
}
#'
#' @rdname format_text
#' @export
summary_paragraph <- function(park_code) {
  
  if(park_code == "ASIS") {
    text <- list(
      summary_paragraph1 = "Marsh 8 (Valentines) has the highest surface elevation and thus has the highest elevation capital of the four sites. Marsh 6 (Pine Tree) has the second highest surface elevation although elevation capital at this site was below the 0.5 threshold for vulnerability to sea-level rise. Both Marsh 8 (Valentines) and Marsh 6 (Pine Tree) have surface elevations well above the local Mean High Water datum (MHW) and rates of surface elevation change similar to the long-term rate of sea-level rise. Marsh 5 (Pope Bay) and Marsh 11 have lower surface elevations and thus have lower elevation capital. Both Marsh 5 (Pope Bay) and Marsh 11 have surface elevations near the local Mean High Water datum (MHW) and lower rates of surface elevation change that are well below both the long-term and recent rates of sea-level rise.",
      summary_paragraph2 = "Two of these sites - Marsh 8 (Valentines) and Marsh 6 (Pine Tree) - are keeping pace with the local long-term rate of sea-level rise but are unlikely to be able to keep pace with the higher recent rate of sea-level rise. Conversely, the other two sites - Marsh 5 (Pope Bay) and Marsh 11 - are not keeping pace with either the local long-term or recent rates of sea-level rise. Notably, Marsh 5 (Pope Bay) appears to have the most potential issues affecting its long-term resilience - it has a relatively low rate of surface elevation change and low marsh surface elevation and is flooded over 25% of the time."
    )
  } else if (park_code == "CACO") {
    text <- list(
      summary_paragraph1 = "Elevations at the seven marshes vary by almost 5 feet (1.5 meters) with Blackfish Creek and Gut having the highest surface elevation and thus the highest elevation capital and Hatches Harbor restricted, High Toss, and Phrag having the lowest surface elevation and therefore the lowest elevation capital. Interestingly, both Blackfish Creek and Gut have rates of surface elevation change near 0, most likely because they are infrequently inundated and therefore have much lower rates of sediment deposition and belowground productivity (Morris et al. 2002). Similarly, Nauset Marsh has relatively high elevation capital despite its slightly lower surface elevation because local water levels are relatively low due to its protected location within Nauset Bay. The remaining four sites - Hatches Harbor restricted, Hatches Harbor unrestricted, High Toss, and Phrag - all have extremely low elevation capital because the marsh surface elevation at these sites is below the local Mean Sea Level (MSL) datum (Table 3). Notably, the marsh at Phrag has a relatively high rate of surface elevation change despite its low elevation capital because <i>Phragmites australis</i> has extremely high rates of belowground biomass accumulation that contributes to surface elevation gain (Rooth and Stevenson 2000).",
      summary_paragraph2 = "One of these sites - Phrag - is keeping pace with both the local long-term rate of sea-level rise and the higher recent rate of sea-level rise. Conversely, the remaining six sites (Hatches Harbor restricted, Hatches Harbor unrestricted, High Toss, Gut, Blackfish Creek, and Nauset Marsh) are not keeping pace with either the local long-term or recent rates of sea-level rise. Although Phrag appears to be keeping pace with the future predicted low rate of sea-level rise, none of the sites are likely to be able to keep pace with the higher future predicted sea-level rise rates. The marshes at High Toss and Hatches Harbor restricted appear to have the most potential issues affecting their long-term resilience. Hatches Harbor restricted and High Toss are located behind dikes that likely restrict tidal flow and the accumulation of sediments on the marsh surface which in turn limits plant growth, resulting in low or negative rates of surface elevation change (Morris et al. 2002; Vincent et al. 2013)."
    )
  } else if (park_code == "COLO") {
    text <- list(
      summary_paragraph1 = "Marsh 5 has the highest surface elevation and thus has the highest elevation capital of the four sites. Marsh 5 has a surface elevation similar to the local Mean High Water datum (MHW) although the rate of surface elevation change is well below both the long-term and recent rates of sea-level rise. Marsh 19 and Marsh 30 are lower in elevation and therefore have lower elevation capital. Marsh 13 is the lowest of the four sites, has the lowest elevation capital, and has a low rate of surface elevation change that is substantially lower than either the long-term or recent rates of sea-level rise.", 
      summary_paragraph2 = "None of the sites are keeping pace with either the local long-term rate of sea-level rise or the higher recent rate of sea-level rise. Notably, Marsh 13 appears to have the most potential issues affecting its long-term resilience - it has a very low rate of surface elevation change and low marsh surface elevation and is flooded over 50% of the time."
    )
  } else if (park_code == "FIIS") {
    text <- list(
      summary_paragraph1 = "Hospital Point and Watch Hill have the highest surface elevation and thus both sites have relatively high elevation capital, although both are below the 0.5 threshold for higher vulnerability to sea-level rise. Similarly, both Hospital Point and Watch Hill have surface elevations near the local Mean High Water (MHW) datum. Despite their similar surface elevations and elevation capital values, the rate of surface elevation change at Watch Hill appears to be keeping pace with the long-term rate of sea-level rise, whereas the rate of surface elevation change at Hospital Point is not. The marsh at Great Gun has a lower surface elevation, the lowest elevation capital, and has a negative rate of surface elevation change that is substantially lower than either the long-term or recent rates of sea-level rise.",
      summary_paragraph2 = "Only one site - Watch Hill - is keeping pace with the local long-term rate of sea-level rise but is unlikely to be able to keep pace with the higher recent rate of sea-level rise. Conversely, the other two sites - Hospital Point and Great Gun - are not keeping pace with either the local long-term or recent rates of sea-level rise. Notably, Great Gun appears to have the most potential issues affecting its long-term resilience - it has a negative rate of surface elevation change and low marsh surface elevation and is flooded almost 50% of the time. Additionally, Roman et al. (2024) noted that the site has been experiencing a decline in vegetation cover and corresponding increase in bare mudflat, most likely due to the formation of a natural drainage channel through the marsh."
    )
  } else if (park_code == "GATE") {
    text <- list(
      summary_paragraph1 = "Elevations at the eight marshes vary by almost 3 feet (0.9 meters) with JOCO having the highest surface elevation and thus the highest elevation capital and Big Egg Control having the lowest surface elevation and therefore the lowest elevation capital. The higher elevation marshes (Elders East, Elders East NF, JOCO, JOCO REF, and Big Egg Spray-Restored) are flooded much less than the lower elevation marshes (Black Bank, Big Egg Control, and Sandy Hook). With the exception of Big Egg Spray-Restored, the marshes with higher surface elevations and higher elevation capital had higher rates of surface elevation change. One potential explanation for the low rate of surface elevation change at Big Egg Spray-Restored is the period of sediment settling and compaction following the placement of the dredged material - a process that is frequently observed following the deposition of large amounts of sediment deposition through either restoration or the passage of tropical storms (Cahoon et al. 2019; Whelan et al. 2009). The lower elevation marshes - Black Bank and Big Egg Control - had rates of surface elevation change that were negative and were flooded over 25% of the time. Thus the marshes at JOCO and Elders East appear to be more stable as compared to Black Bank or Big Egg.",
      summary_paragraph2 = "Five of the eight sites - Elders East, Elders East NF, JOCO, JOCO REF, and Sandy Hook - are keeping pace with the local long-term rate of sea-level rise. However only two of these sites - Elders East NF and JOCO REF - are keeping pace with the higher recent rate of sea-level rise. Similarly, both Elders East NF and JOCO REF are gaining elevation a rate close to or greater than the predicted future low rate of sea-level rise, and Elders East NF is even gaining elevation a rate close to or greater than the predicted future intermediate-low rate of sea-level rise. The remaining sites (Black Bank, Big Egg Control, and Big Egg Spray-Restored) are not keeping pace with either the local long-term or recent rate of sea-level rise. Notably, Black Bank and Big Egg Control have the most potential issues affecting their long-term resilience - both sites have low elevation capital and negative rates of surface elevation change. Wigand et al. (2014) noted that the marshes at Black Bank and Big Egg were characterized by deteriorating creek-bank edges and marsh platforms that are breaking apart."
    )
  } else if (park_code == "ACAD") {
    text <- list(
      summary_paragraph1 = "Thompson Island has the highest surface elevation and thus has the highest elevation capital of the four sites. Notably, all four sites have elevation capital values above the 0.5 threshold for increased vulnerability to sea-level rise. Bass Harbor has the lowest surface elevation although the rate of surface elevation change is the greatest of the four sites and is comparable to the long-term rate of sea-level rise. Despite its relatively high surface elevation, Schoodic had the lowest elevation capital because mean sea-level at the site is substantially higher and closer to the surface elevation of the site as compared to the other three sites.",
      summary_paragraph2 = "Only one site – Bass Harbor – is keeping pace with the relative long-term rate of sea-level rise but none of the four sites are keeping pace with the higher recent rate of sea-level rise. Similarly, none of the sites are likely to be able to keep pace with the accelerated rates of future predicted sea-level rise. Rates of surface elevation change at all sites are low but this is mostly likely due to the relative geologic stability of the Maine coast as compared to the mid-Atlantic or Gulf region (Piecuch et al. 2018; Sallenger et al. 2012)."
    )
  } else if (park_code == "BOHA") {
    text <- list(
      summary_paragraph1 = "Notably, all three sites have elevation capital values above the 0.5 threshold for increased vulnerability to sea-level rise. Peddocks Island has the highest surface elevation but the lowest rate of surface elevation change and is not keeping pace with the long-term rate of sea-level rise. Conversely, Thompson Island has the lowest surface elevation although the rate of surface elevation change is the greatest of the three sites and is comparable to the long-term rate of sea-level rise. Although rates of surface elevation change at Calf Island and Peddocks Island are relatively low, their higher position within the tidal frame may make these sites somewhat less vulnerable to sea-level rise, as reflected by their relatively high elevation capital values.",
      summary_paragraph2 = "Two sites – Calf Island and Thompson Island - are building surface elevation at a rate comparable to the future low predicted rate of sea-level rise. Additionally, Thompson Island is building surface elevation at a rate comparable to the future intermediate-low rate of sea-level rise. However, none of the sites are keeping pace with the future intermediate, intermediate-high, or high rates of future sea-level rise."
    )
  } else if (park_code == "VIIS") {
    text <- list(
      summary_paragraph1 = "The mangroves at Water Creek had the highest rate of surface elevation change of the two sites and appear to be keeping pace with the local long-term rate of sea-level rise, although it is unlikely that this site will be able to keep pace with the elevated future predicted rates of sea-level rise.",
      summary_paragraph2 = "Conversely, Mary's Creek had a rate of surface elevation change that was equivalent to zero - indicating that the mangroves at the site are not building elevation and are unlikely to be able to keep pace with either current or future rates of sea-level rise."
    )
  } else if (park_code == "SARI") {
    text <- list(
      summary_paragraph1 = "The mangroves at SARI 1 had a relatively high rate of surface elevation change and appear to be keeping pace with both the local long-term and recent rates of sea-level rise.",
      summary_paragraph2 = "Although the site is building elevation at a rate close to or greater than the future predicted low rate of sea-level rise, it is unlikely that the site will be able to keep pace with the higher future predicted sea-level rise scenarios."
    )
  } else if (park_code == "BISC") {
    text <- list(
      summary_paragraph1 = "The mangroves at BISC 2 had the highest rate of surface elevation change of the two sites and appear to be keeping pace with the local long-term rate of sea-level rise, although it is unlikely that this site will be able to keep pace with the elevated future predicted rates of sea-level rise.",
      summary_paragraph2 = "Conversely, BISC 1 had a rate of surface elevation change that was equivalent to zero - indicating that the mangroves at the site are not building elevation and are unlikely to be able to keep pace with either current or future rates of sea-level rise."
    )
  } else if (park_code == "GWMP") {
    text <- list(
      summary_paragraph1 = "River 3 has the highest surface elevation and thus has the highest elevation capital of the ten stations. Conversely, Creek Bank 3 has the lowest surface elevation and thus the lowest elevation capital. All ten stations have surface elevations below the mean-high water tidal datum (0.631 m NAVD88) but above the mean sea level datum (0.231 m NAVD88).",
      summary_paragraph2 = "Notably, a majority of the stations have relatively high rates of surface elevation change and appear to be keeping pace with the local long-term and/or higher recent rates of sea-level rise. In fact, five of these stations appear to be building elevation at a rate close to or above the future low or intermediate-low rates of sea-level rise. The station at River 1A had an extremely low, negative rate of surface elevation change but this could be the result of the short data record (~5 years) at this station. The station at Creek Bank 3 appears to have the most potential issues affecting its long-term resilience - it has a negative rate of surface elevation change, the lowest elevation of the 10 stations, and is flooded almost 50% of the time."
    )
  } else if (park_code == "NACE") {
    text <- list(
      summary_paragraph1 = "KenSET05 3 has the highest surface elevation and thus has the highest elevation capital of the 16 stations. Conversely, Kingman 1 has the lowest surface elevation and thus the lowest elevation capital. All 16 stations have surface elevations below the mean-high water tidal datum (0.662 m NAVD88) and four stations have surface elevations below the mean sea level datum (0.329 m NAVD88).",
      summary_paragraph2 = "Notably, a majority of the stations have relatively high rates of surface elevation change and appear to be keeping pace with the local long-term and/or higher recent rates of sea-level rise. In fact, eleven of these stations appear to be building elevation at a rate close to or above the future low or intermediate-low rates of sea-level rise. The stations Kingman 1 and Kingman 2 appear to have the most potential issues affecting their long-term resilience - they have negative rates of surface elevation change, the lowest elevations of the 16 stations, and are flooded greater than 50% of the time."
    )
  }
  
  return(text)
}