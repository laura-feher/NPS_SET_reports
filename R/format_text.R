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
  } else if (park_code %in% c("ASIS", "COLO", "FIIS", "GATE")) {
    text <- paste("Note that each site has 3 stations, each station (an individual SET) has four arm positions, and each arm has nine pins - thus a total of 108 nested measurements are taken at each site on each sampling event.")
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
  num_sites <- n_distinct(data$site_name)
  
  elev_cap_df <- site_hydro_df %>%
    mutate(., elev_cap_comp = if_else(elev_capital >= 0.5, TRUE, FALSE))
  
  num_greater_sites <- sum(elev_cap_df$elev_cap_comp)
  num_lesser_sites <- sum(!elev_cap_df$elev_cap_comp)
  
  site_name_text_lesser <- elev_cap_df %>%
    filter(!elev_cap_comp) %>%
    pull(site_name) %>%
    knitr::combine_words(.)
  
  site_name_text_greater <- elev_cap_df %>%
    filter(elev_cap_comp) %>%
    pull(site_name) %>%
    knitr::combine_words(.)
  
  all_sites_greater_sentence <- function(park_name, num_sites) {
    paste0("All ", english::english(num_sites), " sites at ", park_name, " had elevation capital values above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  all_sites_lesser_sentence <- function(park_name, num_sites) {
    paste0("All ", english::english(num_sites), " sites at ", park_name, " had elevation capital values below 0.5, indicating a higher vulnerability to sea-level rise.")
  }
  
  one_site_greater_sentence <- function(park_name, num_sites, site_name_text_greater) {
    paste0(site_name_text_greater, " had an elevation capital value above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  one_site_lesser_sentence <- function(park_name, num_sites, site_name_text_lesser) {
    paste0(site_name_text_lesser, " had an elevation capital value below 0.5, indicating a higher vulnerability to sea-level rise.")
  }
  
  any_sites_greater_sentence <- function(park_name, num_sites, num_greater_sites, site_name_text_greater) {
    paste0(english::Words(num_greater_sites), " sites - ", site_name_text_greater, " - had elevation capital values above 0.5, indicating a lower vulnerability to sea-level rise.")
  }
  
  any_sites_lesser_sentence <- function(park_name, num_sites, num_lesser_sites, site_name_text_lesser) {
    paste0("Out of the ", english::english(num_sites), " sites at ", park_name, ", ", english::english(num_lesser_sites), " sites - ", site_name_text_lesser, " - had elevation capital values below 0.5, indicating a higher vulnerability to sea-level rise.")
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
    } else if (park_code == "ASIS") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("Notably, none of the sites gained elevation at a rate close to or greater than the future predicted low, intermediate-low, intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code == "COLO") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("None of the sites gained elevation at a rate close to or greater than the future predicted low, intermediate-low, intermediate, intermediate-high, or high rates of sea-level rise")
      )
    } else if (park_code == "FIIS") {
      text <- list(
        extra_long_slr_text1 = "",
        extra_recent_slr_text1 = "",
        extra_future_slr_text1 = "",
        extra_future_slr_text2 = paste0("None of the sites gained elevation at a rate close to or greater than the future predicted low, intermediate-low, intermediate, intermediate-high, or high rates of sea-level rise")
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
  num_sites <- n_distinct(data_df$site_name)
  
  
  if (rate_type == "relative long-term") {
    col_name <- "long_rate_comp"
  } else if (rate_type == "relative recent") {
    col_name <- "recent_rate_comp"
  }
  
  num_greater_sites <- sum(comps_data[[col_name]]) 
  
  site_name_text <- comps_data %>%
    filter(!!sym(col_name)) %>%
    pull(site_name) %>%
    knitr::combine_words(.)
  
  all_sites_sentence <- function(park_name, num_sites, rate_type) {
    paste0("All ", english::english(num_sites), " site(s) at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise.")
  }
  
  any_sites_sentence <- function(park_name, num_sites, num_greater_sites, site_name_text, rate_type) {
    paste0("Out of the ", english::english(num_sites), " site(s) at ", park_name, ", ", english::english(num_greater_sites), " site(s) - ", site_name_text, " - gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise")
  }
  
  no_sites_sentence <- function(park_name, rate_type) {
    paste0("None of the sites at ", park_name, " gained elevation at a rate close to or greater than the ", rate_type, " rate of sea-level rise")
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
      summary_paragraph2 = "Two of these sites - Marsh 8 (Valentines) and Marsh 6 (Pine Tree) - are keeping pace with the local long-term rate of sea-level rise but are unlikely to be able to keep pace with the higher recent rate of sea-level rise. Conversely, the other two sites - Marsh 5 (Pope Bay) and Marsh 11 - not keeping pace with either the local long-term or recent rates of sea-level rise. Notably, Marsh 5 (Pope Bay) appears to have the most potential issues affecting its long-term resilience - it has a relatively low rate of surface elevation change and low marsh surface elevation and is flooded over 25% of the time."
    )
  } else if (park_code == "CACO") {
    text <- list(
      summary_paragraph1 = "Elevations at the seven marshes vary by almost 5 feet (1.5 meters) with Blackfish Creek and Gut have the highest surface elevation and thus have the highest elevation capital and Hatches Harbor restricted, High Toss, and Phrag having the lowest surface elevation and therefore the lowest elevation capital. Interestingly, both Blackfish Creek and Gut have rates of surface elevation change near 0, most likely because they are infrequently inundated and therefore have much lower rates of sediment deposition and belowground productivity (Morris et al. 2002). Similarly, Nauset Marsh has relatively high elevation capital despite its slightly lower surface elevation because local water levels are relatively low due to its protected location within Nauset Bay. The remaining four sites - Hatches Harbor restricted, Hatches Harbor unrestricted, High Toss, and Phrag - all have extremely low elevation capital because the marsh surface elevation at these sites is below the local Mean Sea Level (MSL) datum (Table 3). Notably, the marsh at Phrag has a relatively high rate of surface elevation change despite its low elevation capital because <i>Phragmites australis</i> has extremely high rates of belowground biomass accumulation that contributes to surface elevation gain (Rooth and Stevenson 2000).",
      summary_paragraph2 = "One of these sites - Phrag - is keeping pace with both the local long-term rate of sea-level rise and the higher recent rate of sea-level rise. Conversely, the remaining six sites (Hatches Harbor restricted, Hatches Harbor unrestricted, High Toss, Gut, Blackfish Creek, and Nauset Marsh) are not keeping pace with either the local long-term or recent rates of sea-level rise. Although Phrag appears to be keeping pace with the future predicted low rate of sea-level rise, none of the sites are likely to be able to keep pace with the higher future predicted sea-level rise rates. The marshes at High Toss and Hatches Harbor restricted appear to have the most potential issues affecting their long-term resilience. Hatches Harbor restricted and High Toss are located behind dikes that likely restrict tidal flow and the accumulation of sediments on the marsh surface which in turn limits plant growth, resulting in low or negative rates of surface elevation change (Morris et al. 2002; Vincent et al. 2013)."
    )
  } else if (park_code == "COLO") {
    text <- list(
      summary_paragraph1 = "Marsh 5 has the highest surface elevation and thus has the highest elevation capital of the four sites. Marsh 5 has a surface elevation similar to the local Mean High Water datum (MHW) although the rate of surface elevation change is well below both the long-term and recent rates of sea-level rise. Marsh 19 and Marsh 30 are lower in elevation therefore have lower elevation capital. Despite its lower surface elevation, the rate of surface elevation change at Marsh 30 is to close to the long-term rate of sea-level rise. Marsh 13 is the lowest of the four sites, has the lowest elevation capital, and has a low rate of surface elevation change that is substantially lower than either the long-term or recent rates of sea-level rise.", 
      summary_paragraph2 = "One of these sites - Marsh 30 - is keeping pace with the local long-term rate of sea-level rise but is unlikely to be able to keep pace with the higher recent rate of sea-level rise. Conversely, the other three sites - Marsh 5, Marsh 13, and Marsh 19 - are not keeping pace with either the local long-term or recent rates of sea-level rise. Notably, Marsh 13 appears to have the most potential issues affecting its long-term resilience - it has a very low rate of surface elevation change and low marsh surface elevation and is flooded over 50% of the time."
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
  }
  
  return(text)
}