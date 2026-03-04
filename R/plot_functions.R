#' Functions for consistent formatting of plots.
#'
#' @param site_set_rates_df dataframe. A dataframe of site-level surface
#'   elevation change rates.
#' @param ncol number. Number of facet columns in the plot.
#' @param panel.spacing.x number. Horizontal spacing of facet panels in pts.
#' @param panel.spacing.y number. Vertical spacing of facet panels in pts.
#' @param strip.text.margin number. Spacing of the top margin of the panel
#'   labels.
#' @param wl_df dataframe. A data frame of water level data downloaded from
#'   Aquarius.
#' @param site_hydro_df dataframe. A dataframe of site-level hydrology metrics,
#'   tidal datums, marsh elevations, and elevation capital.
#' @param time_interval string The time interval that the water level data
#'   was collected on - defaults to "15 min".
#' @param nrow integer. The number of facet rows in the water level plot.
#' @param show.longest.flood TRUE/FALSE. Whether or not to show a blue bar on
#'   the water level plot showing the longest consecutive flood event.
#' @param show.marsh.elev TRUE/FALSE. Whether or not to show a red horizontal
#'   line on the water level plot indicating the NAVD88 elevation of the marsh
#'   surface.
#' @param date.breaks string. The time interval for the date breaks on the
#'   x-axis of the water level plot. Defaults to "2 years".
#' @param x.axis.title string. The x-axis title for the water level plot.
#'   Defaults to "Water level (m NAVD88)".
#' @param park_code string. The 4-letter park unit code.
#' @param site_order vector. A character vector defining the site order.
#' @param elev_digits integer. Number of digits to show for elevation labels on
#'   the plot.
#' @param datum_digits integer. Number of digits to show for the tidal datum
#'   labels on the plot.
#' @param label_padding number. Value for padding around the labels on the
#'   plots.
#' @param facet_nrow integer. Number of facet rows in the plot.
#' @param facet_ncol integer. Number of facet columns in the plot.
#' @param legend_position vector. The x and y position of the plot legend.
#' @param legend_top_margin integer. Size in pts of the legend top margin.
#' @param long_slr_rate_df dataframe. The long-term slr dataframe.
#' @param recent_slr_rate_df dataframe. The recent slr dataframe.
#' @param long_slr_rate_nau dataframe. Optional, the long-term slr dataframe for
#'   Nauset.
#' @param recent_slr_rate_nau dataframe. Optional, the recent slr dataframe for
#'   Nauset.
#' @param future_slr_rate_df dataframe. The future slr rate dataframe.
#' @param future_slr_rate_nau dataframe. Optional, the future slr rate dataframe
#'   for Nauset.
#' @param marsh_elev_df dataframe. The dataframe of marsh surface elevations in
#'   meters NAVD88.
#' @param park_datums_df dataframe. The dataframe of park-level average tidal
#'   datums.
#'
#' @description Functions for consistent formatting of plots. 
#'  `r site_sets_fig_plot()` makes figure 4 showing cumulative surface elevation
#'   change at each site. 
#'   `r site_wl_plot()` makes figure 5 showing the water level data at each site. 
#'   `r site_hydro_plot()` makes figure 6 showing the site-level tidal datums, 
#'   percent time flooded, and elevation capital vs marsh surface elevation. 
#'   `r site_slr_recent_plot()` makes figure 7 showing the comparison between 
#'   long-term and recent SLR vs surface elevation change at each site. 
#'   `r site_slr_future_plot()` makes figure 8 showing the comparison between 
#'   predicted future rates of SLR and surface elevation change for each site. 
#'   `r site_summary_fig()` makes figure 9 showing marsh surface elevation vs. 
#'   surface elevation change, tidal datums, and long-term and recent SLR.
#'
#' @name plot_functions
#' @import dplyr
#' @import ggplot2
#' @import plotly
#'
#' @rdname plot_functions
#' @export
site_sets_fig_plot <- function(site_set_rates_df, ncol, panel.spacing.x, panel.spacing.y, strip.text.margin) {
  
  park <- site_set_rates_df %>%
    distinct(park_code) %>%
    pull(park_code)
  
  site_set_cumu <- site_set_rates_df %>%
    unnest(cols = c(data))
  
  labels <- site_set_rates_df %>%
    ungroup() %>%
    { if (!park %in% c("GWMP", "NACE")) 
      select(., site_name, rate, rate_se) %>%
        mutate(label = paste0(site_name, "<br>", "SEC Rate: ", format_result_vals(rate), " ± ", format_result_vals(rate_se), " mm/yr")) %>%
        pull(label, name = site_name) 
      else 
        select(., station_name, rate, rate_se) %>%
        mutate(label = paste0(station_name, "<br>", "SEC Rate: ", format_result_vals(rate), " ± ", format_result_vals(rate_se), "mm/yr")) %>%
        pull(label, name = station_name)
        }
  
  text = list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = list(color = "black")
  )
  
  p <- ggplot(data = site_set_cumu, aes(x = event_date_UTC, y = mean_cumu)) +
    geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE) +
    geom_point(aes(text = paste("Date:", format(event_date_UTC, "%m/%d/%Y"), "<br>", "SEC:", format(round(mean_cumu, 2), nsmall = 2), "mm")), alpha = 0.6) +
    { if (!park %in% c("GWMP", "NACE"))
      facet_wrap(~site_name, labeller=as_labeller(labels), ncol = ncol)
      else
        facet_wrap(~station_name, labeller = as_labeller(labels), ncol = ncol)
      } +
    labs(subtitle = "Marsh Surface elevation change") +
    scale_x_date(date_labels = "%Y") +
    scale_y_continuous(name = "Surface elevation change (mm)") +
    theme(
      axis.title.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "black"),
      panel.spacing.x = unit(panel.spacing.x, "pt"),
      panel.spacing.y = unit(panel.spacing.y, "pt"),,
      strip.text = element_text(margin = margin(strip.text.margin,1,1,1, "pt")),
      strip.background = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>%
    style(hoverlabel = text)
}
#'
#' @rdname plot_functions
#' @export
site_wl_plot <- function(wl_df, site_hydro_df, time_interval = "15 min", nrow, show.longest.flood = TRUE, show.marsh.elev = TRUE, date.breaks = "2 years", x.axis.title = "Water level (m NAVD88)") {
  
  park <- wl_df %>%
    ungroup() %>%
    distinct(park) %>%
    pull(park)
  
  p <- wl_df %>%
    ungroup() %>%
    {if (!park %in% c("VIIS", "SARI", "BISC", "GWMP", "NACE"))
      complete(., nesting(park, site_name), datetime = seq(min(datetime), max(datetime), by = time_interval)) %>%
        ggplot(data = ., aes(x = datetime, y = water_level))
      else if (park %in% c("VIIS", "SARI", "BISC"))
        ggplot(data = ., aes(x = datetime, y = water_level, group = format(datetime, "%Y-%m"))) 
      else if (park %in% c("GWMP", "NACE"))
        complete(., nesting(park, station_name), datetime = seq(min(datetime), max(datetime), by = time_interval)) %>%
        ggplot(data = ., aes(x = datetime, y = water_level))
      } +
    
    {if (show.longest.flood == TRUE)
      list(geom_rect(data = site_hydro_df, aes(xmin = min_date_longest_flood, xmax = max_date_longest_flood, ymin = -Inf, ymax = Inf, fill = "Longest flood event"), color = "#14747e", inherit.aes = FALSE, alpha = 0.5),
        scale_fill_manual(values = "#14747e", label = "Longest flood event"))} +
    {if (show.marsh.elev == TRUE & park == "BOHA")
        list(geom_hline(data = site_hydro_df, aes(yintercept = elev_navd88, color = site_name)),
             labs(color = "Site"))
      else if (show.marsh.elev == TRUE & park %in% c("GWMP", "NACE"))
        list(geom_hline(data = site_hydro_df, aes(yintercept = elev_navd88, color = station_name)),
             labs(color = "Station"))
      else if (show.marsh.elev == TRUE & !park %in% c("BOHA", "GWMP", "NACE"))
        list(geom_hline(data = site_hydro_df, aes(yintercept = elev_navd88, color = "Marsh surface elevation")),
             scale_color_manual(values = "#ff5a67", label = "Marsh surface elevation"))
      } +
    
    # geom_line(aes(text = paste("Date:", format(datetime, "%m/%d/%Y %H:%M"), "<br>", "Water level:", format(round(water_level, 3), nsmall = 3), "m NAVD88")), alpha = 0.25) + # For some reason the line won't render with plotly when I try to change the text labels
    geom_line(alpha = 0.25) +
    
    {if (!park %in% c("BOHA", "GWMP", "NACE"))
      facet_wrap(~site_name, nrow = nrow, scales = "free_y")} +
    {if (str_detect(date.breaks, "year"))
      scale_x_datetime(date_breaks = date.breaks, date_labels = "%Y")
      else if (str_detect(date.breaks, "month"))
        scale_x_datetime(date_breaks = date.breaks, date_labels = "%m-%Y")
    } +
    scale_y_continuous(name = x.axis.title) +
    {if (show.longest.flood == FALSE & !park %in% c("BOHA", "GWMP", "NACE"))
      theme(
        axis.title.x = element_blank(),
        panel.border = element_rect(fill = NA, color = "black"),
        panel.spacing.y = unit(10, "pt"),,
        strip.text = element_text(margin = margin(5,1,3,1, "pt")),
        strip.background = element_blank(),
        legend.position = "none"
        )
      else if (show.longest.flood == FALSE & park %in% c("BOHA", "GWMP", "NACE"))
        theme(
          axis.title.x = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.spacing.y = unit(10, "pt"),,
          strip.text = element_text(margin = margin(5,1,3,1, "pt")),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom"
        )
      else 
      theme(
      axis.title.x = element_blank(),
      panel.border = element_rect(fill = NA, color = "black"),
      panel.spacing.y = unit(10, "pt"),,
      strip.text = element_text(margin = margin(5,1,3,1, "pt")),
      strip.background = element_blank(),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.direction = "horizontal",
      legend.position = "bottom"
    )
    }
  
  if (show.longest.flood == TRUE) {
    p # ggplotly doesn't work with geom_rect
  } else if (show.longest.flood == FALSE) {
    ggplotly(p)
  }
}
#'
#' @rdname plot_functions
#' @export
site_hydro_plot <- function(
    park_code,
    site_hydro_df,
    site_order,
    elev_digits = 3, 
    datum_digits = 3, 
    label_padding = 0.1, 
    facet_nrow = 3, 
    facet_ncol = 3, 
    legend_position,
    legend_top_margin
) {
  
  # Format site-level hydro data & marsh surface elevation for plotting
  plot_data <- site_hydro_df %>%
    ungroup() %>%
    { if (park_code %in% c("GWMP", "NACE"))
      select(., station_name, elev_navd88, percent_time_flooded, elev_capital) %>%
        pivot_longer(., cols = -c(station_name, elev_navd88), values_to = "Hydro metric")
      else 
        select(., site_name, elev_navd88, MLW, MHW, MSL, percent_time_flooded, elev_capital) %>%
        pivot_longer(., cols = -c(site_name, elev_navd88), values_to = "Hydro metric")
      } %>%
    mutate(label = case_when(name == "percent_time_flooded" & `Hydro metric` < 1 & `Hydro metric` > 0 ~ format(round(`Hydro metric`, 1), nsmall = 1),
                             name == "percent_time_flooded" & (`Hydro metric` >= 1 | `Hydro metric` == 0) ~ format(round(`Hydro metric`, 0), nsmall = 0),
                             name == "MLW" ~ format(round(`Hydro metric`, datum_digits), nsmall = datum_digits),
                             name == "MHW" ~ format(round(`Hydro metric`, datum_digits), nsmall = datum_digits),
                             name == "MSL" ~ format(round(`Hydro metric`, datum_digits), nsmall = datum_digits),
                             name == "elev_capital" ~ format(round(`Hydro metric`, 2), nsmall = 2))) %>%
    mutate(`Marsh surface elevation` = elev_navd88,
           name = case_when(name == "percent_time_flooded" ~ "Percent time flooded (%)",
                            name == "MLW" ~ "MLW (m NAVD88)",
                            name == "MHW" ~ "MHW (m NAVD88)",
                            name == "MSL" ~ "MSL (m NAVD88)",
                            name == "elev_capital" ~ "Elevation capital")) %>%
    { if (park_code %in% c("GWMP", "NACE"))
      mutate(., Site = station_name,
             name = fct_relevel(name, "Percent time flooded (%)", "Elevation capital"))
      else 
        mutate(., Site = site_name,
               name = fct_relevel(name, "MLW (m NAVD88)", "MHW (m NAVD88)", "MSL (m NAVD88)", "Percent time flooded (%)", "Elevation capital"))
      } %>%
    tidyr::drop_na(`Hydro metric`) %>%
    { if (park_code %in% c("GWMP", "NACE"))
      mutate(., Site = if_else(station_name != "unknown", forcats::fct_relevel(Site, station_order), forcats::as_factor(Site)))
      else
        mutate(., Site = if_else(site_name != "unknown", forcats::fct_relevel(Site, site_order), forcats::as_factor(Site)))
      }
  
  # Format a vertical line for the 0.5 threshold of elevation capital
  elev_cap_threshold <- plot_data %>%
    ungroup() %>%
    select(name) %>%
    mutate(y_val = if_else(name == "Elevation capital", 0.5, NA_real_))
  
  # Plot
  ggplot(plot_data, aes(x = `Marsh surface elevation`, y = `Hydro metric`)) +
    geom_point(aes(color = Site)) +
    geom_text_repel(aes(label = label), size = 3, box.padding = label_padding) +
    geom_hline(data = elev_cap_threshold, aes(yintercept = y_val), linetype = "dashed", color = "red") +
    facet_wrap(name~., scales = "free_y", strip.position = "left", nrow = facet_nrow, ncol = facet_ncol) +
    scale_x_continuous(name = "Marsh surface elevation (m NAVD88)") +
    {if (park_code %in% c("GWMP", "NACE"))
      labs(color = "Station")
      } +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.border = element_rect(fill = NA, color = "black"),
          legend.position = legend_position,
          legend.key = element_blank(),
          legend.box.margin = margin(legend_top_margin,0,0,0, unit = "pt"),
          legend.key.spacing.y = unit(-3,"pt"),
          legend.title = element_text(hjust = 0.5),
          axis.title.x = element_text(hjust = 0.25),
          axis.title.y = element_blank()) +
    {if (park_code %in% c("GWMP", "NACE"))
      theme(legend.position = "bottom",
            legend.direction = "horizontal",
            axis.title.x = element_text(hjust = 0.5))
      }
}
#'
#' @rdname plot_functions
#' @export
site_slr_recent_plot <- function(park_code, long_slr_rate_df, recent_slr_rate_df, site_set_rates_df, long_slr_rate_nau = NA, recent_slr_rate_nau = NA) {
  
  # Format slr_rates for plotting
  slr_rates <- long_slr_rate_df %>%
    {if (park_code == "CACO")
      bind_rows(., long_slr_rate_nau) %>%
        select("slr_rate" = MSL.Trends.mm.yr., "SLR rate type" = object_type, Station.Name) %>%
        
        bind_rows(., recent_slr_rate_nau %>%
                    filter(term == "yr") %>%
                    select("slr_rate" = estimate, "SLR rate type" = object_type) %>%
                    mutate(Station.Name = "Nantucket Island, MA")) %>%
        bind_rows(., recent_slr_rate_df %>% 
                    filter(term == "yr") %>%
                    select("slr_rate" = estimate, "SLR rate type" = object_type) %>%
                    mutate(Station.Name = "Boston, MA")) %>%
        mutate(., site_group = if_else(Station.Name == "Boston, MA", "others", "Nauset")) %>%
        select(-Station.Name)
      else
        select(., "slr_rate" = MSL.Trends.mm.yr.,  "SLR rate type" = object_type) %>%
        bind_rows(., recent_slr_rate_df %>% 
                    filter(term == "yr") %>%
                    select("slr_rate" = estimate, "SLR rate type" = object_type))}%>%
    mutate(`SLR rate type` = case_when(`SLR rate type` == "long term slr rate" ~ "Long-term SLR",
                                       `SLR rate type` == "recent slr rate" ~ "Recent SLR")) 
  
  # Get the number of sites - this determines the range of the y-axis
  n_sites <- if_else(park_code != "NACE", length(unique(site_set_rates_df$site_name)), length(unique(site_set_rates_df$station_name)))
  
  # Use expand_grid to create a df with all combos of the slr rates and the y-axis range values so that hover labels will show along the entire line
  slr_rates_expanded <- data.frame(yvals = seq(0, n_sites+1, by = 0.1)) %>%
    expand_grid(., slr_rates) %>%
    {if(park_code == "CACO")
      filter(., (site_group == "others" & yvals >= 1.5) | (site_group == "Nauset" & yvals < 1.5))
      else .} 
  
  # Plot
  pp <- ggplotly(
    {if (park_code != "NACE") 
      ggplot(site_set_rates_df, aes(x = rate, y = site_name))
      else if (park_code == "NACE")
        ggplot(site_set_rates_df, aes(x = rate, y = station_name))
        } +
      geom_errorbar(aes(xmin = rate - rate_se, xmax = rate + rate_se)) +
      {if (park_code != "NACE")
        geom_point(aes(text = paste("SEC rate:", format(round(rate, 2), nsmall = 2), " mm/yr<br>Site:", site_name)))
        else if (park_code == "NACE")
          geom_point(aes(text = paste("SEC rate:", format(round(rate, 2), nsmall = 2), " mm/yr<br>Station:", station_name)))
        } +
      geom_line(data = slr_rates_expanded, aes(x = slr_rate, y = yvals, color = `SLR rate type`, text = paste0(`SLR rate type`, ": <br>", format(round(slr_rate, 2), nsmall = 2), " mm/yr"))) +
      scale_y_discrete(limits = rev) +
      scale_x_continuous(name = "Rate of surface elevation change or SLR (mm/yr)") +
      scale_color_manual(values = c("#14747e", "#1abc9c"), name = "SLR rate type:") +
      theme(axis.title.x = element_text(margin = margin(0,0,20,0, unit = "pt")),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(0,0,60,0, unit = "pt")),
    tooltip = "text"
  ) %>%
    style(hoverlabel = text)
  
  # Move legend to the bottom
  pp %>% layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
}
#'
#' @rdname plot_functions
#' @export
site_slr_future_plot <- function(park_code, future_slr_rate_df, site_set_rates_df, future_slr_rate_nau = NA) {
  
  # Format future_slr_rates for plotting
  future_slr_rates <- future_slr_rate_df %>%
    {if(park_code == "CACO")
      bind_rows(., future_slr_rate_nau) %>%
        select("Future SLR scenario" = scenario_name, future_slr_rate, NOAA_Name) %>%
        mutate(site_group = if_else(NOAA_Name == "Boston", "others", "Nauset")) %>%
        select(-NOAA_Name)
      else
        select(., "Future SLR scenario" = scenario_name, future_slr_rate)} %>%
    mutate(.,`Future SLR scenario` = case_when(`Future SLR scenario` == "low" ~ "Low",
                                             `Future SLR scenario` == "int_low" ~ "Int.-Low",
                                             `Future SLR scenario` == "int" ~ "Int.",
                                             `Future SLR scenario` == "int_high" ~ "Int.-High",
                                             `Future SLR scenario` == "high" ~ "High")) %>%
    mutate(`Future SLR scenario` = fct_relevel(`Future SLR scenario`, "Low", "Int.-Low", "Int.", "Int.-High", "High"))
  
  
  # Get the number of sites - this determines the range of the y-axis
  n_sites <- if_else(park_code != "NACE", length(unique(site_set_rates_df$site_name)), length(unique(site_set_rates_df$station_name)))
  
  # Use expand_grid to create a df with all combos of the slr rates and the y-axis range values so that hover labels will show along the entire line
  future_slr_rates_expanded <- data.frame(yvals = seq(0, n_sites+1, by = 0.1)) %>%
    expand_grid(., future_slr_rates) %>%
    {if(park_code == "CACO")
      filter(., (site_group == "others" & yvals >= 1.5) | (site_group == "Nauset" & yvals < 1.5))
      else .} 
  
  # Plot
  pp <- ggplotly(
    {if (park_code != "NACE")
      ggplot(site_set_rates_df, aes(x = rate, y = site_name))
      else if (park_code == "NACE")
        ggplot(site_set_rates_df, aes(x = rate, y = station_name))
      } +
      geom_errorbar(aes(xmin = rate - rate_se, xmax = rate + rate_se), width = 0.6) +
      {if (park_code != "NACE")
        geom_point(aes(text = paste("SEC rate:", format(round(rate, 2), nsmall = 2), " mm/yr<br>Site:", site_name)))
        else if (park_code == "NACE")
          geom_point(aes(text = paste("SEC rate:", format(round(rate, 2), nsmall = 2), " mm/yr<br>Station:", station_name)))
        } +
      geom_line(data = future_slr_rates_expanded, aes(x = future_slr_rate, y = yvals, color = `Future SLR scenario`, text = paste0(`Future SLR scenario`, ": <br>", format(round(future_slr_rate, 2), nsmall = 2), " mm/yr"))) +
      scale_y_discrete(limits = rev) +
      scale_x_continuous(name = "Rate of surface elevation change or future SLR (mm/yr)") +
      scale_color_manual(values = c("#7fc06e", "#ffcc1b", "#f08e48", "#ff5a67", "#c43060"), name = "Future SLR\nscenario:") +
      theme(axis.title.x = element_text(margin = margin(0,0,20,0, unit = "pt")),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            plot.margin = margin(0,30,30,0, unit = "pt")),
    tooltip = "text"
  ) %>%
    style(hoverlabel = text)
  
  pp
}
#'
#' @rdname plot_functions
#' @export
summary_fig <- function(site_set_rates_df, marsh_elev_df, park_datums_df, long_slr_rate_df, recent_slr_rate_df, label_padding = 0.1) {
  
  park <- unique(site_set_rates_df$park_code)
  
  sites_stations <- if_else(park %in% c("GWMP", "NACE"), "station_name", "site_name")
  # Plot data
  plot_data <- full_join(site_set_rates_df, marsh_elev_df, by = sites_stations)
  
  # Format df of recent SLR
  recent_slr <- recent_slr_rate_df %>%
    filter(term == "yr")
  
  ggplot(plot_data, aes(x = rate, y = elev_navd88)) +
    geom_hline(data = park_datums_df, aes(yintercept = MHW), linetype = "dashed") +
    geom_hline(data = park_datums_df, aes(yintercept = MSL), linetype = "dashed") +
    geom_rect(data = long_slr_rate_df, aes(xmin = MSL.Trends.mm.yr. - long_rate_se, xmax = MSL.Trends.mm.yr. + long_rate_se, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#14747e", alpha = 0.5) +
    geom_vline(data = long_slr_rate_df, aes(xintercept = MSL.Trends.mm.yr.), color = "#14747e") +
    geom_rect(data = recent_slr, aes(xmin = estimate - std.error, xmax = estimate + std.error, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#1abc9c", alpha = 0.5) +
    geom_vline(data = recent_slr, aes(xintercept = estimate), color = "#1abc9c") +
    geom_errorbar(aes(xmin = rate-rate_se, xmax = rate+rate_se, y = elev_navd88)) +
    geom_point() +
    {if (!park %in% c("GWMP", "NACE"))
      geom_text_repel(aes(label = site_name), box.padding = label_padding)
      else if (park %in% c("GWMP", "NACE"))
        geom_text_repel(aes(label = station_name), box.padding = label_padding)
      } +
    annotate("text", x = Inf, y = park_datums_df$MHW, label = "MHW", hjust = -0.1) +
    annotate("text", x = Inf, y = park_datums_df$MSL, label = "MSL", hjust = -0.1) +
    annotate("text", x = long_slr_rate_df$MSL.Trends.mm.yr., y = Inf, label = "Long-term SLR", vjust = -0.3) +
    {if (park %in% c("GATE", "GWMP", "NACE"))
      annotate("text", x = recent_slr$estimate, y = Inf, label = "Recent SLR", vjust = -0.3, hjust = -0.05) 
      else
        annotate("text", x = recent_slr$estimate, y = Inf, label = "Recent SLR", vjust = -0.3)} +
    coord_cartesian(clip = "off") +
    labs(x = "Rate of surface elevation change (mm/yr)", y = "Marsh surface elevation (m NAVD88)") +
    theme(
      plot.margin = unit(c(1,3,1,1), "lines"),
      panel.border = element_rect(fill = NA, color = "black")
    )
}