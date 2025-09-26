#' Functions for consistent formatting of plots.
#'
#' @param ncol number. Number of facet columns in the plot.
#' @param panel.spacing.x number. Horizontal spacing of facet panels in pts. 
#' @param panel.spacing.y number. Vertical spacing of facet panels in pts.
#' @param strip.text.margin number. Spacing of the top margin of the panel labels.
#'
#' @description
#' A short description...
#' 
#' @name plot_functions
#' @import dplyr
#' @import ggplot2
#' @import plotly
#'
#' @examples
#' 
#' @rdname plot_functions
#' @export
site_sets_fig_plot <- function(ncol, panel.spacing.x, panel.spacing.y, strip.text.margin) {
  
  site_set_cumu <- site_set_rates %>%
    unnest(cols = c(data))
  
  labels <- site_set_rates %>%
    ungroup() %>%
    select(site_name, rate) %>%
    mutate(label = paste0(site_name, "<br>", "SEC Rate: ", format_result_vals(rate), " mm/yr")) %>%
    pull(label, name = site_name)
  
  text = list(
    bgcolor = "white",
    bordercolor = "transparent",
    font = list(color = "black")
  )
  
  p <- ggplot(data = site_set_cumu, aes(x = event_date_UTC, y = mean_cumu)) +
    geom_smooth(method = "lm", formula = y ~ x, fullrange = TRUE, se = FALSE) +
    geom_point(aes(text = paste("Date:", format(event_date_UTC, "%m/%d/%Y"), "<br>", "SEC:", format(round(mean_cumu, 2), nsmall = 2), "mm")), alpha = 0.6) +
    facet_wrap(~site_name, labeller=as_labeller(labels), ncol = ncol) +
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
