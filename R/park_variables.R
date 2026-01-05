#' Uses super assignment to assign park-specific variables to the global environment.
#'
#' @param park_code string. The 4-letter park unit code.
#'
#' @export
#'
park_variables <- function(park_code) {
  
  # Output these values to global env using super assignment <<-
  
  if (park_code == "ASIS") {
    inactive_sites <<- c("Pine Tree Study")
    inactive_stations <<- NULL
    site_order <<- c("Marsh 5 (Pope Bay)", "Marsh 6 (Pine Tree)", "Marsh 8 (Valentines)", "Marsh 11")
    wl_sites <<- c("Marsh 5 (Pope Bay), Marsh 6 (Pine Tree), and Marsh 8 (Valentines)")
    n_wl_loggers <<- 3
    dom_veg <<- data.frame(
      site_name = c("Marsh 5 (Pope Bay)", "Marsh 6 (Pine Tree)", "Marsh 8 (Valentines)", "Marsh 11"),
      dom_veg = c("Unvegetated with some <i>S. alterniflora</i>, <i>S. patens</i>, <i>D. spicata</i>", "<i>Spartina alterniflora</i>", "<i>Spartina alterniflora</i>", "<i>Spartina alterniflora</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "FIIS") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Great Gun", "Hospital Point", "Watch Hill")
    wl_sites <<- c("Great Gun, Hospital Point, and Watch Hill")
    n_wl_loggers <<- 3
    dom_veg <<- data.frame(
      site_name = c("Great Gun", "Hospital Point", "Watch Hill"),
      dom_veg = c("Degraded marsh with some <i>S. alterniflora</i> & <i>D. spicata</i>", "<i>Spartina alterniflora</i>/<i>Distichlis spicata</i>", "<i>Spartina alterniflora</i>")
    )
    fig4_height <<- "300px"
  } else if (park_code == "COLO") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Marsh 5", "Marsh 13", "Marsh 19", "Marsh 30")
    wl_sites <<- c("Marsh 19 and Marsh 30")
    n_wl_loggers <<- 2
    dom_veg <<- data.frame(
      site_name = c("Marsh 5", "Marsh 13", "Marsh 19", "Marsh 30"),
      dom_veg = c("<i>Spartina cynosuroides</i>", "<i>Spartina cynosuroides</i>, <i>Peltandra virginica</i>, <i>Schoen.</i> sp.", "<i>Spartina cynosuroides</i>, <i>Peltandra virginica</i>, <i>Schoen.</i> sp.", "<i>Spartina cynosuroides</i>, <i>Peltandra virginica</i>, <i>Schoen.</i> sp.")
    )
    fig4_height <<- "700px"
  } else if (park_code == "CACO") {
    inactive_sites <<- NULL
    inactive_stations <<- 16
    site_order <<- c("Hatches Harbor restricted", "Hatches Harbor unrestricted", "Duck Harbor", "High Toss", "Phrag", "Gut", "Blackfish Creek", "Nauset Marsh")
    wl_sites <<- c("Hatches Harbor restricted, Hatches Harbor unrestricted, Blackfish Creek, and Nauset")
    n_wl_loggers <<- 5
    dom_veg <<- data.frame(
      site_name = c("Hatches Harbor restricted", "Hatches Harbor unrestricted", "Duck Harbor", "High Toss", "Phrag", "Gut", "Blackfish Creek", "Nauset Marsh"),
      dom_veg = c("<i>Spartina alterniflora</i>", "<i>Spartina alterniflora</i>", "Early successional: <i>Salicornia</i>, <i>Atriplex prostrata</i>, <i>S. alterniflora</i>", "Upland/freshwater mix converting to <i>Phragmites australis</i>", "<i>Phragmites australis</i>", "Degraded marsh with some <i>S. alterniflora</i>", "High salt marsh, <i>S. alterniflora</i> & <i>S. patens</i>", "<i>S. alterniflora</i>, <i>S. patens</i>, <i>Salicornia</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "GATE") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Elders East", "Elders East NF", "Black Bank", "JOCO", "JOCO REF", "Big Egg Control", "Big Egg Spray-Restored", "Sandy Hook")
    wl_sites <<- c("Black Bank, Big Egg, JOCO, and Sandy Hook")
    n_wl_loggers <<- 4
    dom_veg <<- data.frame(
      site_name = c("Elders East", "Elders East NF", "Black Bank", "JOCO", "JOCO REF", "Big Egg Control", "Big Egg Spray-Restored", "Sandy Hook"),
      dom_veg = c("Degraded marsh with some <i>S. alterniflora</i>", "Degraded marsh with some <i>S. alterniflora</i>", "Unvegetated sand flat, some <i>S. alterniflora</i>", "<i>Spartina alterniflora</i>", "<i>Spartina alterniflora</i>", "Unvegetated mudflat", "Eroding <i>S. alterniflora</i>, <i>S. Patens</i>, <i>D. spicata</i>", "<i>Spartina alterniflora</i>")
    )
    fig4_height <<- "700px"
  } else {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- NULL
    wl_sites <<- NULL
    n_wl_loggers <<- NULL
    dom_veg <<- NULL
    fig4_height <<- "700px"
  }
}