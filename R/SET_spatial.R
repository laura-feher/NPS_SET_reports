# mapping functions based on https://github.com/jakegross808/pacn-veg-package/blob/main/R/spatial.R

get_station_points <- function(data = data, crosstalk = FALSE, crosstalk_group = "map"){
  
  station_points <- readr::read_csv(here::here("data", params$data_package_id, paste0("station_data_", params$data_package_pub_date, ".csv")), show_col_types = FALSE) %>%
    filter(park_code == params$park_code) %>%
    left_join(., 
              data %>%
                distinct(park_code, park_name, site_name, station_code, station_name),
              by = c("park_code", "park_name", "site_name", "station_code", "station_name")) %>%
    # Convert to sf data frame
    sf::st_as_sf(., coords = c("station_longitude", "station_latitude"), crs = 4326, remove = FALSE) %>% # 4326 is WGS84 CRS
    rename("longitude" = station_longitude, "latitude" = station_latitude) %>%
    mutate(layer = "station",
           pt_label = stringr::str_replace_all(station_code, "_", "-"))
  
  if (crosstalk) {
    station_points <- crosstalk::SharedData$new(station_points, group = crosstalk_group)
  }
  
  return(station_points)
}

get_site_points <- function(data = data, crosstalk = FALSE, crosstalk_group = "map"){
  
  site_points <- get_station_points(data) %>%
    select(-c(station_code, station_name, SET_depth_m, SET_date_established, SET_date_retired, station_status, protected_status, station_notes)) %>%
    sf::st_drop_geometry(.) %>%
    group_by(park_code, park_name, site_name) %>%
    summarise(longitude = mean(longitude, na.rm = TRUE),
              latitude = mean(latitude, na.rm = TRUE)) %>%
    sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    mutate(layer = "site",
           pt_label = site_name) %>%
    ungroup(.)
  
  if (crosstalk) {
    site_points <- crosstalk::SharedData$new(site_points, group = crosstalk_group)
  }
  
  return(site_points)
}

get_all_points <- function(data = data, crosstalk = FALSE, crosstalk_group = "map"){
  
  all_points <- bind_rows(
    get_station_points(data),
    get_site_points(data)
  )
  
  if (crosstalk) {
    all_points <- crosstalk::SharedData$new(all_points, group = crosstalk_group)
  }
  
  return(all_points)
}

map_SETs <- function(data = data, crosstalk = FALSE, crosstalk_group = "map") {
  
  points_data <- get_station_points(data = data, crosstalk = crosstalk, crosstalk_group = crosstalk_group)
  
  # If points is a crosstalk object, extract just the data for functions that need a regular tibble/dataframe
  if (crosstalk) {
    points_data <- points_data$data()
  } 
  
  # Make NPS map Attribution
  NPSAttrib <-
    htmltools::HTML(
      "<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> |
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles'
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>"
    )
  
  # NPS park tiles URLs
  NPSbasic = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSimagery = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSslate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  NPSlight = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
  
  map <- leaflet::leaflet(points_data) %>%
    leaflet::addTiles(group = "Basic", urlTemplate = NPSbasic, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Imagery", urlTemplate = NPSimagery, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Slate", urlTemplate = NPSslate, attribution = NPSAttrib) %>%
    leaflet::addTiles(group = "Light", urlTemplate = NPSlight, attribution = NPSAttrib) %>%
    leaflet::addLayersControl(baseGroups = c("Basic", "Imagery", "Slate", "Light"),
                            options=leaflet::layersControlOptions(collapsed = TRUE)) %>%
    leaflet::addCircleMarkers(lng = ~longitude,
                        lat = ~latitude,
                        label = ~pt_label,
                        clusterOptions = markerClusterOptions(),
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<strong>Site: </strong>", site_name, 
                                        "<br><strong>Station: </strong>", station_code)) %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addLegend(., labels = c("SET stations"), colors = c("blue"), position = "bottomleft") %>%
    leaflet.extras::addResetMapButton()
  
  return(map)
}
