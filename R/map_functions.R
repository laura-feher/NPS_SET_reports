#' Functions for creating the leaflet map showing SET stations, water loggers,
#' and NOAA tide gauges for each park.
#'
#' @param data dataframe. A dataframe of SET pin_heights.
#' @param dp_id string. The data package id number from datastore.
#' @param dp_pub_date string. YYYYMMDD date that the data package was published.
#' @param park_code string. The 4-letter park unit code.
#' @param dp_year string. The year that the data package was published on
#'   datastore.
#' @param crosstalk TRUE/FALSE. Return a crosstalk object? Defaults to FALSE.
#' @param crosstalk_group string. If returning a crosstalk object, the crosstalk
#'   group type. Defaults to "map".
#'
#' @description Mapping functions based on
#'   https://github.com/jakegross808/pacn-veg-package/blob/main/R/spatial.R.
#'   `r get_station_points()` gets the SET station coordinates from the
#'   "station_data" csv in the data package. 
#'   `r get_site_points()` averages the SET station-level coordinates up to the
#'    site-level. 
#'   `r get_all_points()` row binds the station and site-level coordinates into a
#'     single dataframe. 
#'   `r get_waterlogger_points()` contains the hard-coded coordinates for the
#'   individual water level loggers. 
#'   `r get_noaa_tidegauge_points()` contains the hard-corded coordinates for the 
#'   NOAA tide gauges. 
#'   `r map_SETs()` creates the leaflet map of the SET stations, water loggers, 
#'   and NOAA tide gauges.
#'
#' @name map_functions
#' @import dplyr
#' @import readr
#' @import sf
#' @import crosstalk
#' @import htmltools
#' @import leaflet
#' @import leaflet.extras
#' @import keyring
#' @import basemaps
#' @import terra
#' @import tidyterra
#' 
#' @rdname map_functions
#' @export
get_station_points <- function(data = data, dp_id, dp_pub_date, park_code, dp_year, crosstalk = FALSE, crosstalk_group = "map"){
  
  station_points <- readr::read_csv(here::here("data", dp_id, paste0("station_data_", dp_pub_date, ".csv")), show_col_types = FALSE) %>%
    filter(., park_code == params$park_code) %>%
    filter(!(station_code %in% c("JR1S", "JR2S", "JR3S", "EE1S", "EE2S", "EE3S"))) %>% # Exclude shallow SETs at GATE
    filter(site_name != "Pine Tree Study") %>% # Exclude Pine Tree Study at ASIS
    filter(!(station_code) %in% c("M11-3", "M5-2", "M6-4", "M8-4")) %>% # Exclude fenced stations at ASIS
    {if (park_code == "CACO" & as.numeric(dp_year) < 2027)
      filter(., site_name != "Duck Harbor") # Exclude Duck Harbor at CACO for 2025 since there is only 1 year of data so far
      else if (park_code == "CANA" & as.numeric(dp_year) < 2027)
        filter(., site_name != "CANA02") # Exclude CANA02 at CANA for 2025 since there is not enough data yet
      else if (park_code == "CUIS") 
        filter(., site_name != "CUIS04") # leave out CUIS04 - only 3 years of data from 2015-2018
      else if (park_code == "FOFR")
        filter(., site_name != "FOFR16")  #leave out FOFR16 - only 3 years of data from 2015-2018
      else if (park_code == "FOPU")
        filter(., !station_code %in% c("FOPU02A", "FOPU02B", "FOPU02C")) #leave out earlier stations at FOPU
      else if (park_code == "TIMU")
        filter(., site_name %in% c("TIMU03", "TIMU04")) # exclude other sites - not enough data
      else if (park_code == "FOMA")
        filter(., site_name == "FOMA021")
      else .
      } %>%
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
#'
#' @rdname map_functions
#' @export
get_site_points <- function(data = data, dp_id, dp_pub_date, park_code, dp_year, crosstalk = FALSE, crosstalk_group = "map"){
  
  site_points <- get_station_points(data, dp_id, dp_pub_date, park_code, dp_year) %>%
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

get_all_points <- function(data = data, dp_id, dp_pub_date, park_code, dp_year, crosstalk = FALSE, crosstalk_group = "map"){
  
  all_points <- bind_rows(
    get_station_points(data, dp_id, dp_pub_date, park_code, dp_year),
    get_site_points(data, dp_id, dp_pub_date, park_code, dp_year)
  )
  
  if (crosstalk) {
    all_points <- crosstalk::SharedData$new(all_points, group = crosstalk_group)
  }
  
  return(all_points)
}
#'
#' @rdname map_functions
#' @export
get_waterlogger_points <- function(park_code) {
  if (park_code == "ASIS") {
    data.frame(Park = c(rep("ASIS",3)),
               Site = c("Marsh 5 (Pope Bay)", "Marsh 6 (Pine Tree)", "Marsh 8 (Valentines)"),
               Lat = c(38.048201, 38.143613, 38.089134),
               Lon = c(-75.234397, -75.187342, -75.222018))
  } else if (park_code == "ACAD") {
    data.frame(Park = c(rep("ACAD",4)),
               Site = c("Schoodic", "Bass Harbor", "Thompson Island", "Maine Coast Heritage Trust"),
               Lat = c(44.34247, 44.2549, 44.42534, 44.37435),
               Lon = c(-68.05983, -68.34065, -68.3643, -68.32927))
  } else if (park_code == "CACO") {
    data.frame(Park = c(rep("CACO",5)),
               Site = c("Hatches Harbor Inside Dike", "Hatches Harbor Outside Dike", "Blackfish Creek", "Nauset North", "Nauset South"),
               Lat = c(42.064801, 42.064471, 41.906561, 41.824418, 41.816709),
               Lon = c(-70.234242, -70.234815, -69.990951, -69.962908, -69.955279)
               )
  } else if (park_code == "COLO") {
    data.frame(Park = c(rep("COLO",2)),
               Site = c("Marsh 19", "Marsh 30"),
               Lat = c(37.206919, 37.217498),
               Lon = c(-76.759752, -76.76518))
  } else if (park_code == "FIIS") {
    data.frame(Park = c(rep("FIIS",3)),
               Site = c("Watch Hill", "Hospital Point", "Great Gun"),
               Lat = c(40.695744, 40.727839, 40.75853),
               Lon = c(-72.981944, -72.893756, -72.78578))
  } else if (park_code == "GATE") {
    data.frame(Park = c(rep("GATE",6)),
               Site = c("Sandy Hook", "Black Bank", "Big Egg Spray-Restored", "Big Egg Control", "JOCO", "JOCO REF"),
               Lat = c(40.449255, 40.620789, 40.59626, 40.59626, 40.611155, 40.611155),
               Lon = c(-74.000406, -73.833819, -73.82705, -73.82705, -73.786039, -73.786039))
  } else if (park_code == "NACE") {
    data.frame(Park = c("NACE"),
               Site = "Kenilworth",
               Lat = 38.912063,
               Lon = -76.948366)
  } else if (park_code == "GWMP") {
    data.frame(Park = c("GWMP"),
               Site = "Dyke Marsh",
               Lat = 38.766537,
               Lon = -77.048693)
  } else if (park_code == "VIIS") {
    data.frame(Park = c("VIIS"), # Need to get exact coordinates from Kevin
               Site = c("Mary's Creek", "Water Creek"),
               Lat = c(18.367101, 18.351479),
               Lon = c(-64.734781, -64.688395))
  } else if (park_code == "SARI") {
    data.frame(Park = c("SARI"), # Need to get exact coordinates from Kevin
               Site = "SARI 1",
               Lat = 17.766195,
               Lon = -64.756298)
  } else if (park_code == "BISC") {
    data.frame(Park = c("BISC"), # Need to get exact coordinates from Kevin
               Site = c("BISC 1", "BISC 2"),
               Lat = c(25.476786, 25.541969),
               Lon = c(-80.341200, -80.314276))
  } else if (park_code == "CANA") {
    data.frame(Park = c("CANA"), 
               Site = "Dyke Marsh",
               Lat = 28.926843,
               Lon = -80.824903)
  } else if (park_code == "CAHA") {
    data.frame(Park = c("CAHA"), # Need to get exact coordinates from Ches
               Site = c("Ocracoke boat dock"),
               Lat = c(35.189016),
               Lon = c(-75.783196))
  } else if (park_code == "CUIS") {
    data.frame(Park = c("CUIS"),
               Site = "Sea Camp Ranger Station dock",
               Lat = 30.764172,
               Lon = -81.470977)
  } else if (park_code == "FOPU") {
    data.frame(Park = c("FOPU"),
               Site = "Lazaretto Creek dock",
               Lat = 32.014153,
               Lon = -80.884304)
  } else if (park_code == "TIMU") {
    data.frame(Park = c("TIMU"),
               Site = c("Kingsley Plantation dock"),
               Lat = c(30.441042),
               Lon = c(-81.439171))
  } else if (park_code == "FOMA") {
    data.frame(Park = c("FOMA"),
               Site = "Visitors Center dock",
               Lat = 29.714803,
               Lon = -81.234876)
  } else if (park_code == "CALO") {
    data.frame(Park = c("CALO"), # Need to get exact coordinates from Ches
               Site = "Middle Marsh",
               Lat = 34.749887, 
               Lon = -76.424893)
  }
}
#'
#' @rdname map_functions
#' @export
get_noaa_tidegauge_points <- function(park_code) {
  if (park_code == "ACAD") {
    data.frame(Park = "ACAD", 
               Site = NA,
               Name = "Bar Harbor, ME",
               StationID = "8413320",
               Lat = 44.38472222,
               Lon = -68.20083333)
  } else if (park_code == "BOHA") {
    data.frame(Park = "BOHA",
               Site = NA,
               Name = "Boston, MA",
               StationID = 8443970,
               Lat = 42.35055556,
               Lon = -71.05)
  } else if (park_code == "CACO") {
    data.frame(Park = c(rep("CACO",2)),
               Site = c(NA, "Nauset"),
               Name = c("Boston, MA", "Nantucket Island, MA"),
               StationID = c(8443970, 8449130),
               Lat = c(42.35055556, 41.283611),
               Lon = c(-71.05, -70.085556))
  } else if (park_code == "FIIS" | park_code == "GATE") {
    if (park_code == "FIIS"){
      data.frame(Park = "FIIS",
                 Site = NA,
                 Name = "Sandy Hook, NJ",
                 StationID = 8531680,
                 Lat = 40.46666669,
                 Lon = -74.00166667)
    } else if (park_code == "GATE") {
      data.frame(Park = "GATE",
                 Site = NA,
                 Name = "Sandy Hook, NJ",
                 StationID = 8531680,
                 Lat = 40.46666669,
                 Lon = -74.00166667)
    }
  } else if (park_code == "ASIS") {
    data.frame(Park = "ASIS",
               Site = NA, 
               Name = "Ocean City Inlet, MD",
               StationID = 8570283,
               Lat = 38.31861111,
               Lon = -75.08472222)
  } else if (park_code == "NACE" | park_code == "GWMP") {
    if (park_code == "NACE") {
      data.frame(Park = "NACE",
                 Site = NA, 
                 Name = "Washington, DC",
                 StationID = 8594900,
                 Lat = 38.86777778,
                 Lon = -77.0175)
    } else if (park_code == "GWMP") {
      data.frame(Park = "GWMP",
                 Site = NA, 
                 Name = "Washington, DC",
                 StationID = 8594900,
                 Lat = 38.86777778,
                 Lon = -77.0175)
    }
  } else if (park_code == "COLO") {
    data.frame(Park = "COLO",
               Site = NA,
               Name = "Sewells Point, VA",
               StationID = 8638610, 
               Lat = 36.935,
               Lon = -76.31861111)
  } else if (park_code == "CAHA") {
    data.frame(Park = "CAHA",
               Site = NA,
               Name = "Oregon Inlet Marina, NC",
               StationID = 8652587,
               Lat = 35.78527778,
               Lon = -75.53583333)
  } else if (park_code == "FOPU") {
    data.frame(Park = "FOPU",
               Site = NA,
               Name = "Fort Pulaski, GA",
               StationID = 8670870,
               Lat = 32.03361111,
               Lon = -80.90055556)
  } else if (park_code == "FOFR" | park_code == "CUIS") {
    if (park_code == "FOFR") {
      data.frame(Park = "FOFR",
                 Site = NA,
                 Name = "Fernandina Beach, FL",
                 StationID = 8720030,
                 Lat = 30.6675,
                 Lon = -81.46666667)
    } else if (park_code == "CUIS"){
      data.frame(Park = "CUIS",
                 Site = NA,
                 Name = "Fernandina Beach, FL",
                 StationID = 8720030,
                 Lat = 30.6675,
                 Lon = -81.46666667)
    }
  } else if (park_code == "FOMA" | park_code == "TIMU") {
    if (park_code == "FOMA") {
      data.frame(Park = "FOMA",
                 Site = NA,
                 Name = "Mayport (Bar Pilots Dock), FL",
                 StationID = 8720218,
                 Lat = 30.38583333,
                 Lon = -81.41861111)
    } else if (park_code == "TIMU") {
      data.frame(Park = "TIMU",
                 Site = NA,
                 Name = "Mayport (Bar Pilots Dock), FL",
                 StationID = 8720218,
                 Lat = 30.38583333,
                 Lon = -81.41861111)
    }
  } else if (park_code == "CANA") {
    data.frame(Park = "CANA",
               Site = NA,
               Name = "Trident Pier, Port Canaveral, FL",
               StationID = 8721604,
               Lat = 28.4025,
               Lon = -80.585)
  } else if (park_code == "BISC") {
    data.frame(Park = "BISC",
               Site = NA, 
               Name = "Virginia Key, FL",
               StationID = 8723214,
               Lat = 25.71916667,
               Lon = -80.15194444)
  } else if (park_code == "VIIS") {
    data.frame(Park = "VIIS",
               Site = NA,
               Name = "Charlotte Amalie, St Thomas, VI",
               StationID = 9751639,
               Lat = 18.31888889,
               Lon = -64.91805556)
  } else if (park_code == "SARI") {
    data.frame(Park = "SARI",
               Site = NA,
               Name = "Limetree Bay, St Croix, VI",
               StationID = 9751401,
               Lat = 17.695146,
               Lon = -64.753068)
  } else if (park_code == "CALO") {
    data.frame(Park = "CALO",
               Site = NA,
               Name = "Beaufort, NC",
               StationID = 8656483,
               Lat = 34.71666667,
               Lon = -76.66722222)
  }
}
#'
#' @rdname map_functions
#' @export
map_SETs <- function(data = data, park_code, dp_id, dp_year, dp_pub_date, crosstalk = FALSE, crosstalk_group = "map", password = keyring::key_get(service = "NPS Park Tiles"), static = FALSE) {
  
  # Check if the NPS Park Tiles API key is already set. If not, prompt for password.
  # if (any(keyring::key_list()$service == "NPS Park Tiles")) {
  #   message("NPS Park Tiles key is already set")
  # } else {
  #   keyring::key_set(service = "NPS Park Tiles", keyring = "NPS")
  # }
  
  points_data <- get_station_points(data = data, park_code = park_code, dp_id = dp_id, dp_year = dp_year, dp_pub_date = dp_pub_date, crosstalk = crosstalk, crosstalk_group = crosstalk_group)
  {if (park_code == "FOFR")
    wl_points <- NULL
    else
      wl_points <- get_waterlogger_points(park_code = park_code)}
  tide_gauge_points <- get_noaa_tidegauge_points(park_code = park_code)
  
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
  
  if (static == FALSE) {
  # NPS park tiles URLs
  NPSbasic = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=", password)
  NPSimagery = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=", password)
  NPSslate = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=", password) 
  NPSlight = paste0("https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpia2u0auf01p9vbugvcpv/tiles/256/{z}/{x}/{y}@2x?access_token=", password)
  
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
                        clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount();  
      c = 'rgba(0, 0, 255, 0.5);' // Change marker cluster color to blue
      h = 'rgba(255, 255, 255, 1);' // change marker cluster text color to white
    return new L.DivIcon({ html: '<div style=\"color:'+h+'; background-color:'+c+'\"><span>' + childCount + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });

  }")),
                        labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")),
                        popup = ~paste0("<strong>Site: </strong>", site_name, 
                                        "<br><strong>Station: </strong>", station_code)) %>%
    { if(park_code %in% c("BOHA", "FOFR"))
      .
      else 
        leaflet::addCircles(., data = wl_points, lng = ~Lon, lat = ~Lat, color = "red", popup = ~paste0("<strong>Water logger: </strong>", Site), labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold")))} %>%
    # leaflet::addCircles(data = wl_points, lng = ~Lon, lat = ~Lat, color = "red", popup = ~paste0("<strong>Water logger: </strong>", Site), labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold"))) %>%
    leaflet::addCircles(data = tide_gauge_points, lng = ~Lon, lat = ~Lat, color = "green", popup = ~paste0("<strong>NOAA Tide Gauge: </strong>", Name), labelOptions = leaflet::labelOptions(noHide = TRUE, opacity = .9, textOnly = TRUE, offset = c(0,0), direction = "center", style = list("color" = "white", "font-weight" = "bold"))) %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    { if(park_code %in% c("BOHA", "FOFR"))
      . 
      else
        leaflet::addLegend(., labels = c("Water loggers"), colors = c("red"), position = "bottomleft")} %>%
    leaflet::addLegend(., labels = c("NOAA Tide Gauge"), colors = c("green"), position = "bottomleft") %>%
    leaflet::addLegend(., labels = c("SET stations"), colors = c("blue"), position = "bottomleft") %>%
    
    leaflet.extras::addResetMapButton()
  
  # Static maps for ACAD and BOHA
  } else if (static == TRUE) {
    
    # Basemaps were obtained using basemap_geotif() from the basemaps package - e.g. basemap_geotif(ext = ext, map_service = "carto", map_type = "voyager_labels_under", map_dir = here::here("data"))
    
    sf::sf_use_s2(FALSE)
    
    if (park_code == "ACAD") {
      # ACAD ext
      ext <- st_bbox(c(xmin = -68.45444, xmax = -68.02597, ymin = 44.2199, ymax = 44.44373), crs = 4326)
      # basemaps::basemap_geotif(ext = ext, map_service = "carto", map_type = "voyager_labels_under", map_dir = here::here("data"))
      raster_data <- terra::rast(here::here("data", "basemap_carto_voyager_ACAD.tif"))
      
      wl_pts <- wl_points %>%
        sf::st_as_sf(., coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>%
        st_transform(., crs = 3857) %>%
        mutate(lon = st_coordinates(.)[,1],
               lat = st_coordinates(.)[,2],
               pt_type = "water loggers")
      
      park_boundary <- st_read(here::here("data", "ACAD_boundary.shp"), quiet = TRUE) %>%
        st_transform(., crs = 4326) %>%
        st_crop(., ext) %>%
        st_transform(., crs = 3857)

    } else if (park_code == "BOHA") {
      ext <- st_bbox(c(xmin = -71.06300, xmax = -70.86186, ymin = 42.26792, ymax = 42.38442), crs = 4326)
      # basemaps::basemap_geotif(ext = ext, map_service = "carto", map_type = "voyager_labels_under", map_dir = here::here("data"))
      raster_data <- terra::rast(here::here("data", "basemap_carto_voyager_BOHA.tif"))
      
      park_boundary <- st_read(here::here("data", "BOHA_boundary.shp"), quiet = TRUE) %>%
        st_transform(., crs = 4326) %>%
        st_crop(., ext) %>%
        st_transform(., crs = 3857)
      
    }
    
    site_pts <- points_data %>%
      sf::st_drop_geometry(.) %>%
      group_by(park_code, park_name, site_name) %>%
      summarise(longitude = mean(longitude, na.rm = TRUE),
                latitude = mean(latitude, na.rm = TRUE)) %>%
      sf::st_as_sf(., coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
      st_transform(., crs = 3857) %>%
      mutate(lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2],
             pt_type = "SET sites") %>%
      mutate(., site_name = if_else(site_name == "ThompIsland", "Thompson Island", site_name))
    
    tide_pts <- tide_gauge_points %>%
      sf::st_as_sf(., coords = c("Lon", "Lat"), crs = 4326, remove = FALSE) %>%
      st_transform(., crs = 3857) %>%
      mutate(lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2],
             pt_type = "NOAA tide gauge")
    
    map <- ggplot() +
      tidyterra::geom_spatraster_rgb(data = raster_data) +
      geom_sf(data = park_boundary, fill = NA) +
      {if (park_code == "ACAD")
        geom_point(data = wl_pts, aes(x = lon, y = lat, fill = pt_type), shape = 21, size = 2, position = position_nudge(x = 500))
        } +
      geom_sf(data = tide_pts, aes(fill = pt_type), shape = 21, size = 2) +
      geom_sf(data = site_pts, aes(fill = pt_type), shape = 21, size = 2) +
      ggrepel::geom_label_repel(data = site_pts, aes(x = lon, y = lat, label = site_name), seed = 42, point.padding = 5) +
      coord_sf(expand = c(0,0)) +
      {if (park_code == "ACAD")
        scale_fill_manual(values = c("#e41a1c", "#00BA38", "#619CFF"))
        else if (park_code == "BOHA")
          scale_fill_manual(values = c("#e41a1c", "#00BA38"))
        } +
      theme(
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.position = "inside",
        legend.background = element_blank(),
        legend.position.inside = c(0.85, 0.2),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.title = element_blank()
      ) +
      {if (park_code == "ACAD")
        theme(
          legend.position.inside = c(0.85, 0.2)
        ) 
        else if (park_code == "BOHA")
          theme(
            legend.position.inside = c(0.85, 0.85)
          )
        }
  }
  
  return(map)
}