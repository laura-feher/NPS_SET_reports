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
    station_order <<- NULL
    wl_sites <<- c("Marsh 5 (Pope Bay), Marsh 6 (Pine Tree), and Marsh 8 (Valentines)")
    n_wl_loggers <<- 3
    dom_veg <<- data.frame(
      site_name = c("Marsh 5 (Pope Bay)", "Marsh 6 (Pine Tree)", "Marsh 8 (Valentines)", "Marsh 11"),
      dom_veg = c("Unvegetated with some <i>Spartina alterniflora</i>, <i>Spartina patens</i>, <i>Distichlis spicata</i>", "<i>S. alterniflora</i>", "<i>S. alterniflora</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "FIIS") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Great Gun", "Hospital Point", "Watch Hill")
    station_order <<- NULL
    wl_sites <<- c("Great Gun, Hospital Point, and Watch Hill")
    n_wl_loggers <<- 3
    dom_veg <<- data.frame(
      site_name = c("Great Gun", "Hospital Point", "Watch Hill"),
      dom_veg = c("Degraded marsh with some <i>Spartina alterniflora</i> & <i>Distichlis spicata</i>", "<i>S. alterniflora</i>/<i>D. spicata</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "300px"
  } else if (park_code == "COLO") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Marsh 5", "Marsh 13", "Marsh 19", "Marsh 30")
    station_order <<- NULL
    wl_sites <<- c("Marsh 19 and Marsh 30")
    n_wl_loggers <<- 2
    dom_veg <<- data.frame(
      site_name = c("Marsh 5", "Marsh 13", "Marsh 19", "Marsh 30"),
      dom_veg = c("<i>Spartina cynosuroides</i>", "<i>S. cynosuroides</i>, <i>Peltandra virginica</i>, <i>Schoenoplectus</i> sp.", "<i>S. cynosuroides</i>, <i>P. virginica</i>, <i>Schoen.</i> sp.", "<i>S. cynosuroides</i>, <i>P. virginica</i>, <i>Schoen.</i> sp.")
    )
    fig4_height <<- "700px"
  } else if (park_code == "CACO") {
    inactive_sites <<- NULL
    inactive_stations <<- 16
    site_order <<- c("Hatches Harbor restricted", "Hatches Harbor unrestricted", "Duck Harbor", "High Toss", "Phrag", "Gut", "Blackfish Creek", "Nauset Marsh")
    station_order <<- NULL
    wl_sites <<- c("Hatches Harbor restricted, Hatches Harbor unrestricted, Blackfish Creek, and Nauset")
    n_wl_loggers <<- 5
    dom_veg <<- data.frame(
      site_name = c("Hatches Harbor restricted", "Hatches Harbor unrestricted", "Duck Harbor", "High Toss", "Phrag", "Gut", "Blackfish Creek", "Nauset Marsh"),
      dom_veg = c("<i>Spartina alterniflora</i>", "<i>S. alterniflora</i>", "Early successional: <i>Salicornia</i>, <i>Atriplex prostrata</i>, <i>S. alterniflora</i>", "Upland/freshwater mix converting to <i>Phragmites australis</i>", "<i>P. australis</i>", "Degraded marsh with some <i>S. alterniflora</i>", "High salt marsh, <i>S. alterniflora</i> & <i>Spartina patens</i>", "<i>S. alterniflora</i>, <i>S. patens</i>, <i>Salicornia</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "GATE") {
    inactive_sites <<- c("Elders East NF")
    inactive_stations <<- NULL
    site_order <<- c("Elders East", "Elders East NF", "Black Bank", "JOCO", "JOCO REF", "Big Egg Control", "Big Egg Spray-Restored", "Sandy Hook")
    station_order <<- NULL
    wl_sites <<- c("Black Bank, Big Egg, JOCO, and Sandy Hook")
    n_wl_loggers <<- 4
    dom_veg <<- data.frame(
      site_name = c("Elders East", "Elders East NF", "Black Bank", "JOCO", "JOCO REF", "Big Egg Control", "Big Egg Spray-Restored", "Sandy Hook"),
      dom_veg = c("Degraded marsh with some <i>Spartina alterniflora</i>", "Degraded marsh with some <i>S. alterniflora</i>", "Unvegetated sand flat, some <i>S. alterniflora</i>", "<i>S. alterniflora</i>", "<i>S. alterniflora</i>", "Unvegetated mudflat", "Eroding <i>S. alterniflora</i>, <i>Spartina Patens</i>, <i>Distichlis spicata</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "ACAD") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Thompson Island", "Maine Coast Heritage", "Bass Harbor", "Schoodic")
    station_order <<- NULL
    wl_sites <<- c("Thompson Island, Maine Coast Heritage, Bass Harbor and Schoodic")
    n_wl_loggers <<- 4
    dom_veg <<- data.frame(
      site_name =c("Thompson Island", "Maine Coast Heritage", "Bass Harbor", "Schoodic"),
      dom_veg = c("<i>Spartina patens</i>", "<i>S. patens</i>", "<i>S. patens</i>, <i>Spartina alterniflora</i>", "<i>S. patens</i>, <i>Juncus</i> sp.")
    )
    fig4_height <<- "700px"
  } else if (park_code == "BOHA") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Calf Island", "Thompson Island", "Peddocks Island")
    station_order <<- NULL
    wl_sites <<- NULL
    n_wl_loggers <<- NULL
    dom_veg <<- data.frame(
      site_name = c("Calf Island", "Thompson Island", "Peddocks Island"),
      dom_veg = c("<i>Salicornia</i> sp. and mudflat", "<i>Spartina alterniflora</i>", "<i>Spartina patens</i>, <i>Distichlis spicata</i>, <i>Juncus</i> sp.")
    )
    fig4_height <<- "600px"
  } else if (park_code == "VIIS") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("Mary's Creek", "Water Creek")
    station_order <<- NULL
    wl_sites <<- c("Mary's Creek and Water Creek")
    n_wl_loggers <<- 2
    dom_veg <<- data.frame(
      site_name = c("Mary's Creek", "Water Creek"),
      dom_veg = c("<i>Avicennia germinans</i>, <i>Rhizophora mangle</i>, and <i>Laguncularia racemosa</i>", "<i>A. germinans</i>, <i>R. mangle</i>, and <i>L. racemosa</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "SARI") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("SARI 1")
    station_order <<- NULL
    wl_sites <<- c("SARI 1")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("SARI 1"),
      dom_veg = c("<i>Avicennia germinans</i>, <i>Rhizophora mangle</i>, and <i>Laguncularia racemosa</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "BISC") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("BISC 1", "BISC 2")
    station_order <<- NULL
    wl_sites <<- c("BISC 1 and BISC 2")
    n_wl_loggers <<- 2
    dom_veg <<- data.frame(
      site_name = c("BISC 1", "BISC 2"),
      dom_veg = c("<i>Avicennia germinans</i>, <i>Rhizophora mangle</i>, and <i>Laguncularia racemosa</i>", "<i>A. germinans</i>, <i>R. mangle</i>, and <i>L. racemosa</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "GWMP") {
    inactive_sites <<- NULL
    inactive_stations <<- c("Creek Bank 1", "Creek Bank 2", "Creek Bank 3", "Interior 1", "Interior 2", "Interior 3", "River 1", "River 1A", "River 2", "River 3")
    site_order <<- c("Dyke Marsh")
    station_order <<- c("Creek Bank 1", "Creek Bank 2", "Creek Bank 3", "Interior 1", "Interior 2", "Interior 3", "River 1", "River 1A", "River 2", "River 3", "Dyke SET 01", "Dyke SET 02", "Dyke SET 03", "Dyke SET 05", "Dyke SET 07", "Dyke SET 08", "Dyke SET 09", "Dyke SET 13", "Dyke SET 15", "Dyke SET 16", "Dyke SET 17", "Dyke SET 18")
    wl_sites <<- c("Dyke Marsh")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      station_name = c("Creek Bank 1", "Creek Bank 2", "Creek Bank 3", "Interior 1", "Interior 2", "Interior 3", "River 1", "River 1A", "River 2", "River 3", "Dyke SET 01", "Dyke SET 02", "Dyke SET 03", "Dyke SET 05", "Dyke SET 07", "Dyke SET 08", "Dyke SET 09", "Dyke SET 13", "Dyke SET 15", "Dyke SET 16", "Dyke SET 17", "Dyke SET 18"),
      dom_veg = c("<i>Typha latifolia</i>, <i>Peltandra virginica</i>, <i>Sagittaria</i> sp., <i>Phragmites australis</i>", rep("<i>T. latifolia</i>, <i>P. virginica</i>, <i>Sagitattaria</i> sp., <i>P. australis</i>", 21))
      )
    fig4_height <<- "600px"
  } else if (park_code == "NACE") {
    inactive_sites <<- NULL
    inactive_stations  <<- c("Kenilworth 1", "Kenilworth 2", "Kenilworth 3", "Kenilworth 4", "Kenilworth 5", "Kingman 1", "Kingman 2", "Kingman 3", "Kingman 4", "Kingman 5")
    site_order <<- c("Kenilworth Marsh", "Kingman Lake")
    station_order <<- c("Kenilworth 1", "Kenilworth 2", "Kenilworth 3", "Kenilworth 4", "Kenilworth 5", "KenSET01", "KenSET02", "KenSET04", "KenSET05", "KenSET06", "KenSET07", "Kingman 1", "Kingman 2", "Kingman 3", "Kingman 4", "Kingman 5")
    wl_sites <<- c("Kenilworth Marsh")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      station_name = c("Kenilworth 1", "Kenilworth 2", "Kenilworth 3", "Kenilworth 4", "Kenilworth 5", "KenSET01", "KenSET02", "KenSET04", "KenSET05", "KenSET06", "KenSET07", "Kingman 1", "Kingman 2", "Kingman 3", "Kingman 4", "Kingman 5"),
      dom_veg = c("<i>Typha latifolia</i>, <i>Peltandra virginica</i>, <i>Sagittaria</i> sp., <i>Phragmites australis</i>", rep("<i>T. latifolia</i>, <i>P. virginica</i>, <i>Sagittaria</i> sp., <i>P. australis</i>", 10), rep("Unvegetated mudflat", 4), "<i>P. australis</i>")
    )
    fig4_height <<- "700px"
  } else if (park_code == "CANA") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("CANA121", "CANA 221")
    station_order <<- NULL
    wl_sites <<- c("Apollo Beach Visitors Center Dock")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("CANA121", "CANA 221"),
      dom_veg = c("<i>Spartina alterniflora</i>, <i>Avicennia germinans</i>, <i>Rhizophora mangle</i>", "<i>S. alterniflora</i>, <i>A. germinans</i>, <i>R. mangle</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "CAHA") {
   inactive_sites <<- NULL
   inactive_stations <<- NULL
   site_order <<-  c("CAHA11", "CAHA122 Sandy Bay", "CAHA222  Ocracoke")
   station_order <- NULL
   wl_sites <<- c("Ocracoke boat dock")
   n_wl_loggers <<- 1
   dom_veg <<- data.frame(
     site_name = c("CAHA11", "CAHA122 Sandy Bay", "CAHA222  Ocracoke"),
     dom_veg = c("<i>Juncus roemarianus</i>, <i>Spartina alterniflora</i>", "<i>S. alterniflora</i>", "<i>S. alterniflora</i>")
   )
   fig4_height <<- "600px"
  } else if (park_code == "CUIS") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("CUIS121", "CUIS221")
    station_order <<- NULL
    wl_sites <<- c("Sea Camp Ranger Station dock")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("CUIS121", "CUIS221"),
      dom_veg = c("<i>Spartina alterniflora</i>, <i>Batis maritima</i>, <i>Salicornia bigelovii</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "FOFR") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("FOFR121", "FOFR221")
    station_order <<- NULL
    wl_sites <<- NULL
    n_wl_loggers <<- NULL
    dom_veg <<- data.frame(
      site_name = c("FOFR121", "FOFR221"),
      dom_veg = c("<i>Spartina alterniflora</i>", "<i>Scirpus robustus</i>, <i>Juncus roemarianus</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "FOPU") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("FOPU02", "FOPU121")
    station_order <<- NULL
    wl_sites <<- "Lazaretto Creek dock"
    n_wl_loggers <<- 1 
    dom_veg <<- data.frame(
      site_name = c("FOPU02", "FOPU121"),
      dom_veg = c("<i>Spartina alterniflora</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "TIMU") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("TIMU03", "TIMU04")
    station_order <<- NULL
    wl_sites <<- c("Kingsley Plantation dock")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("TIMU03", "TIMU04"),
      dom_veg = c("<i>Spartina alterniflora</i>", "<i>S. alterniflora</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "FOMA") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("FOMA021")
    station_order <<- NULL
    wl_sites <<- c("Visitors Center dock")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("FOMA021"),
      dom_veg = c("<i>Spartina alterniflora</i>, <i>Batis maritima</i>")
    )
    fig4_height <<- "400px"
  } else if (park_code == "CALO") {
    inactive_sites <<- NULL
    inactive_stations <<- NULL
    site_order <<- c("CALO01")
    station_order <<- NULL
    wl_sites <<- c("Middle Marsh")
    n_wl_loggers <<- 1
    dom_veg <<- data.frame(
      site_name = c("CALO01"),
      dom_veg = c("<i>Juncus roemarianus</i>")
    )
    fig4_height <<- "400px"
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