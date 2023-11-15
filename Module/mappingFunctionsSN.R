if ("geojsonio" %in% rownames(installed.packages()) == FALSE) (install.packages("geojsonio")); library("geojsonio");
if ("leaflet" %in% rownames(installed.packages()) == FALSE) (install.packages("leaflet")); library("leaflet");
if ("leafem" %in% rownames(installed.packages()) == FALSE) (install.packages("leafem")); library("leafem");
if ("geojsonR" %in% rownames(installed.packages()) == FALSE) (install.packages("geojsonR")); library("geojsonR");
if ("jsonlite" %in% rownames(installed.packages()) == FALSE) (install.packages("jsonlite")); library("jsonlite");


testkarte <- basiskarte()
showMap(testkarte)



showMap <- function(map){
 
  map 
}

filterKreise <- function(map){


}


basiskarte <- function() {
  ##Map erzeugen
  # WMS-Dienste:
  wms_webatlas <- "https://geodienste.sachsen.de/wms_geosn_webatlas-sn/guest"
  #wms_grb <- "https://geodienste.sachsen.de/wmts_geosn_webatlas-sn/guest"
  #wms_verwaltung <- "https://geodienste.sachsen.de/wms_geosn_verwaltungseinheiten/guest"
  #wms_ortho <- "https://geodienste.sachsen.de/wms_geosn_dop-rgb/guest?"
 
 #min-width:320px; min-height: 230px;
 map <- leaflet(options = leafletOptions(
  zoomControl = FALSE,
   boxZoom = FALSE,
   doubleClickZoom = FALSE,
   dragging = FALSE,
   keyboard = FALSE,
   scrollWheelZoom = FALSE,
   touchZoom = FALSE
#   zoomControl = TRUE,
#   boxZoom = TRUE,
#   doubleClickZoom = TRUE,
#   dragging = TRUE,
#   keyboard = TRUE,
#   scrollWheelZoom = TRUE,
#   touchZoom = TRUE
   
   )) %>% 
   setView(lng = 13.75747, lat = 51.07907, zoom = 1) %>%
   addWMSTiles(
     wms_webatlas,
     layers = c("Siedlung",
                "Vegetation",
                "Gewaesser",
                "Verkehr", 
                #"Administrative_Einheiten",
                "Beschriftung"
     ),
     options = WMSTileOptions(format = "image/png", transparent = TRUE)
#   ) %>% 
#   addWMSTiles(
#     wms_verwaltung,
#     layers = c("Landkreis_Kreisfreie_Stadt"
#     ),
#     options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>% fitBounds(11.768364, 50.129238, 15.043435, 51.726628)
#   %>%  addMouseCoordinates()
#   Ausdehnung
#   11.7683640000000000,50.1292389999999983 : 15.0434350000000006,51.7266279999999981

 
 geojson <- readLines("./kreise_grob.geojson", warn = FALSE) %>%
   paste(collapse = "\n") %>%
   fromJSON(simplifyVector = FALSE)
 geojson$style = list(
   stroke = TRUE,
   color = "#A3F",
   weight = 1,
   opacity = 1.0,
   fill = TRUE,
   fillColor = "#A3F",
   fillOpacity = 0.2
 )
 
 map$alleKreise <- geojson
 map %>% leaflet::addGeoJSON(map$alleKreise)
}

