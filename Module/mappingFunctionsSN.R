mapping() %>% addKreise()



addKreise <- function(map, data){
  library(geojsonR)
  library(jsonlite)
  #getwd()
  res <- Dump_From_GeoJson("./kreise_grob_Test1.geojson")
  #utl <- geojsonR::TO_GeoJson$new()
  res2 <- geojsonR::FROM_GeoJson("./kreise_grob.geojson")
  geojson <- jsonlite::fromJSON(res)
  #map %>% addGeoJSON(geojson)
  map %>% #addGeoJSON(geojson)
  addGeoJSON(
    map = map, 
    geojson = geojson,
    layerId = 'kreise',
    group = NULL,
    stroke = TRUE,
    color = "#03F",
    weight = 5,
    opacity = 0.5,
    fill = TRUE,
    fillColor = "#03F",
    fillOpacity = 0.2,
    dashArray = NULL,
    smoothFactor = 1
  )
}


mapping <- function() {
  ##Map erzeugen
  library(leaflet)
  library(leafem)
  # WMS-Dienste:
 wms_grb <- "https://geodienste.sachsen.de/wmts_geosn_webatlas-sn/guest"
 wms_webatlas <- "https://geodienste.sachsen.de/wms_geosn_webatlas-sn/guest"
 wms_verwaltung <- "https://geodienste.sachsen.de/wms_geosn_verwaltungseinheiten/guest"
 wms_ortho <- "https://geodienste.sachsen.de/wms_geosn_dop-rgb/guest?"
 
 #min-width:320px; min-height: 230px;
 map <- leaflet(options = leafletOptions(
#   zoomControl = FALSE,
#   boxZoom = FALSE,
#   doubleClickZoom = FALSE,
#   dragging = FALSE,
#   keyboard = FALSE,
#   scrollWheelZoom = FALSE,
#   touchZoom = FALSE
   zoomControl = TRUE,
   boxZoom = TRUE,
   doubleClickZoom = TRUE,
   dragging = TRUE,
   keyboard = TRUE,
   scrollWheelZoom = TRUE,
   touchZoom = TRUE
   
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
   ) %>% 
   addWMSTiles(
     wms_verwaltung,
     layers = c("Landkreis_Kreisfreie_Stadt"
     ),
     options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>% fitBounds(11.768364, 50.129238, 15.043435, 51.726628
    ) %>% addMouseCoordinates()
 #   Ausdehnung
 #   11.7683640000000000,50.1292389999999983 : 15.0434350000000006,51.7266279999999981

 
 map
 }

