




showMap <- function(map){
  map 
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
###########################
basiskarteDEKreise <- function() {
  ##Map erzeugen
  # WMS-Dienste:
  wms_webatlas <- "https://sgx.geodatenzentrum.de/wms_basemapde"
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
    
  )) %>% 
    setView(lng = 11, lat = 50.87907, zoom = 10) %>%
    addWMSTiles(
      wms_webatlas,
      layers = c("de_basemapde_web_raster_grau"
      ),
      options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>% fitBounds(5.7, 47.529238, 15.043435, 54.226628#)
    ) %>%  addMouseCoordinates()
  
  
  geojson <- readLines("./kreise_deutschland.geojson", warn = FALSE) %>%
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
  #  map
}
###########################
basiskarteDELaender <- function() {
  ##Map erzeugen
  # WMS-Dienste:
  wms_webatlas <- "https://sgx.geodatenzentrum.de/wms_basemapde"
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
    
  )) %>% 
    setView(lng = 11, lat = 50.87907, zoom = 10) %>%
    addWMSTiles(
      wms_webatlas,
      layers = c("de_basemapde_web_raster_grau"
      ),
      options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>% fitBounds(5.7, 47.529238, 15.043435, 54.226628#)
    ) %>%  addMouseCoordinates()
  
  
  geojson <- readLines("./laender_deutschland.geojson", warn = FALSE) %>%
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
  
  map$alleLaender <- geojson
  map %>% leaflet::addGeoJSON(map$alleLaender)
  #  map
}
testkarte <- basiskarteDELaender()
showMap(testkarte)

