




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
# basiskarteDELaender <- function() {
#   ##Map erzeugen
#   # WMS-Dienste:
#   wms_webatlas <- "https://sgx.geodatenzentrum.de/wms_basemapde"
# 
#     map <- leaflet(options = leafletOptions(
#     zoomControl = FALSE,
#     boxZoom = FALSE,
#     doubleClickZoom = FALSE,
#     dragging = FALSE,
#     keyboard = FALSE,
#     scrollWheelZoom = TRUE,
#     touchZoom = FALSE
#     
#   )) %>% 
#     setView(lng = 11, lat = 50.87907, zoom = 10
#     ) %>% addWMSTiles(
#       wms_webatlas,
#       layers = c("de_basemapde_web_raster_grau"
#       ),
#       options = WMSTileOptions(format = "image/png", transparent = TRUE)
#     ) %>% fitBounds(5.7, 47.529238, 15.043435, 54.226628#)
#     ) %>%  addMouseCoordinates()
#   
#   
#     # Lade das GeoJSON-File
#     geojson <- geojsonio::geojson_read("./laender_deutschland.geojson", what = "sp")
# 
#   
#   map$alleLaender <- geojson
#   map %>% addPolygons(data = geojson, stroke = TRUE, color = "#A3F", weight = 1,   opacity = 1.0,     fill = TRUE, fillColor = "#A3F",  fillOpacity = 0.2)
# 
# 
#  map
# }
erzeugeKarteLaender <- function () {
  # Lade das GeoJSON-File
  geojson <- st_as_sf(geojsonio::geojson_read("./laender_deutschland.geojson", what = "sp"))
  # add rownumber
  geojson <- geojson %>% dplyr::mutate(id = row_number())
  
  map.daten <- geojson
  # Erstelle eine Leaflet-Karte
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 11, lat = 50.87907, zoom = 6) %>%
    fitBounds(5.7, 47.329238, 15.043435, 54.426628)
  
  # Definiere Highlight-Eigenschaften und füge die Polygone hinzu
  hl_opts <- highlightOptions(
    color = "#CC0000", weight = 3, bringToFront = TRUE)
  map <- map %>%
    addPolygons(data = geojson, layerId = ~id, group = "laender", highlightOptions = hl_opts, stroke = TRUE, color = "#A3F", weight = 1,   opacity = 1.0,     fill = TRUE, fillColor = "#A3F",  fillOpacity = 0.2)
  
  map
}
manageHighlightLaenderClick <- function (input, rv, session){
  rv$selected <- NULL
  new_selected <- req(input$Map_shape_click)
  isolate(old_selected <- rv$selected)
  if (is.null(old_selected) || new_selected$.nonce != old_selected$.nonce) {
    shiny::validate(
      need(new_selected$group!="selection", message=FALSE)
    )
    rv$selected <- new_selected
    i <- which(geojson$id==new_selected$id)
    karte_filtered <- geojson[i,]
    leafletProxy("Map") %>%
      clearGroup("selection") %>%
      addPolygons(
        group = "selection",
        data = karte_filtered,
        fillColor = "cyan",
        weight = 1.2,
        color = "#666666",
        opacity = 0.4,
        fillOpacity = 0.8)
    updateSelectInput(session, "bundesland", selected = karte_filtered$Land)
  } else {
    rv$selected <- NULL
    leafletProxy("Map") %>%
      clearGroup("selection")
  }
}
manageHighlightLaenderSelect <- function (input, rv){
  rv$selected <- NULL
  print(input$bundesland)
  new_selected <- filter(geojson, Land ==req(input$bundesland))
  #print(new_selected)
  isolate(old_selected <- rv$selected)
  if (is.null(old_selected) || new_selected$.Land != old_selected$Land) {
    print("validate")
    #    shiny::validate(
    #      need(new_selected$group!="selection", message=FALSE)
    #    )
    rv$selected <- new_selected
    i <- which(geojson$id==new_selected$id)
    print(i)
    karte_filtered <- new_selected
    leafletProxy("Map") %>%
      clearGroup("selection") %>%
      addPolygons(
        group = "selection",
        data = karte_filtered,
        fillColor = "cyan",
        weight = 1.2,
        color = "#666666",
        opacity = 0.4,
        fillOpacity = 0.8)
  } else {
    print("else")
    rv$selected <- NULL
    leafletProxy("Map") %>%
      clearGroup("selection")
  }
}
testkarte <- basiskarteDELaender()
showMap(testkarte)

