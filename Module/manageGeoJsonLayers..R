removeAllGeometries <- function(map){
  #  leaflet::removeGeoJSON(map, )
  #  leaflet::clearShapes()
  #  leaflet::
}
addKreis <- function(map, kreisschluessel, daten, style, mouseOverEffekt){
}
addAlleKreise <- function(map){
  
}
removeKreis <- function(map){
}
highlightKreis <- function(map, kreisschluessel, daten, style){
}

filterKreise <- function(map){
  
}

highlightMouseOVerEinAus <- function(map, layerID="kreise", einAus="ein"){
  highlightOptions<-NULL
  if (einAus=="ein"){
    highlightOptions(
      stroke = NULL,
      color = NULL,
      weight = NULL,
      opacity = NULL,
      fill = NULL,
      fillColor = NULL,
      fillOpacity = NULL,
      dashArray = NULL,
      bringToFront = NULL,
      sendToBack = NULL
    )
  } else {
    highlightOptions(
      stroke = TRUE,
      color = "#A3F",
      weight = 1,
      opacity = 1.0,
      fill = TRUE,
      fillColor = "#A3F",
      fillOpacity = 0.2
    )
  }
  
}
