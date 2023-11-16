library(leaflet)
library("shiny")

ui <- fluidPage(
  mainPanel(
    tags$h3('Karte', style = 'color:#337ab7;'),
    selectInput(
      inputId = "bundesland",
      label = "1. Wähle deine Map!",
      choices = c("Baden-Württemberg", "Berlin", "Brandenburg", "Freie Hansestadt Bremen", "Freie und Hansestadt Hamburg", "Freistaat Bayern", "Freistaat Sachsen", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"),
      selected = character(0)
    ),
    leafletOutput('Map', width=500, height=500)
  )
)
# Lade das GeoJSON-File
geojson <- st_as_sf(geojsonio::geojson_read("./Module/laender_deutschland.geojson", what = "sp"))
# add rownumber
geojson <- geojson %>% dplyr::mutate(id = row_number())


server <- function(input, output, session) {
  testmap <- function () {
    
    print(map)
    map.daten <- geojson
    # Erstelle eine Leaflet-Karte
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 11, lat = 50.87907, zoom = 6) %>%
      fitBounds(5.7, 47.329238, 15.043435, 54.426628)
    
    # Füge die Polygone hinzu
    hl_opts <- highlightOptions(
      color = "#CC0000", weight = 3, bringToFront = TRUE)
    map <- map %>%
      addPolygons(data = geojson, layerId = ~id, group = "laender", highlightOptions = hl_opts, stroke = TRUE, color = "#A3F", weight = 1,   opacity = 1.0,     fill = TRUE, fillColor = "#A3F",  fillOpacity = 0.2)
    
    map
  }
  karte <- testmap()
  output$Map <- renderLeaflet({
    karte
    })
  ########
  output$click_on_shape <- renderPrint({
    input$map_shape_click
  })
  
  rv <- reactiveValues()
  rv$selected <- NULL
  observeEvent(input$Map_click, {

    new_selected <- req(input$Map_shape_click)
    isolate(old_selected <- rv$selected)
    print("new_selected")
    print(new_selected)
    print("old_selected")
    print(old_selected)
    if (is.null(old_selected) || new_selected$.nonce != old_selected$.nonce) {
      print("validate(")
      shiny::validate(
           need(new_selected$group!="selection", message=FALSE)
      )
      rv$selected <- new_selected
      print("i")
      i <- which(geojson$id==new_selected$id)
      print(i)
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
      print("karte_filtered")
      print(karte_filtered)
      
    } else {
      rv$selected <- NULL
      leafletProxy("Map") %>%
        clearGroup("selection")
    }
  })
  
  ########
}


# Run the application 
shinyApp(ui = ui, server = server)