if ("shiny" %in% rownames(installed.packages()) == FALSE) (install.packages("shiny")); library("shiny");
if ("shinycssloaders" %in% rownames(installed.packages()) == FALSE) (install.packages("shinycssloaders")); library("shinycssloaders");
if ("shinyWidgets" %in% rownames(installed.packages()) == FALSE) (install.packages("shinyWidgets")); library("shinyWidgets");
if ("shinybusy" %in% rownames(installed.packages()) == FALSE) (install.packages("shinybusy")); library("shinybusy");
if ("shinyalert" %in% rownames(installed.packages()) == FALSE) (install.packages("shinyalert")); library("shinyalert");
if ("shinyjs" %in% rownames(installed.packages()) == FALSE) (install.packages("shinyjs")); library("shinyjs");
if ("DT" %in% rownames(installed.packages()) == FALSE) (install.packages("DT")); library("DT");
if ("leaflet" %in% rownames(installed.packages()) == FALSE) (install.packages("leaflet")); library("leaflet");
if ("geojsonR" %in% rownames(installed.packages()) == FALSE) (install.packages("geojsonR")); library("geojsonR");
if ("tidyr" %in% rownames(installed.packages()) == FALSE) (install.packages("tidyr")); library("tidyr");
if ("stringr" %in% rownames(installed.packages()) == FALSE) (install.packages("stringr")); library("stringr");
if ("dplyr" %in% rownames(installed.packages()) == FALSE) (install.packages("dplyr")); library("dplyr");
if ("sp" %in% rownames(installed.packages()) == FALSE) (install.packages("sp")); library("sp");
if ("sf" %in% rownames(installed.packages()) == FALSE) (install.packages("sf")); library("sf");
if ("geosphere" %in% rownames(installed.packages()) == FALSE) (install.packages("geosphere")); library("geosphere");
if ("geojsonio" %in% rownames(installed.packages()) == FALSE) (install.packages("geojsonio")); library("geojsonio");
if ("leaflet" %in% rownames(installed.packages()) == FALSE) (install.packages("leaflet")); library("leaflet");
if ("leafem" %in% rownames(installed.packages()) == FALSE) (install.packages("leafem")); library("leafem");
if ("geojsonR" %in% rownames(installed.packages()) == FALSE) (install.packages("geojsonR")); library("geojsonR");
if ("jsonlite" %in% rownames(installed.packages()) == FALSE) (install.packages("jsonlite")); library("jsonlite");


# source functions
source('./mappingFunctionsSN.R');
source('./manageGeoJsonLayers.R')


shinyUI <- fluidPage(
  mainPanel(
    tags$h3('Karte', style = 'color:#337ab7;'),
    selectInput(
      inputId = "bundesland",
      label = "1. Wähle deine Map!",
      choices = c("Baden-Württemberg", "Berlin", "Brandenburg", "Freie Hansestadt Bremen", "Freie und Hansestadt Hamburg", "Freistaat Bayern", "Freistaat Sachsen", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"),
      selected = character(0)
    ),
    fluidRow(
      column(
        tags$div("Minimale Größe"),
        leafletOutput('uiMapMin', width=320, height=250),
        fluidRow(
          column(
            actionButton(
              inputId = 'higlight_uiMapMin',
              label = 'Highlight ein/aus',
              style = 'background-color: lightblue;',
              placeholder = 'bitte Kreisschlüssel eingeben'
            ),width = 11
          ),
          column(
            textInput(
              inputId = 'higlight_uiMapMin',
              label = '',
              placeholder = 'bitte Kreisschlüssel eingeben'
            ),width = 12
          )
        ),
        width = 7
      ),
      column(
        tags$div("Minimale Größe (Kreise Deutschland)"),
        leafletOutput('uiMapMinDEKreise', width=250, height=250),
        width = 3
      )
      ,
      column(
        tags$div("Minimale Größe (Länder Deutschland)"),
        leafletOutput('uiMapMinDELaender', width=250, height=250),
        width = 3
      )
    ),
    fluidRow(
      column(
        tags$div("Mittlere Größe"),
        leafletOutput('uiMapMedium', width=640, height=500),
        width = 7
      ),
      column(
        tags$div("Mittlere Größe (Kreise Deutschland)"),
        leafletOutput('uiMapMediumDEKreise', width=400, height=500),
        width = 3
      )
      ,
      column(
        tags$div("Mittlere Größe (Länder Deutschland)"),
        leafletOutput('uiMapMediumDELaender', width=400, height=500),
        width = 3
      )
    )
  )
);

shinyServer<-function(input, output, session) {
  
  # options  
  options(
    DT.options = list(
      pageLength = 1,
      scrollX = FALSE,
      autoWidth = TRUE,
      rownames = FALSE
    )
  )
  
  # reactive values
  rv <- reactiveValues(
    mapSN = NULL,
    mapDEKreise = NULL,
    mapDELaender = NULL,
    uiMapMin = NULL,
    leafletProxy = NULL
  )  
  rv$mapSN <- basiskarte()
  rv$mapDEKreise <- basiskarteDEKreise()
  rv$mapDELaender <- basiskarteDELaender()
  reactive
  ({
    output$Controls <- renderUI
    ({#rv$uiMapMin
      tmpVar <- renderLeaflet(rv$mapSN)
      output$uiMapMin <- tmpVar
      output$uiMapMedium <- tmpVar
      #tmpVarDEKreise <- renderLeaflet(rv$mapDEKreise)
      tmpVarDELaender <- renderLeaflet(rv$mapDELaender)
      
      ##hier muss wahrscheinlich der LeafletProxy ran
      ## siehe https://stackoverflow.com/questions/32897064/zoom-leaflet-map-to-default-in-rshiny
      #setView(map=tmpVarDE,lng = 11, lat = 51.37907, zoom = 5)
      #output$uiMapMinDEKreise <- tmpVarDEKreise
      #output$uiMapMediumDEKreise <- tmpVarDEKreise
      
      output$uiMapMinDELaender <- tmpVarDELaender
      output$uiMapMediumDELaender <- tmpVarDELaender
      
      
    })
  })
} 
  # Run the application 
  shinyApp(ui = shinyUI, server = shinyServer)