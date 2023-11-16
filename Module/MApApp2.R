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
if ("htmltools" %in% rownames(installed.packages()) == FALSE) (install.packages("htmltools")); library("htmltools");


# source functions
source('./mappingFunctionsSN.R');
source('./manageGeoJsonLayers.R')

ui <- fluidPage(
  mainPanel(
    tags$h3('Karte', style = 'color:#337ab7;'),
    selectInput(
      inputId = "bundesland",
      label = "1. Wähle deine Map!",
      choices = c("Baden-Württemberg" = "08", "Berlin" = "11", "Brandenburg" = "12", 
                  "Freie Hansestadt Bremen" = "04", "Freie und Hansestadt Hamburg" = "02", 
                  "Freistaat Bayern" = "09", "Freistaat Sachsen" = "14", "Hessen" = "06",
                  "Mecklenburg-Vorpommern" = "13", "Niedersachsen" = "03", "Nordrhein-Westfalen" = "05",
                  "Rheinland-Pfalz" = "07", "Saarland" = "10", "Sachsen-Anhalt" = "15",
                  "Schleswig-Holstein" = "01", "Thüringen" = "16"),
      selected = character(0)
    ),
    
    ##Kartenelement - kann an beliebige Stelle in der GUI
    leafletOutput('Map', width=500, height=500),
    selectInput(
      inputId = "sn",
      label = "Kreisauswahl",
      choices = c("Landkreis Bautzen" = "14625", "Kreisfreie Stadt Chemnitz" = "14511", 
                  "Kreisfreie Stadt Dresden" = "14612", "Landkreis Erzgebirgskreis" = "14521", 
                  "Landkreis Görlitz" = "14626", "Landkreis Leipzig" = "14729", 
                  "Kreisfreie Stadt Leipzig" = "14713", "Landkreis Meißen" = "14627", 
                  "Landkreis Mittelsachsen" = "14522", "Landkreis Nordsachsen" = "14730", 
                  "Landkreis Sächsische Schweiz-Osterzgebirge" = "14628", 
                  "Landkreis Vogtlandkreis" = "14523", "Landkreis Zwickau" = "14524"),
      selected = character(0)
    ),
    
    ##Kartenelement - kann an beliebige Stelle in der GUI
    leafletOutput('MapSN', width=500, height=500)
  )
)

server <- function(input, output, session) {
  ##Begin Server-Code für Karte
  ######## 
  observeEvent(input$bundesland, manageHighlightLaenderSelect (input, rv))
  
  karte <- erzeugeKarteLaender()
  output$Map <- renderLeaflet({
    karte
    })
  output$click_on_shape <- renderPrint({
    input$map_shape_click
  })
  rv <- reactiveValues()
  observeEvent(input$Map_click, manageHighlightLaenderClick (input, rv, session))
  ##Ende 
  ########

  
  observeEvent(input$sn, manageHighlightSNSelect (input, rv))
  karteSN <- erzeugeKarteSN()
  output$MapSN <- renderLeaflet({
    karteSN
  })
  output$click_on_shape <- renderPrint({
    input$MapSN_shape_click
  })
  rv <- reactiveValues()
  observeEvent(input$MapSN_click, manageHighlightSNClick (input, rv, session))
}


# Run the application 
shinyApp(ui = ui, server = server)