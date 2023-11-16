library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(plotly)
library(sf)

# Datensatz ----

Fragen <- read_excel("Fragen_Quiz.xlsx")
df <- read_csv2("daten_quiz.csv")
Fragen <- Fragen %>% mutate(id = paste0(Statistik_Code, Wert_Code))
df <- df %>% mutate(id = paste0(Statistik_Code, Wert_Code))

source("utils4.R")

# UI ----
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("REGIO-Quiz"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("Punktzahl"),
      uiOutput("Fragesidebar")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        uiOutput("Controls")
      ),
      fluidRow(
        uiOutput("Chart")
      )
    )
  )
)


server <- function(input, output, session) {
  gestartet <- reactiveVal()
  gestartet(FALSE)
  Ergebnis <- reactiveVal()
  Ergebnis(0)
  i <- reactiveVal(1)
  
  # Aufbau Start-Bildschirm ----
  createStartScreenUI <- function() {
    tagList(
      h1("Herzlich Willkommen!"),
      br(),
      selectInput(
        inputId = "bundesland",
        label = "1. Wähle dein Bundesland aus!",
        choices = c("Sachsen", "Bayern"),
        selected = character(0)
      ),
      br(),
      selectInput(
        inputId = "thema",
        label = "2. Wähle dein Thema",
        choices = c(unique(Fragen$Thema)),
        selected = character(0),
        multiple = TRUE
      ),
      sliderInput(
        inputId = "anzahlfragen",
        label = "3. Wähle die Anzahl der Fragen:",
        min = 1,
        max = 4,
        value = 2,
        step = 1
      ),
      actionButton(
        inputId = "starten",
        label = "Quiz starten!",
        class = "btn-lg btn-success"
      )
    )
  }
  
  selectedfragen <- reactive({
    req(input$thema)
    req(input$anzahlfragen)
    fragenziehen(Fragen, input$thema, input$anzahlfragen)
  })
  
  # Refactored observe block
  observe({
    if (!gestartet()) {
      output$Controls <- renderUI({
        createStartScreenUI()
      })
      
      output$Punktzahl <- renderUI({
        p("Wähle aus und beginne das Quiz!")
      })
      
      output$Chart <- renderUI({
        # test
        renderText(selectedfragen())
      })
    }
  })
  
  
  observeEvent(
    input$starten,
    gestartet(TRUE)
  )
  
  # Spielende ----
  
  # Function to create UI for the end of the game
  createEndGameUI <- function() {
    tagList(
      h1("Spiel beendet!"),
      br(),
      p("Hier kannst du dein Ergebnis als Zertifikat herunterladen..."),
      br(),
      downloadButton(outputId = "downloadcertificate")
    )
  }
  
  # Refactored observe block
  observe({
    req(input$anzahlfragen)
    if (i() == input$anzahlfragen + 1) {
      output$Controls <- renderUI({
        createEndGameUI()
      })
      
      output$Chart <- renderUI({
        # NULL
      })
      
      output$Fragesidebar <- renderUI({})
      
      showNotification("Spiel beendet!")
    }
  })
  
  
  # Richtig-Falsch ----
  observeEvent(input$richtigbutton, {
    Ergebnis(Ergebnis() + 1)
    removeModal()
  })
  
  observeEvent(input$falschbutton, {
    removeModal()
  })
  
  # Plotauswertung ----
  tolistenbuttons <- reactive(list(input$richtigbutton, input$falschbutton))

#### Karte ####  
  
  kartenmodell_kreise <- st_read("VG5000_KRS.shp")
  kartenmodell_bundeslaender <- st_read("VG5000_LAN.shp")
  
  kartenmodell_kreise_sachsen <- kartenmodell_kreise[c(352:364), c("AGS", "GEN", "geometry")]
  kartenmodell_bundeslaender_sachsen <- kartenmodell_bundeslaender[c(14), c("AGS", "GEN", "geometry")]
  
  kartenmodell_AGS_Label <- data.frame(
    AGS = as.character(c(unique(df$AGS))),
    AGS_Label = c(unique(df$AGS_Label))
    )
  
  kartenmodell_kreise_sachsen <- 
    full_join(kartenmodell_kreise_sachsen, kartenmodell_AGS_Label)
    
  # Function to create the chart UI
  createChartUI <- function() {
    tagList(
      fluidRow(
        tags$i(fragezeile() %>% select(Info) %>% pull()) # Info zur Statistik
      ),
      fluidRow(
        # selectInput(
        #   inputId = "Jahr",
        #   label = "Jahr auswählen",
        #   choices = df %>%
        #     filter(id == selectedfragen()[i()]) %>% 
        #     select(Jahr) %>%
        #     unique() %>%
        #     pull()
        # ),

        output$p <- renderPlot(
          {
            data_karte_kreis_auswahl <- reactive({
              kartenmodell_kreise_sachsen %>%
                filter(AGS_Label == richtigeAntwort()) 
            })
            
            ggplot() +
              # Kreise
              geom_sf(
                data = kartenmodell_kreise_sachsen,
                aes(text = paste(GEN)),
                fill = NA,
                col = "#c7c8c9"
              ) +
              #ausgewählter Landkreis
              geom_sf(
                data = data_karte_kreis_auswahl(),
                fill = "#63acbe",
                col = "#c7c8c9",
                size = 5
              ) +
              # Umrandung mit sachsenkarte
              geom_sf(
                data = kartenmodell_bundeslaender_sachsen, # Umrandung Karte
                fill = NA,
                col = "#9E9FA1",
                size = 5
              ) +
              theme(
                panel.background = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.title = element_text(size = 7),
                legend.text = element_text(size = 7)
              ) +
              labs(caption = "© GeoBasis-DE / BKG (2023)")
          }
        )
      ),
      fluidRow(
        column(6,
               offset = 6, align = "right",
               actionButton(
                 inputId = "naechsteFrage",
                 label = "Nächste Frage",
                 icon = icon("arrow-right"),
                 class = "btn-info"
               )
        )
      )
    )
  }
  
  # Refactored observeEvent block
  observeEvent(tolistenbuttons(), {
    if (gestartet()) {
      output$Chart <- renderUI({
        createChartUI()
      })
    }
  })
  
  observeEvent(input$naechsteFrage, {
    i(i() + 1)
    output$Chart <- renderUI({
      # Leeren
    })
  })
  
  merkmalsauspraegung <- reactive({
    Fragen %>%
      filter(id == selectedfragen()[i()]) %>%
      select(Merkmal) %>%
      pull()
  })
  
  richtigeAntwort <- reactive({
    req(merkmalsauspraegung())
    maxfinden(df, selectedfragen()[i()], merkmalsauspraegung())
  })
  
  falscheAntwort <- reactive({
    req(merkmalsauspraegung())
    falscheAntwortenziehen(df, selectedfragen()[i()], richtigeAntwort(), merkmalsauspraegung())
  })
  
  auswahlfragen <- reactive({
    c(richtigeAntwort(), falscheAntwort()[1:2]) %>% sample()
  })
  
  fragezeile <- reactive({
    req(selectedfragen())
    Fragen %>% filter(id == selectedfragen()[i()])
  })
  
  # Quiz-Controls ----
  # Function to create the quiz controls UI
  createQuizControlsUI <- function() {
    tagList(
      h1(fragezeile() %>% select(Frage) %>% pull()),
      radioButtons(
        inputId = "A1",
        label = "Wähle die richtige Antwort:",
        choices = auswahlfragen(),
        selected = character(0)
      ),
      br(),
      renderText(paste0("Antwort: ", req(input$A1), " wählen?")),
      br(),
      actionButton("antworten", "Auswählen", class = "btn-lg btn-primary")
    )
  }
  
  # Refactored observe block for quiz controls
  observe({
    if (gestartet()) {
      output$Controls <- renderUI({
        createQuizControlsUI()
      })
      
      output$Punktzahl <- renderUI({
        renderText(paste0("Punktzahl: ", Ergebnis()))
      })
      
      output$Fragesidebar <- renderUI({
        renderText(paste0("Frage ", i(), " von ", length(selectedfragen())))
      })
    }
  })
  
  
  # Auswahl auswerten ----
  # Function to evaluate the selected answer and show the appropriate modal
  evaluateAnswer <- function() {
    req(input$A1)
    if (input$A1 == richtigeAntwort()) {
      showModal(richtig)
    } else {
      showModal(falsch)
    }
  }
  
  # Refactored observeEvent block for answer evaluation
  observeEvent(input$antworten, {
    evaluateAnswer()
  })
}

shinyApp(ui, server)