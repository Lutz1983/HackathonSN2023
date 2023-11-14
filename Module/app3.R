library(shiny)
library(tidyverse)
library(readxl)

# Datensatz ----

Fragen <- read_excel("Fragen_Quiz.xlsx")
df <- read_csv2("daten_quiz.csv")

source("utils3.R")

# UI ----
ui <- fluidPage(
  titlePanel("REGIO-Quiz"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("Punktzahl"),
      uiOutput("Fragesidebar")
    ),
    mainPanel(
      fluidRow(
        p("-Quizbereich-"),
        uiOutput("Controls")
      ),
      fluidRow(
        p("-Auswertung-"),
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
        value = 2 
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
        #test
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
    if (i() == input$anzahlfragen + 1) { # hardcoded Anzahl Fragen
      output$Controls <- renderUI({
        createEndGameUI()
      })
      
      output$Chart <- renderUI({
        #NULL
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
  
  # Function to create the chart UI
  createChartUI <- function() {
    tagList(
      fluidRow(
        tags$i(fragezeile() %>% select(Info) %>% pull()) # Info zur Statistik
      ),
      fluidRow(
        selectInput(
          inputId = "Jahr",
          label = "Jahr auswählen", 
          choices = df %>% 
            filter(Statistik_Code == selectedfragen()[i()], Merkmal == "Insgesamt") %>% 
            select(Jahr) %>% 
            unique() %>% 
            pull()
        ),
        output$p <- renderPlot({
          ggplot(
            df %>% 
              filter(Statistik_Code == selectedfragen()[i()], Jahr == input$Jahr, Merkmal == "Insgesamt"),
            aes(x = reorder(AGS_Label, desc(Anzahl)), y = Anzahl) 
          ) + 
            geom_col() + 
            theme_bw()+
            xlab("Kreise") +
            ylab("Merkmal")
        }, res = 96)
      ),
      fluidRow(
        actionButton(
          inputId = "naechsteFrage",
          label = "Nächste Frage"
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
      filter(Statistik_Code == selectedfragen()[i()]) %>% 
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
    Fragen %>% filter(Statistik_Code == selectedfragen()[i()])
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
      actionButton("antworten", "Auswählen", class = "btn-lg btn-success")
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
