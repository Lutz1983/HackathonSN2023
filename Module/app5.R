library(shiny)
library(plotly)
library(tidyverse)
library(readxl)


# Datensatz ----

Fragen <- read_excel("Fragen_Quiz.xlsx")
df <- read_csv2("daten_quiz.csv")
df$Wert <- as.numeric(df$Wert)
Fragen <- Fragen %>% mutate(id = paste0(Statistik_Code, Wert_Code))
df <- df %>% mutate(id = paste0(Statistik_Code, Wert_Code))
df <- df %>% mutate(BL = str_sub(as.character(AGS), 1,2))

source("utils5.R")

# UI ----
ui <- fluidPage(
  titlePanel("REGIO-Quiz"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("Punktzahl"),
      uiOutput("Fragesidebar"),
      uiOutput("Neustart")
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
        label = "1. Wähle deine Map!",
        choices = c("Sachsen"),
        selected = character(0)
      ),
      br(),
      selectInput(
        inputId = "thema",
        label = "2. Wähle dein Thema:",
        choices = c(unique(Fragen$Thema)),
        selected = character(0),
        multiple = TRUE
      ),
      sliderInput(
        inputId = "anzahlfragen",
        label = "3. Wähle die Anzahl der Fragen:",
        min = 1,
        max = 5,
        value = 2,
        step = 1
      ),
      radioButtons(
        inputId = "anzahlauswahl",
        label = "4. Wähle den Schwierigkeitsgrad:",
        choices = c("Daten-Entdecker", "Analytischer Abenteurer", "Statistik-Superstar")
      ),
      actionButton(
        inputId = "starten",
        label = "Quiz starten!",
        class = "btn-lg btn-success"
      )
    )
  }

   # selectedfragen <- reactive({"12411BEVSTD__Bevoelkerungsstand__Anzahl"})
   # selectedfragen <- reactive({"12411BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre..."})
   
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
      
      output$Chart <- renderUI({})
      output$Fragesidebar <- renderUI({})
      output$Neustart <- renderUI({})
    }
  })


  observeEvent(
    input$starten, {
      req(input$thema)
    gestartet(TRUE)
      }
  )

  # Spielende ----

  # Function to create UI for the end of the game
  createEndGameUI <- function(rangtext) {
    tagList(
      h1("Herzlichen Glückwunsch!"),
      br(),
      tags$h3(rangtext),
      # p("Hier kannst du dein Ergebnis als Zertifikat herunterladen..."),
      # br(),
      # downloadButton(outputId = "downloadcertificate")
    )
  }

  # Refactored observe block
  observe({
    req(input$anzahlfragen)
    if (i() == input$anzahlfragen + 1) {
      output$Controls <- renderUI({
        if (Ergebnis() / (i()-1) >=.75) {
        rangtext <- "Sehr stark! Du bist ein Statistik-Visionär!"
        } else if (Ergebnis() / (i()-1) <=.25) {
          rangtext <- "Alles klar! Du bist Daten-Entdecker!"
        } else {
          rangtext <- "Sehr gut! Du bist ein Analytischer Stratege!"
          }
        
        createEndGameUI(rangtext)
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

  # Function to create the chart UI
  createBarChartUI <- function() {
    tagList(
      fluidRow(
        h2("Auswertung"),
        tags$i(fragezeile() %>% select(Info) %>% pull()) # Info zur Statistik
      ),
      fluidRow(
        selectInput(
          inputId = "Jahr",
          label = "Jahr auswählen",
          choices = df %>%
            filter(id == selectedfragen()[i()]) %>% 
            select(Jahr) %>%
            unique() %>%
            pull(),
          selected = df %>%
            filter(id == selectedfragen()[i()]) %>% 
            select(Jahr) %>%
            max() 
        ),
        selectInput(
          inputId = "Auspraegung",
          label = "Merkmalsausprägung auswählen:",
          choices = df %>%
            filter(id == selectedfragen()[i()]) %>%
            select(Merkmal) %>%
            unique() %>%
            pull(),
          selected = Fragen %>%
            filter(id == selectedfragen()[i()]) %>%
            select(Merkmal) %>%
            pull()
        ),
        output$p <- renderPlot(
          {
            ggplot(
              df %>%
                filter(id == selectedfragen()[i()], 
                       Jahr == input$Jahr, 
                       Merkmal == input$Auspraegung),
              aes(x = reorder(AGS_Label, Wert), 
                  y = Wert,
                  fill = ifelse(AGS_Label == richtigeAntwort(), "high", "Normal"))
            ) +
              geom_col() +
              scale_fill_manual("legend",
                                values = c("high" = "#63acbe", "Normal" = "#d3d3d3"))+
              guides(fill = FALSE)+
              theme_minimal() +
              coord_flip() +
              xlab("Kreise") +
              ylab(input$Auspraegung)
          },
          res = 96
        )
      ),
      fluidRow(
        column(6,
          offset = 6, align = "right",
          nextButton
        )
      )
    )
  }
  
  
  # Function to create the line chart UI
  createLineChartUI <- function() {
    tagList(
      fluidRow(
        h2("Auswertung"),
        tags$i(fragezeile() %>% select(Info) %>% pull()) # Info zur Statistik
      ),
      fluidRow(
        selectInput(
          inputId = "Auspraegung",
          label = "Merkmalsausprägung auswählen:",
          choices = df %>%
            filter(id == selectedfragen()[i()]) %>%
            select(Merkmal) %>%
            unique() %>%
            pull(),
          selected = Fragen %>%
            filter(id == selectedfragen()[i()]) %>%
            select(Merkmal) %>%
            pull()
        ),
        output$p <- renderPlotly(
          {
            ggplotly(ggplot(
              df %>%
                filter(id == selectedfragen()[i()],
                       Merkmal == input$Auspraegung,
                       AGS_Label %in% auswahlfragen())) +
              geom_line(aes(x = Jahr, 
                            y = Wert,
                            colour = AGS_Label)) +
              theme_minimal() +
              xlab("Jahr") +
              ylab(input$Auspraegung))
          }
        )
      ),
      fluidRow(
        column(6,
               offset = 6, align = "right",
               nextButton
        )
      )
    )
  }
  
  
  
  
  

  # Refactored observeEvent block
  observeEvent(tolistenbuttons(), {
    req(gestartet())
    req(fragezeile())
    if (gestartet() & fragezeile() %>% select(Grafik) %>% pull() == "Balkendiagramm") {
      output$Chart <- renderUI({
        createBarChartUI()
      })
    } else {
      output$Chart <- renderUI({createLineChartUI()})
    }
  })

  observeEvent(input$naechsteFrage, {
    i(i() + 1)
    output$Chart <- renderUI({
      # Leeren
    })
  })
  
  # Zur Auswertung update
  observeEvent(tolistenbuttons(), {
    req(gestartet())
    if (i() == input$anzahlfragen) {
      updateActionButton(session = session, 
                         inputId = "naechsteFrage", 
                         label = "Zur Auswertung!", 
                         icon = icon("arrow-right"))
    }
  })
  
  #Quizfragen auswählen
  merkmalsauspraegung <- reactive({
    Fragen %>%
      filter(id == selectedfragen()[i()]) %>%
      select(Merkmal) %>%
      pull()
  })

  richtigeAntwort <- reactive({
    req(merkmalsauspraegung())
    req(selectedfragen())
    req(i())
    req(fragezeile())
    
    if (fragezeile() %>% 
        select("Antwort_min_max") %>% 
        pull() == "max") {
      maxfinden(df, selectedfragen()[i()], merkmalsauspraegung())
    } else{
      minfinden(df, selectedfragen()[i()], merkmalsauspraegung())
    }
  })

  falscheAntwort <- reactive({
    req(merkmalsauspraegung())
    falscheAntwortenziehen(df, selectedfragen()[i()], richtigeAntwort(), merkmalsauspraegung())
  })

  auswahlfragen <- reactive({
    req(input$anzahlauswahl)
    anz <- reactiveVal()
    if (input$anzahlauswahl == "Daten-Entdecker") {
      anz(2)
    } else if (input$anzahlauswahl == "Analytischer Abenteurer") {
      anz(3)
    } else {anz(5)}
    c(richtigeAntwort(), unique(falscheAntwort())[1:anz()]) %>% sample(replace = FALSE)
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
      
      output$Neustart <- renderUI({
        actionButton(inputId = "neustart", label = "Quiz neustarten")
      })
      
    }
  })

    observeEvent(input$neustart,{
      gestartet(FALSE)
      Ergebnis(0)
      i(1)
      })

  # Auswahl auswerten ----
  # Function to evaluate the selected answer and show the appropriate modal
  evaluateAnswer <- function() {
    req(input$A1)
    if (input$A1 == richtigeAntwort()) {
      showModal(richtig)
    } else {
      showModal(modalDialog(
        title = "Leider Falsch",
        paste0("Deine Antwort war leider falsch! Richtig wäre ", richtigeAntwort(),"."),
        footer = actionButton("falschbutton", "Ok", class = "btn btn-danger")
      ))
    }
  }

  # Refactored observeEvent block for answer evaluation
  observeEvent(input$antworten, {
    evaluateAnswer()
  })
}

shinyApp(ui, server)
