library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
library(shinyWidgets)
library(shinyjs)

#Packages leaftlet
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



# Datensatz ----

Fragen <- read_excel("Fragen_Quiz.xlsx")
df <- read_csv2("daten_quiz.csv")
df$Wert <- as.numeric(df$Wert)
Fragen <- Fragen %>% mutate(id = paste0(Statistik_Code, Wert_Code))
df <- df %>% mutate(id = paste0(Statistik_Code, Wert_Code))
df <- df %>% mutate(BL = str_sub(as.character(AGS), 1, 2))

source("utils5.R")
source('mappingFunctionsSN.R')

# UI ----
ui <- fluidPage(
  useShinyjs(),
  setBackgroundColor("#ADD8E6"),
  titlePanel(h1("RegioChallenge", style = "background-color:coral;"),
             windowTitle = "RegioChallenge"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("Pre-alpha version 0.0.14"),
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
  
  createleafletUI <- function() {
    leafletOutput('Map', width=500, height=500)
  }
  
  createStartScreenUI <- function() {
    tagList(
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
      createleafletUI(),
      br(),
      selectInput(
        inputId = "thema",
        label = "2. Wähle dein Themengebiet:",
        choices = c(unique(Fragen$Thema)),
        selected = character(0),
        multiple = TRUE
      ),
      sliderInput(
        inputId = "anzahlfragen",
        label = "3. Wähle die Anzahl der Level:",
        min = 1,
        max = 5,
        value = 2,
        step = 1
      ),
      radioButtons(
        inputId = "anzahlauswahl",
        label = "4. Wähle deinen Schwierigkeitsgrad:",
        choices = c("Daten-Entdecker", "Analytischer Abenteurer", "Statistik-Superstar")
      ),
      actionButton(
        inputId = "starten",
        label = "Quiz starten!",
        class = "btn-lg btn-success"
      )
    )
  }

  # selectedfragen <- reactive({c("61511BAU004__Durchschnittlicher_Kaufwert_je_qm__EUR",
  #                               "21111BIL013__Allgemeinbildende_Schulen__Anzahl",
  #   "45412GAST04__Geoeffnete_Beherbergungsbetriebe__Anzahl",
  #   "33111FLC005__Bodenflaeche__ha")})
  
  #randomisiert
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
      
      # output$Chart <- renderUI({
      #   createleafletUI()
      # })
      output$Fragesidebar <- renderUI({})
      output$Neustart <- renderUI({})
    }
  })

  observeEvent(
    input$starten,
    {
      req(input$thema)
      gestartet(TRUE)
    }
  )

  # Spielende ----

  # Function to create UI for the end of the game
  createEndGameUI <- function(rangtext) {
    tagList(
      h1("Super Leistung!"),
      br(),
      tags$h3(rangtext)
    )
  }

  # Refactored observe block
  observe({
    req(input$anzahlfragen)
    if (i() == input$anzahlfragen + 1) {
      output$Controls <- renderUI({
        if (Ergebnis() / (i() - 1) >= .75) {
          rangtext <- "Sehr stark! Du bist ein Statistik-Visionär!"
        } else if (Ergebnis() / (i() - 1) <= .25) {
          rangtext <- "Alles klar! Du bist Zahlen-Entdecker!"
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
            req(input$Auspraegung)
            req(input$Jahr)
            if (df %>%
                filter(
                  id == selectedfragen()[i()],
                  Jahr == input$Jahr,
                  Merkmal == input$Auspraegung
                ) %>% nrow() > 0) {
            ggplot(
              df %>%
                filter(
                  id == selectedfragen()[i()],
                  Jahr == input$Jahr,
                  Merkmal == input$Auspraegung
                ),
              aes(
                x = reorder(AGS_Label, Wert),
                y = Wert,
                fill = ifelse(AGS_Label == richtigeAntwort(), "High", "Normal")
              )
            ) +
              geom_col() +
              scale_fill_manual("legend",
                values = c("High" = "#FF7F50", "Normal" = "#d3d3d3")
              ) +
              guides(fill = FALSE) +
              theme_minimal() +
                theme(plot.background = element_rect(fill = "#daedf4"),
                      panel.grid = element_blank(),
                      axis.title.y = element_blank()) +
              coord_flip() +
              ylab(input$Auspraegung) }
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
         
        output$p <- renderPlotly({
          req(input$Auspraegung)
          if(df %>%
             filter(
               id == selectedfragen()[i()],
               Merkmal == input$Auspraegung,
               AGS_Label %in% auswahlfragen()
             ) %>% nrow()> 0) {
          ggplotly(ggplot(
            df %>%
              filter(
                id == selectedfragen()[i()],
                Merkmal == input$Auspraegung,
                AGS_Label %in% auswahlfragen()
              )
          ) +
            geom_line(aes(
              x = Jahr,
              y = Wert,
              colour = AGS_Label
            )) +
            theme_minimal() +
            theme(plot.background = element_rect(fill = "#daedf4"),
                  panel.background = element_rect(fill = "#f0f7fc")) +
            xlab("Jahr") +
            ylab(input$Auspraegung))}
        }) 
      ),
      fluidRow(
        column(6,
          offset = 6, align = "right",
          nextButton
        )
      )
    )
  }

  # Function to create the pie chart UI
  createPieChartUI <- function() {
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
            pull()
        ),
        output$p <- renderPlotly({
          req(input$Jahr)
          df_Wert <- reactive({
            df %>%
              filter(
                id == selectedfragen()[i()],
                Jahr == input$Jahr
              ) %>%
              select(Wert) %>%
              deframe()
          })
          df_AGS_Label <- reactive({
            df %>%
              filter(
                id == selectedfragen()[i()],
                Jahr == input$Jahr
              ) %>%
              select(AGS_Label) %>%
              deframe()
          })
          if (df_AGS_Label() %>% length() > 0){
          plot_ly(
            data = df %>%
              filter(
                id == selectedfragen()[i()],
                Jahr == input$Jahr
              ),
            labels = reorder(df_AGS_Label(), df_Wert()),
            values = df_Wert(),
            type = "pie",
            hole = 0.6,
            showlegend = FALSE,
            marker = list(colors = ifelse(
              df_AGS_Label() == richtigeAntwort(),
              "#FF7F50",
              "#D3D3D3"
            )),
            textposition = "inside"
          ) %>% layout(plot_bgcolor = "#daedf4",
                       paper_bgcolor ="#daedf4")}
        })
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
  observeEvent(input$antworten, ignoreInit = TRUE, {
    req(gestartet())
    req(fragezeile())
    req(input$A1)
      if (gestartet() & fragezeile() %>%
        select(Grafik) %>%
        pull() == "Balkendiagramm") {
        output$Chart <- renderUI({
          createBarChartUI()
        })
      } else if (gestartet() & fragezeile() %>%
        select(Grafik) %>%
        pull() == "Liniendiagramm" ) {
        output$Chart <- renderUI({
          createLineChartUI()
        })
      } else {
        output$Chart <- renderUI({
          createPieChartUI()
        })
      }
  
  })

  observeEvent(input$naechsteFrage, {
    i(i() + 1)
    output$Chart <- renderUI({return(NULL)})
    output$p <- renderUI({return(NULL)})
  })

  # Zur Auswertung update
  observeEvent(tolistenbuttons(), {
    req(gestartet())
    if (i() == input$anzahlfragen) {
      updateActionButton(
        session = session,
        inputId = "naechsteFrage",
        label = "Zur Auswertung!",
        icon = icon("arrow-right")
      )
    }
  })

  # Quizfragen auswählen
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
    } else {
      minfinden(df, selectedfragen()[i()], merkmalsauspraegung())
    }
  })

  falscheAntwort <- reactive({
    req(merkmalsauspraegung())
    falscheAntwortenziehen(
      df,
      selectedfragen()[i()],
      richtigeAntwort(),
      merkmalsauspraegung()
    )
  })

  auswahlfragen <- reactive({
    req(input$anzahlauswahl)
    anz <- reactiveVal()
    if (input$anzahlauswahl == "Daten-Entdecker") {
      anz(2)
    } else if (input$anzahlauswahl == "Analytischer Abenteurer") {
      anz(3)
    } else {
      anz(5)
    }
    c(c(unique(falscheAntwort()) %>% sample(replace = FALSE))[1:anz()], richtigeAntwort())
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
      output$Chart <- renderUI({})

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

  observeEvent(input$neustart, {
    gestartet(FALSE)
    Ergebnis(0)
    i(1)
  })

  # Auswahl auswerten ----
  # Function to evaluate the selected answer and show the appropriate modal
  evaluateAnswer <- function() {
    req(input$A1)
    shinyjs::hide("antworten")
    if (input$A1 == richtigeAntwort()) {
      showModal(richtig)
    } else {
      showModal(modalDialog(
        title = "Leider Falsch",
        paste0("Deine Antwort war leider falsch! Richtig wäre ", richtigeAntwort(), "."),
        footer = actionButton("falschbutton", "Ok", class = "btn btn-danger")
      ))
    }
  }

  observeEvent(input$antworten, {
    evaluateAnswer()
  })
}

shinyApp(ui, server)
