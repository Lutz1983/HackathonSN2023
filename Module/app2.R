library(shiny)
library(tidyverse)
library(readxl)


# Datensatz ----

Fragen <- read_excel("Fragen_Quiz.xlsx")

df <- tibble(
  Statistik_Code = c(11111, 11111, 11111, 31111, 31111, 31111),
  Zeit = c(2021, 2021, 2021, 2021, 2021, 2021),
  AGS_Code = c(14730, 14625, 14666, 14730, 14625, 14666),
  AGS_Label = c("Nordsachsen", "Vogtland", "Dresden", "Nordsachsen", "Vogtland", "Dresden"),
  Zwei_Code = c(NA, NA, NA, NA, NA, NA),
  Wert = c(1, 2, 3, 4, 5, 6)
)


dfalt <- tibble(
  Bundesland = c("Sachsen", "Sachsen", "Sachsen", "Sachsen"),
  Frage = c(
    "Wie heißt die größte Stadt Sachsens?",
    "Wie heißt der flächenmäßig größte Landkreis Sachsens?",
    "Wie heißt die Landeshauptstadt Sachsens?",
    "Wie heißt die östlichste Stadt Deutschlands?"
  ),
  Antwort1 = c("Leipzig, kreisfreie Stadt", "Bautzen", "Plauen", "Hannover"),
  Antwort2 = c("Dresden, Landeshauptstadt", "Mittelsachsen", "Dresden", "Görlitz"),
  Antwort3 = c("Zwickau", "Leipzig, Land", "Chemnitz", "Meißen"),
  Istrichtig1 = c(TRUE, TRUE, FALSE, FALSE),
  Istrichtig2 = c(FALSE, FALSE, TRUE, TRUE),
  Istrichtig3 = c(FALSE, FALSE, FALSE, FALSE)
)


# TODO in utils.R verschieben
# Funktionen -----
ziehen <- function(df) {
  sample(seq(nrow(df)), nrow(df), replace = FALSE)
}

maxfinden <- function(df, Statistikcode) {
  df %>%
    filter(Statistik_Code == Statistikcode, Zeit == max(Zeit), is.na(Zwei_Code)) %>%
    filter(Wert == max(Wert)) %>%
    select(AGS_Label) %>%
    pull() %>%
    return()
}

minfinden <- function(df, Statistikcode) {
  df %>%
    filter(Statistik_Code == Statistikcode, Zeit == max(Zeit), is.na(Zwei_Code)) %>%
    filter(Wert == min(Wert)) %>%
    select(AGS_Label) %>%
    pull() %>%
    return()
}

falscheAntwortenziehen <- function(df, Statistikcode, richtigeAntwort) {
  df %>%
    filter(
      Statistik_Code == Statistikcode,
      Zeit == max(Zeit),
      is.na(Zwei_Code),
      AGS_Label != richtigeAntwort
    ) %>%
    select(AGS_Label) %>%
    pull()
}

fragenziehen <- function(df, themen, anzahl) {
  df %>%
    filter(Thema %in% themen) %>%
    select(Statistik_Code) %>%
    pull() %>%
    sample(anzahl, replace = FALSE) %>%
    return()
}



richtig <- modalDialog(
  title = "Korrekt",
  "Deine Antwort ist richtig!",
  footer = actionButton("richtigbutton", "Ok", class = "btn btn-success")
)

falsch <- modalDialog(
  title = "Leider Falsch",
  "Deine Antwort war leider falsch!",
  footer = actionButton("falschbutton", "Ok", class = "btn btn-danger")
)


# UI ----
ui <- fluidPage(
  titlePanel("REGIO-Quiz"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("Punktzahl")
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

  zufall <- reactiveVal()
  zufall(ziehen(df))

  i <- reactiveVal(1)


  # Aufbau Start-Bildschirm ----
  observe({
    if (gestartet() == FALSE) {
      output$Controls <- renderUI({
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
            max = 4, # nrow(nrowfragen()), # TODO nrow(unique(df$StatistiK_code))
            value = 2 # floor(mean(c(nrowfragen(),1)))
          ),
          actionButton(
            inputId = "starten",
            label = "Quiz starten!",
            class = "btn-lg btn-success"
          )
        )
      })

      output$Punktzahl <- renderUI({
        p("Wähle aus und beginne das Quiz!")
      })

      output$Chart <- renderUI({
        # Kein Inhalt
        # test
        renderText(selectedfragen())
      })
    }
  })

  selectedfragen <- reactive({
    req(input$thema)
    req(input$anzahlfragen)
    fragenziehen(Fragen, input$thema, input$anzahlfragen)
  })

  observeEvent(
    input$starten,
    gestartet(TRUE)
  )

  # Spielende ----
  observe({
    req(input$anzahlfragen)
    if (i() == input$anzahlfragen + 1) { # hardcoded Anzahl Fragen
      output$Controls <- renderUI({
        tagList(
          h1("Spiel beendet!"),
          br(),
          p("Hier kannst du dein Ergebnis als Zertifikat herunterladen..."),
          br(),
          downloadButton(outputId = "downloadcertificate")
        )
      })
      output$Chart <- renderUI({
        # leer
      })
      showNotification("Spiel beendet!")

      # output$downloadcertificate <- downloadHandler(
      #   filename = paste0("certificate", Sys.Date(), ".pdf"),
      #   content = ,
      # )
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
  
  observeEvent(tolistenbuttons(), {
    if (gestartet() == TRUE) {
    output$Chart <- renderUI({
      # TODO passenden Chart anzeigen
      tagList(
        fluidRow(p(fragezeile() %>% select(Info) %>% pull())),
        fluidRow(
          actionButton(
            inputId = "naechsteFrage",
            label = "Nächste Frage"
          )
        )
      )
    })
    }
  })

  observeEvent(input$naechsteFrage, {
    i(i() + 1)
    output$Chart <- renderUI({
      # Leeren
    })
  })

  richtigeAntwort <- reactive(maxfinden(df, selectedfragen()[i()]))
  falscheAntwort <- reactive({
    falscheAntwortenziehen(df, selectedfragen()[i()], richtigeAntwort())
  })

  auswahlfragen <- reactive({
    c(richtigeAntwort(), falscheAntwort()[1:2]) %>% sample()
  })

  fragezeile <- reactive({
    req(selectedfragen())
    Fragen %>% filter(Statistik_Code == selectedfragen()[i()])
  })
  
  # Quiz-Controls ----
  observe({
    if (gestartet() == TRUE) {
      
      output$Controls <- renderUI({
        tagList(
          h1(fragezeile() %>% select(Frage) %>% pull()),
          radioButtons(
            inputId = "A1",
            label = "Wähle die richtig Antwort:",
            choices = auswahlfragen(),
            selected = character(0)
          ),
          br(),
          renderText(paste0("Antwort: ", req(input$A1), " wählen?")),
          br(),
          actionButton("antworten", "Auswählen", class = "btn-lg btn-success")
        )
      })

      output$Punktzahl <- renderUI({
        renderText(paste0("Punktzahl: ", Ergebnis()))
      })
    }
  })

  # Auswahl auswerten ----
  observeEvent(input$antworten, {
    if (input$A1 == richtigeAntwort()) {
      showModal(richtig)
    } else {
      showModal(falsch)
    }
  })
}

shinyApp(ui, server)
