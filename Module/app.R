library(shiny)
library(tidyverse)


# Datensatz? ----

Fragen <- tibble(

)


df <- tibble(
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
            label = "Wähle dein Bundesland aus!",
            choices = c("Sachsen", "Bayern"),
            selected = character(0)
          ),
          br(),
          selectInput(
            inputId = "thema",
            label = "Wähle dein Thema",
            choices = c("A", "B"),
            selected = character(0),
            multiple = TRUE
          ),
          sliderInput(
            inputId = "anzahlfragen",
            label = "Wähle die Anzahl der Fragen:",
            min = 1,
            max = 4, # TODO nrow(unique(df$StatistiK_code))
            value = floor(mean(c(4,1)))
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
      })
    }
  })

  observeEvent(
    input$starten,
    gestartet(TRUE)
  )

# Spielende ----  
  observe({
    if (i() == 5) { # hardcoded Anzahl Fragen
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
  
  
  # Plotauswertung ----
  observeEvent(input$richtigbutton, {
    Ergebnis(Ergebnis() + 1)
    

    output$Chart <- renderUI({
      # TODO passenden Chart anzeigen
      tagList(
        fluidRow(p("Chart xx")),
        fluidRow(
          actionButton(
            inputId = "naechsteFrage",
            label = "Nächste Frage"
          )
        )
      )
    })

    removeModal()
  })

  observeEvent(input$falschbutton, {
    removeModal()
  })
  
  observeEvent(input$naechsteFrage,{
    i(i() + 1)
    output$Chart <- renderUI({
      #Leeren
    })
  })
  

  # Quiz-Controls ----
  observe({
    if (gestartet() == TRUE) {
      output$Controls <- renderUI({
        tagList(
          h1(df[zufall()[i()], ] %>% select(Frage) %>% pull()),
          radioButtons(
            inputId = "A1",
            label = "Wähle die richtig Antwort:",
            choices = df[zufall()[i()], ] %>%
              select(Antwort1, Antwort2, Antwort3) %>%
              pivot_longer(starts_with("Antwort"), values_to = "Antworten") %>%
              select("Antworten") %>% pull(),
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

  #Auswahl auswerten ----
  observeEvent(input$antworten, {
    if (
      df[zufall()[i()], ] %>% select(starts_with("Istrichtig") &
        ends_with(
          df[zufall()[i()], ] %>%
            select(Antwort1, Antwort2, Antwort3) %>%
            pivot_longer(starts_with("Antwort"), values_to = "Antworten") %>%
            filter(Antworten == req(input$A1)) %>%
            select(name) %>%
            pull() %>% str_sub(-1)
        )) %>% pull()) {
      showModal(richtig)
    } else {
      showModal(falsch)
    }
  })
}

shinyApp(ui, server)
