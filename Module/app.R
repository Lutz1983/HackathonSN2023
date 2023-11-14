library(shiny)

quizApp <- function() {

ui <- fluidPage(
  mod_ui_quiz("quiz")
)

server <- function(input, output, session) {
  mod_server_quiz("quiz")
}

shinyApp(ui, server)

}

#Ausführen
quizApp()
