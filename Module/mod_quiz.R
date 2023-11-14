
# 

mod_ui_quiz <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput("quizcontrol")
  )
}

mod_server_quiz <- function(id, start) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$start,
    output$quizcontrol <- renderUI({
      h1("gestartet...")
    })
    )
  })
}

# mod_ui_quiz("quiz")
# mod_server_quiz("quiz")