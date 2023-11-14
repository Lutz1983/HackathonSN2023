
# 

mod_ui_quiz <- function(id) {
  tagList(
    h1("Quiz")
  )
}

mod_server_quiz <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}

# mod_ui_quiz("quiz")
# mod_server_quiz("quiz")