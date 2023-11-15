# utils.R ----

maxfinden <- function(df, ident, merkmalsauspraegung) {
  df %>%
    filter(id == ident, Merkmal == merkmalsauspraegung) %>%
    filter(Jahr == max(Jahr)) %>% 
    filter(Wert == max(Wert)) %>%
    select(AGS_Label) %>%
    pull() 
}

minfinden <- function(df, ident, merkmalsauspraegung) {
  df %>%
    filter(id == ident, Merkmal == merkmalsauspraegung) %>%
    filter(Jahr == max(Jahr)) %>% 
    filter(Wert == min(Wert)) %>%
    select(AGS_Label) %>%
    pull() 
}

falscheAntwortenziehen <- function(df, ident, richtigeAntwort, merkmalsauspraegung) {
  df %>%
    filter(
      id == ident,
      Merkmal == merkmalsauspraegung,
      AGS_Label != richtigeAntwort
    ) %>% 
    filter(Jahr == max(Jahr)) %>%
    select(AGS_Label) %>%
    pull()
}

fragenziehen <- function(df, themen, anzahl) {
  df %>%
    filter(Thema %in% themen) %>%
    select(id) %>%
    pull() %>%
    sample(anzahl, replace = FALSE) 
}

richtig <- modalDialog(
  title = "Korrekt",
  "Deine Antwort ist richtig!",
  footer = actionButton("richtigbutton", "Ok", class = "btn btn-success")
)

nextButton <- actionButton(
  inputId = "naechsteFrage",
  label = "Nächste Frage",
  icon = icon("arrow-right"),
  class = "btn-info"
)