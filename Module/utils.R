# utils.R ----

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