# utils.R ----

maxfinden <- function(df, Statistikcode, merkmalsauspraegung) {
  df %>%
    filter(Statistik_Code == Statistikcode, Merkmal == merkmalsauspraegung) %>%
    filter(Jahr == max(Jahr)) %>% 
    filter(Wert == max(Wert)) %>%
    select(AGS_Label) %>%
    pull() %>%
    return()
}

minfinden <- function(df, Statistikcode, merkmalsauspraegung) {
  df %>%
    filter(Statistik_Code == Statistikcode, Jahr == max(Jahr), Merkmal == merkmalsauspraegung) %>%
    filter(Jahr == max(Jahr)) %>% 
    filter(Wert == min(Wert)) %>%
    select(AGS_Label) %>%
    pull() %>%
    return()
}

falscheAntwortenziehen <- function(df, Statistikcode, richtigeAntwort, merkmalsauspraegung) {
  df %>%
    filter(
      Statistik_Code == Statistikcode,
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
    select(Statistik_Code) %>%
    pull() %>%
    sample(anzahl, replace = FALSE) 
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