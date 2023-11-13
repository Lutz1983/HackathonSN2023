library(httr)
library(vroom)
library(tidyverse)

downloadFlat <- function(
    user = "GAST",
    pass = "GAST",
    EVAS,
    regionalvariable = NULL,
    regionalkey = NULL,
    classifyingvariable1 = NULL,
    classifyingkey1 = NULL,
    classifyingvariable2 = NULL,
    classifyingkey2 = NULL,
    classifyingvariable3 = NULL,
    classifyingkey3 = NULL,
    startyear = NULL,
    endyear = NULL,
    transpose = "false",
    job = "false",
    language = "de") {
  
  df <- GET(paste0(
    "https://www.regionalstatistik.de/genesisws/rest/2020/data/",
    "tablefile?",
    "username=", user,
    "&password=", pass,
    "&name=", EVAS,
    "&startyear=", startyear,
    "&endyear=", endyear,
    "&regionalvariable=", regionalvariable,
    "&regionalkey=", regionalkey,
    "&classifyingvariable1=", classifyingvariable1,
    "&classifyingkey1=", classifyingkey1,
    "&area=free&compress=false&transpose=false&format=ffcsv",
    "&job=false&stand=01.01.1970%2001%3A00&language=de"
  ))


  dfcontent <- rawToChar(df$content)

  if (language == "de") {
    df <- vroom(dfcontent, delim = ";")
  }
  if (language != "de") {
    df <- vroom(dfcontent, delim = ",")
  }

  n <- df %>%
    select(-ends_with("Code"), -ends_with("Label"), -Zeit) %>%
    names()
  return(df %>% mutate_at(n, as.double))
}
