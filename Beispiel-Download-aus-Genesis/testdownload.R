#Test Download ------

user <- "GAST"
pass <- "GAST"
EVAS <- "11111-01-01-4"

source("Beispiel-Download-aus-Genesis/download.R")

data <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2020, endyear = 2020)
str(data)
