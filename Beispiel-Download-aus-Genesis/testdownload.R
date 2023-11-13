#Test Download ------

user <- "GAST"
pass <- "GAST"
EVAS <- "21111-01-03-4-B"

source("Beispiel-Download-aus-Genesis/download.R")

data <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2020, endyear = 2020)
str(data)
