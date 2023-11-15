#### 11111-01-01-4	Gebietsfläche in qkm ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "11111-01-01-4"

source("API_download.R")

`11111-01-01-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`11111-01-01-4`)

`11111-01-01-4` <- `11111-01-01-4` %>%
  drop_na(FLC006__Gebietsflaeche__qkm) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Zeit = year(dmy(Zeit))) %>%
  mutate(Wert_Code = "FLC006__Gebietsflaeche__qkm") %>%
  mutate(Wert_Label = "Gebietsfläche") %>%
  rename(Wert = `FLC006__Gebietsflaeche__qkm`)

#### 12411-01-01-4	Bevölkerung insgesamt ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "12411-01-01-4"

source("API_download.R")

`12411-01-01-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`12411-01-01-4`)

`12411-01-01-4` <- `12411-01-01-4` %>%
  drop_na(BEVSTD__Bevoelkerungsstand__Anzahl) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Zeit = year(dmy(Zeit))) %>%
  mutate(Wert_Code = "BEVSTD__Bevoelkerungsstand__Anzahl") %>%
  mutate(Wert_Label = "Bevölkerungsstand") %>%
  rename(Wert = `BEVSTD__Bevoelkerungsstand__Anzahl`)

#### 12411-07-01-4	Durchschnittsalter der Bevölkerung insgesamt ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "12411-07-01-4"

source("API_download.R")

`12411-07-01-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`12411-07-01-4`)

`12411-07-01-4` <- `12411-07-01-4` %>%
  mutate("2_Auspraegung_Label" = if_else(is.na(BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...15), "insgesamt", `2_Auspraegung_Label`)) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre... = rowSums(across(c(BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...14, BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...15)), na.rm = TRUE)) %>%
  filter(BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre... != 0) %>%
  select(-BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...14, -BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...15) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Zeit = year(dmy(Zeit))) %>%
  mutate(Wert_Code = "BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...") %>%
  mutate(Wert_Label = "Durchschnittsalter der Bevölkerung") %>%
  rename(Wert = `BEV519__Durchschnittsalter_der_Bevoelkerung__Jahre...`)

#### 13312-01-05-4	Erwerbstätige nach Wirtschaftszweigen - in Tausend ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "13312-01-05-4"

source("API_download.R")

`13312-01-05-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`13312-01-05-4`)

`13312-01-05-4` <- `13312-01-05-4` %>%
  drop_na() %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "ID0002__Erwerbstaetige_im_Jahresdurchschnitt__1000") %>%
  mutate(Wert_Label = "Erwerbstätige im Jahresdurchschnitt") %>%
  rename(Wert = `ID0002__Erwerbstaetige_im_Jahresdurchschnitt__1000`)

#### 21111-01-03-4	allgemeinbildende Schulen - Grundschulen ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "21111-01-03-4"

source("API_download.R")

`21111-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`21111-01-03-4`)

`21111-01-03-4` <- `21111-01-03-4` %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  select(-`BIL003__Schueler/-innen_an_allgemeinbildenden_Schulen__Anzahl`) %>%
  drop_na(BIL013__Allgemeinbildende_Schulen__Anzahl) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "BIL013__Allgemeinbildende_Schulen__Anzahl") %>%
  mutate(Wert_Label = "Allgemeinbildende Schulen") %>%
  rename(Wert = `BIL013__Allgemeinbildende_Schulen__Anzahl`)

#### 21111-01-03-4	allgemeinbildende Schulen - Schüler ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "21111-01-03-4"

source("API_download.R")

`21111-01-03-4a` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`21111-01-03-4a`)

`21111-01-03-4a` <- `21111-01-03-4a` %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  filter(`2_Auspraegung_Label` == "Insgesamt") %>%
  select(-`2_Auspraegung_Label`, -`2_Merkmal_Label`) %>%
  rename(`2_Auspraegung_Label` = `3_Auspraegung_Label`, `2_Merkmal_Label` = `3_Merkmal_Label`) %>%
  select(-`BIL013__Allgemeinbildende_Schulen__Anzahl`) %>%
  drop_na(`BIL003__Schueler/-innen_an_allgemeinbildenden_Schulen__Anzahl`) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "BIL003__Schueler/-innen_an_allgemeinbildenden_Schulen__Anzahl") %>%
  mutate(Wert_Label = "Schüler/-innen an allgemeinbildenden Schulen") %>%
  rename(Wert = `BIL003__Schueler/-innen_an_allgemeinbildenden_Schulen__Anzahl`)

#### 21311-01-01-4	Studierende - insgesamt ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "21311-01-01-4"

source("API_download.R")

`21311-01-01-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`21311-01-01-4`)

`21311-01-01-4` <- `21311-01-01-4` %>%
  drop_na(`HS-W02__Studierende_(im_Kreisgebiet)__Anzahl`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  select(-`2_Auspraegung_Label`, -`2_Merkmal_Label`) %>%
  rename(`2_Auspraegung_Label` = `4_Auspraegung_Label`, `2_Merkmal_Label` = `4_Merkmal_Label`) %>%
  mutate(Zeit = as.numeric(substr(Zeit, 4, 7))) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "HS-W02__Studierende_(im_Kreisgebiet)__Anzahl") %>%
  mutate(Wert_Label = "Studierende (im Kreisgebiet)") %>%
  rename(Wert = `HS-W02__Studierende_(im_Kreisgebiet)__Anzahl`)

#### 31111-01-02-4	Baugenehmigungen - Wohngebäude - insgesamt ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "31111-01-02-4"

source("API_download.R")

"31111-01-02-4" <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str("31111-01-02-4")

"31111-01-02-4" <- `31111-01-02-4` %>%
  drop_na(`BAU015__Wohngebaeude__Anzahl`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "BAU015__Wohngebaeude__Anzahl") %>%
  mutate(Wert_Label = "Wohngebäude") %>%
  rename(Wert = `BAU015__Wohngebaeude__Anzahl`)

#### 33111-01-02-4	Bodenfläche - Nutzung - in ha ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "33111-01-02-4"

source("API_download.R")

`33111-01-02-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`33111-01-02-4`)

`33111-01-02-4` <- `33111-01-02-4` %>%
  drop_na(`FLC005__Bodenflaeche__ha`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Zeit = year(dmy(Zeit))) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "FLC005__Bodenflaeche__ha") %>%
  mutate(Wert_Label = "Bodenfläche ") %>%
  rename(Wert = `FLC005__Bodenflaeche__ha`)

#### 41241-01-03-4	Erntestatistik - Hektarerträge ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "41241-01-03-4"

source("API_download.R")

`41241-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`41241-01-03-4`)

`41241-01-03-4` <- `41241-01-03-4` %>%
  drop_na(`ERT001__Hektarertraege__dt/ha`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "ERT001__Hektarertraege__dt/ha") %>%
  mutate(Wert_Label = "Hektarerträge") %>%
  rename(Wert = `ERT001__Hektarertraege__dt/ha`)

names(`41241-01-03-4`)

#### 45412-01-03-4	Tourismus - Geöffnete Beherbergungsbetriebe ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "45412-01-03-4"

source("API_download.R")

`45412-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`45412-01-03-4`)

`45412-01-03-4` <- `45412-01-03-4` %>%
  drop_na(`GAST04__Geoeffnete_Beherbergungsbetriebe__Anzahl`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "GAST04__Geoeffnete_Beherbergungsbetriebe__Anzahl") %>%
  mutate(Wert_Label = "Geöffnete Beherbergungsbetriebe") %>%
  rename(Wert = `GAST04__Geoeffnete_Beherbergungsbetriebe__Anzahl`)

names(`45412-01-03-4`)

#### 45412-03-02-4	Tourismus - Gästeübernachtungen - Herkunft Ausland ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "45412-03-02-4"

source("API_download.R")

`45412-03-02-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`45412-03-02-4`)

`45412-03-02-4` <- `45412-03-02-4` %>%
  drop_na(`GAST02__Gaesteuebernachtungen__Anzahl`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "GAST02__Gaesteuebernachtungen__Anzahl") %>%
  mutate(Wert_Label = "Gästeübernachtungen") %>%
  rename(Wert = `GAST02__Gaesteuebernachtungen__Anzahl`)

#### 46251-01-03-4	Kraftfahrzeugbestand - PKW ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "46251-01-03-4"

source("API_download.R")

`46251-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`46251-01-03-4`)

`46251-01-03-4` <- `46251-01-03-4` %>%
  drop_na(`VER012__Kraftfahrzeugbestand__Anzahl...15`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Zeit = year(dmy(Zeit))) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "VER012__Kraftfahrzeugbestand__Anzahl...15") %>%
  mutate(Wert_Label = "Kraftfahrzeugbestand") %>%
  rename(Wert = `VER012__Kraftfahrzeugbestand__Anzahl...15`)

#### 53111-01-01-4	Handwerkszählung ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "53111-01-01-4"

source("API_download.R")

`53111-01-01-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`53111-01-01-4`)

`53111-01-01-4` <- `53111-01-01-4` %>%
  drop_na(`UNT019__Handwerksunternehmen__Anzahl`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "UNT019__Handwerksunternehmen__Anzahl") %>%
  mutate(Wert_Label = "Handwerksunternehmen") %>%
  rename(Wert = `UNT019__Handwerksunternehmen__Anzahl`)

#### 61511-01-03-4	Statistik der Kaufwerte für Bauland ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "61511-01-03-4"

source("API_download.R")

`61511-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`61511-01-03-4`)

`61511-01-03-4` <- `61511-01-03-4` %>%
  drop_na(`BAU004__Durchschnittlicher_Kaufwert_je_qm__EUR`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "BAU004__Durchschnittlicher_Kaufwert_je_qm__EUR") %>%
  mutate(Wert_Label = "Durchschnittlicher Kaufwert je qm ") %>%
  rename(Wert = `BAU004__Durchschnittlicher_Kaufwert_je_qm__EUR`)

#### 82411-01-03-4	VGR der Länder: Umverteilungsrechnung ####

user <- "RE010213"
pass <- "Datenimport_23"
EVAS <- "82411-01-03-4"

source("API_download.R")

`82411-01-03-4` <- downloadFlat(EVAS = EVAS, user = user, pass = pass, startyear = 2015, endyear = NA, regionalkey = "14*")

str(`82411-01-03-4`)

`82411-01-03-4` <- `82411-01-03-4` %>%
  drop_na(`EKM014__verfueg._Einkommen_der_priv._Haushalte_je_Einwohner__EUR`) %>%
  filter(`1_Auspraegung_Code` > 1000) %>%
  mutate(Tabelle = EVAS) %>%
  mutate(Wert_Code = "EKM014__verfueg._Einkommen_der_priv._Haushalte_je_Einwohner__EUR") %>%
  mutate(Wert_Label = "verfüg. Einkommen der priv. Haushalte je Einwohner") %>%
  rename(Wert = `EKM014__verfueg._Einkommen_der_priv._Haushalte_je_Einwohner__EUR`)

#### Zusammenführung ####

daten_quiz <- list(
  `11111-01-01-4`,
  `12411-01-01-4`,
  `12411-07-01-4`,
  `13312-01-05-4`,
  `21111-01-03-4`,
  `21111-01-03-4a`,
  `21311-01-01-4`,
  `31111-01-02-4`,
  `33111-01-02-4`,
  `41241-01-03-4`,
  `45412-01-03-4`,
  `45412-03-02-4`,
  `46251-01-03-4`,
  `53111-01-01-4`,
  `61511-01-03-4`,
  `82411-01-03-4`
)

daten_quiz <- reduce(daten_quiz, full_join)

daten_quiz <- daten_quiz %>%
  rename(
    Jahr = Zeit,
    AGS = `1_Auspraegung_Code`,
    AGS_Label = `1_Auspraegung_Label`,
    Merkmal = `2_Auspraegung_Label`,
    Merkmal_Label = `2_Merkmal_Label`
  ) %>%
  select(
    Statistik_Code,
    Statistik_Label,
    Tabelle,
    Jahr,
    AGS,
    AGS_Label,
    Merkmal,
    Merkmal_Label,
    Wert,
    Wert_Code,
    Wert_Label
  ) %>%
  mutate_all(~ replace(., is.na(.), "Insgesamt")) %>%
  mutate(Wert = as.numeric(Wert))

write.csv2(daten_quiz, file = "daten_quiz.csv")
