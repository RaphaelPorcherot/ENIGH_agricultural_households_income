# Le novità sono poche:

# - la variabile n_tipo_prod classifica le aziende gestite dalle famiglie agricole (in senso ampio) in 5 gruppi invece che tre: come vedrai dalla descrizione dei gruppi si tiene conto anche delle attività forestali, di pesca, di caccia e di raccolta e della loro importanza relativa
# - c'è una nuova variabile (n_autonsumo2) che indica la quota di produzione autoconsumata che ho calcolato a partire dai dati della tabella AGROCONSUMOche permette un'analisi più dettagliata; vedremo se usarla nel nostro articolo, per il pilot study OCSE gli dirò di usare la variabile precedente (basata si dato della tabella AGRO
# - la variabile con la quota di entrate aziendali da misure di politica economica (n_apoyo) è stata corretta considerando la nuova variable n_support che include solamente gli aiuti a fondo perduto (ad esempio ho eliminato Tandas po el Bienestar e Credito Ganadero)
# - la variabile n_tipo_act ha lo stesso significato di prima (1= famiglie con attività di produzione agricola) ma è stata ricalcolata a partire dalla nuova variabile n_tipo_prod.
#
# Se tu puoi integrare questo script con le parti del tuo file qmd con i soli passi di R per svolgere l'analisi poi passiamo lo script a Francesco Vanni per metterli in condizione di eventualemnte creare qualche nuovo grafico o tabella per il Report OECD  coerentemente con la nostra analisi. Un'altra cosa che dovresti fare è aggiungere al file concentradohogar le variabili con le soglie e i decili/quintili di reddito delle famiglie, così tutti usano sempre gli stessi in ogni elaborazione.

# Packages and options

library(languageserver)
library(here) # Manage file paths relative to project root (reproducibility)
library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)

library(skimr) # Quick and clean data summaries
library(survey) # Analyse d’enquêtes complexes (pondérations, stratification, etc.)
library(srvyr) # Tidyverse wrapper for survey
library(convey) # Poverty and inequality measures for complex survey data

library(gtsummary) # Tableaux de synthèse et résumés statistiques pour les data frames and models
library(doBy) # Fonctions pour résumés, agrégations, transformations groupées
library(reldist) # Calculs de distributions relatives et indices de répartition
library(ggstats) # Extensions for ggplot2 with statistical layers and summaries
library(ggtext) # Improved text rendering in ggplot2 (markdown, HTML)

library(ggridges) # for joyplot https://r-charts.com/distribution/ggridges/
library(scales) # for label_number function
library(viridis) # Perceptually uniform color palettes for ggplot2
library(glue) # String interpolation (clean and readable text construction)
library(ggplot2)
library(laeken) # Indicators for social exclusion, poverty, inequality (EU-SILC type data)

library(kableExtra) # Enhanced tables for knitr (HTML/PDF styling)
library(knitr) # Engine for dynamic report generation (R Markdown / Quarto)

options(survey.lonely.psu = "adjust")
options(scipen = 999)
theme_gtsummary_language(
  language = "en"
)

# PART 1 : PREPARING THE DATABASE FOR THE PILOT STUDY ON MEXICO
# April 2026

source(here("script", "mexico_data_part1.R"))

# PART 2 STATISTICAL TREATMENTS FOR THE PILOT STUDY ON MEXICO
# May 2026

source(here("script", "mexico_stat_part2.R"))
