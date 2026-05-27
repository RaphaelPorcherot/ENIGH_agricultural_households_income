# Packages and options ----

library(here) # Manage file paths relative to project root (reproducibility)
library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(tibble)

library(broom.helpers)
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
library(RColorBrewer)
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
# FUNCTIONS ----

source(here("script", "utils.r"))

# PART 1 : PREPARING THE DATABASE FOR THE PILOT STUDY ON MEXICO ----

#INFO: n_tipo_prod based on AGROPRODUCTO. But household in AGRO not in AGROPRODUCTOS will be classified as not agri because n_tipo_act will be NA. The difficulty is overcome in part 2 in which we explicitely assign a production type to household in AGRO but not in AGROPRODUCTO
#NOTE: we had issue with the key hogar : now solved, by setting its type explicitely to characer(). all hogares are in concentradohogar.
#NOTE: tipoact_agro is unused : we use instead our own is_agri income based definition

source(here("script", "data_prep.r"))
source(here("script", "data_svyr.r"))

# PART 2 : STATISTICAL TREATMENTS FOR THE PILOT STUDY ON MEXICO ---- 

#WARN : when we compute the ratio or the share etc it is always a macro value for the aggragated (agri) household in a given decile
#The aggregate share is substantially lower than the average household ratio, reflecting strong heterogeneity in farm size and a negative correlation between production scale and self-consumption
# In fact the mean of individual ratio for self-consumption is consierably higher -> many, many small farmers heavily rely on self-consumption
# We need to decide which we want (and we might want both, why not)
# For instance
# Il faut qu'on écrive ca à un momen donné dans le papier: We report both (i) the average household-level self-consumption rate and (ii) the aggregate share of self-consumed production. The gap between the two reflects strong heterogeneity in farm size and production structure. Si gap grand :
# forte hétérogénéité
# forte corrélation négative entre taille et autoconsommation
# structure duale agriculture (subsistence vs commercial)
#TODO : regarde la part des vieux programmes agricoles dans le revenu agricole total par décile (sauf que les déciles varient lorsque cette part varie)
#TODO : decile cut off point rajouter smg as line
#TODO:: actually lets compute please a real relative poverty line
#TODO : now that we have corrected the weird D10 deciles starts with farm dual's structure and review the comment and noe.
#TODO : compute the contrafactural income distribution that would be the case w/o support or w/o new support or w/o new direct support
#TODO : compute la distribution qui serait le cas s'il n'y avait que les vieux programmes agricoles
#TODO:: mettre tous sur un seul graphique: overall pop : agri_broad / non agri / sen_agri / non_sen_agri

source(here("script", "stat_basics.r"))
source(here("script", "stat_ineq.r"))
source(here("script", "stat_polagri.r"))

