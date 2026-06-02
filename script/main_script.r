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
library(colorspace)
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

source(here("script", "0_utils.r"))

# PART 1 : PREPARING THE DATABASE FOR THE PILOT STUDY ON MEXICO ----



#TODO:
# size_val1 par tipo_prod (get_proportion())
# edad_med dans les deciles (get_proportion())
# gender dans les deciles (idem)

#INFO: n_tipo_prod based on AGROPRODUCTO. But household in AGRO not in AGROPRODUCTOS will be classified as not agri because n_tipo_act will be NA. The difficulty is overcome in part 2 in which we explicitely assign a production type to household in AGRO but not in AGROPRODUCTO
#NOTE: we had issue with the key hogar : now solved, by setting its type explicitely to characer(). all hogares are in concentradohogar.
#NOTE: tipoact_agro is unused : we use instead our own is_agri income based definition

source(here("script", "1A_data_prep.r"))
source(here("script", "1B_data_svyr.r"))

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

#TODO : now that we have corrected the weird D10 deciles starts with farm dual's structure and review the comment and noe.
#TODO : decile cut off point rajouter smg as line. actually lets compute please a real relative poverty line
#TODO: les intervalles de confiance sont peut etre trop grand pour savoir combien précisément mais on peut savoir si relativement certains déciles ont plus que d'autres : si les intervalles de confiance ne sont pas superposés
# pour aller dans cette direction :
# * refaire compo avec un nombre réduit de modalités pour nvo_tot
# * faire pour la modalité principale d'intérêt get_ratio (on y voit les intervalles)
#TODO: améliorer la détection des déciles significativement différents de la référence
# Actuellement : unreliable = CV > cv_threshold (fiabilité de l'estimation seulement)
# Ne répond pas à : "est-ce que le décile est significativement différent de la référence ?"
# Pistes par cas :
#   - ratio macro/micro   : test de différence décile vs overall via delta method
#                           (chevauchement des IC est approximatif mais rapide)
#   - macro_share         : nécessite d'abord d'ajouter des IC à ref_share_k dans
#                           get_share_macro_overall (ratio de totaux pondérés → delta method dispo)
#                           puis test de différence share_k vs ref_share_k
#   - micro_share         : overall a déjà un IC (svymean) → test de différence directement faisable
# Implémentation suggérée : ajouter un flag `significant` dans tbl en plus de `unreliable`,
# et adapter fill_var dans make_decile_plot pour distinguer
# "above/below significant" vs "above/below non-significant"

#TODO: get_proportion / get_number
# reprendre get_proportion, utilisé pour la description des houeseholds et pour turnover / production type pour les fermes
# nb of farm by turnover across decile
# household by jefe edad median
# household by jefe sexo

#TODO: LORENZ CURVE
# mettre tous sur un seul graphique: overall pop : agri_broad / non agri / sen_agri / non_sen_agri
# compute the contrafactural income distribution that would be the case w/o support or w/o new support or w/o new direct support
# compute la distribution qui serait le cas s'il n'y avait que les vieux programmes agricoles

source(here("script", "2A_stat_basic.r"))
source(here("script", "2B_stat_ineq.r"))
source(here("script", "2C_stat_comp_analysis.r"))

## Visually connect composition and ratio/share by setting graphical parameters ----

### create dictionnary of plot ----

dict_raw <- tibble(
  base = c(
    "share_n_fni_agro_clean_decile", # of income by decile
    "share_support_agro_decile", # of support to agriculture in decile
    "share_nvo_npago_agro_decile",
    "share_n_pro_agrogan_agro_decile",
    "share_n_apoyo_npago_agro_decile",
    "share_n_sembr_vida_agro_decile",
    "share_n_nvo_pago_agro_decile",

    "ratio_autocons1_size_val1_agro_decile",
    "ratio_n_fni_agro_size_val1_decile",
    "ratio_n_fni_agro_n_ftr1_decile",
    "ratio_n_fni_agro_n_ing_cor_decile",

    "ratio_support_agro_ing_cor_decile",
    "ratio_support_agro_ftr1_decile",
    "ratio_support_agro_fni_decile",
    #"ratio_support_agro_support_all_decile",

    "ratio_n_pro_agrogan_agro_n_fni_decile",
    "ratio_n_pro_agrogan_agro_support_decile",

    "ratio_n_nvo_npago_agro_n_fni_decile",
    "ratio_n_nvo_npago_agro_support_decile",

    #"ratio_n_nvo_tot_noagro_nvo_tot_decile",

    "ratio_n_nvo_pago_agro_n_nvo_tot_decile",

    "ratio_n_sembr_vida_agro_n_nvo_npago_decile",
    "ratio_n_nacion_fer_agro_n_nvo_npago_decile",
    "ratio_n_otros_prog_agro_n_nvo_npago_decile"
  ),
  type = NA_character_,
  target_or_num = c(
    "n_fni_agro_clean",
    "n_support_agro",
    "n_nvo_npago_agro",
    "n_pro_agrogan_agro",
    "n_apoyo_npago_agro",
    "n_sembr_vida_agro",
    "n_nvo_pago_agro",

    "n_autoconsumo1_agro",
    "n_fni_agro_clean",
    "n_fni_agro_clean",
    "n_fni_agro_clean",

    "n_support_agro",
    "n_support_agro",
    "n_support_agro",
    #n_support_agro",

    "n_pro_agrogan_agro",
    "n_pro_agrogan_agro",

    "n_nvo_npago_agro",
    "n_nvo_npago_agro",

    #"n_nvo_tot_noagro",

    "n_nvo_pago_agro",

    "n_sembr_vida",
    "n_nacion_fert_agro",
    "n_otros_prog_agro"
  ),
  den = c(
    "self",
    "self",
    "self",
    "self",
    "self",
    "self",
    "self",

    "n_size_val1_agro",
    "n_size_val1_agro",
    "n_ftr1_agro",
    "n_ing_cor_clean",

    "n_ing_cor_clean",
    "n_ftr1_agro",
    "n_fni_agro_clean",
    #"n_support",

    "n_fni_agro_clean",
    "n_support_agro",

    "n_fni_agro_clean",
    "n_support_agro",

    #"n_nvo_tot",

    "n_nvo_tot_agro",

    "n_nvo_npago_agro",
    "n_nvo_npago_agro",
    "n_nvo_npago_agro"
  ),
  den_name = c(
    "self",
    "self",
    "self",
    "self",
    "self",
    "self",
    "self",

    "farm total production",
    "farm total production",
    "farm total resources",
    "total current income",

    "total current income",
    "total current income",
    "farm total resources",
    #"support from new policies to all activities",

    "farm net income",
    "support (all policies)",

    "farm net income",
    "support (all policies)",

    #"all transfers (repayable and non-repayable) to all activities",

    "all transfers (repayable and non-repayable) to agriculture",

    "support from new policies",
    "support from new policies",
    "support from new policies"
  ),
  strat = c(
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",

    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",

    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total",
    #"n_deciles_total",

    "n_deciles_total",
    "n_deciles_total",

    "n_deciles_total",
    "n_deciles_total",

    #"n_deciles_total",

    "n_deciles_total",

    "n_deciles_total",
    "n_deciles_total",
    "n_deciles_total"
  ),
  above = NA_character_,
  below = NA_character_
) |>
  mutate(type = str_extract(base, "^[^_]+")) |>
  group_by(target_or_num) |>
  mutate(
    n_den = n_distinct(den[den != "self"])
  ) |>
  ungroup()
if (any(dict_raw$n_den > 4)) {
  stop("trop de dénominateurs (>4 hors self) pour au moins une variable")
}
dict_raw <- dict_raw |>
  group_by(target_or_num) |>
  mutate(
    transform = match(den, c("self", setdiff(unique(den), "self"))) - 1
  ) |>
  ungroup()

### assign colors ----

col_overall <- "#D55E00"

list_cols <- readRDS(here("output", "list_cols_from_comp_analysis"))

if (exists("list_cols")) {
  pal <- bind_rows(list_cols) |>
    arrange(var) |>
    group_by(var) |>
    slice(1) |>
    ungroup()
}

# add missing colors with Paired

cols <- brewer.pal(n = 12, name = "Paired")
pairs <- rep(seq_along(cols), each = 2)[seq_along(cols)]
list_cols_paired <- split(cols, pairs)

if (exists("pal")) {
  fallback <- dict_raw |> anti_join(pal, by = c("target_or_num" = "var"))
}
fallback <- fallback |>
  distinct(target_or_num) |>
  mutate(
    pair_id = (row_number() - 1) %% length(list_cols_paired) + 1,
    col_fallback = map_chr(pair_id, ~ list_cols_paired[[.x]][2])
  )

### merge and transform ----

dict <- dict_raw |>
  (\(x) {
    if (exists("pal")) left_join(x, pal, by = c("target_or_num" = "var")) else x
  })() |>
  left_join(fallback, by = "target_or_num") |>
  mutate(
    base_col = if (exists("pal")) coalesce(col, col_fallback) else col_fallback
  ) |>
  mutate(
    above = if_else(
      transform %in% c(0, 1),
      base_col,
      mapply(adaptive_transform, base_col, transform)
    )
  ) |>
  mutate(below = lighten(above, .50))

# -----------------------------

source(here("script", "2D_stat_share_analysis.r"))
source(here("script", "2E_stat_ratio_analysis.r"))

# SAVE RESULTS ----

saveRDS(res, here("output", "part2_results"))

# THE END --- 
