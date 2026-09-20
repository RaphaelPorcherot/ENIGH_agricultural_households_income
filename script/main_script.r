# Packages and options ----

library(here) # Manage file paths relative to project root (reproducibility)
library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

library(broom.helpers)
library(survey) # Analyse d’enquêtes complexes (pondérations, stratification, etc.)
library(srvyr) # Tidyverse wrapper for survey
library(convey) # Poverty and inequality measures for complex survey data

library(gtsummary) # Tableaux de synthèse et résumés statistiques pour les data frames and models
library(ggtext) # Improved text rendering in ggplot2 (markdown, HTML)

library(scales) # for label_number function
library(RColorBrewer)
library(colorspace)
library(viridis) # Perceptually uniform color palettes for ggplot2
library(glue) # String interpolation (clean and readable text construction)
library(ggplot2)

# Libraries removed because no function of theirs is called anywhere in script/:
#   forcats, doBy, reldist, ggstats, ggridges, laeken
# and three more that appear only in COMMENTED-OUT exploratory code:
#   skimr     -> `skim()` in 1A_data_prep.r (lines ~298-300)
#   knitr     -> `kable()` in 2B_stat_ineq.r (lines ~341, ~519)
#   kableExtra-> `kable_styling()` in 2B_stat_ineq.r (lines ~344, ~522)
# Uncomment the matching library() below if you re-enable those lines.
#
# This is not cosmetic. `reldist` imports densEstBayes, which imports rstan and
# its whole Stan toolchain; loading it pulled 49 packages (~400 MB) into
# renv.lock that the analysis never touches. Dropping these nine library() calls
# takes the dependency closure from 152 to 103 packages, which is what a
# collaborator's renv::restore() has to install.
#
# library(skimr)      # Quick and clean data summaries
# library(knitr)      # Engine for dynamic report generation (R Markdown / Quarto)
# library(kableExtra) # Enhanced tables for knitr (HTML/PDF styling)

options(survey.lonely.psu = "adjust")
options(scipen = 999)
theme_gtsummary_language(
  language = "en"
)

# FUNCTIONS and create output dir ----

source(here("script", "0_utils.r"))
create_output_dirs()

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

# PARTLY DONE: every decile is now formally tested against the REFERENCE, by
# exactly the mechanism this TODO described - one svytotal() on the whole set of
# domain totals, delta method on the difference, using the full joint covariance
# matrix so that Cov(decile, reference) is propagated. Each analysis produces a
# companion plot_signif_*.pdf, and the tables carry diff / diff_SE / diff_IC_* /
# p_value / p_value_adj / signif. The design-based linear trend across deciles is
# reported too (trend_slope / trend_SE / trend_p).
#
# TODO, what remains: DECILE vs DECILE comparisons (e.g. D1 - D10). They are now
# almost free - the full covariance matrix of all decile totals is already
# computed inside every estimator call - but nothing exposes them yet. The
# machinery is .test_contrast() in 0_utils.r: it takes arbitrary weight vectors
# a_est / b_est / a_ref / b_ref, so a D1-vs-D10 contrast is just two unit vectors
# instead of one unit vector and one pooled vector. Worth adding if the paper
# needs to claim that two specific deciles differ.

#NOTE (was a WARN, no longer true): ratios and shares are no longer macro-only.
# Every analysis is now computed with three estimators - macro (ratio of the
# aggregates), micro (mean of the individual ratios) and median (median of the
# individual ratios) - so the comparison below can be made directly from the
# output instead of being asserted.
#The aggregate share is substantially lower than the average household ratio, reflecting strong heterogeneity in farm size and a negative correlation between production scale and self-consumption
# In fact the mean of individual ratio for self-consumption is consierably higher -> many, many small farmers heavily rely on self-consumption
# We need to decide which we want (and we might want both, why not)
# For instance
# Il faut qu'on écrive ca à un momen donné dans le papier: We report both (i) the average household-level self-consumption rate and (ii) the aggregate share of self-consumed production. The gap between the two reflects strong heterogeneity in farm size and production structure. Si gap grand :
# forte hétérogénéité
# forte corrélation négative entre taille et autoconsommation
# structure duale agriculture (subsistence vs commercial)

#TODO : now that we have corrected the weird D10 deciles starts with farm dual's structure and review the comment and noe.
# for instance we had previously
# #(no longer true, legacy) chez les riches mexicains, la possession d’une ferme n’est pas uniquement productive : il y a plus de fermiers dans D10 que dans D9, mais manifestement les D10 ce n'est pas que de l'industrie agricole à grande échelle. Les fermes dans D10 ne sont pas la source principale de la richesse ? Pourtant effectivement qqchose comme 70 % du revenus de D10 vient des fermes
# Une minorité de très grosses fermes capte l’essentiel du revenu agricole: D10 semble être lui-même dual !
# D9 = bourgeoisie agricole productive
# grandes exploitations commerciales
# relativement homogènes
# revenus encore diversifiés
# D10 = élite économique rurale hybride
# très grandes exploitations ultra-rentables
# ménages riches avec petites fermes
# forte dispersion patrimoniale

#TODO : decile cut off point rajouter smg as line. actually lets compute please a real relative poverty line

#DONE, and the premise was wrong: "si les intervalles de confiance ne sont pas
# superposés" is NOT a test. Non-overlapping intervals do imply a difference, but
# overlapping ones imply nothing - and here the estimates are correlated (shares
# sum to 1, so they are strongly NEGATIVELY correlated), which makes the overlap
# rule badly misleading. The difference is now estimated directly with its own
# variance; see the plot_signif_*.pdf figures.
#TODO, still open from this note: redo the composition with fewer modalities for
# nvo_tot, so that the components are individually better estimated.

#DONE: the three cases listed below are all implemented.
#   - ratio macro/micro : delta-method test of the decile against the pooled
#                         reference, using the full joint covariance matrix.
#   - macro_share       : get_share_macro_overall() now returns ref_SE /
#                         ref_IC_low / ref_IC_high, and each decile's share is
#                         tested against its own demographic weight N_g / sum N.
#   - micro_share       : same delta-method test against the overall mean.
#   - medians           : survey version of Mood's median test (share of the
#                         decile's households below the overall median vs 50 %).
# A `signif` flag (plus signif_adj, Holm-corrected over the ten deciles) sits in
# tbl next to `unreliable`, and every analysis produces a plot_signif_*.pdf.
#TODO, the one sub-item not done as described: make_decile_plot() marks the
# non-significant deciles with a small "ns" rather than splitting fill_var into
# four levels (above/below x significant/non-significant). The four-level fill is
# a one-line change if the paper wants it on the main figures rather than on the
# companion ones.

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
    "ratio_n_precios_gar_agro_n_nvo_npago_decile",
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
    "n_precios_gar_agro",
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

# 2C leaves `list_cols` in the global environment, so in a full run the object is
# already there and reading the file back would be a no-op. The file is only
# needed when 2D/2E are re-run WITHOUT re-running 2C (a fresh session, or the
# composition step commented out): keep it as that fallback, but never let a
# stale file silently override the palette just computed by 2C.
if (!exists("list_cols")) {
  list_cols <- readRDS(here("output", "list_cols_from_comp_analysis"))
  message(
    "\n\u2139\ufe0f  list_cols read back from output/ (2C was not run in this session)"
  )
}

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

# THE END ---
