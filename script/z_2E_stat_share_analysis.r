# ASSESSING THE EFFECTIVENES OF AGRICULTURAL POLICIES

# -----------------------------
# -----------------------------

#TODO: reprendre la caption de share_macro mais aussi share_micro pour expliquer ce que ca veut dire d'être au-dessus ou en dessouss

# TODO: améliorer la détection des déciles significativement différents de la référence
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

# Share ----
## Share of support to agriculture by decile ----

basename <- "share_support_decile"
set_attribute(basename)

num_name <- "support from all policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

caption_base <- paste(
  "Non-repayable payments is financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c(
    "Each household contributes equally regardless of ",
    if (is_share_plot) num_name else den_name,
    " size"
  ),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_share_analysis(
  design = mysvyr,
  d = d,
  target = target,
  # den = strat,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall,
  debug_outer = TRUE,
  debug_inner = FALSE
)
## Share of support from new programs to agriculture by decile ----

basename <- "share_nvo_npago_decile"
set_attribute(basename)

num_name <- "support from new policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

caption_base <- paste(
  "New support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "All these subsidies are non-repayables. In particular, not included are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "A smaller part of support from new policies goes to non-agricultural activities : they are not considered in these estimates.",
  # "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c(
    "Each household contributes equally regardless of ",
    if (is_share_plot) num_name else den_name,
    " size"
  ),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_share_analysis(
  design = mysvyr,
  d = d,
  target = target,
  # den = strat,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall,
  debug_outer = TRUE,
  debug_inner = FALSE
)
## Share of support from old programs to agriculture by decile ----

#WARN: poor agricultural households appear to get their fair share of PROCAMPO !
# survey::svytotal(
#   ~n_pro_agrogan_agro,
#   subset(
#     mysvyr,
#     n_is_agri_broad == "agri_broad" & n_deciles_total == "D1"
#   ),
#   na.rm = TRUE
# ) / survey::svytotal(
#   ~n_pro_agrogan_agro,
#   subset(mysvyr, n_is_agri_broad == "agri_broad"),
#   na.rm = TRUE
# )

basename <- "share_n_pro_agrogan_agro_decile"
set_attribute(basename)

num_name <- "support from old policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)
caption_base <- paste(
  "Old support programs consist of PROCAMPO and AGROGAN.",
  # "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c(
    "Each household contributes equally regardless of ",
    if (is_share_plot) num_name else den_name,
    " size"
  ),
  sep = "\n"
)
universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_share_analysis(
  design = mysvyr,
  d = d,
  target = target,
  # den = strat,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall,
  debug_outer = TRUE,
  debug_inner = FALSE
)
## Share of support from other policies by decile ----

basename <- "share_support_decile"
set_attribute(basename)

num_name <- "non-repayable support to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

caption_base <- paste(
  "Non-repayable payments is financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c(
    "Each household contributes equally regardless of ",
    if (is_share_plot) num_name else den_name,
    " size"
  ),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_share_analysis(
  design = mysvyr,
  d = d,
  target = target,
  # den = strat,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall,
  debug_outer = TRUE,
  debug_inner = FALSE
)

# THE END ----
