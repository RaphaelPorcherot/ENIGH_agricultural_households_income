# COMPOSITION ANALYSIS


list_cols <- list()

# of income by decile ----

#FORMULA:
# n_ing_cor = n_fni_agro + # containts support from apoyo, nvo, and agrogan
#   n_ingr_noagro + # contains support from nvo
#   n_trabajo +
#   n_otros_trab +
#   n_rentas +
#   n_transfer +
#   n_estim_alqu +
#   n_otros_ing

basename <- "composition_income_decile"
col_pal <- "cividis"
den <- "n_ing_cor_clean"
den_name <- "total current income"
strat <- "n_deciles_total"
components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo_bundled",
  transfer = "n_transfer",
  other = "n_otros_ing_bundled"
)
component_labels <- c(
  agri = "Farm net income",
  no_agri = "Non-agricultural self-employment income",
  wage = "Wage income",
  transfer = "Social and family transfers",
  other = "Other income sources"
)
universes <- list(
  list(universe = NULL, filter = NULL),
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)
title_macro <- str_c("Composition of ", den_name, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  den_name,
  " across income deciles"
)
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  str_c(
    "Each household contributes equally regardless of ",
    den_name,
    " size."
  ),
  "Source: Based on ENIGH data.",
  sep = "\n"
)
cols <- run_composition_analysis(
  design = mysvyr,
  d = d,
  estimators = c("macro", "micro"),
  components = components,
  component_labels = component_labels,
  den = den,
  strat = strat,
  universes = universes,
  basename = basename,
  title_macro = title_macro,
  title_micro = title_micro,
  caption_macro = caption_macro,
  caption_micro = caption_micro,
  col_pal = col_pal,
  debug_outer = FALSE,
  debug_inner = FALSE
)
list_cols[[length(list_cols) + 1]] <- cols

### DEBUG ----
# make_composition_plot(
#   tbl = tbl,
#   overall = overall,
#   strat = strat,
#   strat_lvl_with_total = strat_lvl_with_total,
#   subtitle = subtitle,
#   caption = caption,
#   component_labels = component_labels,
#   title = title,
#   col_pal = col_pal,
#   direction = 1
# )
#
# run_composition_analysis(
#   design = mysvyr,
#   d = d,
#   estimators = "micro",
#   components = components,
#   component_labels = component_labels,
#   den = den,
#   strat = strat,
#   universes = list(
#     list(universe = "n_is_agri", filter = "agri_narrow"),
#     list(universe = NULL, filter = NULL)
#   ),
#   basename = basename,
#   title_macro = title_macro,
#   title_micro = title_micro,
#   caption_macro = caption_macro,
#   caption_micro = caption_micro,
#   col_pal = col_pal,
#   # debug_outer = FALSE,debug_inner = FALSE
#   debug_outer = FALSE,
#   debug_inner = TRUE
# )
#
# tbl
# unreliable_deciles
# plot_data
# build_ggplot()

# of support (old, new, other programs) to agri units by decile ----

#FORMULA:
# support = apoyo_npago + pro_agrogan + nvo_npago

basename <- "composition_support_npago_agri_decile"
col_pal <- "magma"
den <- "n_support_agro"
den_name <- "support from all policies to agriculture"
strat <- "n_deciles_total"
components <- list(
  other = "n_apoyo_npago_agro",
  old = "n_pro_agrogan_agro",
  new = "n_nvo_npago_agro"
)
component_labels <- c(
  other = "Other subsidies",
  old = "Old programs",
  new = "New programs"
)
universes <- list(
  list(universe = NULL, filter = NULL),
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)
title_macro <- str_c("Composition of ", den_name, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  den_name,
  " across income deciles"
)
caption_base <- paste(
  "Other subsidies may come from federal, state, municipal government levels or else NGOs.",
  "Old support programs consist of PROCAMPO, PROGAN and their contemporary continuation, Producción para el Bien Estar",
  "New support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "All subsidies are non-repayables. In particular, not included are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  sep = "\n"
)
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  str_c(
    "Each household contributes equally regardless of ",
    den_name,
    " size."
  ),
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)

cols <- run_composition_analysis(
  design = mysvyr,
  d = d,
  estimators = c("macro", "micro"),
  components = components,
  component_labels = component_labels,
  den = den,
  strat = strat,
  universes = universes,
  basename = basename,
  title_macro = title_macro,
  title_micro = title_micro,
  caption_macro = caption_macro,
  caption_micro = caption_micro,
  col_pal = col_pal,
  begin = .3,
  end = .8,
  debug_outer = FALSE,
  debug_inner = FALSE
  # debug = TRUE # # → tous les objets sont dans ton env, tu peux appeler make_composition_plot directement
)

list_cols[[length(list_cols) + 1]] <- cols

### DEBUG ----

# make_composition_plot(
#   tbl = tbl,
#   overall = overall,
#   strat = strat,
#   strat_lvl_with_total = strat_lvl_with_total,
#   subtitle = subtitle,
#   caption = caption,
#   component_labels = component_labels,
#   title = title,
#   col_pal = col_pal
# )

# of support from new policies to agriculture by decile ----

#FORMULA:
# nvo_npago = sembr_vida +
#   agromercados +
#   precios_gar +
#   nacion_fer +
#   desarollo_rur +
#   otros_prog,

basename <- "composition_nvo_npago_agro_decile"
col_pal <- "inferno"
den <- "n_nvo_npago_agro"
den_name <- "support from new policies to agriculture"
strat <- "n_deciles_total"
components <- list(
  sembr_vida = "n_sembr_vida_agro",
  agromercados = "n_agromercados_agro",
  precios_gar = "n_precios_gar_agro",
  nacion_fer = "n_nacion_fert_agro",
  desarollo_rur = "n_desarollo_rur_agro",
  otros_prog = "n_otros_prog_agro"
)
component_labels <- c(
  sembr_vida = "Sembrando Vida",
  agromercados = "Agromercados",
  precios_gar = "Precios de Garantia",
  nacion_fer = "Nacional de Fertilizantes",
  desarollo_rur = "Desarollo Rural",
  otros_prog = "Others"
)
universes <- list(
  list(universe = NULL, filter = NULL),
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)
title_base <- "support from new policies to agriculture"
title_macro <- str_c("Composition of ", title_base, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  title_base,
  " across income deciles"
)
caption_base <- paste(
  "New support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "All these subsidies are non-repayables. In particular, not included are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "A smaller part of support from new policies goes to non-agricultural activities : they are not considered in these estimates.",
  sep = "\n"
)
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  str_c(
    "Each household contributes equally regardless of ",
    den_name,
    " size."
  ),
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
cols <- run_composition_analysis(
  design = mysvyr,
  d = d,
  estimators = c("macro", "micro"),
  components = components,
  component_labels = component_labels,
  den = den,
  strat = strat,
  universes = universes,
  basename = basename,
  title_macro = title_macro,
  title_micro = title_micro,
  caption_macro = caption_macro,
  caption_micro = caption_micro,
  col_pal = col_pal,
  begin = .1,
  end = 0.80,
  debug_outer = FALSE,
  debug_inner = FALSE
  # debug = TRUE # # → tous les objets sont dans ton env, tu peux appeler make_composition_plot directement
)

list_cols[[length(list_cols) + 1]] <- cols

### DEBUG ----
# make_composition_plot(
#   tbl = tbl,
#   overall = overall,
#   strat = strat,
#   strat_lvl_with_total = strat_lvl_with_total,
#   subtitle = subtitle,
#   caption = caption,
#   component_labels = component_labels,
#   title = title,
#   col_pal = col_pal
# )

# of all transfers from new policies to agriculture by decile ----


# n_sembr_vida_agro -> #000004FF
# n_agromercados_agro -> #2C0B57FF
# n_precios_gar_agro -> #6B186EFF
# n_nacion_fert_agro -> #A82E5FFF
# n_desarollo_rur_agro -> #DD513AFF
# n_otros_prog_agro -> #F98C0AFF

#FORMULA:
# nvo_tot = sembr_vida +
#   agromercados +
#   precios_gar +
#   nacion_fer +
#   desarollo_rur +
#   otros_prog +
#   tand_bien +
#   credito_gan

basename <- "composition_nvo_tot_agro_decile"
col_pal <- "inferno"
den <- "n_nvo_tot_agro"
den_name <- "repayable and non-repayable transfers received from new policies"
strat <- "n_deciles_total"
components <- list(
  sembr_vida = "n_sembr_vida_agro",
  agromercados = "n_agromercados_agro",
  precios_gar = "n_precios_gar_agro",
  nacion_fer = "n_nacion_fert_agro",
  desarollo_rur = "n_desarollo_rur_agro",
  otros_prog = "n_otros_prog_agro",
  tand_bien = "n_tand_bien_agro",
  credito_gan = "n_credito_gan_agro"
)
component_labels <- c(
  sembr_vida = "Sembrando Vida",
  agromercados = "Agromercados",
  precios_gar = "Precios de Garantia",
  nacion_fer = "Nacional de Fertilizantes",
  desarollo_rur = "Desarollo Rural",
  otros_prog = "Others",
  tand_bien = "Tandas para el Bien-Estar",
  credito_gan = "Credito a la Palabra"
)
universes <- list(
  list(universe = NULL, filter = NULL),
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)
title_base <- "transfers from new policies to agriculture"
title_macro <- str_c("Composition of ", title_base, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  title_base,
  " across income deciles"
)
caption_base <- paste(
  "Non-repayable transfers programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "Repayable transfers programs comprise of Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "A smaller part of support from new policies goes to non-agricultural activities : they are not considered in these estimates.",
  sep = "\n"
)
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  str_c(
    "Each household contributes equally regardless of ",
    den_name,
    " size."
  ),
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
cols <- run_composition_analysis(
  design = mysvyr,
  d = d,
  estimators = c("macro", "micro"),
  components = components,
  component_labels = component_labels,
  den = den,
  strat = strat,
  universes = universes,
  basename = basename,
  title_macro = title_macro,
  title_micro = title_micro,
  caption_macro = caption_macro,
  caption_micro = caption_micro,
  col_pal = col_pal,
  begin = .1,
  debug_outer = FALSE,
  debug_inner = FALSE
  # debug = TRUE # # → tous les objets sont dans ton env, tu peux appeler make_composition_plot directement
)
list_cols[[length(list_cols) + 1]] <- cols

### DEBUG ----
# make_composition_plot(
#   tbl = tbl,
#   overall = overall,
#   strat = strat,
#   strat_lvl_with_total = strat_lvl_with_total,
#   subtitle = subtitle,
#   caption = caption,
#   component_labels = component_labels,
#   title = title,
#   col_pal = col_pal
# )

# of all transfers (bundled as npago et pago) from new policies to agriculture by decile ----


# n_sembr_vida_agro -> #000004FF
# n_agromercados_agro -> #2C0B57FF
# n_precios_gar_agro -> #6B186EFF
# n_nacion_fert_agro -> #A82E5FFF
# n_desarollo_rur_agro -> #DD513AFF
# n_otros_prog_agro -> #F98C0AFF

#FORMULA:
# nvo_tot = nvo_npago + nvo_pago

basename <- "composition_nvo_tot_agro_bundled_decile"
col_pal <- "mako"

den <- "n_nvo_tot_agro"
den_name <- "repayable and non-repayable transfers (bundled) received from new policies"
strat <- "n_deciles_total"
components <- list(
  nvo_npago = "n_nvo_npago_agro",
  nvo_pago = "n_nvo_pago_agro"
)
component_labels <- c(
  nvo_npago = "Non-repayable (=support)",
  nvo_pago = "Repayable"
)
universes <- list(
  list(universe = NULL, filter = NULL),
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)
title_base <- "transfers (bundled) from new policies to agriculture"
title_macro <- str_c("Composition of ", title_base, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  title_base,
  " across income deciles"
)
caption_base <- paste(
  "Non-repayable transfers programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "Repayable transfers programs comprise of Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "A smaller part of support from new policies goes to non-agricultural activities : they are not considered in these estimates.",
  sep = "\n"
)
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  str_c(
    "Each household contributes equally regardless of ",
    den_name,
    " size."
  ),
  caption_base,
  "Source: Based on ENIGH data.",
  sep = "\n"
)

cols <- run_composition_analysis(
  design = mysvyr,
  d = d,
  estimators = c("macro", "micro"),
  components = components,
  component_labels = component_labels,
  den = den,
  strat = strat,
  universes = universes,
  basename = basename,
  title_macro = title_macro,
  title_micro = title_micro,
  caption_macro = caption_macro,
  caption_micro = caption_micro,
  col_pal = col_pal,
  begin = 0.25,
  end = 0.75,
  debug_outer = FALSE,
  debug_inner = FALSE
  # debug = TRUE # # → tous les objets sont dans ton env, tu peux appeler make_composition_plot directement
)

list_cols[[length(list_cols) + 1]] <- cols

### DEBUG ----
# make_composition_plot(
#   tbl = tbl,
#   overall = overall,
#   strat = strat,
#   strat_lvl_with_total = strat_lvl_with_total,
#   subtitle = subtitle,
#   caption = caption,
#   component_labels = component_labels,
#   title = title,
#   col_pal = col_pal
# )

# -------------
saveRDS(list_cols, file = here("output", "list_cols_from_comp_analysis"))
message("🤙 lists_cols is saved in output/")

# THE END ----
