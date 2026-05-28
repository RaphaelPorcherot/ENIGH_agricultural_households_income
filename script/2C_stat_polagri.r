# ASSESSING THE EFFECTIVENES OF AGRICULTURAL POLICIES

list_cols <- list()

# Composition ----
## of income by decile ----

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

title_macro <- "Composition of household income across income deciles"
title_micro <- "Average household income composition across income deciles"
caption_macro <- paste(
  "Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
caption_micro <- paste(
  "Shares represent the average of household-level ratios within each decile (micro estimator).",
  "Each household contributes equally regardless of ",
  den_name,
  " size.",
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

## of support (old, new, other programs) to agri units by decile ----

#FORMULA:
# support = apoyo_npago + pro_agrogan + nvo_npago

basename <- "composition_support_npago_agri_decile"
col_pal <- "magma"

den <- "n_support_agro"
strat <- "n_deciles_total"

# den %in% colnames(d)
# "n_apoyo_npago_agro" %in% colnames(d)
# "n_pro_agrogan_agro" %in% colnames(d)
# "n_nvo_npago_agro" %in% colnames(d)

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

title_macro <- "Composition of non-repayable support to agriculture across income deciles"
title_micro <- "Average household composition of non-repayable support to agriculture across income deciles"
caption_base <- paste(
  "Other subsidies may come from federal, state, municipal government levels or else NGOs.",
  "Old support programs consist of PROCAMPO and AGROGAN",
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
  "Each household contributes equally regardless of ",
  den_name,
  " size.",
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

## of non repayable support from new policies to agriculture by decile ----

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
den_name <- "support received from new policies"
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

title_base <- "non-repayable support from new policies to agriculture"
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
  "Each household contributes equally regardless of received policy support size.",
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
  begin = 0,
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

## of all support from new policies to agriculture by decile ----

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
den_name <- "support received from new policies"
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

title_base <- "non-repayable support from new policies to agriculture"
title_macro <- str_c("Composition of ", title_base, " across income deciles")
title_micro <- str_c(
  "Average household composition of ",
  title_base,
  " across income deciles"
)

caption_base <- paste(
  "Non-repayable support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "Repayable support programs comprise of Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
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
  "Each household contributes equally regardless of ",
  den_name,
  " size.",
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
# -----------------------------
# Visually connect composition and ratio/share by setting graphical parameters ----
## create dictionnary of plot ----

dict_raw <- tibble(
  base = c(
    "share_support_decile",
    "ratio_autocons_prod_decile",
    "ratio_support_ing_cor_decile",
    "ratio_support_ftr1_decile",
    "ratio_support_fni_decile"
  ),
  type = NA_character_,
  target_or_num = c(
    "n_support_agro",
    "n_autoconsumo1_agro",
    "n_support_agro",
    "n_support_agro",
    "n_support_agro"
  ),
  den = c(
    "self",
    "n_size_val1_agro",
    "n_ing_cor_clean",
    "n_ftr1_agro",
    "n_fni_agro_clean"
  ),
  den_name = c(
    "self",
    "total agricultural production",
    "total current income",
    "farm total resources",
    "farm net income"
  ),
  strat = c(
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

## assign colors ----

col_overall <- "#D55E00"

pal <- bind_rows(list_cols) |>
  arrange(var) |>
  group_by(var) |>
  slice(1) |>
  ungroup()

# add missing colors with Paired

cols <- brewer.pal(n = 12, name = "Paired")
pairs <- rep(seq_along(cols), each = 2)[seq_along(cols)]
list_cols_paired <- split(cols, pairs)

fallback <- dict_raw |>
  distinct(target_or_num) |>
  mutate(
    pair_id = (row_number() - 1) %% length(list_cols_paired) + 1,
    col_fallback = map_chr(pair_id, ~ list_cols_paired[[.x]][2])
  )

## merge and transform ----
dict <- dict_raw |>
  left_join(pal, by = c("target_or_num" = "var")) |>
  left_join(fallback, by = "target_or_num") |>
  mutate(base_col = coalesce(col, col_fallback)) |>
  mutate(
    above = if_else(
      transform == 1,
      base_col,
      mapply(adaptive_transform, base_col, transform)
    )
  ) |>
  mutate(below = lighten(above, .50))

# -----------------------------

# Share ----
## Share of support by decile ----

basename <- "share_support_decile"
set_attribute(basename)

num_name <- str_to_sentence("non-repayable support to agriculture")
base_title <- str_c(
  num_name,
  # " in ",
  # den_name,
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

caption_base <- paste(
  "Self-consumed production' corresponds to production consumed by the household operating an agricultural production unit rather than sold.",
  "Total production is the value of sold productio, the estimated value of self-consumption and of non-monetary exchanges of production output.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c("Each household contributes equally regardless of ", den_name, " size"),
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
  col_overall = col_overall
)

# Ratio ----
## Non-repayable to new policies support to agriculture ----
# TODO: NPAGO/PAGO in NVO_AGRO
# "Non-repayable new policies comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
#  "Repayable new policies are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",

## Agriculture and non-agriculture in new policies support----
# TODO: AGRO/NOAGRO in NVO
#title_macro <- "Composition of support from new policies across income deciles"
#"Some households reporting non-agriculture independent activities report supports from new policies"

## Self-consumption in total turnover/production ----
#WARN: we need to understand the discrpenacy between tipoact, which is encoded by INEGHI, and the self-declaration of the actiity which got the support fromsocial programs which may agri in a quite contradictory manner with the first element:
# two possible explanation
# in NOAGRO are classified activities that are in 1 to 3, so not agricultural. But not agriculturla activities may have agriculturla input (fertilizantes for a small shop growing its own food or whatever)
# NVO have been extended to non agricultural activities such as Microcredits for instance
# La présence de bénéficiaires de programmes agricoles dans la table NOAGRO ne constitue pas nécessairement une incohérence statistique. La classification NOAGRO repose sur l’activité du negocio codée par l’enquête, tandis que l’activité associée au programme est auto-déclarée par le répondant. Cette dissociation reflète probablement la forte pluriactivité des ménages ruraux mexicains ainsi que le caractère transversal des nouveaux programmes sociaux, qui peuvent soutenir des activités agricoles secondaires au sein de ménages principalement engagés dans des activités commerciales, industrielles ou de services.
#TODO: check what kind of combination exists in NOAGRO between tipoact and nvo_act1, nvo_act2
#TODO: décider si on le fait aussi pour la somme des deux valeurs de l'autoconsommation

basename <- "ratio_autocons_prod_decile"
set_attribute(basename)

num_name <- "self-consumed agricultural production"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)

caption_base <- paste(
  "Self-consumed production' corresponds to production consumed by the household operating an agricultural production unit rather than sold.",
  "Total production is the value of sold productio, the estimated value of self-consumption and of non-monetary exchanges of production output.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c("Each household contributes equally regardless of ", den_name, " size"),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
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
  col_overall = col_overall
)

### from agro self employemnet ----

### from non agro self employment ----

#TODO: still somehting to do; self-consumption from agro in all

## Support in total current income ----

#NOTE: The micro and macro estimators of income composition yield very similar results across deciles because the denominator — total household income — is precisely the variable used to construct the deciles, making it relatively homogeneous within each group.
#This contrasts sharply with the autoconsumption-to-production ratio, where the denominator varies by orders of magnitude within deciles, driving a large wedge between the two estimators. For income composition stratified by income deciles, the choice between micro and macro estimators is therefore largely inconsequential, and both can be reported interchangeably.

basename <- "ratio_support_ing_cor_decile"
set_attribute(basename)

base_title <- str_c(
  "non-repayable policy payments in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)

caption_base <- paste(
  "Non-repayable payments is financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Total current income is the sum of labour income, independant (either from agricultural activities or not), capital income, imputed rents, social transfers and others incomes (e.g. remesas). It is equivaled income based on the square root equivalence scale.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c("Each household contributes equally regardless of ", den_name, " size"),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
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
  col_overall = col_overall
)

## Support in farm total ressources (entrate aziendale) ----

basename <- "ratio_support_ftr1_decile"
set_attribute(basename)

num_name <- "non-repayable policy payments"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)

title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)

caption_base <- paste(
  "Non-repayable payments are financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Farm total resources includes sales value, estimated value of self-consumption and of non-monetary exchanges and direct policy payments",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c("Each household contributes equally regardless of ", den_name, " size"),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
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
  col_overall = col_overall
)

## Support in farm net income (gross of fixed capital depreciation) ----
#WARN:
# - Negative CI bounds in some deciles (e.g. D5 agri_broad: 59.6% [-16.8, 136.1]) are not a
#   bug but reflect genuine estimation uncertainty: few households receive support in that decile,
#   and those who do show highly dispersed support/fni ratios, inflating the within-decile variance.
# - The micro estimator is unreliable for agri_broad: many households have agricultural activity
#   as a secondary source of income, resulting in sparse and highly variable support/fni ratios
#   across deciles; prefer macro for agri_broad or restrict micro results to agri_narrow.

basename <- "ratio_support_fni_decile"
set_attribute(basename)

num_name <- "non-repayable policy payments"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)

caption_base <- paste(
  "Non-repayable payments are financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation. ",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
extra_text <- paste(
  "Bars represent the average of household-level values within each decile.",
  str_c("Each household contributes equally regardless of ", den_name, " size"),
  sep = "\n"
)

universes <- list(
  list(universe = "n_is_agri_broad", filter = "agri_broad"),
  list(universe = "n_is_agri", filter = "agri_narrow")
)

run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
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
  col_overall = col_overall
)

# THE END ----
