#TODO: make sure plot caption are right

# Self-consumption ----
## Agricultural: in total turnover/production ----
#WARN: we need to understand the discrpenacy between tipoact, which is encoded by INEGHI, and the self-declaration of the actiity which got the support fromsocial programs which may agri in a quite contradictory manner with the first element: two possible explanation
# in NOAGRO are classified activities that are in 1 to 3, so not agricultural. But not agriculturla activities may have agriculturla input (fertilizantes for a small shop growing its own food or whatever)
# NVO have been extended to non agricultural activities such as Microcredits for instance
# La présence de bénéficiaires de programmes agricoles dans la table NOAGRO ne constitue pas nécessairement une incohérence statistique. La classification NOAGRO repose sur l’activité du negocio codée par l’enquête, tandis que l’activité associée au programme est auto-déclarée par le répondant. Cette dissociation reflète probablement la forte pluriactivité des ménages ruraux mexicains ainsi que le caractère transversal des nouveaux programmes sociaux, qui peuvent soutenir des activités agricoles secondaires au sein de ménages principalement engagés dans des activités commerciales, industrielles ou de services.
#TODO: check what kind of combination exists in NOAGRO between tipoact and nvo_act1, nvo_act2
#TODO: décider si on le fait aussi pour la somme des deux valeurs de l'autoconsommation

basename <- "ratio_autocons1_size_val1_agro_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
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
  "Total production is the value of sold production, the estimated value of self-consumption and of non-monetary exchanges of production output.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## Non agricultural: in total turnover/production (NOAGRO) ----

#TODO: still somehting to do; self-consumption from agro in all

# Farm net income ----

## in farm total production ----

basename <- "ratio_n_fni_agro_size_val1_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "farm net income"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
  "Total production is the value of sold production, the estimated value of self-consumption and of non-monetary exchanges of production output.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)
## in farm total ressources (entrate aziendale) ----

basename <- "ratio_n_fni_agro_n_ftr1_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "farm net income"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
  "Farm total resources includes sales value, estimated value of self-consumption and of non-monetary exchanges and support from policies.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in total current income ----

basename <- "ratio_n_fni_agro_n_ing_cor_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "farm net income"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
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
  str_c(
    "Each household contributes equally regardless of ",
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Support to agriculture (n_support_agro)----

## in total current income ----

#NOTE: The micro and macro estimators of income composition yield very similar results across deciles because the denominator — total household income — is precisely the variable used to construct the deciles, making it relatively homogeneous within each group.
#This contrasts sharply with the autoconsumption-to-production ratio, where the denominator varies by orders of magnitude within deciles, driving a large wedge between the two estimators. For income composition stratified by income deciles, the choice between micro and macro estimators is therefore largely inconsequential, and both can be reported interchangeably.

basename <- "ratio_support_agro_ing_cor_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from all policies to agriculture"
base_title <- str_c(
  "non-repayable policy payments in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)

caption_base <- paste(
  "Support are non-repayable transfers to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
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
  str_c(
    "Each household contributes equally regardless of ",
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in farm total ressources (entrate aziendale) ----

basename <- "ratio_support_agro_ftr1_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from all policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Support are non-repayable transfers to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
  "Farm total resources includes sales value, estimated value of self-consumption and of non-monetary exchanges and support from policies",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in farm net income (gross of fixed capital depreciation) ----
#WARN:
# - Negative CI bounds in some deciles (e.g. D5 agri_broad: 59.6% [-16.8, 136.1]) are not a
#   bug but reflect genuine estimation uncertainty: few households receive support in that decile,
#   and those who do show highly dispersed support/fni ratios, inflating the within-decile variance.
# - The micro estimator is unreliable for agri_broad: many households have agricultural activity
#   as a secondary source of income, resulting in sparse and highly variable support/fni ratios
#   across deciles; prefer macro for agri_broad or restrict micro results to agri_narrow.

basename <- "ratio_support_agro_fni_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from all policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Support are non-repayable transfers to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
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
  str_c(
    "Each household contributes equally regardless of ",
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in all support (to agri or not agri) ----
#NOTE: this is mainly the case of total transfers, so this is what we do : n_nvo_tot_agri / n_nvo_tot
# Support from old policies to agriculture ----

## in farm net income (gross of fixed capital depreciation) ----

basename <- "ratio_n_pro_agrogan_agro_n_fni_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from old policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Old support programs consist of PROCAMPO, PROGAN, and Producción para el Bienestar.",
  "PROCAMPO is a direct per-hectare cash transfer introduced in 1993 to compensate smallholders for trade liberalization under NAFTA, covering staple crops (maize, beans, wheat, sorghum, among others).",
  "PROGAN is a per-head livestock subsidy targeting small cattle, sheep and goat producers. ",
  "Producción para el Bienestar is the successor program to PROCAMPO, relaunched in 2019 under the current administration with a focus on subsistence and indigenous farmers.",
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in total support to agriculture ----

basename <- "ratio_n_pro_agrogan_agro_support_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from old policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Old support programs consist of PROCAMPO, PROGAN, and Producción para el Bienestar.",
  "PROCAMPO is a direct per-hectare cash transfer introduced in 1993 to compensate smallholders for trade liberalization under NAFTA, covering staple crops (maize, beans, wheat, sorghum, among others).",
  "PROGAN is a per-head livestock subsidy targeting small cattle, sheep and goat producers. ",
  "Producción para el Bienestar is the successor program to PROCAMPO, relaunched in 2019 under the current administration with a focus on subsistence and indigenous farmers.",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_support_broad", filter = "recipients_broad"),
  list(universe = "n_recip_support_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Support from new policies to agriculture ----

## in farm net income (gross of fixed capital depreciation) ----

basename <- "ratio_n_nvo_npago_agro_n_fni_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from new policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "New support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "All these subsidies are non-repayables. In particular, not included are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
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
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in total support to agriculture ----

basename <- "ratio_n_nvo_npago_agro_support_decile"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
num_name <- "support from new policies to agriculture"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "New support programs comprise of Sembrando Vida, Agromercados Sociales y Sustentables, Precios de Garantía, Nacional de Fertilizantes, Desarrollo Rural, and other smaller programs.",
  "All these subsidies are non-repayables. In particular, not included are Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_support_broad", filter = "recipients_broad"),
  list(universe = "n_recip_support_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# All transfers from new policies to non agriculture ----

## in support from new policies (all activities) ----

# Repayable transfers from new policies to agriculture ----
## in all transfers to agriculture ----

basename <- "ratio_n_nvo_pago_agro_n_nvo_tot_decile"
num_name <- "repayable transfers from new policies to agriculture"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Repayable transfers to agriculture includes Tandas para el Bienestar and Crédito Ganadero a la Palabra.",
  "Tandas para el Bienestar is an interest-free microcredit programme (MXN 25,000) targeting micro-entrepreneurs excluded from formal financial services, in municipalities with medium to very high marginalization.",
  # " Repaid in monthly instalments, it also provides business training and advisory support. Launched in 2019 under the Secretaría de Economía.",
  "Crédito Ganadero a la Palabra is an interest-free in-kind credit programme targeting small livestock producers (up to 35 animal units), providing breeding cattle, equipment, and veterinary inputs. ",
  # "Repayment is made in kind through the first offspring, with no collateral or credit history required. Launched in 2019 under SADER with an initial budget of MXN 4 billion.",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
  # "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_nvo_tot_broad", filter = "recipients_broad"),
  list(universe = "n_recip_nvo_tot_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Sembrando Vida in agriculture ----

## in support from new policies to agriculture ----

basename <- "ratio_n_sembr_vida_agro_n_nvo_npago_decile"
num_name <- "support from Sembrando Vida"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  "Sembrando Vida is a non-repayable monthly cash transfer of MXN 6,450 (~USD 320) paid directly to smallholder farmers owning or holding 2.5 hectares available for agroforestry, targeting municipalities with high social deprivation.",
  "Launched in 2019 under the Secretaría de Bienestar, it is Mexico's largest agricultural support programme, covering ~430,000 beneficiaries across 24 states in 2024.", # "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.","Support to agriculture comes from old, new or other programs (state and NGOs).",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_nvo_npago_broad", filter = "recipients_broad"),
  list(universe = "n_recip_nvo_npago_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Nacion Fertilizantes in agriculture ----

## in support from new policies ----

basename <- "ratio_n_nacion_fer_agro_n_nvo_npago_decile"
num_name <- "support from National de Fertilizantes"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  #TODO: CHECK THIS CAPTION. The two lines below used to describe SEMBRANDO
  # VIDA (amount, targeting, number of beneficiaries): the text had been
  # copy-pasted from the Sembrando Vida block and was factually wrong for this
  # figure. Replace the placeholder with a real description of Nacional de Fertilizantes
  # (n_nacion_fert_agro) at the same level of detail, then delete this TODO.
  "PLACEHOLDER - TO BE COMPLETED: description of Nacional de Fertilizantes (n_nacion_fert_agro): what the programme pays, to whom, since when, how many beneficiaries.",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_nvo_npago_broad", filter = "recipients_broad"),
  list(universe = "n_recip_nvo_npago_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Others new programs in agriculture ----

## in support from new policies ----

basename <- "ratio_n_otros_prog_agro_n_nvo_npago_decile"
num_name <- "support from other new programs"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
caption_base <- paste(
  #TODO: CHECK THIS CAPTION. The two lines below used to describe SEMBRANDO
  # VIDA (amount, targeting, number of beneficiaries): the text had been
  # copy-pasted from the Sembrando Vida block and was factually wrong for this
  # figure. Replace the placeholder with a real description of the other new support programmes
  # (n_otros_prog_agro) at the same level of detail, then delete this TODO.
  "PLACEHOLDER - TO BE COMPLETED: description of the other new support programmes (n_otros_prog_agro): what the programme pays, to whom, since when, how many beneficiaries.",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_nvo_npago_broad", filter = "recipients_broad"),
  list(universe = "n_recip_nvo_npago_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Precios Garantias in agriculture ----
# n_precios_gar_agro
## in support from new policies ----

#FIXED: this block used to carry the basename of the Sembrando Vida block above
# ("ratio_n_sembr_vida_agro_n_nvo_npago_decile"). set_attribute() therefore
# reloaded Sembrando Vida's numerator and denominator, so Precios de Garantia
# was never estimated and its 12 output files silently overwrote Sembrando
# Vida's. The basename below is the correct one and the matching entry has been
# added to dict_raw in main_script.r.
basename <- "ratio_n_precios_gar_agro_n_nvo_npago_decile"
num_name <- "support from Precios de Garantia"
set_attribute(basename)
if (num %in% pal$var) {
  inherited_col <- pal$col[pal$var == num]
  message("\n---------------------------------------------")
  message(num, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(num, " — No color inherited from comp_analysis")
}
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Ratio of ", base_title)
title_micro <- str_c("Average individual ratio of ", base_title)
#TODO: CHECK THIS CAPTION. The caption shown here used to describe SEMBRANDO
# VIDA, copy-pasted along with the wrong basename. The first line below only
# restates the official programme name from dict_new_variables.csv; complete
# it at the same level of detail as the Sembrando Vida block (amount,
# targeting, number of beneficiaries), then delete the PLACEHOLDER line and
# this TODO.
caption_base <- paste(
  "Precios de Garantia a Productos Alimentarios Basicos is a non-repayable support scheme guaranteeing a minimum purchase price for basic food products.",
  "PLACEHOLDER - TO BE COMPLETED: amount paid, targeting criteria, launch year and number of beneficiaries of Precios de Garantia (n_precios_gar_agro).",
  "Support to agriculture comes from old, new or other programs (state and NGOs).",
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
    if (exists("strat")) den_name else num_name,
    " size"
  ),
  sep = "\n"
)
# UNIVERSE RESTRICTED TO RECIPIENTS.
# The denominator of this ratio is a support aggregate, which is zero for most
# agricultural households, so the household-level ratio is undefined for them and
# the micro/median estimators dropped them silently. Restricting the universe
# makes that part of the definition instead of an implicit footnote. The take-up
# analyses at the end of this script report how large this universe actually is.
universes <- list(
  list(universe = "n_recip_nvo_npago_broad", filter = "recipients_broad"),
  list(universe = "n_recip_nvo_npago_narrow", filter = "recipients_narrow")
)
run_ratio_analysis(
  design = mysvyr,
  d = d,
  num = num,
  den = den,
  strat = strat,
  basename = basename,
  universes = universes,
  estimators = c("macro", "micro", "median"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# n_nvo_tot_agri / n_nvo_tot ----

# ------------------------
# FROM COMP left übrig

#  1 n_agromercados_agro  #2C0B57FF
#  3 n_credito_gan_agro   #FCFFA4FF
#  4 n_desarollo_rur_agro #DD513AFF
#  7 n_nacion_fert_agro   #A82E5FFF
# 10 n_otros_ing_bundled  #FFEA46FF
# 11 n_otros_prog_agro    #F98C0AFF
# 15 n_tand_bien_agro     #FAC127FF
# 16 n_trabajo_bundled    #7C7B78FF
# 17 n_transfer           #BCAF6FFF

# TAKE-UP of agricultural support ----
#
# Companion of the seven ratio analyses above, whose universe is restricted to
# the recipients of a given support aggregate. Those answer "among households
# that receive something, how much does programme X represent?"; these answer
# "how many households receive anything at all, and does coverage vary across
# the income distribution?".
#
# The pair matters because the two can point in opposite directions: a programme
# concentrated on a handful of large farms shows a high ratio and a low take-up,
# while a broad but shallow transfer shows the reverse. Reading either number
# alone is what makes a support scheme look progressive or regressive at will.

takeup_caption <- paste(
  "Share of households of the decile receiving a strictly positive amount from",
  "the support aggregate, within the universe stated above.",
  "Read together with the corresponding ratio figures, which are estimated on",
  "these recipients only.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)

takeup_specs <- list(
  list(
    short = "support",
    label = "any agricultural support (old, new or other programmes)"
  ),
  list(
    short = "nvo_tot",
    label = "the new programmes (repayable and non-repayable)"
  ),
  list(
    short = "nvo_npago",
    label = "the new non-repayable programmes"
  )
)

for (sp in takeup_specs) {
  for (u in list(
    list(suffix = "broad", universe = "n_is_agri_broad", filter = "agri_broad"),
    list(suffix = "narrow", universe = "n_is_agri", filter = "agri_narrow")
  )) {
    run_takeup_analysis(
      design = mysvyr,
      d = d,
      recip_var = str_c("n_recip_", sp$short, "_", u$suffix),
      strat = "n_deciles_total",
      universe = u$universe,
      filter = u$filter,
      basename = str_c(sp$short, "_decile"),
      title = str_c(
        "Share of agricultural households receiving ",
        sp$label,
        ", by income decile"
      ),
      caption = takeup_caption,
      col_bar = col_overall,
      col_overall = "grey30"
    )
  }
}
