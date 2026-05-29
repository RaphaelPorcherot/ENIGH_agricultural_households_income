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
  estimators = c("macro", "micro"), # ou juste c("macro")
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
  estimators = c("macro", "micro"), # ou juste c("macro")
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
  estimators = c("macro", "micro"), # ou juste c("macro")
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
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

# Support to agriculture ---- 
# n_support_agro 

## in total current income ----

#NOTE: The micro and macro estimators of income composition yield very similar results across deciles because the denominator — total household income — is precisely the variable used to construct the deciles, making it relatively homogeneous within each group.
#This contrasts sharply with the autoconsumption-to-production ratio, where the denominator varies by orders of magnitude within deciles, driving a large wedge between the two estimators. For income composition stratified by income deciles, the choice between micro and macro estimators is therefore largely inconsequential, and both can be reported interchangeably.

basename <- "ratio_support_ing_cor_decile"
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
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in farm total ressources (entrate aziendale) ----

basename <- "ratio_support_ftr1_decile"
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
  estimators = c("macro", "micro"), # ou juste c("macro")
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

basename <- "ratio_support_fni_decile"
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
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

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
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in total transfers to agriculture ----

# Support from new policies to agriculture ---- 
#  8 n_nvo_npago_agro     #FE9F6DFF

## in farm net income (gross of fixed capital depreciation) ----
## in total transfers to agriculture ----
## in total support (to agri or not agri) ----

# Repayable transfers from new poliicies to agriculture ----
#  9 n_nvo_pago_agro      #49C1AD

## in farm net income (gross of fixed capital depreciation) ----
## in total transfers to agriculture ----

# Sembrando Vida in agriculture ---- 
# 14 n_sembr_vida_agro    #000004FF

## in support to agriculture ----
## in total transfers to agriculture ----

# Precios Garantias in agriculture ---- 
# 12 n_precios_gar_agro   #6B186EFF

## in support to agriculture ----
## in total transfers to agriculture ----

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
