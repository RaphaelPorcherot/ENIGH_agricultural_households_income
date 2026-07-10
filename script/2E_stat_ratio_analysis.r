
# Self-consumption ----
## Agricultural: in total turnover/production ----

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

# Support to agriculture (n_support_agro)----

## in total current income ----


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
  estimators = c("macro", "micro"), # ou juste c("macro")
  title_macro = title_macro,
  title_micro = title_micro,
  caption_base = caption_base,
  extra_text = extra_text,
  col_above = col_above,
  col_below = col_below,
  col_overall = col_overall
)

## in all support (to agri or not agri) ----

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
  estimators = c("macro", "micro"), # ou juste c("macro")
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
  estimators = c("macro", "micro"), # ou juste c("macro")
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

# Precios Garantias in agriculture ----
# n_precios_gar_agro
## in support from new policies ----

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
