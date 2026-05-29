# SHARE ANALYSIS

# NOTE:
# Two complementary analytical lenses on agricultural income variables:
# 1. RATIO ANALYSIS (run_ratio_analysis, informed by run_composition_analysis)
#    Question: "How important is VAR relative to another variable, for households in a given universe?"
#    e.g. What share of total household income comes from agroforestry subsidies, by decile?
#    Composition analysis first identifies which components are reliably estimated;
#    ratio analysis then quantifies their relative weight.
# 2. SHARE ANALYSIS (run_share_analysis)
#    Question: "How is the aggregate of VAR distributed across households, by decile?"
#    Macro: what fraction of the total (e.g. total subsidy pool) accrues to each decile?
#           Compared against the equal-distribution benchmark (proportional to decile size).
#    Micro: what is the average individual household's share of the total, by decile?
#           Compared against the mean individual share across all households in the universe.

#TODO: reprendre ratio pour voir ce qu'on peut regarder aussi
# share of self-consumption in total self-consumption

#TODO: reprendre la caption de share_macro mais aussi share_micro pour expliquer ce que ca veut dire d'être au-dessus ou en dessouss

# in COMP: of income by decile ----
## Share of current income by decile ----
#TODO: maybe but likely to be quite irrelevant

## Share of farm net income by decile ----

basename <- "share_n_fni_agro_clean_decile"
set_attribute(basename)

num_name <- "farm net income"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
caption_base <- paste(
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
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

# in COMP: of support (old, new, other programs) to agriculture by decile ----
## Share of support to agriculture by decile ----

basename <- "share_support_agro_decile"
set_attribute(basename)

num_name <- "support from all policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
caption_base <- paste(
  "Non-repayable payments is financial support to agricultural activities, including all social programs (old and new) and all subsidies received from state or NGOs. It excludes for instance microcredits.",
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
## Share of support from new programs to agriculture by decile ----

basename <- "share_nvo_npago_agro_decile"
set_attribute(basename)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}

num_name <- "support from new policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
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
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}

num_name <- "support from old policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
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

basename <- "share_n_apoyo_npago_agro_decile"
set_attribute(basename)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}

num_name <- "support from other policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
caption_base <- paste(
  "Support from other policies to agriculture are non-repayable transfers received from state or NGOs. There are repayable transfers from such actors, but they are not considered here.",
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

# in COMP: of support from new policies to agriculture by decile  ----

#TODO: check micro composition to assess whether focus on some specific variables may be relevant
# Credito
# Precios Garantia

## Share of support from new policies by decile ----
#TODO: maybe but likely to be quite irrelevant

## Share of sembrando vido by decile ----

basename <- "share_n_sembr_vida_agro_decile"
set_attribute(basename)
num_name <- "support from Sembrando Vida"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}

title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
caption_base <- paste(
  "Sembrando Vida is a non-repayable monthly cash transfer of MXN 6,450 (~USD 320) paid directly to smallholder farmers owning or holding 2.5 hectares available for agroforestry, targeting municipalities with high social deprivation.",
  "Launched in 2019 under the Secretaría de Bienestar, it is Mexico's largest agricultural support programme, covering ~430,000 beneficiaries across 24 states in 2024.", # "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
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

# in COMP: of all transfers (bundled as npago et pago) from new policies to agriculture by decile ----

## Share of all transfers from new policies by decile ----
#TODO: maybe but likely to be quite irrelevant

## Share of repayable transfers by decile ----
#NOTE: support from new is already done above

basename <- "share_n_nvo_pago_agro_decile"
set_attribute(basename)
num_name <- "repayable transfers from new policies to agriculture"
base_title <- str_c(
  num_name,
  if (is_share_plot) NULL else str_c(" in ", den_name),
  " across income decile"
)
# Check color inheritance from comp_analysis
if (target %in% pal$var) {
  inherited_col <- pal$col[pal$var == target]
  message("\n---------------------------------------------")
  message(target, " — Color inherited from comp_analysis: ", inherited_col)
  message("---------------------------------------------\n")
} else {
  message(target, " — No color inherited from comp_analysis")
}
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average household share of ", base_title)
caption_base <- paste(
  "Tandas para el Bienestar is an interest-free microcredit programme (MXN 25,000) targeting micro-entrepreneurs excluded from formal financial services, in municipalities with medium to very high marginalization.",
  # " Repaid in monthly instalments, it also provides business training and advisory support. Launched in 2019 under the Secretaría de Economía.",
  "Crédito Ganadero a la Palabra is an interest-free in-kind credit programme targeting small livestock producers (up to 35 animal units), providing breeding cattle, equipment, and veterinary inputs. ",
  # "Repayment is made in kind through the first offspring, with no collateral or credit history required. Launched in 2019 under SADER with an initial budget of MXN 4 billion.",
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
