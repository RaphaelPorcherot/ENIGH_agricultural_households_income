# ASSESSING THE EFFECTIVENESS OF AGRICULTURAL POLICIES
# Set graphical parameters ----

col_overall <- "#D55E00"
cols <- brewer.pal(n = 12, name = "Paired")
pairs <- rep(seq_along(cols), each = 2)[seq_along(cols)]
list_cols <- split(cols, pairs)
i <- 0
# Share ----
## Share of support by decile ----
### agri_broad ----

#TODO: todo

### agri_narrow ----

#TODO: todo

# Ratio ----
## Self-consumption in total turnover/production ----
#WARN: we need to understand the discrpenacy between tipoact, which is encoded by INEGHI, and the self-declaration of the actiity which got the support fromsocial programs which may agri in a quite contradictory manner with the first element:
# two possible explanation
# in NOAGRO are classified activities that are in 1 to 3, so not agricultural. But not agriculturla activities may have agriculturla input (fertilizantes for a small shop growing its own food or whatever)
# NVO have been extended to non agricultural activities such as Microcredits for instance
# La présence de bénéficiaires de programmes agricoles dans la table NOAGRO ne constitue pas nécessairement une incohérence statistique. La classification NOAGRO repose sur l’activité du negocio codée par l’enquête, tandis que l’activité associée au programme est auto-déclarée par le répondant. Cette dissociation reflète probablement la forte pluriactivité des ménages ruraux mexicains ainsi que le caractère transversal des nouveaux programmes sociaux, qui peuvent soutenir des activités agricoles secondaires au sein de ménages principalement engagés dans des activités commerciales, industrielles ou de services.
# TODO: check what kind of combination exists in NOAGRO between tipoact and nvo_act1, nvo_act2
#TODO: décider si on le fait aussi pour la somme des deux valeurs de l'autoconsommation

### from agro self employemnet ----

i <- i + 1
col_below <- list_cols[[i]][1]
col_above <- list_cols[[i]][2]

num <- "n_autoconsumo1_agro"
den <- "n_size_val1_agro"
strat <- "n_deciles_total"
basename <- "ratio_autocons_prod_decile"

den_name <- "total agricultural production"
base_title <- str_c(
  "non-repayable policy payments in ",
  den_name,
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

### from non agro self employment ----

#TODO: still somehting to do; self-consumption from agro in all

## Support in total current income ----

#NOTE: The micro and macro estimators of income composition yield very similar results across deciles because the denominator — total household income — is precisely the variable used to construct the deciles, making it relatively homogeneous within each group.
#This contrasts sharply with the autoconsumption-to-production ratio, where the denominator varies by orders of magnitude within deciles, driving a large wedge between the two estimators. For income composition stratified by income deciles, the choice between micro and macro estimators is therefore largely inconsequential, and both can be reported interchangeably.

i <- i + 1
col_below <- list_cols[[i]][1]
col_above <- list_cols[[i]][2]

num <- "n_support_agro"
den <- "n_ing_cor_clean"
strat <- "n_deciles_total"
basename <- "ratio_support_ing_cor_decile"

den_name <- "total current income"
base_title <- str_c(
  "non-repayable policy payments in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

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

i <- i + 1
col_below <- list_cols[[i]][1]
col_above <- list_cols[[i]][2]

num <- "n_support_agro"
den <- "n_ftr1_agro"
strat <- "n_deciles_total"
basename <- "ratio_support_ftr1_decile"

num_name <- "non-repayable policy payments"
den_name <- "farm total resources"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

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

i <- i + 1
col_below <- list_cols[[i]][1]
col_above <- list_cols[[i]][2]

num <- "n_support_agro"
den <- "n_fni_agro"
strat <- "n_deciles_total"
basename <- "ratio_support_fni_decile"

num_name <- "non-repayable policy payments"
den_name <- "farm net income"
base_title <- str_c(
  num_name,
  " in ",
  den_name,
  " across income decile"
)
title_macro <- str_c("Share of ", base_title)
title_micro <- str_c("Average individual share of ", base_title)

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

# --------------

# Support in farm net income ----

# Violet
col_above <- "#6a3d9a"
col_below <- "#cab2d6"

title_macro <- "Share of direct policy payments in farm net income across income decile"
caption_macro <- paste(
  "Farm net income is sales minus all costs, operational or else, net of production taxes. It is gross of fixed capital depreciation.",
  "Direct policy payments correspond to non-repayable financial support received by agricultural activities, including all social programs, old and new. It excludes for instance microcredits.",
  "Bar colors indicate whether the decile is above (darker) or below (lighter) the overall mean of individual ratios.",
  "The dashed black line shows the LOESS trend across deciles.",
  "The red dotted line and shaded band represent the overall ratio and its 99% confidence interval.",
  "Error bars represent 99% confidence intervals.",
  "Source: Based on ENIGH data.",
  sep = "\n"
)
title_micro <- "Average individual share of direct policy payments in farm net income by income decile"
extra_text <- paste(
  "Bars represent the average of household-level ratios within each decile.",
  "Each household contributes equally regardless of farm net income size",
  sep = "\n"
)
caption_micro <- paste(extra_text, caption_macro, sep = "\n")

## MACRO agri_broad ----
# # Vert
# values = c("Above" = "#33a02c", "Below" = "#b2df8a")
# # Rouge/orange
# values = c("Above" = "#e31a1c", "Below" = "#fb9a99")
# # Orange
# values = c("Above" = "#ff7f00", "Below" = "#fdbf6f")
name <- "macro_ratio_support_ftr1_decile"
universe <- "n_is_agri_broad"
filter <- "agri_broad"

strat <- "n_deciles_total"
num <- "n_support_agro"
den <- "n_fni_agro_clean"

tbl <- get_ratio_macro(
  design = mysvyr,
  numerator = num,
  denominator = den,
  strat_var = strat,
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# Ratio of total (macro, this is not a mean of ratio which would be micro)
overall <- get_ratio_macro(
  mysvyr,
  numerator = num,
  denominator = den,
  strat_var = universe, # juste pour avoir le bon subset
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

tbl <- tbl |>
  mutate(
    above_mean = ifelse(
      ratio >= overall$ratio,
      "Above",
      "Below"
    )
  )

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# Graphique
plot <- ggplot(
  tbl,
  aes(x = .data[[names(tbl)[1]]], y = ratio, fill = above_mean)
) +
  # ribbon overall : x en numérique pour couvrir tout le graphe
  annotate(
    "rect",
    xmin = 0.5,
    xmax = 10.5,
    ymin = overall$IC_low,
    ymax = overall$IC_high,
    fill = col_overall,
    alpha = 0.15
  ) +
  geom_col(width = 0.7, alpha = 0.9, color = "white") +
  geom_errorbar(
    aes(ymin = IC_low, ymax = IC_high),
    width = 0.2,
    color = "grey30"
  ) +
  geom_text(
    aes(label = paste0(round(ratio, 1), "%")),
    vjust = 1.5,
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  geom_smooth(
    aes(group = 1, fill = NULL), # annule l'héritage du fill global
    method = "loess",
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = overall$ratio,
    color = col_overall,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_label(
    data = data.frame(x = "D9", y = overall$ratio + 1.2),
    aes(
      x = x,
      y = y,
      label = paste0("Overall: ", round(overall$ratio, 1), "%")
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = col_overall,
    linewidth = 0.2, # <-- ici
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Above" = col_above, "Below" = col_below),
    # values = c("Above" = "#b15928", "Below" = "#ffff99"),
    name = "Comparison to overall ratio"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = title_macro,
    subtitle = subtitle,
    x = "Income decile",
    y = "%",
    caption = caption_macro
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)
custom_save(
  plot,
  str_c(
    "plot_",
    name,
    "_",
    str_remove(filter, ".*_")
  ),
  type = "fig"
)

## MACRO agri_narrow ----

name <- "macro_ratio_support_ftr1_decile"
# universe <- "n_is_agri_broad"
# filter <- "agri_broad"
universe <- "n_is_agri"
filter <- "agri_narrow"

strat <- "n_deciles_total"
num <- "n_support_agro"
den <- "n_fni_agro_clean"

tbl <- get_ratio_macro(
  design = mysvyr,
  numerator = num,
  denominator = den,
  strat_var = strat,
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# Ratio of total (macro, this is not a mean of ratio which would be micro)
overall <- get_ratio_macro(
  mysvyr,
  numerator = num,
  denominator = den,
  strat_var = universe, # juste pour avoir le bon subset
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

tbl <- tbl |>
  mutate(
    above_mean = ifelse(
      ratio >= overall$ratio,
      "Above",
      "Below"
    )
  )

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# Graphique
plot <- ggplot(
  tbl,
  aes(x = .data[[names(tbl)[1]]], y = ratio, fill = above_mean)
) +
  # ribbon overall : x en numérique pour couvrir tout le graphe
  annotate(
    "rect",
    xmin = 0.5,
    xmax = 10.5,
    ymin = overall$IC_low,
    ymax = overall$IC_high,
    fill = col_overall,
    alpha = 0.15
  ) +
  geom_col(width = 0.7, alpha = 0.9, color = "white") +
  geom_errorbar(
    aes(ymin = IC_low, ymax = IC_high),
    width = 0.2,
    color = "grey30"
  ) +
  geom_text(
    aes(label = paste0(round(ratio, 1), "%")),
    vjust = 1.5,
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  geom_smooth(
    aes(group = 1, fill = NULL), # annule l'héritage du fill global
    method = "loess",
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = overall$ratio,
    color = col_overall,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_label(
    data = data.frame(x = "D9", y = overall$ratio + 1.2),
    aes(
      x = x,
      y = y,
      label = paste0("Overall: ", round(overall$ratio, 1), "%")
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = col_overall,
    linewidth = 0.2, # <-- ici
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Above" = col_above, "Below" = col_below),
    name = "Comparison to overall ratio"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = title_macro,
    subtitle = subtitle,
    x = "Income decile",
    y = "%",
    caption = caption_macro
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)
custom_save(
  plot,
  str_c(
    "plot_",
    name,
    "_",
    str_remove(filter, ".*_")
  ),
  type = "fig"
)

## MICRO agri_broad ----

name <- "micro_ratio_support_ftr1_decile"
universe <- "n_is_agri_broad"
filter <- "agri_broad"

strat <- "n_deciles_total"
num <- "n_support_agro"
den <- "n_ftr1_agro"

tbl <- get_ratio_micro(
  design = mysvyr,
  numerator = num,
  denominator = den,
  strat_var = strat,
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# Ratio of total (macro, this is not a mean of ratio which would be micro)
overall <- get_ratio_micro(
  mysvyr,
  numerator = num,
  denominator = den,
  strat_var = universe, # juste pour avoir le bon subset
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

tbl <- tbl |>
  mutate(
    above_mean = ifelse(
      ratio >= overall$ratio,
      "Above",
      "Below"
    )
  )

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# Graphique
plot <- ggplot(
  tbl,
  aes(x = .data[[names(tbl)[1]]], y = ratio, fill = above_mean)
) +
  # ribbon overall : x en numérique pour couvrir tout le graphe
  annotate(
    "rect",
    xmin = 0.5,
    xmax = 10.5,
    ymin = overall$IC_low,
    ymax = overall$IC_high,
    fill = col_overall,
    alpha = 0.15
  ) +
  geom_col(width = 0.7, alpha = 0.9, color = "white") +
  geom_errorbar(
    aes(ymin = IC_low, ymax = IC_high),
    width = 0.2,
    color = "grey30"
  ) +
  geom_text(
    aes(label = paste0(round(ratio, 1), "%")),
    vjust = 1.5,
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  geom_smooth(
    aes(group = 1, fill = NULL), # annule l'héritage du fill global
    method = "loess",
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = overall$ratio,
    color = col_overall,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_label(
    data = data.frame(x = "D9", y = overall$ratio + 1.2),
    aes(
      x = x,
      y = y,
      label = paste0("Overall: ", round(overall$ratio, 1), "%")
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = col_overall,
    linewidth = 0.2, # <-- ici
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Above" = col_above, "Below" = col_below),
    # values = c("Above" = "#b15928", "Below" = "#ffff99"),
    name = "Comparison to overall ratio"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = title_micro,
    subtitle = subtitle,
    x = "Income decile",
    y = "%",
    caption = caption_micro,
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)
custom_save(
  plot,
  str_c(
    "plot_",
    name,
    "_",
    str_remove(filter, ".*_")
  ),
  type = "fig"
)

## MICRO agri_narrow ----

name <- "micro_ratio_support_ftr1_decile"
universe <- "n_is_agri"
filter <- "agri_narrow"

strat <- "n_deciles_total"
num <- "n_support_agro"
den <- "n_ftr1_agro"

tbl <- get_ratio_micro(
  design = mysvyr,
  numerator = num,
  denominator = den,
  strat_var = strat,
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# Ratio of total (macro, this is not a mean of ratio which would be micro)
overall <- get_ratio_micro(
  mysvyr,
  numerator = num,
  denominator = den,
  strat_var = universe, # juste pour avoir le bon subset
  filter_var = universe,
  filter_value = filter
) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

tbl <- tbl |>
  mutate(
    above_mean = ifelse(
      ratio >= overall$ratio,
      "Above",
      "Below"
    )
  )

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# Graphique
plot <- ggplot(
  tbl,
  aes(x = .data[[names(tbl)[1]]], y = ratio, fill = above_mean)
) +
  # ribbon overall : x en numérique pour couvrir tout le graphe
  annotate(
    "rect",
    xmin = 0.5,
    xmax = 10.5,
    ymin = overall$IC_low,
    ymax = overall$IC_high,
    fill = col_overall,
    alpha = 0.15
  ) +
  geom_col(width = 0.7, alpha = 0.9, color = "white") +
  geom_errorbar(
    aes(ymin = IC_low, ymax = IC_high),
    width = 0.2,
    color = "grey30"
  ) +
  geom_text(
    aes(label = paste0(round(ratio, 1), "%")),
    vjust = 1.5,
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  geom_smooth(
    aes(group = 1, fill = NULL), # annule l'héritage du fill global
    method = "loess",
    se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = overall$ratio,
    color = col_overall,
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_label(
    data = data.frame(x = "D9", y = overall$ratio + 1.2),
    aes(
      x = x,
      y = y,
      label = paste0("Overall: ", round(overall$ratio, 1), "%")
    ),
    inherit.aes = FALSE,
    fill = "white",
    color = col_overall,
    linewidth = 0.2, # <-- ici
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Above" = col_above, "Below" = col_below),
    name = "Comparison to overall ratio"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = title_micro,
    subtitle = subtitle,
    x = "Income decile",
    y = "%",
    caption = caption_micro
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)
custom_save(
  plot,
  str_c(
    "plot_",
    name,
    "_",
    str_remove(filter, ".*_")
  ),
  type = "fig"
)
# Composition of income by decile ----
## MACRO pop ----

name <- "macro_composition_income_decile"
universe <- NULL
filter <- NULL
suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"

components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_macro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- calcul overall : gestion NULL ---
overall <- map_dfr(names(components), function(comp_name) {
  if (is.null(universe)) {
    design_tmp <- update(mysvyr, .total = "Total")
    get_ratio_macro(
      design = design_tmp,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = ".total"
    ) |>
      mutate(component = comp_name) # <-- manquait ici
  } else {
    get_ratio_macro(
      design = mysvyr,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = universe,
      filter_var = universe,
      filter_value = filter
    ) |>
      mutate(component = comp_name)
  }
}) |>
  mutate(across(c(ratio, SE, IC_low, IC_high), ~ round(. * 100, 2)))

colnames(overall)[1] <- colnames(tbl)[1]

# dans custom_save
custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", suffix)
)
# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Composition of household income across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — ",
      if (is.null(filter)) "total population" else str_remove(filter, ".*_"),
      " households"
    ),
    x = "Income decile",
    y = "Share of total current household income (%)",
    caption = paste(
      "Notes: Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      "Error bars not shown — see saved table for 99% confidence intervals by component and decile.",
      str_c(
        "Universe: ",
        if (is.null(filter)) "total population" else filter,
        " | Source: Based on ENIGH data."
      ),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", suffix),
  type = "fig"
)

## MACRO agri_broad ----

name <- "macro_composition_income_decile"
universe <- "n_is_agri_broad"
filter <- "agri_broad"
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"
components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_macro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- total overall (toutes deciles confondus) ---
overall <- map_dfr(names(components), function(comp_name) {
  get_ratio_macro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = universe,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Composition of household income across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — ",
      str_remove(filter, ".*_"),
      " households"
    ),
    x = "Income decile",
    y = "Share of total current household income (%)",
    caption = paste(
      "Notes: Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      "Error bars not shown — see saved table for 99% confidence intervals by component and decile.",
      str_c("Universe: ", filter, " | Source: Based on ENIGH data."),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", str_remove(filter, ".*_")),
  type = "fig"
)

## MACRO agri_narrow ----

name <- "macro_composition_income_decile"
universe <- "n_is_agri"
filter <- "agri_narrow"
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"

components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_macro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- total overall (toutes deciles confondus) ---
overall <- map_dfr(names(components), function(comp_name) {
  get_ratio_macro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = universe,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )
colnames(overall)[1] <- colnames(tbl)[1]

custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", str_remove(filter, ".*_"))
)

# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Composition of household income across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — ",
      str_remove(filter, ".*_"),
      " households"
    ),
    x = "Income decile",
    y = "Share of total current household income (%)",
    caption = paste(
      "Notes: Shares represent the ratio of aggregated component totals to aggregated total income within each decile (macro estimator).",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      "Error bars not shown — see saved table for 99% confidence intervals by component and decile.",
      str_c("Universe: ", filter, " | Source: Based on ENIGH data."),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", str_remove(filter, ".*_")),
  type = "fig"
)

## MICRO pop ----

name <- "micro_composition_income_decile"
universe <- NULL
filter <- NULL
suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"
components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_micro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- calcul overall : gestion NULL ---
overall <- map_dfr(names(components), function(comp_name) {
  if (is.null(universe)) {
    design_tmp <- update(mysvyr, .total = "Total")
    get_ratio_micro(
      design = design_tmp,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = ".total"
    ) |>
      mutate(component = comp_name) # <-- manquait ici
  } else {
    get_ratio_macro(
      design = mysvyr,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = universe,
      filter_var = universe,
      filter_value = filter
    ) |>
      mutate(component = comp_name)
  }
}) |>
  mutate(across(c(ratio, SE, IC_low, IC_high), ~ round(. * 100, 2)))

colnames(overall)[1] <- colnames(tbl)[1]

# dans custom_save
custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", suffix)
)
# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Average household income composition across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — average individual shares — ",
      if (is.null(filter)) "total population" else str_remove(filter, ".*_"),
      " households"
    ),
    y = "Mean share of total household income (%)",
    caption = paste(
      "Notes: Shares represent the average of household-level ratios (component / total income) within each decile (micro estimator).",
      "Each household contributes equally regardless of income size, reflecting the 'typical household' rather than the aggregate structure.",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      str_c(
        "Universe: ",
        if (is.null(filter)) "total population" else filter,
        " | Source: Based on ENIGH data."
      ),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", suffix),
  type = "fig"
)

## MICRO agri_broad ----

name <- "micro_composition_income_decile"
universe <- "n_is_agri_broad"
filter <- "agri_broad"
suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"
components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_micro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- calcul overall : gestion NULL ---
overall <- map_dfr(names(components), function(comp_name) {
  if (is.null(universe)) {
    design_tmp <- update(mysvyr, .total = "Total")
    get_ratio_micro(
      design = design_tmp,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = ".total"
    ) |>
      mutate(component = comp_name) # <-- manquait ici
  } else {
    get_ratio_macro(
      design = mysvyr,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = universe,
      filter_var = universe,
      filter_value = filter
    ) |>
      mutate(component = comp_name)
  }
}) |>
  mutate(across(c(ratio, SE, IC_low, IC_high), ~ round(. * 100, 2)))

colnames(overall)[1] <- colnames(tbl)[1]

# dans custom_save
custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", suffix)
)
# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Average household income composition across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — average individual shares — ",
      if (is.null(filter)) "total population" else str_remove(filter, ".*_"),
      " households"
    ),
    y = "Mean share of total household income (%)",
    caption = paste(
      "Notes: Shares represent the average of household-level ratios (component / total income) within each decile (micro estimator).",
      "Each household contributes equally regardless of income size, reflecting the 'typical household' rather than the aggregate structure.",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      str_c(
        "Universe: ",
        if (is.null(filter)) "total population" else filter,
        " | Source: Based on ENIGH data."
      ),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", suffix),
  type = "fig"
)

## MICRO agri_narrow ----

name <- "micro_composition_income_decile"
universe <- "n_is_agri"
filter <- "agri_narrow"
suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
strat <- "n_deciles_total"
den <- "n_ing_cor_clean"
components <- list(
  agri = "n_fni_agro_clean",
  no_agri = "n_ingr_noagro_clean",
  wage = "n_trabajo",
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

# --- calcul ---
tbl <- map_dfr(names(components), function(comp_name) {
  get_ratio_micro(
    design = mysvyr,
    numerator = components[[comp_name]],
    denominator = den,
    strat_var = strat,
    filter_var = universe,
    filter_value = filter
  ) |>
    mutate(component = comp_name)
}) |>
  mutate(
    ratio = round(ratio * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  )

# correct levels order
strat_lvl <- levels(as.factor(d[[strat]]))
tbl <- tbl |>
  mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
  arrange(.data[[strat]])

# --- calcul overall : gestion NULL ---
overall <- map_dfr(names(components), function(comp_name) {
  if (is.null(universe)) {
    design_tmp <- update(mysvyr, .total = "Total")
    get_ratio_micro(
      design = design_tmp,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = ".total"
    ) |>
      mutate(component = comp_name) # <-- manquait ici
  } else {
    get_ratio_macro(
      design = mysvyr,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = universe,
      filter_var = universe,
      filter_value = filter
    ) |>
      mutate(component = comp_name)
  }
}) |>
  mutate(across(c(ratio, SE, IC_low, IC_high), ~ round(. * 100, 2)))

colnames(overall)[1] <- colnames(tbl)[1]

# dans custom_save
custom_save(
  bind_rows(tbl, overall),
  str_c(name, "_", suffix)
)
# --- plot ---

overall_plot <- overall |>
  mutate(!!strat := "Total")
colnames(overall_plot)[1] <- strat # au cas où

plot_data <- tbl |>
  bind_rows(overall_plot) |>
  mutate(
    !!strat := factor(.data[[strat]], levels = strat_lvl_with_total),
    component = factor(
      component,
      levels = names(component_labels),
      labels = component_labels
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = .data[[names(plot_data)[1]]], y = ratio, fill = component)
) +
  geom_col(width = 0.7, alpha = 0.8, color = "white") +
  geom_text(
    aes(label = ifelse(ratio > 5, paste0(round(ratio, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5
  ) +
  geom_vline(
    xintercept = 10.5, # entre D10 (position 10) et Total (position 11)
    linetype = "dashed",
    color = "grey50"
  ) +
  scale_fill_viridis_d(
    option = "cividis",
    direction = 1,
    name = "Income component"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title = "Average household income composition across income deciles",
    subtitle = str_c(
      "Breakdown by main income sources — average individual shares — ",
      if (is.null(filter)) "total population" else str_remove(filter, ".*_"),
      " households"
    ),
    y = "Mean share of total household income (%)",
    caption = paste(
      "Notes: Shares represent the average of household-level ratios (component / total income) within each decile (micro estimator).",
      "Each household contributes equally regardless of income size, reflecting the 'typical household' rather than the aggregate structure.",
      "Farm net income: agricultural self-employment income net of production taxes with policy-related payments.",
      "Transfers: monetary or in-kind contributions received by household members without compensation.",
      "Other income sources: capital income, imputed rent and other residual income sources.",
      str_c(
        "Universe: ",
        if (is.null(filter)) "total population" else filter,
        " | Source: Based on ENIGH data."
      ),
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot)

custom_save(
  plot,
  str_c("plot_", name, "_", suffix),
  type = "fig"
)
# THE END ----
