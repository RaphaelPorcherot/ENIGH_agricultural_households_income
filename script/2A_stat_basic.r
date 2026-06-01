# TABLE and BOX PLOT of QUANTILES ----
## Table of cutoff points ----
tres_long <- mysvyr |>
  summarize(
    inc = survey_quantile(
      n_ing_equivaled,
      quantiles = seq(0.1, 0.9, 0.1),
      na.rm = TRUE,
      vartype = c("se", "ci"),
      level = 0.99,
      interval_type = "beta",
      qrule = "math"
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("quantile", "stat"),
    names_pattern = "inc_q(\\d+)(?:_(se|low|upp))?"
  ) |>
  mutate(
    Decile = paste0("D", as.numeric(quantile) / 10),
    stat = dplyr::recode(
      stat,
      se = "Standard error",
      low = "Lower bound (CI : 99%)",
      upp = "Higher bound (CI : 99%)",
      .default = "Treshold"
    )
  ) |>
  select(-quantile) |>
  pivot_wider(names_from = stat, values_from = value)

quantile_cutoff <- tres_long
custom_save(quantile_cutoff)

## plot of cutoff point ----
deciles_plot <- left_join(deciles, deciles_ic, by = "quantile")

plot_quantile_cutoff <- ggplot(
  deciles_plot,
  aes(x = factor(quantile), y = revenu, fill = quantile)
) +
  geom_col(alpha = 0.9) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2, color = "black") +
  scale_x_discrete(
    labels = paste0("D", 1:9)
  ) +
  scale_fill_viridis(
    option = "cividis",
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10),
    labels = scales::label_number(scale = 1e-3, suffix = "", big.mark = ",")
  ) +
  labs(
    title = "Decile cut-off for annual income and confidence intervals at 99%",
    subtitle = "Square root equivalence scale",
    x = "Income deciles",
    y = "Current income per capita (thousands of MXN)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  theme_minimal(base_size = 13)

custom_save(plot_quantile_cutoff, type = "fig")
print(plot_quantile_cutoff)

## Detailed table ----
t_mean <- make_tbl("mean (99% CI)", "{mean}", ci_method = "svymean") |>
  modify_header(
    label ~ "**Total current income (thousands of MXN)**",
    all_stat_cols(stat_0 = FALSE) ~ "**{level}**"
  ) |>
  modify_spanning_header(
    all_stat_cols(stat_0 = FALSE) ~ "**Income quintiles**"
  )
t_median <- make_tbl(
  "median (99% CI)",
  "{median}",
  ci_method = "svymedian.beta"
)
t_extreme <- make_tbl("min - max", "{min} - {max}")
t_quart <- make_tbl("Q1 - Q3", "{p25} - {p75}")

quantile_detail <- tbl_stack(
  list(t_mean, t_median, t_extreme, t_quart),
  quiet = TRUE
)

custom_save(as_tibble(quantile_detail), "basic_quantiles_details")

# DATASET description ----

# Calcul du total
total <- mysvyr |>
  summarize(
    proportion = survey_mean() * 100,
    total = survey_total() / 1e6
  ) |>
  select(-ends_with("_se")) |>
  mutate(
    proportion = round(proportion, 1),
    total = round(total, 1),
    across(everything(), as.character)
  )

## Agricultural households ----

summary_is_agri <- mysvyr |>
  group_by(n_is_agri) |>
  summarize(
    proportion = survey_mean(vartype = "ci"),
    total = survey_total(vartype = "ci")
  ) |>
  mutate(
    across(starts_with("proportion"), ~ round(.x * 100, 1)),
    across(starts_with("total"), ~ round(.x / 1e6, 1)),
    proportion = paste0(
      proportion,
      " [",
      proportion_low,
      "-",
      proportion_upp,
      "]"
    ),
    total = paste0(total, " [", total_low, "-", total_upp, "]")
  ) |>
  select(n_is_agri, proportion, total)

summary_is_agri$n_is_agri <- c(
  "agri. (broad sense)",
  "agri. (narrow sense)",
  "non-agri."
)
# Ajouter le label "Total" dans la colonne n_is_agri
total_line <- total
total_line$n_is_agri <- "Total"

summary_is_agri <- bind_rows(summary_is_agri, total_line) |>
  rename(
    "Type" = n_is_agri,
    "prop. (%)" = proportion,
    "n (millions of ind.)" = total
  )
custom_save(summary_is_agri)

## Self-employed households ----

summary_is_SEN <- mysvyr |>
  group_by(n_is_self_employed_narrow) |>
  summarize(
    proportion = survey_mean(vartype = "ci"),
    total = survey_total(vartype = "ci")
  ) |>
  mutate(
    across(starts_with("proportion"), ~ round(.x * 100, 1)),
    across(starts_with("total"), ~ round(.x / 1e6, 1)),
    proportion = paste0(
      proportion,
      " [",
      proportion_low,
      "-",
      proportion_upp,
      "]"
    ),
    total = paste0(total, " [", total_low, "-", total_upp, "]")
  ) |>
  select(n_is_self_employed_narrow, proportion, total)

summary_is_SEN$n_is_self_employed_narrow <- c(
  "not self-employed",
  "self-employed in agri.",
  "self-employed not in agri."
)
total_line <- total
total_line$n_is_self_employed_narrow <- "Total"

summary_is_SEN <- bind_rows(summary_is_SEN, total_line) |>
  rename(
    "Type" = n_is_self_employed_narrow,
    "prop. (%)" = proportion,
    "n (millions of ind.)" = total
  )

custom_save(summary_is_SEN)

# FARM characteristics ----
## Turnover as a function of farm size and production type ----
df_res <- mysvyr |>
  group_by(n_is_agri_broad, n_size_class_agro, n_tipo_prod_agro) |>
  summarise(
    total = survey_total(vartype = "ci", level = 0.99)
  ) |>
  ungroup()
#For consistency reasons we restrict ourself to farms owned by households who we identied as agricultural based on a strictly positive farm net income.
### tbl and plot of absolute levels ----
farm_turnover_size_prod <- df_res |>
  filter(n_is_agri_broad != "not_agri") |>
  select(-n_is_agri_broad) |>
  group_by(n_size_class_agro) |>
  mutate(
    pct = total / sum(total) * 100
  ) |>
  ungroup()

custom_save(farm_turnover_size_prod)

plot_farm_turnover_size_prod <- ggplot(
  farm_turnover_size_prod,
  aes(x = n_size_class_agro, y = total, fill = n_tipo_prod_agro)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8,
    position = position_dodge(width = 0.7)
  ) +
  geom_errorbar(
    aes(ymin = total_low, ymax = total_upp, group = n_tipo_prod_agro),
    width = 0.2,
    alpha = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  scale_fill_viridis_d(
    "Production type",
    option = "rocket",
    begin = .2,
    end = .8
  ) +
  scale_y_continuous(
    labels = function(x) x / 1e2,
    breaks = pretty_breaks(n = 6) # R choisit ~6 breaks "jolis"
  ) +
  labs(
    title = "Mexican farms according to annual turnover and production type",
    x = "Annual turnover (MXN)",
    y = "Number of farms (x100)",
    caption = "Note: Farm types have been defined considering the share of different production in the total revenues of farms.\nA farm is classified as specialised into a given production when its revenues represent at least two thirds of total turnover.\nIn mixed farms the threshold is not reached neither by crops cultivation nor by livestock production."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

custom_save(plot_farm_turnover_size_prod, type = "fig")
print(plot_farm_turnover_size_prod)

### tbl and plot of relative proportions ----

# mysvyr |>
#   filter(n_is_agri_broad == "agri_broad") |>
#   group_by(n_deciles_total) |>
#   summarise(
#     prop = survey_mean(n_tipo_prod_agro == "Livestock", vartype = "ci")
#   )
#
# svymean(
#   ~ I(n_tipo_prod_agro == "Livestock"),
#   subset(mysvyr, n_deciles_total == "D10" & n_is_agri_broad == "agri_broad")
# )
#
# mean(
#   d$n_tipo_prod_agro == "Livestock" &
#     d$n_deciles_total == "D10" &
#     d$n_is_agri_broad == "agri_broad"
# )

farm_turnover_size_prod_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_size_class_agro",
  target_var = "n_tipo_prod_agro",
  # filter_var = "n_is_agri",
  # filter_value = "agri_narrow"
  filter_var = "n_is_agri_broad",
  filter_value = "agri_broad"
) |>
  rename(
    pct = prop,
    pct_low = IC_low,
    pct_upp = IC_high
  ) |>
  mutate(
    pct_low = pct_low * 100,
    pct_upp = pct_upp * 100,
    pct = pct * 100,
  )
custom_save(farm_turnover_size_prod_pct)

# correct levels order
n_size_class_agro_lvl <- levels(as.factor(d$n_size_class_agro))
farm_turnover_size_prod_pct <- farm_turnover_size_prod_pct |>
  mutate(
    n_size_class_agro = factor(
      n_size_class_agro,
      levels = n_size_class_agro_lvl
    )
  ) |>
  arrange(n_size_class_agro, n_size_class_agro)

plot_farm_turnover_size_prod_pct <- ggplot(
  farm_turnover_size_prod_pct,
  aes(x = n_size_class_agro, y = pct, fill = n_tipo_prod_agro)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8,
    # position = position_dodge(width = 0.7)
  ) +
  geom_errorbar(
    data = farm_turnover_size_prod_pct |>
      dplyr::filter(n_tipo_prod_agro == "Crops") |>
      mutate(pct_low = 100 - pct_low, pct_upp = 100 - pct_upp),
    aes(x = n_size_class_agro, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(
    "Production type",
    option = "rocket",
    begin = .2,
    end = .8
  ) +
  labs(
    title = "Mexican farms according to annual turnover and production type",
    x = "Annual turnover (MXN)",
    y = "(%)",
    caption = "Note: Farm types have been defined considering the share of different production in the total revenues of farms.\nA farm is classified as specialised into a given production when its revenues represent at least two thirds of total turnover.\nConfidence intervals are displayed only for the main production category (Crops) for visual clarity. Other categories exhibit similar or slightly larger uncertainty patterns."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

custom_save(plot_farm_turnover_size_prod_pct, type = "fig")
print(plot_farm_turnover_size_prod_pct)
# HOUSEHOLDS characteristics ----
## Prepare tbl ----

tbl <- mysvyr |>
  select(
    n_is_agri_broad,
    n_is_agri,
    n_is_self_employed_narrow,
    n_is_loc_rural,
    tot_integ,
    menores,
    edad_jefe,
    sexo_jefe,
    educa_jefe,
    n_ing_cor_clean,
    n_ing_equivaled,
    n_below_smg_epc,
    est_socio,
    n_etnia,
    n_acc_alim1,
    n_edad_jefe_med
  ) |>
  mutate(
    educa_jefe_group = case_when(
      educa_jefe %in% c(1, 2, 3) ~ "primary or less",
      educa_jefe %in% c(4, 5, 6) ~ "lower secondary",
      educa_jefe %in% c(7, 8) ~ "upper secondary",
      educa_jefe %in% c(9, 10) ~ "tertiary or undergraduate",
      educa_jefe == 11 ~ "postgraduate"
    ),
    # transformer en factor et définir l'ordre
    educa_jefe_group = factor(
      educa_jefe_group,
      levels = c(
        "primary or less",
        "lower secondary",
        "upper secondary",
        "tertiary or undergraduate",
        "postgraduate"
      )
    )
  ) |>
  mutate(
    est_socio = factor(
      est_socio,
      levels = c(1, 2, 3, 4),
      labels = c("low", "lower-middle", "upper-middle", "high")
    ),
    sexo_jefe = factor(
      sexo_jefe,
      levels = c(1, 2),
      labels = c("male", "female")
    ),
    educa_jefe = factor(
      educa_jefe,
      levels = c(1:11),
      labels = c(
        "no formal education",
        "preschool",
        "incomplete primary school (6-12)",
        "completed primary school (6-12)",
        "incomplete middle school (12-15)",
        "completed middle school (12-15)",
        "incomplete high school (15-18)",
        "completed high school (15-18)",
        "incomplete undergraduate/tertiary",
        "completed undergraduate/tertiary",
        "postgraduate"
      )
    ),
    n_is_self_employed_narrow = factor(
      n_is_self_employed_narrow,
      levels = c("sen_agri", "sen_not_agri", "not_sen"),
      labels = c(
        "self-employed in agriculture",
        "self-employed not in agriculture",
        "non self-employed"
      )
    ),
    n_acc_alim1 = factor(
      n_acc_alim1,
      levels = 1:2,
      labels = c(
        "yes",
        "no"
      )
    ),
    n_etnia = factor(
      n_etnia,
      levels = 1:2,
      labels = c(
        "indigenous",
        "non indigenous"
      )
    ),
    n_is_agri_broad = factor(
      n_is_agri_broad,
      levels = c("agri_broad", "not_agri"),
      labels = c(
        "agricultural households",
        "non-agricultural households"
      )
    ),
    n_is_loc_rural = factor(
      n_is_loc_rural,
      levels = c("rural", "not_rural"),
      labels = c(
        "rural areas (locality < 2,500 inhabitants)",
        "non-rural areas"
      )
    )
  ) |>
  select(-educa_jefe)

## agri_broad vs the rest ----
tbl_csv <- tbl |>
  select(-n_edad_jefe_med) |> # filter(decile_total %in% paste0("D", 1:10)) |>
  # filter(decile_total %in% paste0("D", 1:10)) |>
  tbl_svysummary(
    by = n_is_agri_broad,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "Relation to agripastoral activities",
      n_is_self_employed_narrow = "Household's type",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      n_below_smg_epc = "Income relative to minimum wage"
    )
  ) |>
  add_ci(
    conf.level = 0.99,
    method = list(
      all_continuous() ~ "svymean",
      all_categorical() ~ "svyprop.logit"
    ),
    include = everything(),
    statistic = list(
      all_continuous() ~ "{conf.low}, {conf.high}",
      all_categorical() ~
        "{conf.low}%, {conf.high}%"
    ),
    #  pattern = "{stat} (99% CI {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  # modify_footnote(
  # update = list(
  #   all_continuous() ~ "Median (p25, p75). 99% confidence interval reported in a separate column.",
  #   all_categorical() ~ "Weighted percentage. 99% confidence interval reported in a separate column."
  # )
  # )|>
  add_p()

household_char_agri_broad <- as_tibble(tbl_csv, col_labels = TRUE)
custom_save(household_char_agri_broad)
## sen_narrow vs the rest ----
tbl_csv <- tbl |>
  select(-n_is_agri_broad, -n_edad_jefe_med) |> # filter(decile_total %in% paste0("D", 1:10)) |>
  tbl_svysummary(
    by = n_is_self_employed_narrow,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "Relation to agripastoral activities",
      # n_is_self_employed_narrow = "Household's type",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      n_below_smg_epc = "Income relative to minimum wage"
    )
  ) |>
  add_ci(
    conf.level = 0.99,
    method = list(
      all_continuous() ~ "svymean",
      all_categorical() ~ "svyprop.logit"
    ),
    include = everything(),
    statistic = list(
      all_continuous() ~ "{conf.low}, {conf.high}",
      all_categorical() ~
        "{conf.low}%, {conf.high}%"
    ),
    #  pattern = "{stat} (99% CI {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  # modify_footnote(
  # update = list(
  #   all_continuous() ~ "Median (p25, p75). 99% confidence interval reported in a separate column.",
  #   all_categorical() ~ "Weighted percentage. 99% confidence interval reported in a separate column."
  # )
  # )|>
  add_p()

household_char_agri_narrow <- as_tibble(tbl_csv, col_labels = TRUE)
custom_save(household_char_agri_narrow)

## ethny, age, gender ---- 

tbl_agri <- subset(tbl, n_is_agri_broad == "agricultural households")
tbl_csv <- tbl_agri |>
  select(-n_is_agri_broad) |>
  tbl_svysummary(
    by = n_etnia,
    type = list(menores ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "Relation to agripastoral activities",
      n_is_self_employed_narrow = "Occupational status",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      # n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      n_n_below_smg_epc = "Income relative to minimum wage"
    )
  ) |>
  add_ci(
    conf.level = 0.99,
    method = list(
      all_continuous() ~ "svymean",
      all_categorical() ~ "svyprop.logit"
    ),
    include = everything(),
    statistic = list(
      all_continuous() ~ "{conf.low}, {conf.high}",
      all_categorical() ~
        "{conf.low}%, {conf.high}%"
    ),
    #  pattern = "{stat} (99% CI {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  add_p()
ethnic <- as_tibble(tbl_csv, col_labels = TRUE)

tbl_csv <- tbl_agri |>
  select(-edad_jefe, -n_is_agri_broad) |>
  tbl_svysummary(
    by = n_edad_jefe_med,
    type = list(menores ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "Relation to agripastoral activities",
      n_is_self_employed_narrow = "Occupational status",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      # edad_jefe="Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      n_below_smg_epc = "Income relative to minimum wage"
    )
  ) |>
  add_ci(
    conf.level = 0.99,
    method = list(
      all_continuous() ~ "svymean",
      all_categorical() ~ "svyprop.logit"
    ),
    include = everything(),
    statistic = list(
      all_continuous() ~ "{conf.low}, {conf.high}",
      all_categorical() ~
        "{conf.low}%, {conf.high}%"
    ),
    #  pattern = "{stat} (99% CI {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  add_p()
age <- as_tibble(tbl_csv, col_labels = TRUE)

tbl_csv <- tbl_agri |>
  select(-n_edad_jefe_med, -n_is_agri_broad) |> # filter(decile_total %in% paste0("D", 1:10)) |>
  tbl_svysummary(
    by = sexo_jefe,
    type = list(menores ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "Relation to agripastoral activities",
      n_is_self_employed_narrow = "Occupational status",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      # sexo_jefe="Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      n_below_smg_epc = "Income relative to minimum wage"
    )
  ) |>
  add_ci(
    conf.level = 0.99,
    method = list(
      all_continuous() ~ "svymean",
      all_categorical() ~ "svyprop.logit"
    ),
    include = everything(),
    statistic = list(
      all_continuous() ~ "{conf.low}, {conf.high}",
      all_categorical() ~
        "{conf.low}%, {conf.high}%"
    ),
    #  pattern = "{stat} (99% CI {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  add_p()
gender <- as_tibble(tbl_csv, col_labels = TRUE)
# The following warnings were returned during `add_p()`:
# ! For variable `est_socio` (`sexo_jefe`) and "statistic" and "p.value"
#   statistics: Chi-squared approximation may be incorrect
# CAR TROP PEU D'OBS WOMEN  : 793 000
custom_save(ethnic, "household_char_agri_broad_by_ethnicity")
custom_save(gender, "household_char_agri_broad_by_gender")
custom_save(age, "household_char_agri_broad_by_age")

# dr <- list(ethnic, age, gender)
# names(dr) <- c("ethnic", "age", "gender")


