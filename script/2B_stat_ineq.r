# Farm dual's structure ----
## Shares of n_fni by decile ----

share_fni_decile_pct <- get_share_macro(
  design = mysvyr,
  target_var = "n_fni_agro_clean",
  strat_var = "n_deciles_total"
) |>
  mutate(
    share = round(share * 100, 2),
    SE = round(SE * 100, 2),
    IC_low = round(IC_low * 100, 2),
    IC_high = round(IC_high * 100, 2)
  ) |>
  select(-target_var)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
share_fni_decile_pct <- share_fni_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, share_fni_decile_pct)

custom_save(share_fni_decile_pct)

## Distribution of agri household and of fni across decile ----

pop_tot <- mysvyr |>
  group_by(n_deciles_total) |>
  summarise(
    n = survey_total(
      vartype = c("se", "ci"),
      level = 0.99,
      interval_type = "beta"
    )
  ) |>
  ungroup() |>
  rename(decile = n_deciles_total) |>
  mutate(
    pct_low = 100 * n_low / sum(n),
    pct_upp = 100 * n_upp / sum(n),
    pct = 100 * n / sum(n),
    type = "All households",
    cols = viridis(10, option = "cividis", direction = 1)[row_number()]
  ) |>
  select(decile, pct, everything(), -matches("^n"))

pop_sub <- mysvyr |>
  group_by(n_is_agri_broad, n_deciles_total) |>
  summarise(
    n = survey_total(
      vartype = c("se", "ci"),
      level = 0.99,
      interval_type = "beta"
    )
  ) |>
  ungroup() |>
  filter(n_is_agri_broad == "agri_broad") |>
  select(-n_is_agri_broad) |>
  rename(decile = n_deciles_total) |>
  mutate(
    pct_low = 100 * n_low / sum(n),
    pct_upp = 100 * n_upp / sum(n),
    pct = 100 * n / sum(n),
    type = "Agricultural households",
    cols = viridis(10, option = "magma", direction = 1)[6]
  ) |>
  select(decile, pct, everything(), -matches("^n"))

pop <- bind_rows(pop_tot, pop_sub)

share <- share_fni_decile_pct |>
  mutate(type = "Farm net income") |>
  rename(decile = n_deciles_total) |>
  select(-SE) |>
  mutate(
    cols = viridis(10, option = "plasma", direction = -1)[9]
  ) |>
  set_names(colnames(pop))
df_plot <- bind_rows(pop, share) |>
  mutate(
    decile = factor(decile, levels = paste0("D", 1:10))
  )
df_save <- df_plot |> select(-cols)
custom_save(df_save, "agri_house_fni_decile_pct")

# Définir les couleurs spécifiques pour les deux types
cols_manual <- df_plot |>
  filter(type %in% c("Agricultural households", "Farm net income")) |>
  distinct(type, cols) |>
  deframe() # crée un named vector type -> color
df_plot <- df_plot |>
  mutate(
    fill_var = ifelse(
      type %in% c("Agricultural households", "Farm net income"),
      type,
      paste0("no_legend_", type, 1:10)
    )
  )

fill_var_manual <- df_plot |> select(fill_var, cols) |> deframe()

plot_agri_house_fni_decile_pct <- ggplot(df_plot, aes(x = decile)) +
  # 1️⃣ All households
  geom_col(
    data = df_plot |> filter(type == "All households"),
    aes(y = pct, fill = fill_var),
    alpha = 0.5,
    width = 0.9,
    show.legend = FALSE
  ) +
  # geom_errorbar(
  #   data = df_plot |> filter(type == "All households"),
  #   aes(ymin = pct_low, ymax = pct_upp),
  #   width = 0.2,
  #   alpha = 0.5
  # ) +
  # 2️⃣ Agricultural households & Farm net income
  geom_col(
    data = df_plot |>
      filter(type %in% c("Agricultural households", "Farm net income")),
    aes(y = pct, fill = fill_var, group = type),
    width = 0.5,
    position = position_dodge(width = 0.6),
    alpha = 0.8
  ) +
  geom_errorbar(
    data = df_plot |>
      filter(type %in% c("Agricultural households", "Farm net income")),
    aes(ymin = pct_low, ymax = pct_upp, group = type),
    width = 0.2,
    position = position_dodge(width = 0.6)
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 10) # R choisit ~6 breaks "jolis"
  ) +
  scale_fill_manual(
    values = fill_var_manual,
    breaks = c("Agricultural households", "Farm net income") # seulement ces 2 dans la légende
  ) +
  labs(
    title = "Distribution of agricultural households and farm net income across income deciles of the whole population",
    y = "(%)",
    x = "Income deciles",
    fill = "" # titre vide pour la légende
  ) +
  theme_minimal(base_size = 14)

custom_save(plot_agri_house_fni_decile_pct, type = "fig")
print(plot_agri_house_fni_decile_pct)

## Farm annual turnover across income decile ----
# d |> select(n_size_class_agro) |> distinct()
# mysvyr |>
#   filter(n_is_agri_broad == "agri_broad") |>
#   group_by(n_deciles_total) |>
#   summarise(
#     prop = survey_mean(n_size_class_agro == "[0; 2 000]", vartype = "ci")
#   )
#
# svymean(
#   ~ I(n_acc_alim1 == 1),
#   subset(mysvyr, n_deciles_total == "D10" & n_is_agri_broad == "agri_broad")
# )
# mean(
#   d$n_acc_alim1 == 1 &
#     d$n_deciles_total == "D10" &
#     d$n_is_agri_broad == "agri_broad"
# )
# d |> filter(n_is_agri == "agri_narrow") |> group_by(n_deciles_total) |> summarise(mean_ing = mean(n_ing_cor_clean))

### agri_broad ----
#INFO: chez les riches mexicains, la possession d’une ferme n’est pas uniquement productive : il y a plus de fermiers dans D10 que dans D9, mais manifestement les D10 ce n'est pas que de l'industrie agricole à grande échelle. Les fermes dans D10 ne sont pas la source principale de la richesse ? Pourtant effectivement qqchose comme 70 % du revenus de D10 vient des fermes
# Une minorité de très grosses fermes capte l’essentiel du revenu agricole: D10 semble être lui-même dual !
# D9 = bourgeoisie agricole productive
# grandes exploitations commerciales
# relativement homogènes
# revenus encore diversifiés
# D10 = élite économique rurale hybride
# très grandes exploitations ultra-rentables
# ménages riches avec petites fermes
# forte dispersion patrimoniale

farm_turnover_decile_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_size_class_agro",
  filter_var = "n_is_agri_broad",
  filter_value = "agri_broad"
  # filter_var = "n_is_agri",
  # filter_value = "agri_narrow"
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
custom_save(farm_turnover_decile_pct)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
farm_turnover_decile_pct <- farm_turnover_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, n_size_class_agro)

plot_farm_turnover_decile_broad_pct <- ggplot(
  farm_turnover_decile_pct,
  aes(x = n_deciles_total, y = pct, fill = n_size_class_agro)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8,
    # position = position_dodge(width = 0.7)
  ) +
  geom_errorbar(
    data = farm_turnover_decile_pct |>
      dplyr::filter(n_size_class_agro == "[0; 2 000]"),
    aes(x = n_deciles_total, ymin = 100 - pct_low, ymax = 100 - pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(
    "Turnover (MXN)",
    option = "mako",
    begin = 1,
    end = 0.2
  ) +
  labs(
    title = "Mexican farms according to annual turnover and income decile of farmers",
    subtitle = "Universe: broad definition of agricultural households",
    x = "Income decile",
    y = "(%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

custom_save(plot_farm_turnover_decile_broad_pct, type = "fig")
print(plot_farm_turnover_decile_broad_pct)

### agri_narrow ----

farm_turnover_decile_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_size_class_agro",
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
  filter_var = "n_is_agri",
  filter_value = "agri_narrow"
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
custom_save(farm_turnover_decile_pct)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
farm_turnover_decile_pct <- farm_turnover_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, n_size_class_agro)

plot_farm_turnover_decile_narrow_pct <- ggplot(
  farm_turnover_decile_pct,
  aes(x = n_deciles_total, y = pct, fill = n_size_class_agro)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8,
    # position = position_dodge(width = 0.7)
  ) +
  geom_errorbar(
    data = farm_turnover_decile_pct |>
      dplyr::filter(n_size_class_agro == "[0; 2 000]"),
    aes(x = n_deciles_total, ymin = 100 - pct_low, ymax = 100 - pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_d(
    "Turnover (MXN)",
    option = "mako",
    begin = 1,
    end = 0.2
  ) +
  labs(
    title = "Mexican farms according to annual turnover and income decile of farmers",
    subtitle = "Universe: narrow definition of agricultural households",
    x = "Income decile",
    y = "(%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

custom_save(plot_farm_turnover_decile_narrow_pct, type = "fig")
print(plot_farm_turnover_decile_narrow_pct)

#NOTE: the previous observation applies, D10 agri households have smaller farm than D9.

# Income inequalities compared ----
## on agri_broad ----
### Gini ----
myconv <- mysvyr |> convey_prep()
gini_result <- myconv |>
  group_by(n_is_agri_broad) |>
  summarise(
    gini_square = survey_gini(n_ing_equivaled, vartype = "ci"),
    .groups = "drop"
  )

gini_result_renamed <- gini_result |>
  mutate(
    Group = recode(
      n_is_agri_broad,
      "agri_broad" = "Agricultural households (broad)",
      "not_agri" = "Non-agricultural households"
    )
  ) |>
  mutate(
    gini_square = round(gini_square, 3),
    gini_square_low = round(gini_square_low, 3),
    gini_square_upp = round(gini_square_upp, 3)
  ) |>
  rename(
    "Gini coefficient" = gini_square,
    "Lower bound (CI 99%)" = gini_square_low,
    "Upper bound (CI 99%)" = gini_square_upp
  ) |>
  select(-n_is_agri_broad) |>
  select(Group, everything())

# kable(gini_result_renamed,
#       caption = "Gini coefficient of net income for agricultural (broad) vs non-agricultural households, with 99% confidence intervals (square root equivalence scale",
#       align = "c") |>
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                 full_width = FALSE,
#                 position = "center") |>
#   row_spec(0, bold = TRUE, color = "white", background = "#4B8BBE")

custom_save(gini_result_renamed, "gini_agri_broad")

gini_lbl <- gini_result |>
  mutate(
    txt = paste0(
      "<i>",
      c("agri", "not agri"),
      "</i>:<br>",
      round(gini_square, 3),
      "  [",
      round(gini_square_low, 3),
      "-",
      round(gini_square_upp, 3),
      "]"
    )
  ) |>
  select(txt) |>
  pull()

gini_lbl <- data.frame(
  x = .1,
  y = .9,
  label = paste0(
    "<b>",
    "gini index:<br><br>",
    "</b>",
    gini_lbl[1],
    "<br>",
    gini_lbl[2]
  )
)

### Lorenz plot and tbl ----

ggplot(df_plot, aes(quantile)) +
  geom_ribbon(aes(ymin = IC_lower, ymax = IC_upper, group = group), alpha = 0.3)

myconv <- convey_prep(mysvyr)
# svylorenz(~ n_ing_equivaled,
#   myconv,
#   quantiles= seq(0,1,.05),
#   na.rm=TRUE,
#   plot=TRUE
# )
#
z99 <- qnorm(0.995)

lorenz_agri_broad <- invisible(survey::svyby(
  ~n_ing_equivaled,
  ~n_is_agri_broad,
  myconv,
  convey::svylorenz,
  quantiles = seq(0, 1, .05),
  na.rm = TRUE,
  color = "purple",
  plot = FALSE
))

L_values <- as.data.frame(lorenz_agri_broad[, grep(
  "^L",
  names(lorenz_agri_broad)
)])
se_L_values <- as.data.frame(lorenz_agri_broad[, grep(
  "^se.L",
  names(lorenz_agri_broad)
)])
quantiles <- as.numeric(gsub("^L\\(|\\)$", "", names(L_values)))

df_plot <- data.frame(
  quantile = rep(quantiles, 2),
  L = c(as.numeric(L_values[1, ]), as.numeric(L_values[2, ])),
  se_L = c(as.numeric(se_L_values[1, ]), as.numeric(se_L_values[2, ])),
  group = rep(c("agri_broad", "not_agri"), each = length(quantiles))
)
df_plot$IC_lower <- df_plot$L - z99 * df_plot$se_L
df_plot$IC_upper <- df_plot$L + z99 * df_plot$se_L

plot_lorenz_agri_broad <- ggplot(df_plot, aes(x = quantile)) +
  geom_ribbon(
    aes(ymin = IC_lower, ymax = IC_upper, group = group),
    # color = NA,
    alpha = 0.2
  ) +
  geom_line(aes(y = L, color = group), linewidth = 1) +
  scale_color_manual(
    values = c(
      "agri_broad" = viridis(10, option = "magma", direction = 1)[6],
      "not_agri" = viridis(10, option = "cividis", direction = 1)[3]
    ),
    labels = c("Agricultural households (broad)", "Non-agricultural") # Renaming legend labels
  ) +
  # scale_fill_manual(
  #   values = c(
  #     "agri_broad" = viridis(10, option = "inferno", direction = 1)[10],
  #     "not_agri" = viridis(10, option = "inferno", direction = 1)[1]
  #   ),
  #   labels = c("Agricultural households (broad)", "Non-agricultural") # Renaming legend labels
  # ) +
  geom_richtext(
    data = gini_lbl,
    aes(x = x, y = y, label = label),
    fill = "white", # fond blanc derrière le texte
    label.color = "black",
    hjust = 0,
    vjust = 1,
    size = 5
  ) +
  annotate(
    "segment",
    x = 0,
    xend = 1,
    y = 0,
    yend = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Lorenz curves with 99% confidence intervals",
    subtitle = "Square root equivalence scale",
    y = "Cumulative share of equivaled income",
    x = "Cumulative share of population",
    color = "group"
    # fill = "group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )
print(plot_lorenz_agri_broad)

custom_save(df_plot, "lorenz_agri_broad")
custom_save(plot_lorenz_agri_broad, type = "fig")

## on agri_narrow ----
### Gini ----

myconv <- mysvyr |> convey_prep()
gini_result <- myconv |>
  group_by(n_is_self_employed_narrow) |>
  summarise(
    gini_square = survey_gini(n_ing_equivaled, vartype = "ci"),
    .groups = "drop"
  ) |>
  filter(n_is_self_employed_narrow != "not_sen")

gini_result_renamed <- gini_result |>
  mutate(
    Group = recode(
      n_is_self_employed_narrow,
      "sen_agri" = "Self-employed agricultural households",
      "sen_not_agri" = "Self-employed non-agricultural households "
    )
  ) |>
  mutate(
    gini_square = round(gini_square, 3),
    gini_square_low = round(gini_square_low, 3),
    gini_square_upp = round(gini_square_upp, 3)
  ) |>
  rename(
    "Gini coefficient" = gini_square,
    "Lower bound (CI 99%)" = gini_square_low,
    "Upper bound (CI 99%)" = gini_square_upp
  ) |>
  select(-n_is_self_employed_narrow) |>
  select(Group, everything())
#
# kable(gini_result_renamed,
#       caption = "Gini coefficient of net income for self-employed agricultural (narrow) vs non-agricultural households, with 99% confidence intervals (square root equivalence scale)",
#       align = "c") |>
#   kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
#                 full_width = FALSE,
#                 position = "center") |>
#   row_spec(0, bold = TRUE, color = "white", background = "#4B8BBE")

custom_save(gini_result_renamed, "gini_agri_narrow")

gini_lbl <- gini_result |>
  mutate(
    txt = paste0(
      "<i>",
      c("agri self-emp", "not agri self-emp"),
      "</i>:<br>",
      round(gini_square, 3),
      "  [",
      round(gini_square_low, 3),
      "-",
      round(gini_square_upp, 3),
      "]"
    )
  ) |>
  select(txt) |>
  pull()
gini_lbl <- data.frame(
  x = .1,
  y = .9,
  label = paste0(
    "<b>",
    "gini index:<br><br>",
    "</b>",
    gini_lbl[1],
    "<br>",
    gini_lbl[2]
  )
)

### Lorenz plot and tbl ----

myconv <- convey_prep(mysvyr)
# svylorenz(~ n_ing_equivaled,
#   myconv,
#   quantiles= seq(0,1,.05),
#   na.rm=TRUE,
#   plot=TRUE
# )
#
z99 <- qnorm(0.995)

lorenz_self_employed_narrow <- survey::svyby(
  ~n_ing_equivaled,
  ~n_is_self_employed_narrow,
  myconv,
  convey::svylorenz,
  quantiles = seq(0, 1, .05),
  na.rm = TRUE,
  color = "purple",
  plot = FALSE
)

L_values <- as.data.frame(lorenz_self_employed_narrow[, grep(
  "^L",
  names(lorenz_self_employed_narrow)
)])
L_values <- L_values[-1, ]
se_L_values <- as.data.frame(lorenz_self_employed_narrow[, grep(
  "^se.L",
  names(lorenz_self_employed_narrow)
)])
se_L_values <- se_L_values[-1, ]
quantiles <- as.numeric(gsub("^L\\(|\\)$", "", names(L_values)))

df_plot <- data.frame(
  quantile = rep(quantiles, 2),
  L = c(as.numeric(L_values[1, ]), as.numeric(L_values[2, ])),
  se_L = c(as.numeric(se_L_values[1, ]), as.numeric(se_L_values[2, ])),
  group = rep(c("sen_agri", "sen_not_agri"), each = length(quantiles))
)

df_plot$IC_lower <- df_plot$L - z99 * df_plot$se_L
df_plot$IC_upper <- df_plot$L + z99 * df_plot$se_L

plot_lorenz_agri_narrow <- ggplot(df_plot, aes(x = quantile)) +
  geom_ribbon(
    aes(ymin = IC_lower, ymax = IC_upper, group = group),
    alpha = 0.2
  ) +
  geom_line(aes(y = L, color = group), linewidth = 1) +
  scale_color_manual(
    values = c(
      "sen_agri" = viridis(10, option = "magma", direction = 1)[6],
      "sen_not_agri" = viridis(10, option = "cividis", direction = 1)[3]
    ),
    labels = c(
      "Self-employed agricultural households",
      "Self-employed non-agricultural households "
    ) # Renaming legend labels
  ) +
  # scale_fill_manual(
  #   values = c(
  #     "sen_agri" = viridis(10, option = "turbo", direction = 1)[10],
  #     "sen_not_agri" = viridis(10, option = "turbo", direction = 1)[1]
  #   ),
  #   labels = c(
  #     "Self-employed agricultural households",
  #     "Self-employed non-agricultural households "
  #   ) # Renaming legend labels
  # ) +
  geom_richtext(
    data = gini_lbl,
    aes(x = x, y = y, label = label),
    fill = "white", # fond blanc derrière le texte
    label.color = "black",
    hjust = 0,
    vjust = 1,
    size = 5
  ) +
  annotate(
    "segment",
    x = 0,
    xend = 1,
    y = 0,
    yend = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Lorenz curves with 99% confidence intervals",
    subtitle = "Square root equivalence scale",
    y = "Cumulative share of equivaled income",
    x = "Cumulative share of population",
    color = "group"
    # fill = "group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )
print(plot_lorenz_agri_narrow)
custom_save(plot_lorenz_agri_narrow, type = "fig")

# Drivers of inequalities ----
## general ----

#TODO: there is something missing here check the .qmd. Most likely the tbl of HOUSEHOLD characteristic
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
      n_is_agri = "relation to agripastoral activities",
      n_is_self_employed_narrow = "occupational status",
      tot_integ = "total number of household members",
      menores = "number of household members below 12",
      edad_jefe = "household's head age",
      sexo_jefe = "household's head sex",
      educa_jefe_group = "household's head education",
      est_socio = "socio-economic status",
      # n_etnia = "ethnic self-description",
      n_acc_alim1 = "concerned about food availability (last month)",
      n_below_smg_epc = "income relative to minimum wage"
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
    #  pattern = "{stat} (99% ci {ci})",
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
      n_is_agri = "relation to agripastoral activities",
      n_is_self_employed_narrow = "occupational status",
      tot_integ = "total number of household members",
      menores = "number of household members below 12",
      # edad_jefe="household's head age",
      sexo_jefe = "household's head sex",
      educa_jefe_group = "household's head education",
      est_socio = "socio-economic status",
      n_etnia = "ethnic self-description",
      n_acc_alim1 = "concerned about food availability (last month)",
      n_below_smg_epc = "income relative to minimum wage"
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
    #  pattern = "{stat} (99% ci {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  add_p()
age <- as_tibble(tbl_csv, col_labels = TRUE)

tbl_csv <- tbl_agri |>
  select(-n_edad_jefe_med, -n_is_agri_broad) |> # filter(decile_total %in% paste0("d", 1:10)) |>
  tbl_svysummary(
    by = sexo_jefe,
    type = list(menores ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      n_is_agri = "relation to agripastoral activities",
      n_is_self_employed_narrow = "occupational status",
      tot_integ = "total number of household members",
      menores = "number of household members below 12",
      edad_jefe = "household's head age",
      # sexo_jefe="household's head sex",
      educa_jefe_group = "household's head education",
      est_socio = "socio-economic status",
      n_etnia = "ethnic self-description",
      n_acc_alim1 = "concerned about food availability (last month)",
      n_below_smg_epc = "income relative to minimum wage"
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
    #  pattern = "{stat} (99% ci {ci})",
    style_fun = all_continuous() ~ purrr::partial(style_number, digits = 1)
  ) |>
  bold_labels() |>
  italicize_levels() |>
  add_overall(last = TRUE) |>
  add_p()
gender <- as_tibble(tbl_csv, col_labels = TRUE)
# the following warnings were returned during `add_p()`:
# ! for variable `est_socio` (`sexo_jefe`) and "statistic" and "p.value"
#   statistics: chi-squared approximation may be incorrect
# car trop peu d'obs women  : 793 000

custom_save(ethnic, "basic_carac_ethnic.csv")
custom_save(age, "basic_carac_age.csv")
custom_save(gender, "basic_carac_gender.csv")

dr <- list(ethnic, age, gender)
names(dr) <- c("ethnic", "age", "gender")
## Focus on ethnicity ----

### total ----
etnia_decile_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_etnia"
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
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
  ) |>
  mutate(
    n_etnia = ifelse(n_etnia == 1, "indigenous", "non-indigenous")
  )
custom_save(etnia_decile_pct)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
etnia_decile_pct <- etnia_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, n_etnia)

# plot
plot_etnia_decile_pct <- ggplot(
  etnia_decile_pct,
  aes(x = n_deciles_total, y = pct, fill = n_etnia)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Ethnic self-identification",
    option = "mako",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = etnia_decile_pct |> dplyr::filter(n_etnia == "non-indigenous"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Ethnicity of households according to income decile",
    subtitle = "Universe: whole population",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "Households are classified according to the socio-demographic characteristics of the reference person (jefe).",
      "Indigenous households are those whose reference person answered yes to the question: ",
      "“De acuerdo con la sua cultura, ¿ella (él) se considera indígena?” (table POBLACION, question Autoadscripción étnica).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot_etnia_decile_pct)
custom_save(plot_etnia_decile_pct, type = "fig")

### agri_broad ----
# svymean(
#   ~ I(n_etnia == 1),
#   subset(mysvyr, n_deciles_total == "D10" & n_is_agri_broad == "agri_broad")
# )

etnia_decile_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_etnia",
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
  ) |>
  mutate(
    n_etnia = ifelse(n_etnia == 1, "indigenous", "non-indigenous")
  )
custom_save(etnia_decile_pct)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
etnia_decile_pct <- etnia_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, n_etnia)

# plot
plot_etnia_decile_broad_pct <- ggplot(
  etnia_decile_pct,
  aes(x = n_deciles_total, y = pct, fill = n_etnia)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Ethnic self-identification",
    option = "mako",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = etnia_decile_pct |> dplyr::filter(n_etnia == "non-indigenous"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Ethnicity of agricultural households according to income decile",
    subtitle = "Universe: agri_broad",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "Households are classified according to the socio-demographic characteristics of the reference person (jefe).",
      "Indigenous households are those whose reference person answered yes to the question: ",
      "“De acuerdo con la sua cultura, ¿ella (él) se considera indígena?” (table POBLACION, question Autoadscripción étnica).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot_etnia_decile_broad_pct)
custom_save(plot_etnia_decile_broad_pct, type = "fig")

### agri_narrow ----

etnia_decile_pct <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_etnia",
  filter_var = "n_is_agri",
  filter_value = "agri_narrow"
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
  ) |>
  mutate(
    n_etnia = ifelse(n_etnia == 1, "indigenous", "non-indigenous")
  )
custom_save(etnia_decile_pct)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
etnia_decile_pct <- etnia_decile_pct |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, n_etnia)

# plot
plot_etnia_decile_narrow_pct <- ggplot(
  etnia_decile_pct,
  aes(x = n_deciles_total, y = pct, fill = n_etnia)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Ethnic self-identification",
    option = "mako",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = etnia_decile_pct |> dplyr::filter(n_etnia == "non-indigenous"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Ethnicity of agricultural households according to income decile",
    subtitle = "Universe: agri_narrow",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "Households are classified according to the socio-demographic characteristics of the reference person (jefe).",
      "Indigenous households are those whose reference person answered yes to the question: ",
      "“De acuerdo con la sua cultura, ¿ella (él) se considera indígena?” (table POBLACION, question Autoadscripción étnica).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

print(plot_etnia_decile_narrow_pct)
custom_save(plot_etnia_decile_narrow_pct, type = "fig")

## Odd ratio with controls ----
#TODO: we should do this but on a relative poverty variable not on minimum wage which is an extreme and restrictive measure of poverty

mysvyr <- mysvyr |>
  mutate(
    n_below_smg_epc_num = as.numeric(n_below_smg_epc == "below")
  )

mod_poverty_ethnicity <- svyglm(
  n_below_smg_epc_num ~
    n_etnia +
    edad_jefe +
    sexo_jefe +
    educa_jefe +
    est_socio,
  design = mysvyr,
  family = quasibinomial()
)

or_poverty_ethnicity <- broom::tidy(
  mod_poverty_ethnicity,
  exponentiate = TRUE,
  conf.int = TRUE,
  conf.level = 0.99
)

tbl_or_poverty_ethnicity <- mod_poverty_ethnicity |>
  tbl_regression(
    exponentiate = TRUE,
    conf.level = 0.99,
    label = list(
      n_etnia ~ "Ethnic self-identification",
      edad_jefe ~ "Household head age",
      sexo_jefe ~ "Household head sex",
      educa_jefe ~ "Household head education",
      est_socio ~ "Socio-economic status"
    )
  ) |>
  bold_labels()

## Focus on concerns for food ----
### total ----

acc_alim1_decile <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_acc_alim1"
  # filter_var = "n_is_agri",
  # filter_value = "agri_narrow"
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
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
  ) |>
  mutate(
    n_acc_alim1 = ifelse(n_acc_alim1 == 1, "yes", "no")
  )
custom_save(acc_alim1_decile)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
acc_alim1_decile <- acc_alim1_decile |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, acc_alim1_decile)

plot_acc_alim1_decile <- ggplot(
  acc_alim1_decile,
  aes(x = n_deciles_total, y = pct, fill = n_acc_alim1)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Concern about\nfood availability",
    option = "viridis",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = acc_alim1_decile |> dplyr::filter(n_acc_alim1 == "yes"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Households according to income decile and concern about food availability",
    subtitle = "Universe: whole population",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "The “concern about food availability” is a self-reported condition expressed by the household head answering:",
      "«En los últimos tres meses, por falta de dinero o recursos ¿alguna vez usted se preocupó de que la comida se acabara?» (table HOGARES, variable acc_alim1).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )
custom_save(plot_acc_alim1_decile, type = "fig")
print(plot_acc_alim1_decile)

### agri_broad ----

acc_alim1_decile <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_acc_alim1",
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
  ) |>
  mutate(
    n_acc_alim1 = ifelse(n_acc_alim1 == 1, "yes", "no")
  )
custom_save(acc_alim1_decile)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
acc_alim1_decile <- acc_alim1_decile |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, acc_alim1_decile)

plot_acc_alim1_decile_broad <- ggplot(
  acc_alim1_decile,
  aes(x = n_deciles_total, y = pct, fill = n_acc_alim1)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Concern about\nfood availability",
    option = "viridis",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = acc_alim1_decile |> dplyr::filter(n_acc_alim1 == "yes"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Agricultural households according to income decile and concern about food availability",
    subtitle = "Universe: agri_broad",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "The “concern about food availability” is a self-reported condition expressed by the household head answering:",
      "«En los últimos tres meses, por falta de dinero o recursos ¿alguna vez usted se preocupó de que la comida se acabara?» (table HOGARES, variable acc_alim1).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )
custom_save(plot_acc_alim1_decile_broad, type = "fig")
print(plot_acc_alim1_decile_broad)

#NOTE :une dissociation entre revenu monétaire et sécurité alimentaire subjective : tant pour narrow que pour broad, la ruralité/agriculture crée une vulnérabilité spécifique, car dans la pop totalles plus riche ne sont pas très inquiet
#WARN: The function is meant to be a handy way to compute proportions and confidence intervals of the proportions of a given target var (say acc_alim1) stratified by a given strat var (say the decile) in a subset of the survey based on filter_var == filter_value
# there is no issue with the function as we find the same awkard 40% of richest agricultural households concerned about food availability through coherence checks -> it seems to be a result
# mysvyr |>
#   filter(n_is_agri_broad == "agri_broad") |>
#   group_by(n_deciles_total) |>
#   summarise(
#     prop = survey_mean(n_acc_alim1 == 1, vartype = "ci")
#   )
#
# svymean(
#   ~ I(n_acc_alim1 == 1),
#   subset(mysvyr, n_deciles_total == "D10" & n_is_agri_broad == "agri_broad")
# )
# mean(
#   d$n_acc_alim1 == 1 &
#     d$n_deciles_total == "D10" &
#     d$n_is_agri_broad == "agri_broad"
# )

### agri_narrow ----

acc_alim1_decile <- get_proportion(
  design = mysvyr,
  strat_var = "n_deciles_total",
  target_var = "n_acc_alim1",
  filter_var = "n_is_agri",
  filter_value = "agri_narrow"
  # filter_var = "n_is_agri_broad",
  # filter_value = "agri_broad"
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
  ) |>
  mutate(
    n_acc_alim1 = ifelse(n_acc_alim1 == 1, "yes", "no")
  )
custom_save(acc_alim1_decile)

# correct levels order
decile_lvl <- levels(as.factor(d$n_deciles_total))
acc_alim1_decile <- acc_alim1_decile |>
  mutate(n_deciles_total = factor(n_deciles_total, levels = decile_lvl)) |>
  arrange(n_deciles_total, acc_alim1_decile)

plot_acc_alim1_decile_narrow <- ggplot(
  acc_alim1_decile,
  aes(x = n_deciles_total, y = pct, fill = n_acc_alim1)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8
  ) +
  scale_fill_viridis_d(
    name = "Concern about\nfood availability",
    option = "viridis",
    begin = 0.2,
    end = 0.8,
    direction = -1
  ) +
  geom_errorbar(
    data = acc_alim1_decile |> dplyr::filter(n_acc_alim1 == "yes"),
    aes(x = n_deciles_total, ymin = pct_low, ymax = pct_upp),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Agricultural households according to income decile and concern about food availability",
    subtitle = "Universe: agri_broad",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "The “concern about food availability” is a self-reported condition expressed by the household head answering:",
      "«En los últimos tres meses, por falta de dinero o recursos ¿alguna vez usted se preocupó de que la comida se acabara?» (table HOGARES, variable acc_alim1).",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    plot.caption = element_text(size = 10)
  )

custom_save(plot_acc_alim1_decile_narrow, type = "fig")
print(plot_acc_alim1_decile_narrow)

