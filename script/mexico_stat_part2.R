# PART 2 STATISTICAL TREATMENTS FOR THE PILOT STUDY ON MEXICO
#TODO : compute share autoconsumo1 and 2 share autoconsumo à calculer dans part 2
# n_autoconsumo = if_else(n_size_val > 0, (auto_tri * 4) / n_size_val, 0),
# share of self-consumption on total output : à calculer dans part2
# n_autoconsumo2 = if_else(
#   n_size_val2 > 0,
#   valestim / n_size_val2,
#   0
#)
#TODO : compute share support in valor prod (entrate aziendale : valore de la produzione vendita + autoconsumata + intercambiata + support) share apoyo a calculer dans part 2 (réintroduire au début +  vérifier : “entrate aziendali” = revenus/ressources d’exploitation et NON n_fni)
# n_apoyo = case_when(
#   n_size_val + n_support > 0 & n_size_val > 0 ~ n_support /
#     (n_size_val + n_support),
#   n_size_val + n_support > 0 & n_size_val == 0 ~ 1,
#   TRUE ~ 0
# ),
#TODO: compute share support in n_fni
#TODO: compute share support in n_ing_cor
#TODO: compute the contrafactural income distribution that would be the case w/o support

# FUNCTIONS ----

## Detailed statistics in custom layout ----

make_tbl <- function(label, stat, ci_method = NULL) {
  df <- mysvyr |>
    select(decile_total_squareOECD, n_ing_squareOECD) |>
    mutate(n_ing_squareOECD = n_ing_squareOECD / 1e3)

  tbl <- df |>
    tbl_svysummary(
      include = n_ing_squareOECD,
      label = n_ing_squareOECD ~ label,
      statistic = all_continuous() ~ stat,
      by = decile_total_squareOECD,
      digits = all_continuous() ~ 1
    ) |>
    bold_labels() |>
    italicize_levels() |>
    add_overall() |>
    modify_footnote(all_stat_cols() ~ NA)

  if (!is.null(ci_method)) {
    tbl <- tbl |>
      add_ci(
        conf.level = 0.99,
        method = list(all_continuous() ~ ci_method),
        pattern = "{stat} ({ci})",
        style_fun = all_continuous() ~ purrr::partial(
          style_number,
          digits = 1
        )
      )
  }

  tbl
}
## Ratio ----
### Proportion de target_var stratifiés par strat_var, avec filtre optionnel ----
##  proportions et IC exacts beta pour tous les niveaux de target_var
##  ex :  n_tipo_prod pour tous les niveaux de n_size_class
## Utile car svyby/svymean ne gèrent pas facilement toutes les combinaisons ni les filtres dynamiques.

get_proportion_IC_all <- function(
  design,
  strat_var,
  target_var,
  filter_var = NULL,
  filter_value = NULL
) {
  # récupérer tous les niveaux
  strat_levels <- levels(as.factor(design$variables[[strat_var]]))
  target_levels <- levels(as.factor(design$variables[[target_var]]))

  # créer tous les couples
  params <- tidyr::crossing(
    strat_level = factor(strat_levels, levels = strat_levels),
    target_level = factor(target_levels, levels = target_levels)
  )

  # fonction interne pour un couple
  calc_prop <- function(strat_level, target_level) {
    # sous-design dynamique
    if (!is.null(filter_var) && !is.null(filter_value)) {
      design_sub <- subset(
        design,
        get(filter_var) == filter_value & get(strat_var) == strat_level
      )
    } else {
      design_sub <- subset(design, get(strat_var) == strat_level)
    }

    # si pas de données pour ce couple
    if (nrow(design_sub$variables) == 0) {
      return(tibble(
        strat_var = strat_level,
        target_var = target_level,
        prop = NA_real_,
        IC_low = NA_real_,
        IC_high = NA_real_
      ))
    }

    # formule dynamique
    f <- reformulate(paste0("I(", target_var, " == '", target_level, "')"))

    # proportion avec IC exact (beta)
    p <- svyciprop(f, design = design_sub, method = "beta")

    # extraire valeurs numériques
    prop_val <- as.numeric(coef(p))
    IC <- as.numeric(confint(p))

    tibble(
      strat_var = strat_level,
      target_var = target_level,
      prop = prop_val,
      IC_low = IC[1],
      IC_high = IC[2]
    )
  }

  # appliquer à tous les couples
  results <- params |>
    pmap_dfr(~ calc_prop(..1, ..2))

  # renommer les colonnes pour refléter les noms réels des variables
  results <- results |>
    rename(
      !!strat_var := strat_var,
      !!target_var := target_var
    )

  return(results)
}

### Shares of total by subgroup ----
## svyciprop ne sait pas faire part d’une sous-population dans un total continu
# at 99%
z_99 <- qnorm(0.995) # 2.576

# to compute var and cov by decile
get_cov <- function(dec) {
  t2 <- svytotal(
    ~ cbind(
      n_fni_by_decile = n_fni_clean * (decile_total_squareOECD == dec),
      n_fni_tot = n_fni_clean
    ),
    design = mysvyr,
    na.rm = TRUE
  )

  coefs <- setNames(coef(t2), c(paste0("n_fni_", dec), "n_fni_tot"))
  vc <- vcov(t2)
  colnames(vc) <- rownames(vc) <- c(paste0("n_fni_", dec), "n_fni_tot")
  names(t2) <- c(paste0("n_fni_", dec), "n_fni_tot")
  list(
    X = coefs[1], # total du décile
    Y = coefs[2], # total global
    VarX = vc[1, 1],
    VarY = vc[2, 2],
    CovXY = vc[1, 2]
  )
}

# delta method per se
get_ratio_IC99 <- function(dec) {
  covs <- get_cov(dec)

  X <- covs$X
  Y <- covs$Y
  VarX <- covs$VarX
  VarY <- covs$VarY
  CovXY <- covs$CovXY

  # Ratio
  ratio <- X / Y

  # SE via delta method
  SE <- sqrt(VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3)

  # IC 99%
  ci_low <- ratio - z_99 * SE
  ci_high <- ratio + z_99 * SE

  tibble(
    decile = dec,
    ratio = ratio,
    SE = SE,
    IC99_low = ci_low,
    IC99_high = ci_high
  )
}

## Saving in csv and in res list ----

res <- list()

custom_save <- function(
  object,
  object_name = NULL,
  type = "processed",
  width = 12,
  height = 8,
  dpi = 300
) {
  stopifnot(requireNamespace("here", quietly = TRUE))
  stopifnot(requireNamespace("readr", quietly = TRUE))

  # save in res list
  filename <- if_else(
    is.null(object_name),
    deparse(substitute(object)),
    object_name
  )

  deparse(substitute(object))
  res[[filename]] <<- object
  # create output dir if not already existing
  output_dir <- here("output", type)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (is_tibble(object)) {
    # save in csv
    output_path <- file.path(output_dir, str_c(filename, ".csv"))

    readr::write_csv(object, output_path)

    message("✅ TIBBLE saved in: ", output_path)
    invisible(output_path)
  } else if (is_ggplot(object)) {
    output_path <- file.path(output_dir, str_c(filename, ".png"))

    ggsave(
      filename = output_path,
      plot = object,
      width = width,
      height = height,
      dpi = dpi,
      units = "in"
    )

    message("✅ PLOT saved in:", output_path)
    invisible(output_path)
  }
}

## Function to correct negative income ----

set.seed(123)
replace_negatives <- function(x) {
  pos <- x[x > 0]
  if (length(pos) == 0) {
    return(x)
  } # pas de positifs
  q1 <- quantile(pos, 0.25, na.rm = TRUE)
  candidates <- pos[pos <= q1]
  if (length(candidates) == 0) {
    candidates <- min(pos)
  }
  neg_idx <- which(x < 0)
  x[neg_idx] <- sample(candidates, length(neg_idx), replace = TRUE)
  return(x)
}

# -----------
#
# LOADING and ADDING NEW VARIABLES ----

# open input
d <- readRDS(
  here("output", "data", "concentradohogar_rev8.rds")
)
## TEMP : consistency checks ----
# d8 <- read_csv2(here("output", "data", "_concentradohogar_rev8.csv"))
# d7 <- read_csv2(here("output", "data", "_concentradohogar_rev7.csv"))
# d8 |> skim(n_ing_cor)
# d7 |> skim(n_ing_cor)
# d |> skim(n_ing_cor)

## add new variables and correct type ----
d <- d |>
  # remove 0 before the code for educa_jefe, est_dis, and num
  mutate(
    upm = as.numeric(upm),
    est_dis = as.numeric(est_dis),
    educa_jefe = as.numeric(educa_jefe)
  ) |>
  # correcting negative income from autonomous (agri and not agri) employment
  mutate(
    # make sure there is no NA in n_fn
    n_fni_agro = coalesce(n_fni_agro, 0),
    n_fni_agro_clean = replace_negatives(n_fni_agro),
    n_ingr_noagro_clean = replace_negatives(n_ingr_noagro)
  ) |>
  # computing current income taking into account the replace_negative()
  mutate(
    n_ing_cor_clean = n_ing_cor -
      n_fni_agro -
      n_ingr_noagro +
      n_fni_agro_clean +
      n_ingr_noagro_clean,
    # human readable production types
    n_tipo_prod_agro = case_when(
      n_tipo_prod_agro == "0" ~ "No harvest yet",
      n_tipo_prod_agro == "1" ~ "Crops",
      n_tipo_prod_agro == "2" ~ "Livestock",
      n_tipo_prod_agro == "3" ~ "Mixed crops-livestock",
      n_tipo_prod_agro == "4" ~ "Mixed farm-primary",
      n_tipo_prod_agro == "5" ~ "Primary non-farm",
      TRUE ~ NA_character_
    ),
    # human readable farm size
    n_size_class_agro = recode_factor(
      as.factor(n_size_class_agro),
      "1" = "[0; 2000]",
      "2" = "]2 000; 10 000]",
      "3" = "]10 000; 50 000]",
      "4" = "]50 000; 200 000]",
      "5" = "> 200 000"
    ),
    is_loc_rural = if_else(tam_loc == 4, "rural", "not_rural"),
    # create a new group for other sources of income
    n_otros_ing_bundled = n_rentas + n_estim_alqu + n_otros_ing
  ) |>
  # creating sorting variable using clean version of n_fni and n_ingr_noagr
  mutate(
    percent_farm = coalesce(n_fni_agro_clean / n_ing_cor_clean * 100, 0),
    percent_ingr_noagr = coalesce(
      n_ingr_noagro_clean / n_ing_cor_clean * 100,
      0
    ),
    is_agri = case_when(
      percent_farm == 0 ~ "not_agri",
      percent_farm < 50 ~ "agri_broad",
      TRUE ~ "agri_strict"
    ),
    is_self_employed_narrow = case_when(
      is_agri == "agri_strict" ~ "sen_agri",
      percent_ingr_noagr >= 50 ~ "sen_not_agri",
      TRUE ~ "not_sen"
    )
  ) |>
  # finally we compute big sorting variable (only two levels)
  mutate(
    is_agri_broad = if_else(
      is_agri %in% c("agri_broad", "agri_strict"),
      "agri_broad",
      "not_agri"
    ),
    is_self_employed = if_else(
      is_self_employed_narrow %in% c("sen_not_agri", "sen_agri"),
      "seng",
      "not_sen"
    )
  )
d |> select(is_agri_broad) |> distinct()

## get median age from survey object ----
med_age <- d |>
  as_survey_design(upm, strata = est_dis, weights = factor) |>
  summarise(med_age = survey_median(edad_jefe, na.rm = TRUE)) |>
  pull(med_age)
## put it back into d to create a new variable ----
d <- d |>
  mutate(
    edad_jefe_med = case_when(
      edad_jefe <= med_age ~ glue("below median age ({round(med_age,1)})"),
      edad_jefe > med_age ~ glue(
        "strictly above median age ({round(med_age,1)})"
      ),
    )
  )
## add per capita equivalent income with square root ----
d <- d |>
  mutate(
    uc_squareOECD = sqrt(tot_integ),
  ) |>
  mutate(
    n_ing_squareOECD = n_ing_cor_clean / uc_squareOECD
  ) |>
  # add below minimum wage (equivalnt income povert line)
  mutate(
    below_smg_epc = ifelse(
      n_ing_squareOECD < smg * 4,
      "below",
      "above"
    )
  )

# EDGE CASES ----
## NON AGRI ----

# This requires a correction (they need to be included)
# NON AGRI with FARM : no farm income, but a farm
non_agri_with_farm <- d |>
  filter(is_agri == "not_agri" & !is.na(n_size_class_agro)) |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has a farm"
  )

# NON AGRI but with production activities in 1 to 4 : no farm income, but harvested farm product
non_agri_with_harvested_prod <- d |>
  filter(n_tipo_act_agro == 1 & is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has harvested farm production"
  )

# NON AGRI but with production activities in 0 : no farm income, declared product but no harvested farm product
non_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

## AGRI ----

# This does not require a correction (they are already included)
# AGRI but no production activities in AGROPRODUCTO : farm income, no farm production
agri_with_no_prod <- d |>
  filter(is.na(n_tipo_act_agro) & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has no declared product, nor farm production"
  )

# AGRI but production activities in AGROPRODUCTO of type 5 = "PRIMARY NON FARM": farm income, no farm production
agri_with_primary_non_farm_prod <- d |>
  filter(n_tipo_act_agro == 5 & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has primary non farm production"
  )

# AGRI but production activities in AGROPRODUCTO of type 0 = no harvest yet, farm income, declared product, no production yet
agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

## Compute and visualise ----

edge_cases <- bind_rows(
  non_agri_with_farm,
  non_agri_with_harvested_prod,
  non_agri_with_no_harvested_prod,
  agri_with_no_prod,
  agri_with_primary_non_farm_prod,
  agri_with_no_harvested_prod
) |>
  mutate(
    sum_total = d |>
      summarise(sum = sum(factor)) |>
      pull(),
    n_total = d |>
      summarise(n = n()) |>
      pull(),
    sum_pct = sum / sum_total * 100,
    n_pct = n / n_total * 100,
  ) |>
  select(type_income, type_agroproducto, everything(), -sum_total, n_total)

message("\n\n-----------------------\nEdges cases found:\n")
print(edge_cases, width = Inf)
message("-----------------------\n\n")

## Save edges cases tbl ----

custom_save(edge_cases, "diagnostics")

## Correct edges cases ----
d |> select(is_agri_broad) |> distinct()
# Dealing with NON AGRI with farm or harvested prod or no harvested prod
d <- d |>
  mutate(
    # flagging households to be changed
    non_agri_with_farm = is_agri == "not_agri" & !is.na(n_size_class_agro),
    non_agri_with_harvested_prod = is_agri == "not_agri" & n_tipo_act_agro == 1,
    non_agri_with_no_harvested_prod = is_agri == "not_agri" &
      n_tipo_act_agro == 0,
  ) |>
  mutate(
    #INFO: non agri with farm or farm product must be included in is_agri_broad
    is_agri = case_when(
      non_agri_with_farm |
        non_agri_with_harvested_prod |
        non_agri_with_no_harvested_prod ~ "agri_broad",
      TRUE ~ is_agri,
    ),
    is_agri_broad = case_when(
      non_agri_with_farm |
        non_agri_with_harvested_prod |
        non_agri_with_no_harvested_prod ~ "agri_broad",
      TRUE ~ is_agri_broad
    ),
    #INFO: non agri with farm or farm product have no n_fni but they may have a productive specialisation if they produced something
    # by construction no harvested product means that they are in "no production yet" == n_tipo_prod_agro == n_tipo_act_agro
    # non agri with farm or harvested product with productive specialisation means that they consumed their production but did not sell it.
    # They should retain their productive specialisation
    # And we should only recode those non agri with farm/harvested product who is.na(n_tipo_prod_agro) -> they should be in n_tipo_prod_agro == 0 == no production/harvest yet
    n_tipo_prod_agro = case_when(
      (non_agri_with_farm |
        non_agri_with_harvested_prod |
        non_agri_with_no_harvested_prod) &
        is.na(n_tipo_prod_agro) ~ "No harvest yet",
      TRUE ~ n_tipo_prod_agro
    ),
    n_tipo_act_agro = case_when(
      (non_agri_with_farm |
        non_agri_with_harvested_prod |
        non_agri_with_no_harvested_prod) &
        is.na(n_tipo_prod_agro) ~ 0,
      TRUE ~ n_tipo_act_agro
    )
  )

#INFO: now dealing with farmers with no prod, either harvested or not (they are no agri with no farm !): should be considered as no harvest yet : n_tipo_prod_agro == n_tipo_act_agro == 0
d <- d |>
  mutate(
    agri_with_no_prod = is.na(n_tipo_act_agro) & is_agri != "not_agri"
  ) |>
  mutate(
    n_tipo_prod_agro = case_when(
      agri_with_no_prod ~ "No harvest yet",
      TRUE ~ n_tipo_prod_agro
    ),
    n_tipo_act_agro = case_when(
      agri_with_no_prod ~ 0,
      TRUE ~ n_tipo_act_agro
    )
  )

d |> select(is_agri_broad) |> distinct()
## Check edges cases after correction ----
non_agri_with_farm <- d |>
  filter(is_agri == "not_agri" & !is.na(n_size_class_agro)) |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has a farm"
  )
non_agri_with_harvested_prod <- d |>
  filter(n_tipo_act_agro == 1 & is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has harvested farm production"
  )
non_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )
agri_with_no_prod <- d |>
  filter(is.na(n_tipo_act_agro) & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has no declared product, nor farm production"
  )
agri_with_primary_non_farm_prod <- d |>
  filter(n_tipo_act_agro == 5 & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has primary non farm production"
  )
agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

edge_cases_after <- bind_rows(
  non_agri_with_farm,
  non_agri_with_harvested_prod,
  non_agri_with_no_harvested_prod,
  agri_with_no_prod,
  agri_with_primary_non_farm_prod,
  agri_with_no_harvested_prod
) |>
  mutate(
    sum_total = d |>
      summarise(sum = sum(factor)) |>
      pull(),
    n_total = d |>
      summarise(n = n()) |>
      pull(),
    sum_pct = sum / sum_total * 100,
    n_pct = n / n_total * 100,
  ) |>
  select(type_income, type_agroproducto, everything(), -sum_total, n_total)

message("\n\n-----------------------\nEdges cases after correction:\n")
print(edge_cases_after, width = Inf)
message("-----------------------\n\n")

## Save edges cases after correction ----

custom_save(edge_cases_after, "diagnostics")

# GENERATE the survey object ----
mysvyr <- d |> as_survey_design(upm, strata = est_dis, weights = factor)
# -----------

# ADDING QUANTILES TO HOUSEHOLDS BASED ON EQUIVALED INCOME ----

## add decile cut off points ----

tres <- mysvyr |>
  summarize(
    inc = survey_quantile(
      n_ing_squareOECD,
      quantiles = seq(0.1, 0.9, 0.1),
      na.rm = TRUE,
      vartype = c("se", "ci"),
      level = 0.99,
      interval_type = "beta",
      qrule = "math"
    )
  )
# Extraire les valeurs de seuils
deciles <- tres |>
  select(starts_with("inc_q")) |>
  select(!ends_with("_se") & !ends_with("_low") & !ends_with("_upp")) |>
  pivot_longer(
    everything(),
    names_to = "quantile",
    values_to = "revenu"
  ) |>
  mutate(
    quantile = as.numeric(gsub("inc_q", "", quantile)) / 100
  )
# Récupérer les bornes inférieures et supérieures des IC
deciles_ic <- tres |>
  select(contains("_low"), contains("_upp")) |>
  pivot_longer(
    everything(),
    names_to = "quantile",
    values_to = "value"
  ) |>
  mutate(
    type = ifelse(grepl("_low", quantile), "low", "upp"),
    quantile = as.numeric(gsub("inc_q|_low|_upp", "", quantile)) / 100
  ) |>
  pivot_wider(names_from = type, values_from = value)

decile_breaks <- c(-Inf, deciles$revenu, Inf)

# add quantiles to which each household belongs based on equivalent income
mysvyr <- mysvyr |>
  mutate(
    decile_total_squareOECD = cut(
      n_ing_squareOECD,
      breaks = decile_breaks,
      # labels = paste0("Q", 1:5, "(5)"),
      labels = paste0("D", 1:10),
      include.lowest = TRUE
    )
  )

# save as d

d <- mysvyr$variables

custom_save(d, "main_database")

# TABLE and BO PLOT of QUANTILES ----
## Table of cutoff points ----
tres_long <- mysvyr |>
  summarize(
    inc = survey_quantile(
      n_ing_squareOECD,
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
    labels = scales::label_number(scale = 1e-3, suffix = " M", big.mark = ",")
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

custom_save(plot_quantile_cutoff, "fig")
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

custom_save(as_tibble(quantile_detail))

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
  group_by(is_agri) |>
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
  select(is_agri, proportion, total)

summary_is_agri$is_agri <- c(
  "agri. (broad sense)",
  "agri. (narrow sense)",
  "non-agri."
)
# Ajouter le label "Total" dans la colonne is_agri
total_line <- total
total_line$is_agri <- "Total"

summary_is_agri <- bind_rows(summary_is_agri, total_line) |>
  rename(
    "Type" = is_agri,
    "prop. (%)" = proportion,
    "n (millions of ind.)" = total
  )
custom_save(summary_is_agri)

## Self-employed households ----

summary_is_SEN <- mysvyr |>
  group_by(is_self_employed_narrow) |>
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
  select(is_self_employed_narrow, proportion, total)

summary_is_SEN$is_self_employed_narrow <- c(
  "not self-employed",
  "self-employed in agri.",
  "self-employed not in agri."
)
total_line <- total
total_line$is_self_employed_narrow <- "Total"

summary_is_SEN <- bind_rows(summary_is_SEN, total_line) |>
  rename(
    "Type" = is_self_employed_narrow,
    "prop. (%)" = proportion,
    "n (millions of ind.)" = total
  )

custom_save(summary_is_SEN)

# HOUSEHOLDS characteristics ----
## Prepare tbl ----

tbl <- mysvyr |>
  select(
    is_agri_broad,
    is_agri,
    is_self_employed_narrow,
    is_loc_rural,
    tot_integ,
    menores,
    edad_jefe,
    sexo_jefe,
    educa_jefe,
    n_ing_cor_clean,
    n_ing_squareOECD,
    below_smg_epc,
    est_socio,
    n_etnia,
    n_acc_alim1,
    edad_jefe_med
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
    is_self_employed_narrow = factor(
      is_self_employed_narrow,
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
    is_agri_broad = factor(
      is_agri_broad,
      levels = c("agri_broad", "not_agri"),
      labels = c(
        "agricultural households",
        "non-agricultural households"
      )
    ),
    is_loc_rural = factor(
      is_loc_rural,
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
  select(-edad_jefe_med) |> # filter(decile_total %in% paste0("D", 1:10)) |>
  # filter(decile_total %in% paste0("D", 1:10)) |>
  tbl_svysummary(
    by = is_agri_broad,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      is_agri = "Relation to agripastoral activities",
      is_self_employed_narrow = "Household's type",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      below_smg_epc = "Income relative to minimum wage"
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
  select(-is_agri_broad, -edad_jefe_med) |> # filter(decile_total %in% paste0("D", 1:10)) |>
  tbl_svysummary(
    by = is_self_employed_narrow,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}, {p75})",
      all_categorical() ~ "{p} %"
    ),
    label = list(
      is_agri = "Relation to agripastoral activities",
      # is_self_employed_narrow = "Household's type",
      tot_integ = "Total number of household members",
      menores = "Number of household members below 12",
      edad_jefe = "Household's head age",
      sexo_jefe = "Household's head sex",
      educa_jefe_group = "Household's head education",
      est_socio = "Socio-economic status",
      n_etnia = "Ethnic self-description",
      n_acc_alim1 = "Concerned about food availability (last month)",
      below_smg_epc = "Income relative to minimum wage"
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

household_char_agri_strict <- as_tibble(tbl_csv, col_labels = TRUE)
custom_save(household_char_agri_strict)

# FARM characteristics ----
## Turnover as a function of farm size and production type ----
df_res <- mysvyr |>
  group_by(is_agri_broad, n_size_class_agro, n_tipo_prod_agro) |>
  summarise(
    total = survey_total(vartype = "ci", level = 0.99)
  ) |>
  ungroup()
#For consistency reasons we restrict ourself to farms owned by households who we identied as agricultural based on a strictly positive farm net income.
### tbl and plot of absolute levels ----
farm_turnover_size_prod <- df_res |>
  filter(is_agri_broad != "not_agri") |>
  select(-is_agri_broad) |>
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

custom_save(plot_farm_turnover_size_prod)

### tbl and plot of relative proportions ----

res <- get_proportion_IC_all(
  design = mysvyr,
  strat_var = "n_size_class_agro",
  target_var = "n_tipo_prod_agro",
  filter_var = "is_agri",
  filter_value = "agri_broad"
)

farm_turnover_size_prod_pct <- res |>
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

plot_farm_turnover_size_prod_pct <- ggplot(
  farm_turnover_size_prod_pct,
  aes(x = n_size_class_agro, y = pct, fill = n_tipo_prod_agro)
) +
  geom_col(
    width = 0.7,
    alpha = 0.8,
    # position = position_dodge(width = 0.7)
  ) +
  # geom_errorbar(
  #   aes(ymin = pct_low, ymax = pct_upp, group = n_tipo_prod),
  #   width = 0.2,
  #   alpha = 0.6,
  #   position = position_dodge(width = 0.7)
  # ) +
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
    caption = "Note: Farm types have been defined considering the share of different production in the total revenues of farms.\nA farm is classified as specialised into a given production when its revenues represent at least two thirds of total turnover.\nIn mixed farms the threshold is not reached neither by crops cultivation nor by livestock production."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# geom_errorbar(
# aes(ymin = pct_low, ymax = pct_upp, group = n_tipo_prod),
# width = 0.2,
# # position = position_dodge(width = 0.6),
# alpha = 0.5

custom_save(plot_farm_turnover_size_prod_pct)

# -----------

# Farm dual's structure ----
## Shares of n_fni by decile ----

quant <- c(levels(as.factor(mysvyr$variables$decile_total_squareOECD)))
ratios_all <- map_dfr(quant, get_ratio_IC99)

# Préparer le tableau
ratios_table <- ratios_all |>
  mutate(
    `Ratio (%)` = round(ratio * 100, 2),
    SE_pct = round(SE * 100, 2),
    `IC99 Lower (%)` = round(IC99_low * 100, 2),
    `IC99 Upper (%)` = round(IC99_high * 100, 2)
  ) |>
  select(
    Decile = decile,
    `Ratio (%)`,
    SE_pct,
    `IC99 Lower (%)`,
    `IC99 Upper (%)`
  )


mysvyr$variables |> select(decile_total_squareOECD) |> distinct()
get_ratio_by_quantiles <- function(
  design,
  var,
  quant_var,
  quant_values,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  var <- rlang::ensym(var)
  quant_var <- rlang::ensym(quant_var)
  print(var)
  print(rlang::as_name(var))
  print(quant_var)
  print(rlang::as_name(quant_var))
  results <- purrr::map_dfr(quant_values, function(q) {
    # design subset implicite via indicatrice
    t2 <- survey::svytotal(
      stats::as.formula(
        paste0(
          "cbind(",
          "X = ",
          rlang::as_name(var),
          " * (",
          rlang::as_name(quant_var),
          " == '",
          q,
          "'), ",
          "Y = ",
          rlang::as_name(var),
          ")"
        )
      ),
      design = design,
      na.rm = TRUE
    )

    X <- coef(t2)[1]
    Y <- coef(t2)[2]

    vc <- vcov(t2)

    VarX <- vc[1, 1]
    VarY <- vc[2, 2]
    CovXY <- vc[1, 2]

    ratio <- X / Y

    SE <- sqrt(
      VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3
    )

    tibble::tibble(
      quantile = q,
      var = rlang::as_name(var),
      share = ratio,
      SE = SE,
      IC_low = ratio - z * SE,
      IC_high = ratio + z * SE
    )
  })

  return(results)
}

deciles <- paste0("D", 1:10)

res <- get_ratio_by_quantiles(
  design = mysvyr,
  var = n_fni_agro_clean,
  quant_var = decile_total_squareOECD,
  quant_values = deciles
)

# -----------
# THE END ----

get_ratio_by_quantiles <- function(
  design,
  var,
  quant_var,
  quant_values,
  level = 0.99
) {
  #

  z <- qnorm((1 + level) / 2)

  # var_name <- deparse(substitute(n_fni_agro_clean))
  # quant_name <- deparse(substitute(decile_total_squareOECD))
  var_name <- deparse(substitute(var))
  quant_name <- deparse(substitute(quant_var))

  purrr::map_dfr(quant_values, function(q) {
    X_expr <- as.formula(
      paste0("~ I(", var_name, " * (", quant_name, " == '", q, "'))")
    )

    Y_expr <- as.formula(paste0("~ ", var_name))

    t2 <- survey::svytotal(
      stats::as.formula(paste0(
        "cbind(X = ",
        var_name,
        " * (",
        quant_name,
        " == '",
        q,
        "'), ",
        "Y = ",
        var_name,
        ")"
      )),
      design = design,
      na.rm = TRUE
    )

    X <- coef(t2)[1]
    Y <- coef(t2)[2]

    vc <- vcov(t2)

    VarX <- vc[1, 1]
    VarY <- vc[2, 2]
    CovXY <- vc[1, 2]

    ratio <- X / Y

    SE <- sqrt(
      VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3
    )

    tibble::tibble(
      quantile = q,
      var = var_name,
      share = ratio,
      SE = SE,
      IC_low = ratio - z * SE,
      IC_high = ratio + z * SE
    )
  })
}


z_99 <- qnorm(0.995) # 2.576

# to compute var and cov by decile
get_cov <- function(dec) {
  t2 <- svytotal(
    ~ cbind(
      n_fni_by_decile = n_fni_clean * (decile_total_squareOECD == dec),
      n_fni_tot = n_fni_clean
    ),
    design = mysvyr,
    na.rm = TRUE
  )

  coefs <- setNames(coef(t2), c(paste0("n_fni_", dec), "n_fni_tot"))
  vc <- vcov(t2)
  colnames(vc) <- rownames(vc) <- c(paste0("n_fni_", dec), "n_fni_tot")
  names(t2) <- c(paste0("n_fni_", dec), "n_fni_tot")
  list(
    X = coefs[1], # total du décile
    Y = coefs[2], # total global
    VarX = vc[1, 1],
    VarY = vc[2, 2],
    CovXY = vc[1, 2]
  )
}

# delta method per se
get_ratio_IC99 <- function(dec) {
  covs <- get_cov(dec)

  X <- covs$X
  Y <- covs$Y
  VarX <- covs$VarX
  VarY <- covs$VarY
  CovXY <- covs$CovXY

  # Ratio
  ratio <- X / Y

  # SE via delta method
  SE <- sqrt(VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3)

  # IC 99%
  ci_low <- ratio - z_99 * SE
  ci_high <- ratio + z_99 * SE

  tibble(
    decile = dec,
    ratio = ratio,
    SE = SE,
    IC99_low = ci_low,
    IC99_high = ci_high
  )
}
