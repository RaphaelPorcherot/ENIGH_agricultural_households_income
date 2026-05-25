# PART 2 STATISTICAL TREATMENTS FOR THE PILOT STUDY ON MEXICO

#TODO: compute the contrafactural income distribution that would be the case w/o support or w/o new support or w/o new direct support

# FUNCTIONS ----

## Detailed statistics in custom layout ----

make_tbl <- function(label, stat, ci_method = NULL) {
  df <- mysvyr |>
    select(n_deciles_total, n_ing_equivaled) |>
    mutate(n_ing_equivaled = n_ing_equivaled / 1e3)

  tbl <- df |>
    tbl_svysummary(
      include = n_ing_equivaled,
      label = n_ing_equivaled ~ label,
      statistic = all_continuous() ~ stat,
      by = n_deciles_total,
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
## Gini ----

survey_gini <- function(
  x,
  na.rm = FALSE,
  vartype = c("se", "ci", "var", "cv"),
  ...
) {
  if (missing(vartype)) {
    vartype <- "se"
  }
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(srvyr::cur_svy(), x)

  out <- convey::svygini(~`__SRVYR_TEMP_VAR__`, na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype)
  as_srvyr_result_df(out)
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
  filename <- if (is.null(object_name)) {
    deparse(substitute(object))
  } else {
    object_name
  }
  deparse(substitute(object))
  res[[filename]] <<- object

  if (is_tibble(object)) {
    # create output dir if not already existing
    output_dir <- here("output", type)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    # save in csv
    output_path <- file.path(output_dir, str_c(filename, ".csv"))

    readr::write_csv(object, output_path)

    message("✅ TIBBLE saved in: ", output_path)
    invisible(output_path)
  } else if (is_ggplot(object)) {
    # create output dir if not already existing
    output_dir <- here("output", "fig")
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    # save csv
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

## Ratio ----
### PROPORTION des modalités de target_var stratifiés par strat_var, avec filtre optionnel (n_tipo_prod pour niveau de n_size_class) ----

### proportions et IC exacts beta pour tous les niveaux de target_var
### ex : n_tipo_prod pour tous les niveaux de n_size_class
### Utile car svyby/svymean ne gèrent pas facilement toutes les combinaisons ni les filtres dynamiques.

### Advantages:
# Produces a full conditional distribution of a categorical variable within each stratum, rather than a single mean or binary summary.
# Provides greater flexibility in sample restriction and stratification, including dynamic filtering that is not easily handled by svyby().
# Uses beta-based confidence intervals via svyciprop(), which are more robust than standard asymptotic (Wald) intervals for proportions.

# design <- mysvyr
# strat_var <- "n_deciles_total"
# target_var <- "n_acc_alim1"
# filter_var <- "n_is_agri_broad"
# filter_value <- "agri_broad"
# strat_levels <- unique(as.character(design$variables[[strat_var]]))
# target_levels <- unique(as.character(design$variables[[target_var]]))
# strat_level <- strat_levels[2]
# target_level <- target_levels
# method <- "beta"

get_proportion_IC_all <- function(
  design,
  strat_var,
  target_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  method = "beta"
) {
  # récupérer tous les niveaux (et éliminer les faux niveua : les niveaux NA)
  x <- design$variables[[strat_var]]
  strat_levels <- unique(as.character(x[!is.na(x)]))
  y <- design$variables[[target_var]]
  target_levels <- unique(as.character(y[!is.na(y)]))

  params <- tidyr::crossing(
    strat_level = strat_levels,
    target_level = target_levels
  )
  # fonction interne pour un couple
  calc_prop <- function(strat_level, target_level) {
    # sous-design dynamique
    if (!is.null(filter_var) && !is.null(filter_value)) {
      design_sub <- subset(
        design,
        !is.na(design$variables[[strat_var]]) &
          as.character(design$variables[[strat_var]]) == strat_level &
          !is.na(design$variables[[filter_var]]) &
          as.character(design$variables[[filter_var]]) == filter_value
      )
    } else {
      design_sub <- subset(
        design,
        !is.na(design$variables[[strat_var]]) &
          as.character(design$variables[[strat_var]]) == strat_level
      )
    }

    if (nrow(design_sub$variables) == 0) {
      return(tibble(
        strat_var = strat_level,
        target_var = target_level,
        prop = NA_real_,
        IC_low = NA_real_,
        IC_high = NA_real_
      ))
    }

    indicator <- as.character(design_sub$variables[[target_var]]) ==
      target_level
    indicator[is.na(indicator)] <- FALSE

    design_sub <- update(design_sub, indicator = indicator)

    p <- svyciprop(~indicator, design_sub, method = method)
    # extraire valeurs numériques
    prop_val <- as.numeric(coef(p))
    IC <- as.numeric(confint(p), level = level)

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

  results
}

# svyr near equivalent (but less general and not as precise as beta method near 0 , 1 value)
# get_proportion_srvyr <- function(data, strat, target, filter = NULL, value = NULL) {
#
#   d <- data
#
#   if (!is.null(filter)) {
#     d <- d |> filter(.data[[filter]] == value)
#   }
#
#   d |>
#     group_by(.data[[strat]]) |>
#     summarise(
#       prop = survey_mean(.data[[target]] == 1, vartype = "ci"),
#       .groups = "drop"
#     )
# }

### SHARES of total by subgroup ----

### svyciprop ne sait pas faire part d’une variable dans un total continu (acc_alim1 is 1/0, while n_fni is continuous) en stratifiant par decile (ou autre sous-groupe)
### based on delta method at 99%

# Advantages:
# It is an extension of svyby(~var, ~quant_var, svytotal)-style decomposition (delta method, just as syvby), but explicitly constructs each group-level contribution to a global total and converts it into shares.
# It provides a share-of-total decomposition across quantiles, allowing interpretation as each group’s contribution to the overall aggregate rather than within-group averages.
# It adds delta-method standard errors and confidence intervals for ratios of totals, which are not directly provided in standard svyby or basic svytotal outputs.

get_share_by_quant <- function(
  design,
  var,
  quant_var,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  var_name <- deparse(substitute(var))
  quant_name <- deparse(substitute(quant_var))
  quant_values <- levels(
    as.factor(design$variables[[quant_name]])
  )
  purrr::map_dfr(quant_values, function(q) {
    f <- stats::as.formula(str_c(
      "~ cbind(",
      str_c(var_name, "_by_", quant_name, " = "),
      var_name,
      " * (",
      quant_name,
      " == '",
      q,
      "'), ",
      str_c(var_name, "_tot = "),
      var_name,
      ")"
    ))

    t2 <- survey::svytotal(
      f,
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

### RATIO of a variable by the total of that continous variable in a subgroup (decile) ----

get_ratio_by_quant <- function(
  design,
  numerator,
  denominator,
  group_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  group_name <- deparse(substitute(group_var))
  num_name <- deparse(substitute(numerator))
  denom_name <- deparse(substitute(denominator))
  filter_name <- deparse(substitute(filter_var))

  groups <- levels(as.factor(design$variables[[group_name]]))

  purrr::map_dfr(groups, function(g) {
    # subset (domain restriction)
    if (!is.null(filter_var) && !is.null(filter_value)) {
      design_sub <- subset(
        design,
        get(filter_name) == filter_value &
          get(group_name) == g
      )
    } else {
      design_sub <- subset(
        design,
        get(group_name) == g
      )
    }

    #  svytotal for numerator and denominator jointly
    f <- stats::as.formula(
      paste0(
        "~cbind(",
        num_name,
        " = ",
        num_name,
        ", ",
        denom_name,
        " = ",
        denom_name,
        ")"
      )
    )

    t2 <- survey::svytotal(
      f,
      design = design_sub,
      na.rm = TRUE
    )

    X <- coef(t2)[1]
    Y <- coef(t2)[2]

    vc <- vcov(t2)

    VarX <- vc[1, 1]
    VarY <- vc[2, 2]
    CovXY <- vc[1, 2]

    # ratio
    ratio <- X / Y

    # delta method SE
    SE <- sqrt(
      VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3
    )

    # protection denominator
    if (is.na(Y) || Y == 0) {
      return(tibble::tibble(
        group = g,
        ratio = NA_real_,
        SE = NA_real_,
        IC_low = NA_real_,
        IC_high = NA_real_
      ))
    }

    tibble::tibble(
      group = g,
      ratio = ratio,
      SE = SE,
      IC_low = ratio - z * SE,
      IC_high = ratio + z * SE
    )
  })
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
      "1" = "[0; 2 000]",
      "2" = "]2 000; 10 000]",
      "3" = "]10 000; 50 000]",
      "4" = "]50 000; 200 000]",
      "5" = "> 200 000"
    ),
    # total farm ressources is : (sales + self-consumption + non monetary exchanges) + subsidies
    n_tot_farm_resources1_agro = n_support_agro + n_size_val1_agro,

    n_is_loc_rural = if_else(tam_loc == 4, "rural", "not_rural"),
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
    n_is_agri = case_when(
      percent_farm == 0 ~ "not_agri",
      percent_farm < 50 ~ "agri_broad",
      TRUE ~ "agri_narrow"
    ),
    n_is_self_employed_narrow = case_when(
      n_is_agri == "agri_narrow" ~ "sen_agri",
      percent_ingr_noagr >= 50 ~ "sen_not_agri",
      TRUE ~ "not_sen"
    )
  ) |>
  # finally we compute big sorting variable (only two levels)
  mutate(
    n_is_agri_broad = if_else(
      n_is_agri %in% c("agri_broad", "agri_narrow"),
      "agri_broad",
      "not_agri"
    ),
    n_is_self_employed = if_else(
      n_is_self_employed_narrow %in% c("sen_not_agri", "sen_agri"),
      "sen",
      "not_sen"
    )
  )

# d |> select(n_is_agri_broad) |> distinct()
## get median age from survey object ----
med_age <- d |>
  as_survey_design(upm, strata = est_dis, weights = factor) |>
  summarise(med_age = survey_median(edad_jefe, na.rm = TRUE)) |>
  pull(med_age)
## put it back into d to create a new variable ----
d <- d |>
  mutate(
    n_edad_jefe_med = case_when(
      edad_jefe <= med_age ~ glue("below median age ({round(med_age,1)})"),
      edad_jefe > med_age ~ glue(
        "strictly above median age ({round(med_age,1)})"
      ),
    )
  )
## add per capita equivalent income with square root ----
d <- d |>
  mutate(
    n_consumption_unit = sqrt(tot_integ),
  ) |>
  mutate(
    n_ing_equivaled = n_ing_cor_clean / n_consumption_unit
  ) |>
  # add below minimum wage (equivalnt income povert line)
  mutate(
    n_below_smg_epc = ifelse(
      n_ing_equivaled < smg * 4,
      "below",
      "above"
    )
  )

# EDGE CASES ----
## NON AGRI ----

# This requires a correction (they need to be included)
# NON AGRI with FARM : no farm income, but a farm
n_is_non_agri_with_farm <- d |>
  filter(n_is_agri == "not_agri" & !is.na(n_size_class_agro)) |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has a farm"
  )

# NON AGRI but with production activities in 1 to 4 : no farm income, but harvested farm product
n_is_non_agri_with_harvested_prod <- d |>
  filter(n_tipo_act_agro == 1 & n_is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has harvested farm production"
  )

# NON AGRI but with production activities in 0 : no farm income, declared product but no harvested farm product
n_is_non_is_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & n_is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

## AGRI ----

# This does not require a correction (they are already included)
# AGRI but no production activities in AGROPRODUCTO : farm income, no farm production
n_is_agri_with_no_prod <- d |>
  filter(is.na(n_tipo_act_agro) & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has no declared product, nor farm production"
  )

# AGRI but production activities in AGROPRODUCTO of type 5 = "PRIMARY NON FARM": farm income, no farm production
n_is_agri_with_primary_non_farm_prod <- d |>
  filter(n_tipo_act_agro == 5 & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has primary non farm production"
  )

# AGRI but production activities in AGROPRODUCTO of type 0 = no harvest yet, farm income, declared product, no production yet
n_is_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

## Compute and visualise ----

edge_cases <- bind_rows(
  n_is_non_agri_with_farm,
  n_is_non_agri_with_harvested_prod,
  n_is_non_is_agri_with_no_harvested_prod,
  n_is_agri_with_no_prod,
  n_is_agri_with_primary_non_farm_prod,
  n_is_agri_with_no_harvested_prod
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
d |> select(n_is_agri_broad) |> distinct()
# Dealing with NON AGRI with farm or harvested prod or no harvested prod
d <- d |>
  mutate(
    # flagging households to be changed
    n_is_non_agri_with_farm = n_is_agri == "not_agri" &
      !is.na(n_size_class_agro),
    n_is_non_agri_with_harvested_prod = n_is_agri == "not_agri" &
      n_tipo_act_agro == 1,
    n_is_non_is_agri_with_no_harvested_prod = n_is_agri == "not_agri" &
      n_tipo_act_agro == 0,
  ) |>
  mutate(
    #INFO: non agri with farm or farm product must be included in n_is_agri_broad
    n_is_agri = case_when(
      n_is_non_agri_with_farm |
        n_is_non_agri_with_harvested_prod |
        n_is_non_is_agri_with_no_harvested_prod ~ "agri_broad",
      TRUE ~ n_is_agri,
    ),
    n_is_agri_broad = case_when(
      n_is_non_agri_with_farm |
        n_is_non_agri_with_harvested_prod |
        n_is_non_is_agri_with_no_harvested_prod ~ "agri_broad",
      TRUE ~ n_is_agri_broad
    ),
    #INFO: non agri with farm or farm product have no n_fni but they may have a productive specialisation if they produced something
    # by construction no harvested product means that they are in "no production yet" == n_tipo_prod_agro == n_tipo_act_agro
    # non agri with farm or harvested product with productive specialisation means that they consumed their production but did not sell it.
    # They should retain their productive specialisation
    # And we should only recode those non agri with farm/harvested product who is.na(n_tipo_prod_agro) -> they should be in n_tipo_prod_agro == 0 == no production/harvest yet
    n_tipo_prod_agro = case_when(
      (n_is_non_agri_with_farm |
        n_is_non_agri_with_harvested_prod |
        n_is_non_is_agri_with_no_harvested_prod) &
        is.na(n_tipo_prod_agro) ~ "No harvest yet",
      TRUE ~ n_tipo_prod_agro
    ),
    n_tipo_act_agro = case_when(
      (n_is_non_agri_with_farm |
        n_is_non_agri_with_harvested_prod |
        n_is_non_is_agri_with_no_harvested_prod) &
        is.na(n_tipo_prod_agro) ~ 0,
      TRUE ~ n_tipo_act_agro
    )
  )

#INFO: now dealing with farmers with no prod, either harvested or not (they are no agri with no farm !): should be considered as no harvest yet : n_tipo_prod_agro == n_tipo_act_agro == 0
d <- d |>
  mutate(
    n_is_agri_with_no_prod = is.na(n_tipo_act_agro) & n_is_agri != "not_agri"
  ) |>
  mutate(
    n_tipo_prod_agro = case_when(
      n_is_agri_with_no_prod ~ "No harvest yet",
      TRUE ~ n_tipo_prod_agro
    ),
    n_tipo_act_agro = case_when(
      n_is_agri_with_no_prod ~ 0,
      TRUE ~ n_tipo_act_agro
    )
  )

d |> select(n_is_agri_broad) |> distinct()
## Check edges cases after correction ----
n_is_non_agri_with_farm <- d |>
  filter(n_is_agri == "not_agri" & !is.na(n_size_class_agro)) |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has a farm"
  )
n_is_non_agri_with_harvested_prod <- d |>
  filter(n_tipo_act_agro == 1 & n_is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "has harvested farm production"
  )
n_is_non_is_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & n_is_agri == "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "no farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )
n_is_agri_with_no_prod <- d |>
  filter(is.na(n_tipo_act_agro) & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has no declared product, nor farm production"
  )
n_is_agri_with_primary_non_farm_prod <- d |>
  filter(n_tipo_act_agro == 5 & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "has primary non farm production"
  )
n_is_agri_with_no_harvested_prod <- d |>
  filter(n_tipo_act_agro == 0 & n_is_agri != "not_agri") |>
  summarise(sum = sum(factor), n = n()) |>
  mutate(
    type_income = "farm income",
    type_agroproducto = "declared products, but no harvest yet"
  )

edge_cases_after <- bind_rows(
  n_is_non_agri_with_farm,
  n_is_non_agri_with_harvested_prod,
  n_is_non_is_agri_with_no_harvested_prod,
  n_is_agri_with_no_prod,
  n_is_agri_with_primary_non_farm_prod,
  n_is_agri_with_no_harvested_prod
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

# GENERATE the survey object and add quantile based on equivaled income ----
mysvyr <- d |> as_survey_design(upm, strata = est_dis, weights = factor)

## add decile cut off points ----
tres <- mysvyr |>
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
    n_deciles_total = cut(
      n_ing_equivaled,
      breaks = decile_breaks,
      # labels = paste0("Q", 1:5, "(5)"),
      labels = paste0("D", 1:10),
      include.lowest = TRUE
    )
  )

#NOTE: they are more agri households in D10 than in D9.

# mysvyr |>
#   filter(n_is_agri_broad == "agri_broad") |>
#   group_by(n_deciles_total) |>
#   summarise(
#     mean = survey_mean(n_ing_equivaled),
#     n_unweighted = unweighted(n()),
#     n_weighted = survey_total()
#   )
# d |> select(starts_with("n_")) |> colnames() |> sort()
#NOTE:: the following code to check the pct of each income source in total income

# check <- mysvyr |>
#   # filter(n_is_agri == "agri_narrow") |>
#   filter(n_is_agri_broad == "agri_broad") |>
#   group_by(n_deciles_total) |>
#   summarise(
#     pct_fni_agro = survey_total(n_fni_agro) / survey_total(n_ing_cor_clean),
#     pct_trabajo = survey_total(n_trabajo) / survey_total(n_ing_cor_clean),
#     tot_ing_cor_clean = survey_total(n_ing_cor_clean),
#     pct_ingr_noagro = survey_total(n_ingr_noagro) /
#       survey_total(n_ing_cor_clean),
#     pct_otros_ing = survey_total(n_otros_ing) / survey_total(n_ing_cor_clean),
#     pct_rentas = survey_total(n_rentas) / survey_total(n_ing_cor_clean),
#     pct_transfer = survey_total(n_transfer) / survey_total(n_ing_cor_clean),
#     pct_n_estim_alqu = survey_total(n_estim_alqu) /
#       survey_total(n_ing_cor_clean)
#   )
#
# check |>
#   select(-ends_with("_se"), -n_deciles_total, -tot_ing_cor_clean) #|>
# mutate(sum = rowSums(across(everything()))) |>
# select(sum)

# save the main database ----
## (we should not be doing any further changes now)

d <- mysvyr$variables
custom_save(d, "main_database")

# UPDATE NEW VARIABLE DICTIONARY (append new variables if needed) ----

vars_n <- sort(names(d)[str_starts(
  names(d),
  "n_"
)])

cur_dic <- read_csv(
  "dict_new_variables.csv",
  col_types = cols(
    level = col_character()
  )
)

test_new_var <- !vars_n %in%
  {
    cur_dic |> select(variable) |> pull()
  }

if (any(test_new_var)) {
  var_cat <- c(
    "n_below_smg_epc",
    "n_deciles_total",
    "n_edad_jefe_med",
    "n_is_agri",
    "n_is_agri_broad",
    "n_is_agri_with_no_prod",
    "n_is_loc_rural",
    "n_is_non_agri_with_farm",
    "n_is_non_agri_with_harvested_prod",
    "n_is_non_is_agri_with_no_harvested_prod",
    "n_is_self_employed",
    "n_is_self_employed_narrow",
    "",
    "",
    "",
    "",
    "",
    "n_size_class_agro",
    "n_tipo_prod_agro",
    "n_etnia",
    "n_acc_alim1",
    "n_tipo_act_agro"
  )

  new_vars <- vars_n[test_new_var]

  dict_new <- purrr::map_dfr(new_vars, function(v) {
    vals <- d |> pull(all_of(v))

    if (v %in% var_cat) {
      levels <- unique(as.character(vals))
      levels[is.na(levels)] <- "not applicable"

      bind_rows(
        tibble(
          variable = v,
          type = "categorical",
          from_table = NA_character_,
          level = NA_character_,
          definition = NA_character_
        ),
        tibble(
          variable = v,
          type = "categorical",
          from_table = NA_character_,
          level = levels,
          definition = NA_character_
        )
      )
    } else {
      tibble(
        variable = v,
        type = "numeric",
        from_table = NA_character_,
        level = NA_character_,
        definition = NA_character_
      )
    }
  })

  # 🔥 APPEND AU DICTIONARY EXISTANT
  dict_updated <- bind_rows(cur_dic, dict_new) |>
    distinct(variable, type, level, .keep_all = TRUE) |>
    arrange(type, variable)

  readr::write_csv(dict_updated, "dict_new_variables.csv", na = "")

  message(
    "\n\n❗️ New variables appended to dict_new_variables.csv\n📖 Dictionary updated successfully."
  )
} else {
  message(
    "\n\n 📖 No new variable has been created.\n ✅ Dictionary is already up to date."
  )
}
# -----------

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

custom_save(plot_farm_turnover_size_prod)

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

farm_turnover_size_prod_pct <- get_proportion_IC_all(
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

custom_save(plot_farm_turnover_size_prod_pct)

# -----------

# Farm dual's structure ----
## Shares of n_fni by decile ----

share_fni_decile_pct <- get_share_by_quant(
  design = mysvyr,
  var = n_fni_agro_clean,
  quant_var = n_deciles_total,
) |>
  mutate(
    `Ratio (%)` = round(share * 100, 2),
    SE_pct = round(SE * 100, 2),
    `IC99 Lower (%)` = round(IC_low * 100, 2),
    `IC99 Upper (%)` = round(IC_high * 100, 2)
  ) |>
  select(
    Decile = quantile,
    `Ratio (%)`,
    SE_pct,
    `IC99 Lower (%)`,
    `IC99 Upper (%)`
  )

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

ratios <- share_fni_decile_pct |>
  mutate(type = "Farm net income") |>
  select(-SE_pct) |>
  mutate(
    cols = viridis(10, option = "plasma", direction = -1)[9]
  ) |>
  set_names(colnames(pop))
df_plot <- bind_rows(pop, ratios) |>
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

custom_save(plot_agri_house_fni_decile_pct)

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
#TODO: do the graph also for agri_narrow
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

farm_turnover_decile_pct <- get_proportion_IC_all(
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

plot_farm_turnover_decile_pct <- ggplot(
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
    x = "Income decile",
    y = "(%)",
    # caption = "Note: Farm types have been defined considering the share of different production in the total revenues of farms.\nA farm is classified as specialised into a given production when its revenues represent at least two thirds of total turnover.\nIn mixed farms the threshold is not reached neither by crops cultivation nor by livestock production."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

custom_save(plot_farm_turnover_decile_pct)

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

custom_save(gini_result_renamed, "gini_agri_broad.csv")

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
  geom_line(aes(y = L, color = group), linewidth = 1) +
  geom_ribbon(
    aes(ymin = IC_lower, ymax = IC_upper, fill = group),
    alpha = 0.2
  ) +
  scale_color_manual(
    values = c(
      "agri_broad" = viridis(10, option = "magma", direction = 1)[6],
      "not_agri" = viridis(10, option = "cividis", direction = 1)[6]
    ),
    labels = c("Agricultural households (broad)", "Non-agricultural") # Renaming legend labels
  ) +

  scale_fill_manual(
    values = c(
      "agri_broad" = viridis(10, option = "inferno", direction = 1)[10],
      "not_agri" = viridis(10, option = "inferno", direction = 1)[1]
    ),
    labels = c("Agricultural households (broad)", "Non-agricultural") # Renaming legend labels
  ) +
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
    color = "group",
    fill = "group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )

# kable(df_plot)
custom_save(df_plot, "lorenz_agri_broad.csv")
custom_save(plot_lorenz_agri_broad)
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

custom_save(gini_result_renamed, "gini_agri_narrow.csv")

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
  geom_line(aes(y = L, color = group), linewidth = 1) +
  geom_ribbon(
    aes(ymin = IC_lower, ymax = IC_upper, fill = group),
    alpha = 0.2
  ) +
  scale_color_manual(
    values = c(
      "sen_agri" = viridis(10, option = "magma", direction = 1)[6],
      "sen_not_agri" = viridis(10, option = "cividis", direction = 1)[6]
    ),
    labels = c(
      "Self-employed agricultural households",
      "Self-employed non-agricultural households "
    ) # Renaming legend labels
  ) +

  scale_fill_manual(
    values = c(
      "sen_agri" = viridis(10, option = "turbo", direction = 1)[10],
      "sen_not_agri" = viridis(10, option = "turbo", direction = 1)[1]
    ),
    labels = c(
      "Self-employed agricultural households",
      "Self-employed non-agricultural households "
    ) # Renaming legend labels
  ) +
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
    color = "group",
    fill = "group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  )
# kable(df_plot)
custom_save(df_plot, "lorenz_agriStrict.csv")
custom_save(plot_lorenz_agri_narrow)
# Drivers of inequalities ----
## general ----
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
  add_overall(last = true) |>
  add_p()
ethnic <- as_tibble(tbl_csv, col_labels = true)

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
  add_overall(last = true) |>
  add_p()
age <- as_tibble(tbl_csv, col_labels = true)

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
  add_overall(last = true) |>
  add_p()
gender <- as_tibble(tbl_csv, col_labels = true)
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

# svymean(
#   ~ I(n_etnia == 1),
#   subset(mysvyr, n_deciles_total == "D10" & n_is_agri_broad == "agri_broad")
# )
#
etnia_decile_pct <- get_proportion_IC_all(
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
    title = "Ethnicity of agricultural households according to income decile",
    subtitle = "Indigenous households are those whose reference person (jefe) self-identifies as indigenous",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "Agricultural households are defined according to the broad definition",
      #TODO: check ethnicity according to agro_narrow
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
custom_save(plot_etnia_decile_pct)

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



acc_alim1_decile <- get_proportion_IC_all(
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
    title = "Agricultural households according to income decile and concern about food availability",
    subtitle = "Self-reported concern by the household head in the last three months",
    x = "Income decile",
    y = "Percentage (%)",
    caption = paste(
      "Households are defined as agricultural according to the broad definition.",
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
custom_save(plot_acc_alim1_decile)

# Self-consumption ----
## from agro self employemnet ----

share_autoconsumo_decile_pct <- get_ratio_by_quant(
  design = mysvyr,
  numerator = n_autoconsumo1_agro,
  denominator = n_size_val1_agro,
  group_var = n_deciles_total,
  filter_var = n_is_agri_broad,
  filter_value = agri_broad
) |>
  mutate(
    `Ratio (%)` = round(share * 100, 2),
    SE_pct = round(SE * 100, 2),
    `IC99 Lower (%)` = round(IC_low * 100, 2),
    `IC99 Upper (%)` = round(IC_high * 100, 2)
  ) |>
  select(
    Decile = quantile,
    `Ratio (%)`,
    SE_pct,
    `IC99 Lower (%)`,
    `IC99 Upper (%)`
  )

# ratios_table$Decile <- factor(
#   ratios_table$Decile,
#   levels = paste0("D", 1:10)
# )
#
# # Moyenne nationale
# mean_ratio <- mean(ratios_table$`Ratio (%)`)
#
# # Indicateur pour colorer les barres selon > ou < moyenne
# ratios_table <- ratios_table |>
#   mutate(
#     above_mean = ifelse(`Ratio (%)` >= mean_ratio, "Above mean", "Below mean")
#   )
#
# # Graphique
# ggplot(ratios_table, aes(x = Decile, y = `Ratio (%)`, fill = above_mean)) +
#
#   geom_col(width = 0.7, alpha = 0.9, color = "white") +
#
#   geom_errorbar(
#     aes(ymin = `IC99 Lower (%)`, ymax = `IC99 Upper (%)`),
#     width = 0.2,
#     color = "grey30"
#   ) +
#
#   geom_text(
#     aes(label = paste0(round(`Ratio (%)`, 1), "%")),
#     vjust = 1.5,
#     color = "white",
#     size = 3.8,
#     fontface = "bold"
#   ) +
#
#   geom_smooth(
#     aes(group = 1),
#     method = "loess",
#     se = FALSE,
#     color = "black",
#     linewidth = 0.8,
#     linetype = "dashed"
#   ) +
#
#   geom_hline(
#     yintercept = mean_ratio,
#     color = "#D55E00",
#     linetype = "dotted",
#     size = 0.8
#   ) +
#
#   scale_fill_manual(
#     values = c(
#       "Above mean" = "#006400", # dark green
#       "Below mean" = "#90EE90" # light green
#     ),
#     name = "Comparison to mean"
#   ) +
#
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.05)),
#     labels = percent_format(scale = 1)
#   ) +
#
#   labs(
#     title = "Share of self-consumed production in total production by income decile",
#     subtitle = "Agricultural households (agri_broad) — OECD square-root equivalence scale",
#     x = "Income decile",
#     y = "Share of production (%)",
#     caption = paste(
#       "Notes: 'Self-consumed production' corresponds to production consumed by the household rather than sold.",
#       "The indicator reports the share of self-consumed production in total production by income decile.",
#       "Bar colors indicate whether the decile is above (dark green) or below (light green) the national mean.",
#       "The dashed black line shows the LOESS trend across deciles.",
#       "The red dotted line represents the national average share.",
#       "Error bars represent 99% confidence intervals.",
#       "Source: Based on ENIGH data.",
#       sep = "\n"
#     )
#   ) +
#
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text.x = element_text(size = 11),
#     panel.grid.major.x = element_blank(),
#     plot.title = element_text(face = "bold"),
#     plot.subtitle = element_text(face = "italic"),
#     plot.caption = element_text(size = 10)
#   )

## from non agro self employment ----
#TODO: still somehting to do
## self-consumption from agro in all
#TODO: we need to understand the discrpenacy between tipoact, which is encoded by INEGHI, and the self-declaration of the actiity which got the support fromsocial programs which may agri in a quite contradictory manner with the first element

#TODO : compute share support in valor prod (entrate aziendale : valore de la produzione vendita + autoconsumata + intercambiata + support) share apoyo a calculer dans part 2 (réintroduire au début +  vérifier : “entrate aziendali” = revenus/ressources d’exploitation et NON n_fni)
# n_apoyo = case_when(
#   n_size_val + n_support > 0 & n_size_val > 0 ~ n_support /
#     (n_size_val + n_support),
#   n_size_val + n_support > 0 & n_size_val == 0 ~ 1,
#   TRUE ~ 0
# ),
#TODO: compute share support in n_fni
#TODO: compute share support in n_ing_cor
#TODO : compute la distribution qui serait le cas s'il n'y avait que les vieux programmes agricoles
#TODO : regarde la part des vieux programmes agricoles dans le revenu agricole total par décile

# -----------
# THE END ----
