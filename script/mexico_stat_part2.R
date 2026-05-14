#  FUNCTIONS
{
  make_tbl <- function(label, stat, ci_method = NULL) {
    df <- mysvyr |>
      select(decile_total_squareOECD, n_ing_squareOECD) |>
      mutate(n_ing_squareOECD = n_ing_squareOECD / 1e6)

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

  # Calcul des proportions et IC exacts beta pour tous les niveaux de target_var
  # stratifiés par strat_var, avec filtre optionnel.
  # Utile car svyby/svymean ne gèrent pas facilement toutes les combinaisons ni les filtres dynamiques.

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

  # Saving output in output/
  QMD_NAME <- ""

  save_csv <- function(data, filename, qmd = QMD_NAME) {
    stopifnot(requireNamespace("here", quietly = TRUE))
    stopifnot(requireNamespace("readr", quietly = TRUE))

    # date_tag <- format(Sys.Date(), "%Y%m%d")
    # output_dir <- here::here(
    #   "output",
    #   paste0(qmd, "_", date_tag)
    # )

    output_dir <- here("output", "processed")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    output_path <- file.path(output_dir, filename)

    readr::write_csv(data, output_path)

    message("✅ Sauvegardé: ", output_path)
    invisible(output_path)
  }

  set.seed(123)
  # Function to correct negative income
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
}

# LOADING and ADDING NEW VARIABLES
{
  d <- readRDS(
    here("output", "data", "concentradohogar_rev8.rds")
  )
  d8 <- read_csv2(here("output", "data", "_concentradohogar_rev8.csv"))
  d7 <- read_csv2(here("output", "data", "_concentradohogar_rev7.csv"))
  d8 |>
    summarise(
      min_total = min(n_ing_cor, na.rm = TRUE)
    )
  d7 |>
    summarise(
      min_total = min(n_ing_cor, na.rm = TRUE)
    )
  d |>
    summarise(
      min_total = min(n_ing_cor, na.rm = TRUE)
    )

  d |>
    summarise(
      mean = mean(n_ing_cor, na.rm = TRUE),
      sd = sd(n_ing_cor, na.rm = TRUE)
    )

  d7 |>
    summarise(
      mean = mean(n_ing_cor, na.rm = TRUE),
      sd = sd(n_ing_cor, na.rm = TRUE)
    )

  d <- d |>
    # correcting negative income from autonomous (agri and not agri) employment
    mutate(
      # make sure there is no NA in n_fn
      n_fni = coalesce(n_fni, 0),
      n_fni_clean = replace_negatives(n_fni),
      n_ingr_noagr_clean = replace_negatives(n_ingr_noagr)
    ) |>
    # computing current income taking into account the replace_negative()
    mutate(
      n_ing_cor_clean = n_ing_cor -
        n_fni -
        n_ingr_noagr +
        n_fni_clean +
        n_ingr_noagr_clean,
      # human readable production types
      n_tipo_prod = case_when(
        n_tipo_prod == "1" ~ "Crops",
        n_tipo_prod == "2" ~ "Livestock",
        n_tipo_prod == "3" ~ "Mixed crops-livestock",
        n_tipo_prod == "4" ~ "Mixed farm-primary",
        n_tipo_prod == "5" ~ "Primary non-farm",
        TRUE ~ NA_character_
      ),
      # human readable farm size
      n_size_class = recode_factor(
        as.factor(n_size_class),
        "1" = "< 250 000",
        "2" = "]250 000; 500 000]",
        "3" = "]500 000; 1 000 000]",
        "4" = "]1 000 000; 2 000 000]",
        "5" = "]2 000 000; 5 000 000]",
        "6" = "]5 000 000; 10 000 000]",
        "7" = "> 10 000 000"
      ),
      is_loc_rural = if_else(tam_loc == 4, "rural", "not_rural"),
      # create a new group for other sources of income
      n_otros_ing_bundled = n_rentas + n_estim_alqu + n_otros_ing
    ) |>
    # creating sorting variable using clean version of n_fni and n_ingr_noagr
    mutate(
      percent_farm = coalesce(n_fni_clean / n_ing_cor_clean * 100, 0),
      percent_ingr_noagr = coalesce(
        n_ingr_noagr_clean / n_ing_cor_clean * 100,
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

  # get median age from survey object
  med_age <- d |>
    as_survey_design(upm, strata = est_dis, weights = factor) |>
    summarise(med_age = survey_median(edad_jefe, na.rm = TRUE)) |>
    pull(med_age)
  # put it back into d to create a new variable
  d <- d |>
    mutate(
      edad_jefe_med = case_when(
        edad_jefe <= med_age ~ glue("below median age ({round(med_age,1)})"),
        edad_jefe > med_age ~ glue(
          "strictly above median age ({round(med_age,1)})"
        ),
      )
    )

  # add per capita equivalent income with square root
  d_UC <- d |>
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
}

# EDGE CASES
{
  # This requires a correction (they need to be included)
  # NON AGRI with FARM : no farm income, but a farm
  non_agri_with_farm <- d |>
    filter(is_agri == "not_agri" & !is.na(n_size_class)) |>
    summarise(sum = sum(factor), n = n()) |>
    mutate(
      type_income = "no farm income",
      type_agroproducto = "has a farm"
    )

  # NON AGRI but with production activities in 1 to 4 : no farm income, but farm product
  non_agri_with_prod <- d |>
    filter(n_tipo_act %in% c(1, 0) & is_agri == "not_agri") |>
    summarise(sum = sum(factor), n = n()) |>
    mutate(
      type_income = "no farm income",
      type_agroproducto = "has farm production"
    )

  # This does not require a correction (they are already included)
  # AGRI but no production activities in AGROPRODUCTO : farm income, no farm production
  agri_with_no_prod <- d |>
    filter(is.na(n_tipo_act) & is_agri != "not_agri") |>
    summarise(sum = sum(factor), n = n()) |>
    mutate(
      type_income = "farm income",
      type_agroproducto = "has no farm production"
    )

  # AGRI but production activities in AGROPRODUCTO of type 5 = "PRIMARY NON FARM": farm income, no farm production
  agri_with_primary_non_farm_prod <- d |>
    filter(n_tipo_act == 0 & is_agri != "not_agri") |>
    summarise(sum = sum(factor), n = n()) |>
    mutate(
      type_income = "farm income",
      type_agroproducto = "has primary non farm production"
    )

  edge_cases <- bind_rows(
    non_agri_with_farm,
    non_agri_with_prod,
    agri_with_primary_non_farm_prod,
    agri_with_no_prod
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

  # Dealing with non farmer
  d <- d |>
    mutate(
      # flagging households to be changed
      non_agri_with_farm = is_agri == "not_agri" & !is.na(n_size_class),
      non_agri_with_prod = is_agri == "not_agri" & n_tipo_act %in% c(1, 0),
    ) |>
    mutate(
      # non farmer with farm or prod must be in agri_broad
      is_agri = if_else(
        non_agri_with_farm | non_agri_with_prod,
        "agri_broad",
        is_agri
      ),
      is_agri_broad = if_else(
        non_agri_with_farm | non_agri_with_prod,
        "agri_broad",
        is_agri_broad
      ),
      # non farmer with farm or farm product must be inclued in agri with primary no farm prouct
      # agri with farm but no production activites must be classified as primary non farm
      n_tipo_prod = case_when(
        non_agri_with_farm |
          non_agri_with_prod ~ "Primary non-farm",
        TRUE ~ n_tipo_prod
      ),
      n_tipo_act = case_when(
        non_agri_with_farm |
          non_agri_with_prod ~ 0,
        TRUE ~ n_tipo_act
      )
    )

  # Now dealing with farmers with n prod
  d <- d |>
    mutate(
      agri_with_no_prod = is.na(n_tipo_act) & is_agri != "not_agri"
    ) |>
    mutate(
      # non farmer with farm or farm product must be inclued in agri with primary no farm prouct
      # agri with farm but no production activites must be classified as primary non farm
      n_tipo_prod = case_when(
        agri_with_no_prod ~ "Primary non-farm",
        TRUE ~ n_tipo_prod
      ),
      n_tipo_act = case_when(
        agri_with_no_prod ~ 0,
        TRUE ~ n_tipo_act
      )
    )
}

# generate the survey object
mysvyr <- d_UC |> as_survey_design(upm, strata = est_dis, weights = factor)

# ADDING QUANTILES TO HOUSEHOLDS BASED ON EQUIVALED INCOME
{
  # add decile cut off points

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
}

# TABLE and BOXPLOT of QUANTILES
{
  # Table of cutoff points
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

  income_decile_cutoff_sqrt_total <- tres_long

  save_csv(tres_long, "income_decile_cutoff_sqrt_total.csv")

  # plot of cutoff point
  deciles_plot <- left_join(deciles, deciles_ic, by = "quantile")

  gg_income_decile_cutoff_sqrt_total <- ggplot(
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
      labels = scales::label_number(scale = 1e-6, suffix = " M", big.mark = ",")
    ) +
    labs(
      title = "Decile cut-off for annual income and confidence intervals at 99%",
      subtitle = "Square root equivalence scale",
      x = "Income deciles",
      y = "Current income per capita (million MXN)"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    theme_minimal(base_size = 13)

  # Detailed table
  t_mean <- make_tbl("mean (99% CI)", "{mean}", ci_method = "svymean") |>
    modify_header(
      label ~ "**Total current income (millions of MXN)**",
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

  summary_income_decile_sqrt_total <- tbl_stack(
    list(t_mean, t_median, t_extreme, t_quart),
    quiet = TRUE
  )
}
