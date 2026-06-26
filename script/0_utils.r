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
  # if (length(candidates) == 0) {
  #   candidates <- min(pos)
  # }
  neg_idx <- which(x < 0)
  x[neg_idx] <- sample(candidates, length(neg_idx), replace = TRUE)
  x
}

## Custom survey functions ----
### PROPORTION des modalités de target_var stratifiés par strat_var, avec filtre optionnel (n_tipo_prod pour niveau de n_size_class) ----
### proportions et IC exacts beta pour tous les niveaux de target_var
### ex : n_tipo_prod pour tous les niveaux de n_size_class
### Utile car svyby/svymean ne gèrent pas facilement toutes les combinaisons ni les filtres dynamiques.
### Advantages:
# Produces a full conditional distribution of a categorical variable within each stratum, rather than a single mean or binary summary.
# Provides greater flexibility in sample restriction and stratification, including dynamic filtering that is not easily handled by svyby().
# Uses beta-based confidence intervals via svyciprop(), which are more robust than standard asymptotic (Wald) intervals for proportions.

# unit test
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

get_proportion <- function(
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

    p <- svyciprop(~indicator, design_sub, method = method, level = level)
    # extraire valeurs numériques
    prop_val <- as.numeric(coef(p))
    IC <- as.numeric(confint(p)) #level = level)

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

### MACRO SHARES of total by subgroup ----
### svyciprop ne sait pas faire part d’une variable dans un total continu (acc_alim1 is 1/0, while n_fni is continuous) en stratifiant par decile (ou autre sous-groupe)
### based on delta method at 99%
### Advantages:
# It is an extension of svyby(~var, ~quant_var, svytotal)-style decomposition (delta method, just as syvby), but explicitly constructs each group-level contribution to a global total and converts it into shares.
# It provides a share-of-total decomposition across quantiles, allowing interpretation as each group’s contribution to the overall aggregate rather than within-group averages.
# It adds delta-method standard errors and confidence intervals for ratios of totals, which are not directly provided in standard svyby or basic svytotal outputs.

# unit test
# design <- mysvyr
# target_var <- "n_fni_agro_clean"
# strat_var <- "n_deciles_total"
# # filer_var seulement utile si variable non pertinente pour une partie de la pop (ex: rendement/hectare)
# # filter_var <- "n_is_agri_broad"
# # filter_value <- "agri_broad"
# x <- design$variables[[strat_var]]
# strat_levels <- unique(as.character(x[!is.na(x)]))
# strat_level <- strat_levels[2]
# level <- 0.99
# z <- qnorm((1 + level) / 2)

get_share_macro <- function(
  design,
  target_var,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  # domaine global subset restriction
  if (!is.null(filter_var) && !is.null(filter_value)) {
    design_filtered <- subset(
      design,
      !is.na(design$variables[[filter_var]]) &
        design$variables[[filter_var]] == filter_value
    )
  } else {
    design_filtered <- design
  }

  # Niveaux de stratification
  strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
  strat_levels <- strat_levels[!is.na(strat_levels)]

  calc_share_macro <- function(strat_level) {
    # pas de design_sub explicite : on a besoin du design entier pour calculer la variance
    f <- stats::as.formula(
      paste0(
        "~cbind(",
        "part = ",
        target_var,
        " * (",
        strat_var,
        " == '",
        strat_level,
        "'), ",
        "total = ",
        target_var,
        ")"
      )
    )

    t2 <- survey::svytotal(
      f,
      design = design_filtered,
      na.rm = TRUE
    )
    names(t2) <- c("part", "total")

    coefs <- coef(t2)

    X <- coefs[1]
    Y <- coefs[2]

    vc <- vcov(t2)

    VarX <- vc[1, 1]
    VarY <- vc[2, 2]
    CovXY <- vc[1, 2]

    ratio <- X / Y

    SE <- sqrt(
      VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3
    )

    tibble::tibble(
      strat_level = strat_level,
      target_var = target_var,
      share = ratio,
      SE = SE,
      IC_low = ratio - z * SE,
      IC_high = ratio + z * SE
    )
  }

  purrr::map_dfr(strat_levels, calc_share_macro) |>
    dplyr::rename(
      !!strat_var := strat_level
    )
}

get_share_macro_overall <- function(
  design,
  strat_var,
  filter_var = NULL,
  filter_value = NULL
) {
  if (!is.null(filter_var) && !is.null(filter_value)) {
    design_filtered <- subset(
      design,
      !is.na(design$variables[[filter_var]]) &
        design$variables[[filter_var]] == filter_value
    )
  } else {
    design_filtered <- design
  }

  strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
  strat_levels <- strat_levels[!is.na(strat_levels)]

  n_by_strat <- purrr::map_dfr(strat_levels, function(lv) {
    des_sub <- subset(
      design_filtered,
      as.character(design_filtered$variables[[strat_var]]) == lv
    )
    des_sub <- update(des_sub, .one = 1)
    n <- as.numeric(coef(survey::svytotal(~.one, des_sub, na.rm = TRUE)))
    tibble::tibble(!!strat_var := lv, n = n)
  })

  n_total <- sum(n_by_strat$n)

  n_by_strat |>
    mutate(ref_share = n / n_total * 100) |> # *100 car arrondi en % plus loin
    select(-n)
}

### MACRO RATIO of a variable by the total of that continous variable in a subgroup (decile) ----

# unit test
# design <- mysvyr
# numerator <- "n_autoconsumo1_agro"
# denominator <- "n_size_val1_agro"
# strat_var <- "n_deciles_total"
# filter_var <- "n_is_agri_broad"
# filter_value <- "agri_broad"
# level <- 0.99
# strat_levels <- unique(as.character(design$variables[[strat_var]]))
# strat_level <- strat_levels[2]

get_ratio_macro <- function(
  design,
  numerator,
  denominator,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  # domaine global subset restriction
  if (!is.null(filter_var) && !is.null(filter_value)) {
    design_filtered <- subset(
      design,
      !is.na(design$variables[[filter_var]]) &
        design$variables[[filter_var]] == filter_value
    )
  } else {
    design_filtered <- design
  }

  # strat
  strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
  strat_levels <- strat_levels[!is.na(strat_levels)]

  calc_ratio_macro <- function(strat_level) {
    # sous-design dynamique
    design_sub <- subset(
      design_filtered,
      !is.na(design_filtered$variables[[strat_var]]) &
        as.character(design_filtered$variables[[strat_var]]) == strat_level
    )

    f <- stats::as.formula(
      paste0(
        "~cbind(",
        "num = ",
        numerator,
        ", ",
        "den = ",
        denominator,
        ")"
      )
    )

    t2 <- survey::svytotal(
      f,
      design = design_sub,
      na.rm = TRUE
    )
    names(t2) <- c(str_c("num-", numerator), str_c("den-", denominator))

    coefs <- coef(t2)
    X <- coefs[1]
    Y <- coefs[2]

    vc <- vcov(t2)

    VarX <- vc[1, 1]
    VarY <- vc[2, 2]
    CovXY <- vc[1, 2]

    # protection denominator
    if (is.na(Y) || Y == 0) {
      return(tibble::tibble(
        strat_var = strat_level,
        ratio = NA_real_,
        SE = NA_real_,
        IC_low = NA_real_,
        IC_high = NA_real_
      ))
    }

    ratio <- X / Y

    SE <- sqrt(
      VarX / Y^2 + (X^2 * VarY) / Y^4 - 2 * X * CovXY / Y^3
    )

    tibble::tibble(
      strat_var = strat_level,
      ratio = ratio,
      SE = SE,
      IC_low = ratio - z * SE,
      IC_high = ratio + z * SE
    )
  }

  purrr::map_dfr(strat_levels, calc_ratio_macro) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MICRO RATIO : moyenne des ratios individuels par sous-groupe ----
# Contrairement à get_ratio() qui calcule num_total / den_total (ratio de totaux agrégés),
# get_ratio_micro() calcule mean(num_i / den_i) — la moyenne des ratios individuels.
# Utilise srvyr::survey_mean() sur la variable ratio pré-calculée.
# Avantage : reflète le ménage "typique" plutôt que la structure agrégée de production.

get_ratio_micro <- function(
  design,
  numerator,
  denominator,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  # domaine global subset restriction
  if (!is.null(filter_var) && !is.null(filter_value)) {
    design_filtered <- subset(
      design,
      !is.na(design$variables[[filter_var]]) &
        design$variables[[filter_var]] == filter_value
    )
  } else {
    design_filtered <- design
  }

  # ratio individuel
  ratio_var <- paste0(".ratio_micro_", numerator, "_", denominator)
  design_filtered <- update(
    design_filtered,
    .ratio = design_filtered$variables[[numerator]] /
      design_filtered$variables[[denominator]]
  )

  # protection NA Inf
  design_filtered$variables$.ratio <- ifelse(
    is.finite(design_filtered$variables$.ratio),
    design_filtered$variables$.ratio,
    NA_real_
  )

  # niveaux de stratification
  strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
  strat_levels <- strat_levels[!is.na(strat_levels)]

  calc_ratio_micro <- function(strat_level) {
    design_sub <- subset(
      design_filtered,
      !is.na(design_filtered$variables[[strat_var]]) &
        as.character(design_filtered$variables[[strat_var]]) == strat_level
    )

    res <- design_sub |>
      srvyr::as_survey() |>
      srvyr::summarise(
        mean_ratio = srvyr::survey_mean(.ratio, na.rm = TRUE, vartype = "se")
      )

    se <- res$mean_ratio_se
    est <- res$mean_ratio

    tibble::tibble(
      strat_var = strat_level,
      ratio = est,
      SE = se,
      IC_low = est - z * se,
      IC_high = est + z * se
    )
  }

  purrr::map_dfr(strat_levels, calc_ratio_micro) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MICRO SHARE : moyenne des parts individuelles par sous-groupe ----

get_share_micro <- function(
  design,
  target_var,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- qnorm((1 + level) / 2)

  # domaine global subset restriction
  if (!is.null(filter_var) && !is.null(filter_value)) {
    design_filtered <- subset(
      design,
      !is.na(design$variables[[filter_var]]) &
        design$variables[[filter_var]] == filter_value
    )
  } else {
    design_filtered <- design
  }

  # total de la variable sur l'univers entier (dénominateur commun)
  total_universe <- survey::svytotal(
    as.formula(paste0("~", target_var)),
    design = design_filtered,
    na.rm = TRUE
  )
  total_val <- as.numeric(coef(total_universe))

  # share individuel = var_i / total_univers
  design_filtered <- update(
    design_filtered,
    .share_micro = design_filtered$variables[[target_var]] / total_val
  )

  # protection NA Inf
  design_filtered$variables$.share_micro <- ifelse(
    is.finite(design_filtered$variables$.share_micro),
    design_filtered$variables$.share_micro,
    NA_real_
  )

  strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
  strat_levels <- strat_levels[!is.na(strat_levels)]

  calc_share_micro <- function(strat_level) {
    design_sub <- subset(
      design_filtered,
      !is.na(design_filtered$variables[[strat_var]]) &
        as.character(design_filtered$variables[[strat_var]]) == strat_level
    )

    res <- design_sub |>
      srvyr::as_survey() |>
      srvyr::summarise(
        est = srvyr::survey_mean(.share_micro, na.rm = TRUE, vartype = "se")
      )

    tibble::tibble(
      strat_var = strat_level,
      share = res$est,
      SE = res$est_se,
      IC_low = res$est - z * res$est_se,
      IC_high = res$est + z * res$est_se
    )
  }

  purrr::map_dfr(strat_levels, calc_share_micro) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MICRO RATIO : médiane des ratios individuels par sous-groupe ----
#TODO: NEEDS TO WRITE AND COMPUTE IT

## get_ratio_micro_median <- function(
#   design,
#   numerator,
#   denominator,
#   strat_var,
#   filter_var = NULL,
#   filter_value = NULL,
#   level = 0.99
# ) {
#   z <- qnorm((1 + level) / 2)
#
#   if (!is.null(filter_var) && !is.null(filter_value)) {
#     design_filtered <- subset(
#       design,
#       !is.na(design$variables[[filter_var]]) &
#         design$variables[[filter_var]] == filter_value
#     )
#   } else {
#     design_filtered <- design
#   }
#
#   # ratio individuel
#   design_filtered <- update(
#     design_filtered,
#     .ratio = design_filtered$variables[[numerator]] /
#       design_filtered$variables[[denominator]]
#   )
#   design_filtered$variables$.ratio <- ifelse(
#     is.finite(design_filtered$variables$.ratio),
#     design_filtered$variables$.ratio,
#     NA_real_
#   )
#
#   strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
#   strat_levels <- strat_levels[!is.na(strat_levels)]
#
#   calc_median <- function(strat_level) {
#     design_sub <- subset(
#       design_filtered,
#       !is.na(design_filtered$variables[[strat_var]]) &
#         as.character(design_filtered$variables[[strat_var]]) == strat_level
#     )
#     res <- design_sub |>
#       srvyr::as_survey() |>
#       srvyr::summarise(
#         median_ratio = srvyr::survey_quantile(
#           .ratio,
#           quantiles = 0.5,
#           na.rm = TRUE,
#           vartype = "se"
#         )
#       )
#     est <- res$median_ratio_q50
#     se  <- res$median_ratio_q50_se
#     tibble::tibble(
#       strat_var  = strat_level,
#       median     = est,
#       SE         = se,
#       IC_low     = est - z * se,
#       IC_high    = est + z * se
#     )
#   }
#
#   purrr::map_dfr(strat_levels, calc_median) |>
#     dplyr::rename(!!strat_var := strat_var)
# }

### MICRO SHARE : médiane des parts individuelles par sous-groupe ----

# get_share_micro_median <- function(
#   design,
#   target_var,
#   strat_var,
#   filter_var = NULL,
#   filter_value = NULL,
#   level = 0.99
# ) {
#   z <- qnorm((1 + level) / 2)
#
#   if (!is.null(filter_var) && !is.null(filter_value)) {
#     design_filtered <- subset(
#       design,
#       !is.na(design$variables[[filter_var]]) &
#         design$variables[[filter_var]] == filter_value
#     )
#   } else {
#     design_filtered <- design
#   }
#
#   # dénominateur : total univers (inchangé)
#   total_val <- as.numeric(coef(survey::svytotal(
#     as.formula(paste0("~", target_var)),
#     design = design_filtered,
#     na.rm = TRUE
#   )))
#
#   design_filtered <- update(
#     design_filtered,
#     .share_micro = design_filtered$variables[[target_var]] / total_val
#   )
#   design_filtered$variables$.share_micro <- ifelse(
#     is.finite(design_filtered$variables$.share_micro),
#     design_filtered$variables$.share_micro,
#     NA_real_
#   )
#
#   strat_levels <- unique(as.character(design_filtered$variables[[strat_var]]))
#   strat_levels <- strat_levels[!is.na(strat_levels)]
#
#   calc_median <- function(strat_level) {
#     design_sub <- subset(
#       design_filtered,
#       !is.na(design_filtered$variables[[strat_var]]) &
#         as.character(design_filtered$variables[[strat_var]]) == strat_level
#     )
#     res <- design_sub |>
#       srvyr::as_survey() |>
#       srvyr::summarise(
#         median_share = srvyr::survey_quantile(
#           .share_micro,
#           quantiles = 0.5,
#           na.rm = TRUE,
#           vartype = "se"
#         )
#       )
#     est <- res$median_share_q50
#     se  <- res$median_share_q50_se
#     tibble::tibble(
#       strat_var = strat_level,
#       median    = est,
#       SE        = se,
#       IC_low    = est - z * se,
#       IC_high   = est + z * se
#     )
#   }
#
#   purrr::map_dfr(strat_levels, calc_median) |>
#     dplyr::rename(!!strat_var := strat_var)
# }
## Wrapper functions for polagri.r ----
### connecting composition and ratio/share analysis ----

get_col_pal <- function(n, col_pal, direction, begin, end) {
  viridisLite::viridis(
    n = n,
    option = col_pal,
    direction = direction,
    begin = begin,
    end = end
  )
}
shift_hue <- function(col, deg = 15) {
  x <- hex2RGB(col) |>
    as("polarLUV")
  x@coords[, "H"] <- (x@coords[, "H"] + deg) %% 360
  # x@coords[, "H"] <- x@coords[, "H"] + deg
  hex(x)
}
adaptive_transform <- function(col, transform) {
  hcl <- as(hex2RGB(col), "polarLUV")
  L <- hcl@coords[, "L"]
  if (transform == 2) {
    # hue shift léger
    hcl@coords[, "H"] <- hcl@coords[, "H"] + 12
    # couleurs claires : assombrir un peu
    if (L > 70) {
      hcl@coords[, "L"] <- L - 8
    }
    # couleurs sombres : éclaircir un peu
    if (L < 40) {
      hcl@coords[, "L"] <- L + 6
    }
    return(hex(hcl))
  }
  if (transform == 3) {
    return(desaturate(col, .45))
  }
  # if (transform == 3) {
  #   # jouer sur chroma plutôt que luminance
  #   hcl@coords[, "C"] <- hcl@coords[, "C"] * .8
  #   return(hex(hcl))
  # }
  col
}
set_attribute <- function(basename) {
  x <- dict |>
    filter(base == basename)

  type <- x$type[[1]]

  message("\nThis is a ", type, " plot")
  message("\n-----------------------------\n")

  if (type == "ratio") {
    assign("num", x$target_or_num[[1]], envir = .GlobalEnv)
    message("num is ", x$target_or_num[[1]])
    assign("den", x$den[[1]], envir = .GlobalEnv)
    message("den is ", x$den[[1]])
    assign("den_name", x$den_name[[1]], envir = .GlobalEnv)
    message("den_name is ", x$den_name[[1]])

    assign("target", NULL, envir = .GlobalEnv)
    assign("is_share_plot", FALSE, envir = .GlobalEnv)
  } else {
    assign("target", x$target_or_num[[1]], envir = .GlobalEnv)
    message("target is ", x$target_or_num[[1]])
    assign("num", NULL, envir = .GlobalEnv)
    assign("den", NULL, envir = .GlobalEnv)
    assign("den_name", NULL, envir = .GlobalEnv)
    assign("is_share_plot", TRUE, envir = .GlobalEnv)
  }

  assign("strat", x$strat[[1]], envir = .GlobalEnv)
  message("strat is ", x$strat[[1]])
  message("\n-----------------------------\n")
  assign("col_below", x$below[[1]], envir = .GlobalEnv)
  message("col_below is ", x$below[[1]])

  assign("col_above", x$above[[1]], envir = .GlobalEnv)
  message("col_above is ", x$above[[1]])
}

### composition analysis  ----

make_composition_plot <- function(
  tbl,
  overall,
  strat,
  strat_lvl_with_total,
  component_labels,
  title,
  subtitle,
  caption,
  col_pal,
  direction,
  begin,
  end,
  cv_threshold = 0.3,
  debug_inner = FALSE
) {
  overall_plot <- overall |>
    mutate(!!strat := "Total")
  colnames(overall_plot)[1] <- strat

  # déciles avec au moins une composante peu fiable
  unreliable_deciles <- tbl |>
    mutate(cv = abs(SE / ratio)) |>
    filter(!is.na(cv) & cv > cv_threshold) |>
    pull(.data[[strat]]) |>
    unique()

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

  label_data <- plot_data |>
    filter(ratio > 5) |>
    group_by(.data[[strat]]) |>
    arrange(.data[[strat]], as.integer(component)) |>
    mutate(
      y_mid = 100 - (cumsum(ratio) - ratio / 2) # milieu de chaque segment empilé
    ) |>
    ungroup()

  if (debug_inner) {
    assign("plot_data", plot_data, envir = .GlobalEnv)
    assign("label_data", label_data, envir = .GlobalEnv)
    assign("unreliable_deciles", unreliable_deciles, envir = .GlobalEnv)
    message("Inner debug objects assigned")
  }

  p <- ggplot(
    plot_data,
    aes(x = .data[[strat]], y = ratio, fill = component)
  ) +
    geom_col(width = 0.7, alpha = 0.8, color = "white") +
    geom_label(
      data = label_data,
      aes(x = .data[[strat]], y = y_mid, label = paste0(round(ratio, 1), "%")),
      color = "black",
      fill = "white",
      linewidth = 0.15, # épaisseur du bord du rectangle
      size = 3.8,
      fontface = "bold"
    ) +
    geom_vline(xintercept = 10.5, linetype = "dashed", color = "grey50") +
    scale_fill_viridis_d(
      option = col_pal,
      direction = direction,
      begin = begin,
      end = end,
      name = "Component"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.02)),
      labels = percent_format(scale = 1)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Income decile",
      y = "%",
      caption = str_c(
        caption,
        if (length(unreliable_deciles) > 0) {
          paste0(
            "* Decile(s) ",
            paste(unreliable_deciles, collapse = ", "),
            " have at least one component with CV > ",
            cv_threshold * 100,
            "% — full bar unreliable, interpret with caution."
          )
        } else {
          NULL
        },
        sep = "\n"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 12, face = "italic"),
      plot.caption = element_text(size = 10)
    )

  # overlay gris sur les déciles peu fiables et astérisque sur les labels
  if (length(unreliable_deciles) > 0) {
    unreliable_positions <- which(
      levels(plot_data[[strat]]) %in% as.character(unreliable_deciles)
    )
    p <- p +
      annotate(
        "rect",
        xmin = unreliable_positions - 0.5,
        xmax = unreliable_positions + 0.5,
        ymin = -Inf,
        ymax = Inf,
        fill = "grey80",
        alpha = 0.4
      ) +
      geom_label(
        data = data.frame(x = unreliable_deciles, y = Inf),
        aes(x = x, y = y, label = "*"),
        vjust = 1.5,
        color = "grey40",
        fill = "white",
        linewidth = 0.15,
        size = 5,
        inherit.aes = FALSE
      )
  }

  if (debug_inner) {
    assign("plot", plot, envir = .GlobalEnv)
    message("plot objects assigned")
  }

  p
}

run_composition_analysis <- function(
  design,
  d,
  estimators = c("macro", "micro"),
  components,
  component_labels,
  den,
  strat,
  universes, # list of list(universe, filter)
  basename,
  title_macro,
  title_micro,
  caption_macro,
  caption_micro,
  col_pal = "cividis",
  direction = 1,
  begin = 0,
  end = 1,
  cv_threshold = 0.3,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  for (estimator in estimators) {
    estimator_fn <- if (estimator == "macro") {
      get_ratio_macro
    } else {
      get_ratio_micro
    }
    title <- if (estimator == "macro") title_macro else title_micro
    caption_base <- if (estimator == "macro") caption_macro else caption_micro
    name <- str_c(estimator, "_", basename)

    for (u in universes) {
      universe <- u$universe
      filter <- u$filter
      suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
      subtitle <- str_c(
        "Universe: ",
        if (is.null(filter)) "total population" else filter
      )

      # --- calcul déciles ---
      tbl <- map_dfr(names(components), function(comp_name) {
        estimator_fn(
          design = design,
          numerator = components[[comp_name]],
          denominator = den,
          strat_var = strat,
          filter_var = universe,
          filter_value = filter
        ) |>
          mutate(component = comp_name)
      }) |>
        mutate(across(c(ratio, SE, IC_low, IC_high), ~ round(. * 100, 2)))

      strat_lvl <- levels(as.factor(d[[strat]]))
      strat_lvl_with_total <- c(strat_lvl, "Total")

      tbl <- tbl |>
        mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
        arrange(.data[[strat]])

      # --- calcul overall ---
      overall <- map_dfr(names(components), function(comp_name) {
        if (is.null(universe)) {
          design_tmp <- update(design, .total = "Total")
          estimator_fn(
            design = design_tmp,
            numerator = components[[comp_name]],
            denominator = den,
            strat_var = ".total"
          ) |>
            mutate(component = comp_name)
        } else {
          estimator_fn(
            design = design,
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

      custom_save(bind_rows(tbl, overall), str_c(name, "_", suffix))

      # juste avant l'appel à make_composition_plot
      if (debug_outer) {
        assign("tbl", tbl, envir = .GlobalEnv)
        assign("overall", overall, envir = .GlobalEnv)
        assign("strat", strat, envir = .GlobalEnv)
        assign(
          "strat_lvl_with_total",
          strat_lvl_with_total,
          envir = .GlobalEnv
        )
        assign("title", title, envir = .GlobalEnv)
        assign("subtitle", subtitle, envir = .GlobalEnv)
        assign("caption", caption_base, envir = .GlobalEnv)
        message(
          "Debug objects assigned to global env: tbl, overall, ..."
        )
        return(invisible(NULL)) # stoppe après le premier itéré
      }
      # --- plot ---
      plot <- make_composition_plot(
        tbl = tbl,
        overall = overall,
        strat = strat,
        strat_lvl_with_total = strat_lvl_with_total,
        component_labels = component_labels,
        title = title,
        subtitle = subtitle,
        caption = caption_base,
        col_pal = col_pal,
        direction = direction,
        begin = begin,
        end = end,
        cv_threshold = cv_threshold,
        debug_inner = debug_inner
      )
      print(plot)
      custom_save(plot, str_c("plot_", name, "_", suffix), type = "fig")
    }
  }

  comp_vars <- unlist(components, use.names = FALSE)
  col_pal <- get_col_pal(
    n = length(comp_vars),
    col_pal = col_pal,
    direction = direction,
    begin = begin,
    end = end
  )
  cols <- tibble(
    var = comp_vars,
    col = col_pal
  )

  message("\n----------------------------------")
  message("Composition analysis for: ", den)
  message("Saved colors:")
  message("----------------------------------")

  for (i in seq_along(col_pal)) {
    message(cols[i, 1], " -> ", cols[i, 2])
  }
  message("\nAdded to list_cols")
  cols
}

### ratio/share analysis ----

make_decile_plot <- function(
  tbl,
  overall,
  strat_var,
  value_col,
  col_above,
  col_below,
  col_overall,
  title,
  subtitle,
  caption,
  is_share_plot = FALSE,
  ref_tbl = NULL, # <-- nouveau : tibble avec ref_share par décile pour macro_share
  cv_threshold = 0.3
) {
  # si ref_tbl fourni, on merge pour avoir ref_share dans tbl
  if (!is.null(ref_tbl)) {
    tbl <- tbl |> left_join(ref_tbl, by = strat_var)
  }

  tbl <- tbl |>
    mutate(
      above_mean = ifelse(
        .data[[value_col]] >=
          if (!is.null(ref_tbl)) ref_share else overall[[value_col]],
        "Above",
        "Below"
      ),
      cv = abs(SE / .data[[value_col]]),
      unreliable = !is.na(cv) & cv > cv_threshold,
      fill_var = ifelse(unreliable, "Unreliable", above_mean)
    )

  colour_scale <- list(
    values = c(
      "Above" = col_above,
      "Below" = col_below,
      "Unreliable" = "grey70"
    ),
    breaks = c("Above", "Below"),
    name = if (!is.null(ref_tbl)) {
      "Comparison to equal distribution"
    } else if (is_share_plot && is.null(ref_tbl)) {
      "Comparison to mean individual share"
    } else {
      "Comparison to overall ratio"
    }
  )

  use_scientific <- min(tbl[[value_col]], na.rm = TRUE) < 0.1

  p <- ggplot(tbl, aes(x = .data[[strat_var]], y = .data[[value_col]])) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(size = 10)
    ) +
    # bande IC overall — seulement si IC disponible
    (if (!is.na(overall$IC_low) && !is.na(overall$IC_high)) {
      annotate(
        "rect",
        xmin = 0.5,
        xmax = 10.5,
        ymin = overall$IC_low,
        ymax = overall$IC_high,
        fill = col_overall,
        alpha = 0.15
      )
    } else {
      list() # ggplot ignore list() vide
    }) +
    (if (is_share_plot) {
      geom_col(
        aes(color = fill_var),
        fill = "transparent",
        width = 0.7,
        linewidth = 2.5
      )
    } else {
      geom_col(aes(fill = fill_var), width = 0.7, alpha = 0.9, color = "white")
    }) +
    geom_errorbar(
      aes(ymin = IC_low, ymax = IC_high),
      width = 0.2,
      color = "grey30"
    ) +
    geom_label(
      aes(
        label = if (use_scientific) {
          formatC(.data[[value_col]], format = "e", digits = 2)
        } else {
          paste0(round(.data[[value_col]], 1), "%")
        }
      ),
      vjust = 1.5,
      color = "black",
      fill = "white",
      linewidth = 0.15,
      size = 3.8,
      fontface = "bold"
    ) +
    geom_label(
      data = filter(tbl, unreliable),
      aes(x = .data[[strat_var]], y = .data[[value_col]], label = "*"),
      vjust = -0.5,
      color = "grey40",
      fill = "white",
      linewidth = 0.15,
      size = 5,
      inherit.aes = FALSE
    ) +
    geom_smooth(
      aes(group = 1, fill = NULL),
      method = "loess",
      se = FALSE,
      color = "black",
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    # ligne overall : hline classique pour ratio/micro, ligne par décile pour macro_share
    (if (!is.null(ref_tbl)) {
      list(
        geom_line(
          aes(y = ref_share, group = 1),
          color = col_overall,
          linewidth = 0.8,
          linetype = "dotted"
        ),
        geom_label(
          data = tbl |>
            filter(
              as.character(.data[[strat_var]]) == last(levels(tbl[[strat_var]]))
            ),
          aes(
            x = .data[[strat_var]],
            y = ref_share,
            label = paste0("Equal share line")
          ),
          inherit.aes = FALSE,
          fill = "white",
          color = col_overall,
          linewidth = 0.2,
          size = 3.8
        )
      )
    } else {
      list(
        geom_hline(
          yintercept = overall[[value_col]],
          color = col_overall,
          linetype = "dotted",
          linewidth = 0.8
        ),
        geom_label(
          data = data.frame(
            x = "D9",
            y = overall[[value_col]] +
              diff(range(tbl[[value_col]], na.rm = TRUE)) * 0.05
          ),
          aes(
            x = x,
            y = y,
            label = paste0(
              "Overall: ",
              if (use_scientific) {
                formatC(overall[[value_col]], format = "e", digits = 2)
              } else {
                paste0(round(overall[[value_col]], 1), "%")
              }
            )
            # label = paste0("Overall: ", round(overall[[value_col]], 1), "%")
          ),
          inherit.aes = FALSE,
          fill = "white",
          color = col_overall,
          linewidth = 0.2,
          size = 3.8
        )
      )
    }) +
    do.call(
      scale_fill_manual,
      c(colour_scale, list(guide = if (is_share_plot) "none" else "legend"))
    ) +
    do.call(
      scale_color_manual,
      c(colour_scale, list(guide = if (is_share_plot) "legend" else "none"))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = if (use_scientific) {
        label_scientific()
      } else {
        percent_format(scale = 1)
      }
    ) + # scale_y_continuous(
    #   expand = expansion(mult = c(0, 0.05)),
    #   labels = percent_format(scale = 1)
    # ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Income decile",
      y = "%",
      caption = str_c(
        caption,
        paste0(
          "* CV > ",
          cv_threshold * 100,
          "%: estimate unreliable, interpret with caution."
        ),
        sep = "\n"
      )
    )

  p
}

run_one_analysis <- function(
  design,
  d,
  estimator_fn,
  value_col,
  strat,
  universe,
  filter,
  basename,
  title,
  caption,
  col_above,
  col_below,
  col_overall,
  is_share_plot,
  num = NULL,
  den = NULL,
  target_var = NULL,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  subtitle <- str_c("Universe: ", filter)
  name <- str_c(basename, "_", str_remove(filter, ".*_"))

  call_estimator <- function(strat_var_arg, filter_var_arg) {
    if (!is.null(num) && !is.null(den)) {
      estimator_fn(
        design = design,
        numerator = num,
        denominator = den,
        strat_var = strat_var_arg,
        filter_var = filter_var_arg,
        filter_value = filter
      )
    } else {
      estimator_fn(
        design = design,
        target_var = target_var,
        strat_var = strat_var_arg,
        filter_var = filter_var_arg,
        filter_value = filter
      )
    }
  }

  tbl <- call_estimator(strat_var_arg = strat, filter_var_arg = universe)
  strat_lvl <- levels(as.factor(d[[strat]]))
  tbl <- tbl |>
    mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
    arrange(.data[[strat]])

  is_macro_share <- identical(estimator_fn, get_share_macro)

  # overall + ref_tbl selon le type
  if (is_macro_share) {
    ref_tbl <- get_share_macro_overall(
      design = design,
      strat_var = strat,
      filter_var = universe,
      filter_value = filter
    ) |>
      mutate(!!strat := factor(.data[[strat]], levels = strat_lvl))

    # overall pour macro_share = ligne fictive NA (pas utilisée comme hline)
    overall <- tibble::tibble(
      !!strat := NA_character_,
      share = NA_real_,
      SE = NA_real_,
      IC_low = NA_real_,
      IC_high = NA_real_
    )
  } else {
    ref_tbl <- NULL
    overall <- call_estimator(
      strat_var_arg = universe,
      filter_var_arg = universe
    )
    colnames(overall)[1] <- colnames(tbl)[1]
  }

  # arrondi
  round_digits <- if (is_share_plot && is.null(ref_tbl)) {
    max(0, ceiling(-log10(min(abs(tbl[[value_col]]), na.rm = TRUE))) + 2)
  } else {
    2
  }

  tbl <- tbl |>
    mutate(across(
      c(all_of(value_col), SE, IC_low, IC_high),
      ~ round(. * 100, round_digits)
    ))
  overall <- overall |>
    mutate(across(
      any_of(c(value_col, "SE", "IC_low", "IC_high")),
      ~ round(. * 100, round_digits)
    ))

  custom_save(bind_rows(tbl, overall), name)

  if (debug_outer) {
    assign("tbl", tbl, envir = .GlobalEnv)
    assign("overall", overall, envir = .GlobalEnv)
    assign("ref_tbl", ref_tbl, envir = .GlobalEnv)
    assign("strat", strat, envir = .GlobalEnv)
    message(
      "Debug objects assigned to global env: tbl, overall, ref_tbl, strat"
    )
    return(invisible(NULL))
  }

  plot <- make_decile_plot(
    tbl = tbl,
    overall = overall,
    strat_var = strat,
    value_col = value_col,
    col_above = col_above,
    col_below = col_below,
    col_overall = col_overall,
    title = title,
    subtitle = subtitle,
    caption = caption,
    is_share_plot = is_share_plot,
    ref_tbl = ref_tbl
  )

  print(plot)
  custom_save(plot, str_c("plot_", name))
}

run_ratio_analysis <- function(
  design,
  d,
  num,
  den,
  strat,
  basename,
  universes,
  estimators = c("macro", "micro"),
  title_macro,
  title_micro,
  caption_base,
  extra_text,
  col_above,
  col_below,
  col_overall,
  is_share_plot = FALSE,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  caption_macro <- caption_base
  caption_micro <- paste(extra_text, caption_base, sep = "\n")

  for (u in universes) {
    if ("macro" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_ratio_macro,
        value_col = "ratio",
        num = num,
        den = den,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("macro_", basename),
        title = title_macro,
        caption = caption_macro,
        col_above = col_above,
        col_below = col_below,
        col_overall = col_overall,
        is_share_plot = is_share_plot
      )
    }
    if ("micro" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_ratio_micro,
        value_col = "ratio",
        num = num,
        den = den,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("micro_", basename),
        title = title_micro,
        caption = caption_micro,
        col_above = col_above,
        col_below = col_below,
        col_overall = col_overall,
        is_share_plot = is_share_plot
      )
    }
  }
}

run_share_analysis <- function(
  design,
  d,
  target_var,
  strat,
  basename,
  universes,
  estimators = c("macro", "micro"),
  title_macro,
  title_micro,
  caption_base,
  extra_text,
  col_above,
  col_below,
  col_overall,
  is_share_plot = TRUE,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  caption_macro <- caption_base
  caption_micro <- paste(extra_text, caption_base, sep = "\n")

  for (u in universes) {
    if ("macro" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_share_macro,
        value_col = "share",
        target_var = target_var,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("macro_", basename),
        title = title_macro,
        caption = caption_macro,
        col_above = col_above,
        col_below = col_below,
        col_overall = col_overall,
        is_share_plot = is_share_plot
      )
    }
    if ("micro" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_share_micro,
        value_col = "share",
        target_var = target_var,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("micro_", basename),
        title = title_micro,
        caption = caption_micro,
        col_above = col_above,
        col_below = col_below,
        col_overall = col_overall,
        is_share_plot = is_share_plot
      )
    }
  }
}

### DEBUG ----

#  micro share
# test <- get_share_micro(
#   design = mysvyr,
#   target_var = target,
#   strat_var = strat,
#   filter_var = "n_is_agri_broad",
#   filter_value = "agri_broad"
# )

# test <- get_share_micro(
#   design = mysvyr,
#   target_var = target,
#   strat_var = "n_is_agri_broad",
#   filter_var = "n_is_agri_broad",
#   filter_value = "agri_broad"
# )
# print(test)
# survey::svytotal(as.formula(paste0("~", target)), design = mysvyr, na.rm = TRUE)
# THE END ----
