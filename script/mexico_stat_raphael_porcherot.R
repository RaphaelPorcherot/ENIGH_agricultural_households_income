# --- FUNCTIONS ---

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


