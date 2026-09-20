# FUNCTIONS ----
## Create output dir ---- 

create_output_dirs <- function() {
  stopifnot(requireNamespace("here", quietly = TRUE))

  subdirs <- c("data", "diagnostics", "fig", "processed")

  for (sub in subdirs) {
    path <- here::here("output", sub)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message("📁 Output folder ready : ", path)
  }

  invisible(here::here("output"))
}
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

## Saving results to disk ----

# NOTE: custom_save() used to also accumulate every object in a global `res`
# list, saved at the end of main_script.r as output/part2_results. That list
# held the ~320 ggplot objects as well as the tables: a ggplot carries its data
# and its evaluation environment, so the serialised file reached 2.5 GB and
# saveRDS() spent longer writing it than the whole analysis took to run. It was
# also pure duplication - every table is already a .csv and every figure a .pdf
# in output/ - and nothing ever read it back. Removed.

# Figures are written as VECTOR PDF rather than 300 dpi PNG: ~17x smaller on
# disk (7-19 KB instead of ~120 KB per figure), ~3x faster to write, and they
# stay sharp at any zoom in the paper. Override globally with
#   options(enigh.fig_device = "png")
# or per call with custom_save(..., device = "png").
# cairo_pdf is preferred when available because the captions contain accented
# Spanish, guillemets and em-dashes that the base pdf device can only
# approximate; it costs ~0.5 s more per figure.
.fig_device <- function(device) {
  device <- match.arg(device, c("pdf", "png"))
  if (device == "png") {
    return(list(ext = "png", dev = NULL, args = list()))
  }
  if (isTRUE(capabilities("cairo"))) {
    list(ext = "pdf", dev = grDevices::cairo_pdf, args = list())
  } else {
    list(
      ext = "pdf",
      dev = grDevices::pdf,
      args = list(encoding = "WinAnsi", useDingbats = FALSE)
    )
  }
}

# print(plot) inside the analysis loops costs ~1 s per figure and is useless in
# a batch run (the figure is saved anyway). Set options(enigh.show_plots = TRUE)
# when working interactively.
show_plot <- function(p) {
  if (isTRUE(getOption("enigh.show_plots", FALSE))) print(p)
  invisible(p)
}

custom_save <- function(
  object,
  object_name = NULL,
  type = "processed",
  width = 12,
  height = 8,
  dpi = 300,
  device = getOption("enigh.fig_device", "pdf")
) {
  stopifnot(requireNamespace("here", quietly = TRUE))
  stopifnot(requireNamespace("readr", quietly = TRUE))

  filename <- if (is.null(object_name)) {
    deparse(substitute(object))
  } else {
    object_name
  }

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

    dv <- .fig_device(device)
    output_path <- file.path(output_dir, str_c(filename, ".", dv$ext))

    do.call(
      ggsave,
      c(
        list(
          filename = output_path,
          plot = object,
          width = width,
          height = height,
          dpi = dpi,
          units = "in"
        ),
        if (is.null(dv$dev)) list() else list(device = dv$dev),
        dv$args
      )
    )

    message("✅ PLOT saved in: ", output_path)
    invisible(output_path)
  }
}

## Function to correct negative income ----
#
# Negative self-employment income (farm and non-farm) has to be dealt with before
# the income deciles are built, because negative values break the Gini, the
# Lorenz curve and any log transform. Three treatments are available; switch with
#   options(enigh.negative_income = "zero" | "draw" | "keep")
#
# "zero" (DEFAULT) - bottom-coding at 0. Standard practice (LIS, OECD, Eurostat).
#   Deterministic, reproducible, and it cannot create impossible households.
#
# "draw" (the previous behaviour) - replace each negative by a random draw from
#   the bottom quartile of the POSITIVE values of the whole population. Kept only
#   so that the sensitivity check "zero vs draw" can be run without editing code.
#   Measured consequence on ENIGH 2022: 498 households (4.3 % weighted) ended up
#   with n_fni_agro_clean > n_ftr1_agro, i.e. a farm net income larger than the
#   farm's total resources, which cannot exist. 94.6 % of them were loss-making
#   farms. The mechanism is that the draw ignores the household's own scale: a
#   micro-farm with 29 pesos of output could be assigned 4 895 pesos of income,
#   166x its total resources. Those households then contaminate every ratio built
#   on n_fni_agro_clean (max observed individual ratio: 51 190 %) and, through
#   n_ing_cor_clean, the income deciles themselves.
#
# "keep" - no treatment, negatives left as they are. Will break convey/Gini.
#
# NOTE on where the floor is applied: it is applied to the COMPONENT (farm and
# non-farm self-employment income), not to total income. A household with 10 000
# of wages and a 2 000 farm loss is therefore credited with 10 000, not 8 000.
# Flooring the total would be more faithful economically, but percent_farm =
# n_fni_agro_clean / n_ing_cor_clean - which classifies households as
# agricultural - would no longer be computable. State this in the methods.

set.seed(123)
replace_negatives <- function(
  x,
  method = getOption("enigh.negative_income", "zero")
) {
  method <- match.arg(method, c("zero", "draw", "keep"))

  if (method == "keep") {
    return(x)
  }
  if (method == "zero") {
    x[!is.na(x) & x < 0] <- 0
    return(x)
  }

  # method == "draw" : previous behaviour, kept for the sensitivity check
  pos <- x[x > 0]
  if (length(pos) == 0) {
    return(x)
  } # pas de positifs
  q1 <- quantile(pos, 0.25, na.rm = TRUE)
  candidates <- pos[pos <= q1]
  neg_idx <- which(x < 0)
  x[neg_idx] <- sample(candidates, length(neg_idx), replace = TRUE)
  x
}

## Custom survey functions ----

### Shared engine: one-pass domain estimation ----
#
# Every estimator below (proportions, macro/micro ratios, macro/micro shares,
# medians) is a DOMAIN estimator: a quantity computed on the sub-population
# {strat_var == g}, optionally further restricted to {filter_var == filter_value}.
#
# The previous implementation called subset() + svytotal()/svymean() once per
# level of strat_var. That is statistically correct but slow: on this design a
# single svytotal() costs ~4 s whatever the number of columns (up to a few
# dozen), because the variance recursion walks all 560 strata; 10 deciles x
# ~100 analyses is therefore ~1 h.
#
# The engine below builds ONE numeric matrix whose columns are
#     y_i * 1[i belongs to domain g]
# for every (variable, level) pair, and makes a SINGLE svytotal() call.
# This is NOT an approximation:
#   * a domain total IS the total of y * 1[domain] (Sarndal, Swensson & Wretman
#     1992, ch. 10);
#   * survey's subset() keeps the ORIGINAL number of PSUs per stratum in
#     $fpc$sampsize and pads the dropped PSUs back with zero rows inside
#     onestrat(), so the linearised variance it returns for the subset design is
#     exactly the variance of the indicator variable on the full design;
#   * ratios (hence means, shares, proportions, Woodruff intervals) are then
#     obtained by the same delta method as before, but from the FULL joint
#     covariance matrix.
# Checked against the previous implementation on a design of the same shape
# (90 102 units, 560 strata, 10 211 PSUs): agreement to ~1e-16 relative on the
# point estimates AND on the standard errors, for ~9x less time.
#
# NA handling is unchanged: a unit is dropped from a domain as soon as ONE of
# the variables stacked in the same call is missing, which is what
# svytotal(~cbind(a, b), na.rm = TRUE) does.

# Domain bookkeeping shared by every estimator: which units belong to which
# level, and the quantities survey would have computed on the subset design.
#   ok : optional logical vector, extra per-unit validity condition
.domain_setup <- function(
  design,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  ok = NULL
) {
  v <- design$variables
  n <- nrow(v)

  g <- as.character(v[[strat_var]])
  keep <- !is.na(g)
  if (!is.null(filter_var) && !is.null(filter_value)) {
    fv <- as.character(v[[filter_var]])
    keep <- keep & !is.na(fv) & fv == filter_value
  }
  if (!is.null(ok)) {
    keep <- keep & ok & !is.na(ok)
  }

  lv <- if (is.factor(v[[strat_var]])) {
    intersect(levels(v[[strat_var]]), unique(g[keep]))
  } else {
    sort(unique(g[keep]))
  }
  if (length(lv) == 0L) {
    stop("no non-empty level of `", strat_var, "` in this universe")
  }

  idx <- match(g, lv)
  w <- stats::weights(design, "sampling")
  psu <- design$cluster[[1]]
  str <- design$strata[[1]]

  sel <- lapply(seq_along(lv), function(k) keep & !is.na(idx) & idx == k)

  list(
    n = n,
    levels = lv,
    sel = sel,
    w = w,
    n_obs = vapply(sel, sum, integer(1)),
    n_pop = vapply(sel, function(s) sum(w[s]), numeric(1)),
    # survey::degf() on subset(design, domain): PSUs kept minus strata kept
    degf = vapply(
      sel,
      function(s) length(unique(psu[s])) - length(unique(str[s])),
      integer(1)
    )
  )
}

# Weighted domain totals of `vars` for every level, in one svytotal() call.
#   vars : named list of numeric vectors of length n, or a named character
#          vector of variable names.
#   ds   : optional pre-computed .domain_setup() (avoids recomputing it).
# value: ds, augmented with coef / vcov / p / pos(k, j).
.domain_totals <- function(
  design,
  vars,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  ds = NULL
) {
  v <- design$variables

  if (is.character(vars)) {
    nms <- names(vars)
    if (is.null(nms)) nms <- unname(vars)
    vars <- stats::setNames(
      lapply(unname(vars), function(x) as.numeric(v[[x]])),
      nms
    )
  }
  stopifnot(is.list(vars), !is.null(names(vars)))
  p <- length(vars)
  nms <- names(vars)

  if (is.null(ds)) {
    ds <- .domain_setup(
      design,
      strat_var,
      filter_var,
      filter_value,
      ok = Reduce(`&`, lapply(vars, is.finite))
    )
  }

  G <- length(ds$levels)
  M <- matrix(0, nrow = ds$n, ncol = G * p)
  colnames(M) <- as.vector(outer(nms, ds$levels, function(a, b) {
    paste0(b, "::", a)
  }))
  for (k in seq_len(G)) {
    s <- ds$sel[[k]]
    for (j in seq_len(p)) {
      M[s, (k - 1L) * p + j] <- vars[[j]][s]
    }
  }

  tt <- survey::svytotal(M, design, na.rm = FALSE)

  c(
    ds,
    list(
      vars = nms,
      p = p,
      coef = as.numeric(stats::coef(tt)),
      vcov = as.matrix(stats::vcov(tt)),
      pos = function(k, j) (k - 1L) * p + j
    )
  )
}

# Delta-method (Taylor linearisation) CI for the ratio of two estimated totals.
# Same formula as the one previously inlined in get_ratio_macro(); factored out
# so that every estimator shares it.
.delta_ratio <- function(coefs, vcv, i, j, z) {
  X <- coefs[i]
  Y <- coefs[j]
  if (!is.finite(X) || !is.finite(Y) || Y == 0) {
    return(c(est = NA_real_, SE = NA_real_, IC_low = NA_real_, IC_high = NA_real_))
  }
  r <- X / Y
  var_r <- vcv[i, i] / Y^2 + (X^2 * vcv[j, j]) / Y^4 - 2 * X * vcv[i, j] / Y^3
  SE <- sqrt(max(var_r, 0))
  c(est = r, SE = SE, IC_low = r - z * SE, IC_high = r + z * SE)
}

# Same, when numerator and denominator are LINEAR COMBINATIONS of the
# coefficients (get_share_macro(): the denominator is the sum of all decile
# parts, hence correlated with the numerator).
.delta_ratio_lin <- function(coefs, vcv, a, b, z) {
  X <- sum(a * coefs)
  Y <- sum(b * coefs)
  if (!is.finite(X) || !is.finite(Y) || Y == 0) {
    return(c(est = NA_real_, SE = NA_real_, IC_low = NA_real_, IC_high = NA_real_))
  }
  grad <- a / Y - (X / Y^2) * b
  SE <- sqrt(max(as.numeric(t(grad) %*% vcv %*% grad), 0))
  c(est = X / Y, SE = SE, IC_low = X / Y - z * SE, IC_high = X / Y + z * SE)
}

# Korn-Graubard "beta" interval for a proportion, verbatim from
# survey::svyciprop(method = "beta") / survey:::woodruffCI(method = "beta").
#   n_sub : nrow() of the subset design, df : degf() of the subset design.
.beta_ci <- function(p, var_p, n_sub, df, level) {
  alpha <- 1 - level
  if (!is.finite(p) || !is.finite(var_p) || var_p <= 0 || p <= 0 || p >= 1) {
    return(c(NA_real_, NA_real_))
  }
  n_eff <- p * (1 - p) / var_p
  n_eff <- n_eff *
    (stats::qt(alpha / 2, n_sub - 1) / stats::qt(alpha / 2, df))^2
  c(
    stats::qbeta(alpha / 2, n_eff * p, n_eff * (1 - p) + 1),
    stats::qbeta(1 - alpha / 2, n_eff * p + 1, n_eff * (1 - p))
  )
}

# Weighted quantile, "math" rule, identical to survey:::qrule_math().
# (survey exposes the rule only by name inside svyquantile(), so it is restated
# here; the unit test in test_estimators.R checks it against svyquantile().)
.wq_math <- function(x, w, p) {
  keep <- is.finite(x) & is.finite(w) & w != 0
  x <- x[keep]
  w <- w[keep]
  if (!length(x) || !is.finite(p) || p < 0 || p > 1) return(NA_real_)
  ii <- order(x)
  x <- x[ii]
  cumw <- cumsum(w[ii])
  sw <- cumw[length(cumw)]
  le <- cumw <= p * sw
  pos <- if (any(le)) max(which(le)) else 1L
  posnext <- if (pos == length(x)) pos else pos + 1L
  if (p - cumw[pos] / sw <= 0) x[pos] else x[posnext]
}


### Testing a decile AGAINST THE REFERENCE ----
#
# WHY IT IS NOT "do the confidence intervals overlap?"
#   Comparing two 99% intervals by eye is not a 1% test. It is conservative when
#   the two estimates are independent, and it can be badly WRONG when they are
#   correlated - which is exactly the case here, since every decile estimate and
#   the reference are computed from the same sample (and shares, which sum to 1,
#   are strongly NEGATIVELY correlated with each other). Two non-overlapping
#   intervals do imply a difference, but overlapping intervals do NOT imply the
#   absence of one.
#
# WHAT IS DONE INSTEAD
#   The difference itself is estimated:
#       d_g = theta_g - theta_ref,
#       Var(d_g) = Var(theta_g) + Var(theta_ref) - 2 Cov(theta_g, theta_ref)
#   with the covariance term taken from the FULL joint covariance matrix of the
#   domain totals. That matrix is now available for free: the one-pass engine
#   estimates every decile in a single svytotal(), so Cov(theta_g, theta_ref) is
#   simply an entry of vcov(). Both theta_g and theta_ref are ratios of linear
#   combinations of those totals, so the variance of their difference follows by
#   the delta method (Taylor linearisation), i.e. exactly the same machinery that
#   already produces the confidence intervals.
#
#   Degrees of freedom: degf(design) = (number of PSUs) - (number of strata),
#   survey's usual convention for a t reference distribution.
#
# MULTIPLE TESTING
#   Ten deciles are compared to the same reference, so both the raw p-value and
#   a Holm-adjusted one are returned (p_value / p_value_adj, signif / signif_adj).

# Design-based test of
#     (a_est . c) / (b_est . c)   ==   (a_ref . c) / (b_ref . c)
# where c is the vector of estimated totals and a_*, b_* are weight vectors.
.test_contrast <- function(coefs, vcv, a_est, b_est, a_ref, b_ref, df, level) {
  na_row <- tibble::tibble(
    ref = NA_real_, ref_SE = NA_real_,
    ref_IC_low = NA_real_, ref_IC_high = NA_real_,
    diff = NA_real_, diff_SE = NA_real_,
    diff_IC_low = NA_real_, diff_IC_high = NA_real_, p_value = NA_real_
  )
  X1 <- sum(a_est * coefs)
  Y1 <- sum(b_est * coefs)
  X2 <- sum(a_ref * coefs)
  Y2 <- sum(b_ref * coefs)
  if (!is.finite(Y1) || Y1 == 0 || !is.finite(Y2) || Y2 == 0) return(na_row)

  est <- X1 / Y1
  ref <- X2 / Y2
  g_est <- a_est / Y1 - (X1 / Y1^2) * b_est
  g_ref <- a_ref / Y2 - (X2 / Y2^2) * b_ref
  # gradient of the DIFFERENCE of the two ratios w.r.t. the estimated totals
  g <- g_est - g_ref
  se <- sqrt(max(as.numeric(t(g) %*% vcv %*% g), 0))
  # the reference gets its own interval as well: it is the "overall" value of
  # the plots, and computing it here avoids a second svytotal() per analysis
  se_ref <- sqrt(max(as.numeric(t(g_ref) %*% vcv %*% g_ref), 0))
  d <- est - ref
  crit <- if (is.finite(df) && df > 0) {
    stats::qt(1 - (1 - level) / 2, df)
  } else {
    stats::qnorm(1 - (1 - level) / 2)
  }
  p <- if (is.finite(se) && se > 0) {
    2 * stats::pt(-abs(d / se), df = if (is.finite(df) && df > 0) df else Inf)
  } else {
    NA_real_
  }
  tibble::tibble(
    ref = ref, ref_SE = se_ref,
    ref_IC_low = ref - crit * se_ref, ref_IC_high = ref + crit * se_ref,
    diff = d, diff_SE = se,
    diff_IC_low = d - crit * se, diff_IC_high = d + crit * se,
    p_value = p
  )
}

# Apply .test_contrast() to every level of a one-pass domain estimate.
#   num_j / den_j : which of the p variables of .domain_totals() is the numerator
#                   and the denominator of the ESTIMATE
#   ref_num_j / ref_den_j : same, for the REFERENCE. The reference is always
#                   pooled over all the levels (sum over g), which is what
#                   "overall" means in the plots.
.test_levels <- function(dt, num_j, den_j, ref_num_j, ref_den_j, df, level) {
  G <- length(dt$levels)
  n <- length(dt$coef)
  unit <- function(k, j) {
    v <- numeric(n)
    v[dt$pos(k, j)] <- 1
    v
  }
  pooled <- function(j) {
    v <- numeric(n)
    for (k in seq_len(G)) v[dt$pos(k, j)] <- 1
    v
  }
  a_ref <- pooled(ref_num_j)
  b_ref <- pooled(ref_den_j)

  specs <- lapply(seq_len(G), function(k) {
    list(a_est = unit(k, num_j), b_est = unit(k, den_j),
         a_ref = a_ref, b_ref = b_ref)
  })
  out <- purrr::map_dfr(specs, function(sp) {
    .test_contrast(dt$coef, dt$vcov, sp$a_est, sp$b_est, sp$a_ref, sp$b_ref,
                   df = df, level = level)
  })
  .finalise_test(out, level, .trend_test(dt$coef, dt$vcov, specs, df))
}

# Same, but with a LEVEL-SPECIFIC reference (get_share_macro: decile g is
# compared with its own demographic weight N_g / sum N, not with a single
# overall number).
.test_levels_paired <- function(dt, num_j, den_j, ref_num_j, ref_den_j, df, level) {
  G <- length(dt$levels)
  n <- length(dt$coef)
  unit <- function(k, j) {
    v <- numeric(n)
    v[dt$pos(k, j)] <- 1
    v
  }
  pooled <- function(j) {
    v <- numeric(n)
    for (k in seq_len(G)) v[dt$pos(k, j)] <- 1
    v
  }
  b_est <- pooled(den_j)
  b_ref <- pooled(ref_den_j)

  specs <- lapply(seq_len(G), function(k) {
    list(a_est = unit(k, num_j), b_est = b_est,
         a_ref = unit(k, ref_num_j), b_ref = b_ref)
  })
  out <- purrr::map_dfr(specs, function(sp) {
    .test_contrast(dt$coef, dt$vcov, sp$a_est, sp$b_est, sp$a_ref, sp$b_ref,
                   df = df, level = level)
  })
  .finalise_test(out, level, .trend_test(dt$coef, dt$vcov, specs, df))
}

# LINEAR TREND ACROSS THE DECILES
#
# Ten separate tests answer "is decile g different from the reference?" but not
# "does the gap widen with income?", which is the progressivity question. The
# slope of the OLS regression of the gap d_g on the decile rank g is a fixed
# linear combination of the d_g,
#     slope = sum_g c_g d_g,   c_g = (g - gbar) / sum_g (g - gbar)^2
# and each d_g is a smooth function of the same estimated totals, so the slope
# has an exact design-based variance by the delta method: its gradient is
# simply sum_g c_g grad(d_g), and Var = grad' V grad with the SAME covariance
# matrix. Unlike fitting lm() on the ten point estimates, this accounts for the
# correlation between deciles and for their unequal precision.
#
# Reading: slope > 0 means the gap to the reference grows from D1 to D10
# (regressive), slope < 0 that it shrinks (progressive). Units: points of the
# estimate per decile step.
.trend_test <- function(coefs, vcv, specs, df) {
  G <- length(specs)
  if (G < 3) {
    return(list(trend_slope = NA_real_, trend_SE = NA_real_,
                trend_p = NA_real_))
  }
  x <- seq_len(G)
  cx <- (x - mean(x)) / sum((x - mean(x))^2)
  slope <- 0
  grad <- numeric(length(coefs))
  for (k in seq_len(G)) {
    sp <- specs[[k]]
    X1 <- sum(sp$a_est * coefs)
    Y1 <- sum(sp$b_est * coefs)
    X2 <- sum(sp$a_ref * coefs)
    Y2 <- sum(sp$b_ref * coefs)
    if (!is.finite(Y1) || Y1 == 0 || !is.finite(Y2) || Y2 == 0) {
      return(list(trend_slope = NA_real_, trend_SE = NA_real_,
                  trend_p = NA_real_))
    }
    slope <- slope + cx[k] * (X1 / Y1 - X2 / Y2)
    grad <- grad + cx[k] *
      ((sp$a_est / Y1 - (X1 / Y1^2) * sp$b_est) -
         (sp$a_ref / Y2 - (X2 / Y2^2) * sp$b_ref))
  }
  se <- sqrt(max(as.numeric(t(grad) %*% vcv %*% grad), 0))
  p <- if (is.finite(se) && se > 0) {
    2 * stats::pt(-abs(slope / se),
                  df = if (is.finite(df) && df > 0) df else Inf)
  } else {
    NA_real_
  }
  list(trend_slope = slope, trend_SE = se, trend_p = p)
}

.finalise_test <- function(out, level, trend = NULL) {
  alpha <- 1 - level
  out <- out |>
    dplyr::mutate(
      p_value_adj = stats::p.adjust(p_value, method = "holm"),
      signif = is.finite(p_value) & p_value < alpha,
      signif_adj = is.finite(p_value_adj) & p_value_adj < alpha,
      test = "delta-method contrast vs reference"
    )
  if (!is.null(trend)) {
    # constant across rows: it is a property of the whole decile profile
    out <- out |>
      dplyr::mutate(
        trend_slope = trend$trend_slope,
        trend_SE = trend$trend_SE,
        trend_p = trend$trend_p
      )
  }
  out
}

### PROPORTION des modalités de target_var stratifiées par strat_var ----
###
### WHAT IT COMPUTES, exactly
###   for each level g of strat_var and each non-missing level k of target_var
###       prop(g, k) = N_hat(strat = g & target = k) / N_hat(strat = g)
###   i.e. the weighted share of households of modality k INSIDE stratum g.
### DENOMINATOR / missing values
###   by default units with a missing target_var stay in the denominator (they
###   count as "not k" for every k), so the proportions of the modalities sum to
###   1 only when target_var is never missing in the stratum. This is the
###   behaviour of the previous version. Set drop_na_target = TRUE to condition
###   on a non-missing target_var, in which case they sum exactly to 1.
### CONFIDENCE INTERVAL
###   Korn-Graubard "beta" interval (Clopper-Pearson applied to the effective
###   sample size p(1-p)/var(p_hat), itself rescaled by the design df). It stays
###   inside [0, 1] and behaves far better than a Wald interval near 0 and 1.
###   Reproduces survey::svyciprop(method = "beta") to machine precision, in one
###   svytotal() pass instead of one subset design per (stratum x modality).
get_proportion <- function(
  design,
  strat_var,
  target_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  method = "beta",
  drop_na_target = FALSE
) {
  if (!identical(method, "beta")) {
    stop(
      "get_proportion(): the fast engine only implements method = 'beta'. ",
      "Use survey::svyciprop() directly for another method."
    )
  }
  v <- design$variables
  y <- as.character(v[[target_var]])
  target_levels <- if (is.factor(v[[target_var]])) {
    intersect(levels(v[[target_var]]), unique(y[!is.na(y)]))
  } else {
    unique(y[!is.na(y)])
  }

  # one indicator column per modality, plus the domain-size column
  ind <- lapply(target_levels, function(l) as.numeric(!is.na(y) & y == l))
  names(ind) <- paste0(".k", seq_along(target_levels))
  ind$.n <- rep(1, nrow(v))
  if (drop_na_target) {
    # NA target -> NA everywhere, so the unit leaves the domain entirely
    ind <- lapply(ind, function(z) {
      z[is.na(y)] <- NA_real_
      z
    })
  }

  dt <- .domain_totals(
    design,
    vars = ind,
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  K <- length(target_levels)

  purrr::map_dfr(seq_along(dt$levels), function(g) {
    j_n <- dt$pos(g, K + 1L)
    purrr::map_dfr(seq_len(K), function(k) {
      est <- .delta_ratio(dt$coef, dt$vcov, dt$pos(g, k), j_n, z = 1)
      p <- est[["est"]]
      ic <- .beta_ci(p, est[["SE"]]^2, dt$n_obs[g], dt$degf[g], level)
      tibble::tibble(
        strat_var = dt$levels[g],
        target_var = target_levels[k],
        prop = p,
        IC_low = ic[1],
        IC_high = ic[2],
        SE = est[["SE"]],
        n_obs = dt$n_obs[g]
      )
    })
  }) |>
    dplyr::rename(
      !!strat_var := strat_var,
      !!target_var := target_var
    )
}

### MACRO SHARE: share of the aggregate pool going to each subgroup ----
###
### WHAT IT COMPUTES, exactly
###   share(g) = T_hat(target_var | strat = g) / T_hat(target_var | universe)
###   the fraction of the AGGREGATE pool of target_var that accrues to decile g.
###   The shares sum to 1 over the non-missing levels of strat_var: units with a
###   missing strat_var or a missing target_var leave both numerator and
###   denominator (unchanged behaviour).
### READ IT AS
###   a distributive question - "who gets the pool". A decile with 10 % of the
###   households and 30 % of the pool is over-served; the benchmark is
###   get_share_macro_overall() below, not the value 10 %.
### CONFIDENCE INTERVAL
###   delta method on a ratio of totals; the denominator is the sum of the
###   numerators, so both its variance and its covariance with the numerator are
###   propagated (this is what the earlier cbind(part, total) version did).
get_share_macro <- function(
  design,
  target_var,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- stats::qnorm((1 + level) / 2)

  # Two columns per decile: the variable AND a household counter. The counter
  # costs nothing extra (the cost of svytotal() is driven by the number of
  # strata, not of columns) and it is what makes the test below possible: the
  # share of decile g is compared with ITS OWN demographic weight N_g / sum N,
  # using the covariance between the two, which is now an entry of vcov().
  dt <- .domain_totals(
    design,
    vars = list(
      x = as.numeric(design$variables[[target_var]]),
      one = rep(1, nrow(design$variables))
    ),
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  G <- length(dt$levels)
  n <- length(dt$coef)
  b <- numeric(n)
  for (k in seq_len(G)) b[dt$pos(k, 1L)] <- 1 # denominator = sum of the parts

  out <- purrr::map_dfr(seq_len(G), function(k) {
    a <- numeric(n)
    a[dt$pos(k, 1L)] <- 1
    est <- .delta_ratio_lin(dt$coef, dt$vcov, a, b, z)
    tibble::tibble(
      strat_level = dt$levels[k],
      target_var = target_var,
      share = est[["est"]],
      SE = est[["SE"]],
      IC_low = est[["IC_low"]],
      IC_high = est[["IC_high"]],
      n_obs = dt$n_obs[k]
    )
  })

  # reference = N_g / sum(N), the "equal distribution" benchmark, decile by
  # decile. NOTE: N_g here counts the households with a NON-MISSING target_var,
  # so it can differ marginally from get_share_macro_overall() when the variable
  # has missing values; run_one_analysis() checks the two and warns.
  tst <- .test_levels_paired(
    dt, num_j = 1L, den_j = 1L, ref_num_j = 2L, ref_den_j = 2L,
    df = survey::degf(design), level = level
  )

  dplyr::bind_cols(out, tst) |>
    dplyr::rename(!!strat_var := strat_level)
}

### MACRO SHARE reference line: the "equal distribution" benchmark ----
###
### WHAT IT COMPUTES
###   ref_share(g) = N_hat(strat = g | universe) / N_hat(universe) * 100
###   the share of the pool decile g WOULD get if target_var were spread evenly
###   over households. Comparing get_share_macro() with it answers "is this
###   decile over- or under-served relative to its demographic weight?".
### CAVEAT
###   the denominator is not exactly the one of get_share_macro(): here every
###   household of the universe is counted, whereas get_share_macro() drops
###   households whose target_var is missing. The two coincide when target_var
###   has no missing value (the usual case here, since the income components are
###   coalesced to 0 upstream).
### NEW: the benchmark now comes with a confidence interval. The columns are
###   deliberately prefixed ref_* so that they cannot collide with the SE /
###   IC_low / IC_high of the main table when the two are joined in
###   make_decile_plot().
get_share_macro_overall <- function(
  design,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- stats::qnorm((1 + level) / 2)

  dt <- .domain_totals(
    design,
    vars = list(one = rep(1, nrow(design$variables))),
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  G <- length(dt$levels)
  b <- rep(1, G)

  purrr::map_dfr(seq_len(G), function(k) {
    a <- numeric(G)
    a[k] <- 1
    est <- .delta_ratio_lin(dt$coef, dt$vcov, a, b, z)
    tibble::tibble(
      strat_level = dt$levels[k],
      ref_share = est[["est"]] * 100, # *100: rounded as a % downstream
      ref_SE = est[["SE"]] * 100,
      ref_IC_low = est[["IC_low"]] * 100,
      ref_IC_high = est[["IC_high"]] * 100
    )
  }) |>
    dplyr::rename(!!strat_var := strat_level)
}

### MACRO RATIO: ratio of the aggregates inside a subgroup ----
###
### WHAT IT COMPUTES, exactly
###   ratio(g) = T_hat(numerator | strat = g) / T_hat(denominator | strat = g)
###   identical to survey::svyratio(~num, ~den, subset(design, strat == g)).
### READ IT AS
###   a structural question - "out of every peso of <denominator> received by
###   decile g AS A WHOLE, how much comes from <numerator>?". Households
###   contribute in proportion to their size, so one very large farm can drive
###   the whole decile. This is the right estimator for a composition that must
###   add up to 100 %.
### Units with a missing numerator OR denominator are dropped from both.
### CONFIDENCE INTERVAL: delta method, the standard estimator for a ratio of
###   totals under a complex design.
get_ratio_macro <- function(
  design,
  numerator,
  denominator,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- stats::qnorm((1 + level) / 2)

  dt <- .domain_totals(
    design,
    vars = c(num = numerator, den = denominator),
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  out <- purrr::map_dfr(seq_along(dt$levels), function(k) {
    est <- .delta_ratio(dt$coef, dt$vcov, dt$pos(k, 1L), dt$pos(k, 2L), z)
    tibble::tibble(
      strat_var = dt$levels[k],
      ratio = est[["est"]],
      SE = est[["SE"]],
      IC_low = est[["IC_low"]],
      IC_high = est[["IC_high"]],
      n_obs = dt$n_obs[k]
    )
  })

  # reference = the ratio of the pooled totals, i.e. the "overall" line
  tst <- .test_levels(dt, 1L, 2L, 1L, 2L, df = survey::degf(design), level = level)

  dplyr::bind_cols(out, tst) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MICRO RATIO: mean of the individual ratios inside a subgroup ----
###
### WHAT IT COMPUTES, exactly
###   ratio(g) = weighted MEAN over households i of decile g of r_i = num_i/den_i
###            = T_hat(r * 1[g]) / N_hat(g)
###   identical to svymean(~r, subset(design, strat == g), na.rm = TRUE).
### READ IT AS
###   a "typical household" question: every household counts for one, whatever
###   its size. The gap with get_ratio_macro() measures the heterogeneity of
###   farm size and the correlation between scale and the ratio.
### STATISTICAL CAUTIONS (unchanged behaviour, now made explicit)
###   * households with den_i == 0 give a non-finite ratio and are SILENTLY
###     EXCLUDED: the estimate is conditional on den_i != 0. n_obs now reports
###     how many households actually contribute - compare it with the decile
###     size before interpreting;
###   * households with den_i < 0 give a NEGATIVE ratio, which is not a share;
###   * r_i is unbounded above and very heavy-tailed when den_i is small, so the
###     mean is driven by a handful of households and the normal CI is
###     optimistic. This is exactly why get_ratio_median() below is usually the
###     more readable summary.
get_ratio_micro <- function(
  design,
  numerator,
  denominator,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- stats::qnorm((1 + level) / 2)

  v <- design$variables
  r <- as.numeric(v[[numerator]]) / as.numeric(v[[denominator]])
  r[!is.finite(r)] <- NA_real_

  dt <- .domain_totals(
    design,
    vars = list(r = r, one = rep(1, nrow(v))),
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  out <- purrr::map_dfr(seq_along(dt$levels), function(k) {
    est <- .delta_ratio(dt$coef, dt$vcov, dt$pos(k, 1L), dt$pos(k, 2L), z)
    tibble::tibble(
      strat_var = dt$levels[k],
      ratio = est[["est"]],
      SE = est[["SE"]],
      IC_low = est[["IC_low"]],
      IC_high = est[["IC_high"]],
      n_obs = dt$n_obs[k]
    )
  })

  # reference = mean of the individual ratios over the whole universe
  tst <- .test_levels(dt, 1L, 2L, 1L, 2L, df = survey::degf(design), level = level)

  dplyr::bind_cols(out, tst) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MICRO SHARE: mean of the individual shares of the aggregate ----
###
### WHAT IT COMPUTES, exactly
###   share(g) = weighted mean over households i of decile g of
###              x_i / T_hat(x | universe)
###            = mean_hat(x | strat = g) / T_hat(x | universe)
###   The universe total is treated as a CONSTANT (unchanged behaviour), so the
###   profile across deciles is EXACTLY the profile of the decile MEANS of x,
###   divided by one single number; and the "average individual share" reference
###   is mechanically 1 / N_hat(universe).
### READ IT AS
###   "the average household of decile g captures this fraction of the pool".
###   If all you want is to compare deciles, plotting the decile means of x is
###   strictly more readable - the share only rescales them.
### The variance of T_hat is ignored. The neglected terms are O(1/N) here, hence
###   numerically irrelevant, but the CI is formally conditional on T_hat.
get_share_micro <- function(
  design,
  target_var,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99
) {
  z <- stats::qnorm((1 + level) / 2)

  v <- design$variables
  x <- as.numeric(v[[target_var]])
  w <- stats::weights(design, "sampling")

  # universe total: identical to svytotal(~target, design_filtered, na.rm = TRUE)
  sel <- is.finite(x) & is.finite(w)
  if (!is.null(filter_var) && !is.null(filter_value)) {
    fv <- as.character(v[[filter_var]])
    sel <- sel & !is.na(fv) & fv == filter_value
  }
  total_val <- sum(w[sel] * x[sel])

  s <- x / total_val
  s[!is.finite(s)] <- NA_real_

  dt <- .domain_totals(
    design,
    vars = list(s = s, one = rep(1, nrow(v))),
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value
  )

  out <- purrr::map_dfr(seq_along(dt$levels), function(k) {
    est <- .delta_ratio(dt$coef, dt$vcov, dt$pos(k, 1L), dt$pos(k, 2L), z)
    tibble::tibble(
      strat_var = dt$levels[k],
      share = est[["est"]],
      SE = est[["SE"]],
      IC_low = est[["IC_low"]],
      IC_high = est[["IC_high"]],
      n_obs = dt$n_obs[k]
    )
  })

  # reference = mean individual share over the whole universe (= 1 / N_hat)
  tst <- .test_levels(dt, 1L, 2L, 1L, 2L, df = survey::degf(design), level = level)

  dplyr::bind_cols(out, tst) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MEDIANS of an individual ratio / share inside a subgroup ----
#
# WHY A MEDIAN
#   The mean of individual ratios (get_ratio_micro) is hard to read: r_i is
#   unbounded, heavy-tailed, and its mean has no simple interpretation. The
#   weighted MEDIAN of r_i answers a question a reader understands directly:
#   "for the MEDIAN agricultural household of this decile, what fraction of its
#   total support comes from programme X?".
#
# WHAT IS ESTIMATED
#   median(g) = the weighted median over households i of decile g of r_i,
#   using survey's "math" quantile rule (the same rule already used for the
#   decile cut-off points in 1B), i.e. the inverse of the weighted empirical CDF.
#
# CONFIDENCE INTERVAL (this is the important part)
#   NOT est +/- z * SE. A median has no usable asymptotic normal SE here; the
#   textbook construction is the WOODRUFF interval: build a design-based CI for
#   the proportion F_hat(q) of households below the estimated median, then map
#   its two bounds back through the weighted quantile function.
#     interval_type = "beta" (default) uses the Korn-Graubard interval for that
#       proportion, consistent with get_proportion() and with the decile
#       cut-offs of 1B; it stays inside the support and does not assume symmetry.
#     interval_type = "mean" uses a Wald interval for the proportion instead.
#   This reproduces survey::svyquantile(interval.type = , qrule = "math") to
#   machine precision (see test_estimators.R), but in ONE svytotal() call for
#   all deciles instead of one subset design per decile - ~8 s per decile saved.
#   The reported SE is survey's own convention, (upper - lower) / (2 * t_{1-a/2}),
#   i.e. the width of the CI re-expressed as a standard error; it is what feeds
#   the CV-based reliability flag of the plots.
#
# WHO IS IN THE DOMAIN (read this before interpreting)
#   * units with a non-finite r_i (den_i == 0) are excluded, as in the mean;
#   * by default (positive_denominator = TRUE) units with den_i <= 0 are ALSO
#     excluded, because a "share" with a negative or null base is meaningless.
#     Set it to FALSE to reproduce get_ratio_micro()'s domain exactly;
#   * units with num_i == 0 ARE kept. This matters: if more than half of the
#     households of a decile receive nothing of that component, the median is
#     exactly 0 and that is the correct answer, not a bug. The returned column
#     `p_zero` gives the weighted share of households at 0 so you can see it
#     coming, and `n_obs` / `n_pop` give the size of the domain actually used.
#
# WHAT A MEDIAN CANNOT DO
#   Medians are NOT additive: the medians of the components of a total do not
#   sum to the median of the total, and in general do not sum to 100 %.
#   => never stack them in a 100 % bar chart. get_share_median_overall() below
#   returns the components side by side and reports their sum precisely so that
#   the gap is visible rather than hidden.

# Internal worker shared by the three public median functions.
.median_domain <- function(
  design,
  value,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  interval_type = c("beta", "mean"),
  quantile = 0.5
) {
  interval_type <- match.arg(interval_type)
  alpha <- 1 - level

  ds <- .domain_setup(
    design,
    strat_var,
    filter_var,
    filter_value,
    ok = is.finite(value)
  )
  G <- length(ds$levels)
  w <- ds$w
  df_design <- survey::degf(design)

  # step 1 - point estimates, no survey call needed
  qhat <- vapply(
    seq_len(G),
    function(k) .wq_math(value[ds$sel[[k]]], w[ds$sel[[k]]], quantile),
    numeric(1)
  )
  # ... and the REFERENCE quantile, pooled over every level (= "overall")
  keep_all <- Reduce(`|`, ds$sel)
  q_ref <- .wq_math(value[keep_all], w[keep_all], quantile)

  # step 2 - ONE svytotal for, per level: 1[value <= qhat_g] (Woodruff CI),
  #          the domain size, and 1[value <= q_ref] (test against the overall
  #          median, see step 4)
  M <- matrix(0, nrow = ds$n, ncol = 3L * G)
  for (k in seq_len(G)) {
    s <- ds$sel[[k]]
    M[s, 3L * k - 2L] <- as.numeric(value[s] <= qhat[k])
    M[s, 3L * k - 1L] <- 1
    M[s, 3L * k] <- as.numeric(value[s] <= q_ref)
  }
  tt <- survey::svytotal(M, design, na.rm = FALSE)
  co <- as.numeric(stats::coef(tt))
  vc <- as.matrix(stats::vcov(tt))

  # Woodruff interval for the REFERENCE (pooled) median, from the pooled totals
  # of the same svytotal - this is what the plots call "overall", and computing
  # it here avoids a second full estimator call per analysis.
  pool_ind <- sum(co[3L * seq_len(G)])
  pool_n <- sum(co[3L * seq_len(G) - 1L])
  a_pool <- numeric(length(co))
  b_pool <- numeric(length(co))
  a_pool[3L * seq_len(G)] <- 1
  b_pool[3L * seq_len(G) - 1L] <- 1
  se_pool <- {
    Y <- pool_n
    g <- a_pool / Y - (pool_ind / Y^2) * b_pool
    sqrt(max(as.numeric(t(g) %*% vc %*% g), 0))
  }
  p_pool <- pool_ind / pool_n
  df_pool <- {
    psu <- design$cluster[[1]][keep_all]
    str <- design$strata[[1]][keep_all]
    length(unique(psu)) - length(unique(str))
  }
  pci_pool <- if (interval_type == "beta") {
    .beta_ci(p_pool, se_pool^2, sum(ds$n_obs), df_pool, level)
  } else {
    cr <- stats::qt(1 - (1 - level) / 2, df_pool)
    c(p_pool - cr * se_pool, p_pool + cr * se_pool)
  }
  ref_lo <- if (is.na(pci_pool[1]) || pci_pool[1] < 0) NA_real_ else
    .wq_math(value[keep_all], w[keep_all], pci_pool[1])
  ref_hi <- if (is.na(pci_pool[2]) || pci_pool[2] > 1) NA_real_ else
    .wq_math(value[keep_all], w[keep_all], pci_pool[2])
  ref_se <- (ref_hi - ref_lo) /
    (2 * stats::qt(1 - (1 - level) / 2, df_pool))

  # step 3 - Woodruff: CI on the proportion, then back through the quantile fn
  alpha <- 1 - level
  out <- purrr::map_dfr(seq_len(G), function(k) {
    i <- 3L * k - 2L
    j <- 3L * k - 1L
    est_p <- .delta_ratio(co, vc, i, j, z = 1)
    p_hat <- est_p[["est"]]
    se_p <- est_p[["SE"]]
    df <- ds$degf[k]

    pci <- if (interval_type == "beta") {
      .beta_ci(p_hat, se_p^2, ds$n_obs[k], df, level)
    } else {
      crit <- if (is.finite(df) && df > 0) stats::qt(1 - alpha / 2, df) else
        stats::qnorm(1 - alpha / 2)
      c(p_hat - crit * se_p, p_hat + crit * se_p)
    }

    x_k <- value[ds$sel[[k]]]
    w_k <- w[ds$sel[[k]]]
    lo <- if (is.na(pci[1]) || pci[1] < 0) NA_real_ else .wq_math(x_k, w_k, pci[1])
    hi <- if (is.na(pci[2]) || pci[2] > 1) NA_real_ else .wq_math(x_k, w_k, pci[2])

    # survey's convention: half-width re-expressed as a standard error
    qcrit <- if (is.finite(df) && df > 0) stats::qt(1 - alpha / 2, df) else
      stats::qnorm(1 - alpha / 2)
    se_q <- (hi - lo) / (2 * qcrit)

    # step 4 - test against the OVERALL median.
    # There is no usable linearised variance for the difference of two medians,
    # so the survey analogue of MOOD'S MEDIAN TEST is used instead: under the
    # null "the median of decile g equals the overall median", the share of
    # households of g falling below the overall median is 50 %. That share is an
    # ordinary design-based proportion, so it has an exact variance and a
    # Korn-Graubard interval. It also reads directly: "63 % of the households of
    # D1 are below the overall median, instead of 50 %".
    # (the overall median is treated as fixed, as in the classical median test)
    est_b <- .delta_ratio(co, vc, 3L * k, j, z = 1)
    p_b <- est_b[["est"]]
    se_b <- est_b[["SE"]]
    ic_b <- .beta_ci(p_b, se_b^2, ds$n_obs[k], df, level)
    p_val <- if (is.finite(se_b) && se_b > 0) {
      2 * stats::pt(-abs((p_b - 0.5) / se_b),
                    df = if (is.finite(df_design) && df_design > 0) df_design else Inf)
    } else {
      NA_real_
    }

    tibble::tibble(
      strat_var = ds$levels[k],
      med = qhat[k],
      SE = se_q,
      IC_low = lo,
      IC_high = hi,
      n_obs = ds$n_obs[k],
      n_pop = ds$n_pop[k],
      p_zero = sum(w_k[x_k == 0]) / sum(w_k),
      ref = q_ref,
      ref_SE = ref_se,
      ref_IC_low = ref_lo,
      ref_IC_high = ref_hi,
      diff = qhat[k] - q_ref,
      # no valid linearised CI for a difference of medians - see step 4
      diff_SE = NA_real_,
      diff_IC_low = NA_real_,
      diff_IC_high = NA_real_,
      share_below_ref = p_b,
      share_below_SE = se_b,
      share_below_IC_low = ic_b[1],
      share_below_IC_high = ic_b[2],
      p_value = p_val
    )
  })

  # linear trend across the levels, on the scale actually tested (the share of
  # households below the overall median, deviation from 0.5)
  specs <- lapply(seq_len(G), function(k) {
    a <- numeric(length(co)); b <- numeric(length(co))
    a[3L * k] <- 1
    b[3L * k - 1L] <- 1
    list(a_est = a, b_est = b, a_ref = numeric(length(co)),
         b_ref = numeric(length(co)))
  })
  # reference is the constant 0.5, whose gradient is zero: encode it by using
  # the same denominator with a numerator of half its weight
  specs <- lapply(specs, function(sp) {
    sp$a_ref <- 0.5 * sp$b_est
    sp$b_ref <- sp$b_est
    sp
  })
  trend <- .trend_test(co, vc, specs, df_design)

  out |>
    dplyr::mutate(
      p_value_adj = stats::p.adjust(p_value, method = "holm"),
      signif = is.finite(p_value) & p_value < alpha,
      signif_adj = is.finite(p_value_adj) & p_value_adj < alpha,
      test = "median test: share below the overall median vs 50%",
      trend_slope = trend$trend_slope,
      trend_SE = trend$trend_SE,
      trend_p = trend$trend_p
    )
}

# Restrict a ratio to the units where it is a meaningful share.
.make_ratio <- function(num, den, positive_denominator = TRUE) {
  r <- as.numeric(num) / as.numeric(den)
  r[!is.finite(r)] <- NA_real_
  if (positive_denominator) r[!is.finite(den) | den <= 0] <- NA_real_
  r
}

### MEDIAN RATIO ----
### median over households of decile g of num_i / den_i.
### Drop-in replacement for get_ratio_macro / get_ratio_micro in
### run_one_analysis(): same arguments, and the estimate is in a column named
### `ratio` so that value_col = "ratio" keeps working.
get_ratio_median <- function(
  design,
  numerator,
  denominator,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  interval_type = "beta",
  positive_denominator = TRUE
) {
  v <- design$variables
  r <- .make_ratio(v[[numerator]], v[[denominator]], positive_denominator)

  .median_domain(
    design = design,
    value = r,
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value,
    level = level,
    interval_type = interval_type
  ) |>
    dplyr::rename(ratio = med) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MEDIAN SHARE ----
### median over households of decile g of x_i / T_hat(x | universe), i.e. the
### median-household counterpart of get_share_micro().
### WARNING, same as for get_share_micro(): the denominator is a single constant,
### so this is just the decile MEDIAN of x rescaled. It is the median of a share
### OF THE POOL, not the median of a within-household composition share - for
### the latter (e.g. "what part of ITS OWN total support does the median
### household get from programme X?") use get_ratio_median() or
### get_share_median_overall().
get_share_median <- function(
  design,
  target_var,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  interval_type = "beta"
) {
  v <- design$variables
  x <- as.numeric(v[[target_var]])
  w <- stats::weights(design, "sampling")

  sel <- is.finite(x) & is.finite(w)
  if (!is.null(filter_var) && !is.null(filter_value)) {
    fv <- as.character(v[[filter_var]])
    sel <- sel & !is.na(fv) & fv == filter_value
  }
  total_val <- sum(w[sel] * x[sel])

  s <- x / total_val
  s[!is.finite(s)] <- NA_real_

  .median_domain(
    design = design,
    value = s,
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value,
    level = level,
    interval_type = interval_type
  ) |>
    dplyr::rename(share = med) |>
    dplyr::rename(!!strat_var := strat_var)
}

### MEDIAN SHARES OF ALL THE COMPONENTS OF A TOTAL ----
### For every component c of `components` and every level g of strat_var:
###   median over households i of decile g of  component_c(i) / den(i)
### i.e. the COMPOSITION OF THE MEDIAN HOUSEHOLD - "in the total support received
### by the median agricultural family of decile g, what part comes from each
### programme?".
###
### Same arguments as run_composition_analysis() (a named list of component
### variables and a common denominator), so it can be used as a third estimator
### there. Returns a long tibble with one row per (level, component).
###
### READ THE WARNING ABOVE: the medians of the components do NOT sum to 100 %.
### The function computes that sum per level and reports it (invisibly in the
### `sum_of_medians` column, and as a message) so that the discrepancy is
### explicit. Plot these as grouped bars or dots, never stacked.
get_share_median_overall <- function(
  design,
  components,
  den,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  level = 0.99,
  interval_type = "beta",
  positive_denominator = TRUE,
  verbose = TRUE
) {
  stopifnot(is.list(components) || is.character(components))
  if (is.null(names(components))) names(components) <- unlist(components)

  out <- purrr::map_dfr(names(components), function(comp_name) {
    get_ratio_median(
      design = design,
      numerator = components[[comp_name]],
      denominator = den,
      strat_var = strat_var,
      filter_var = filter_var,
      filter_value = filter_value,
      level = level,
      interval_type = interval_type,
      positive_denominator = positive_denominator
    ) |>
      dplyr::mutate(component = comp_name, .before = 1)
  })

  out <- out |>
    dplyr::group_by(.data[[strat_var]]) |>
    dplyr::mutate(sum_of_medians = sum(ratio, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (isTRUE(verbose)) {
    chk <- out |>
      dplyr::distinct(.data[[strat_var]], sum_of_medians) |>
      dplyr::mutate(sum_pct = round(sum_of_medians * 100, 1))
    message(
      "\n--- get_share_median_overall(): medians are NOT additive ---\n",
      "Sum of the component medians, by level of ",
      strat_var,
      " (100 % would be a coincidence):\n",
      paste0(
        "  ",
        chk[[strat_var]],
        " : ",
        chk$sum_pct,
        " %",
        collapse = "\n"
      ),
      "\nPlot these side by side, never stacked. For an exact 100 %",
      " decomposition use get_ratio_macro() (macro composition).\n"
    )
  }

  out
}
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
    geom_vline(
      xintercept = length(strat_lvl_with_total) - 0.5,
      linetype = "dashed",
      color = "grey50"
    ) +
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
      tbl_raw <- map_dfr(names(components), function(comp_name) {
        estimator_fn(
          design = design,
          numerator = components[[comp_name]],
          denominator = den,
          strat_var = strat,
          filter_var = universe,
          filter_value = filter
        ) |>
          mutate(component = comp_name)
      })
      tbl <- tbl_raw |>
        mutate(across(
          any_of(c(
            "ratio", "SE", "IC_low", "IC_high", "ref", "ref_SE",
            "ref_IC_low", "ref_IC_high", "diff", "diff_SE",
            "diff_IC_low", "diff_IC_high"
          )),
          ~ round(. * 100, 2)
        ))

      strat_lvl <- levels(as.factor(d[[strat]]))
      strat_lvl_with_total <- c(strat_lvl, "Total")

      tbl <- tbl |>
        mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
        arrange(.data[[strat]])

      # The stacked bars are only meaningful if the components really add up.
      # The macro estimator is exactly additive when the components exhaust the
      # denominator; the MICRO one is not necessarily, because each component
      # drops its own non-finite ratios (den_i == 0) and therefore may be
      # computed on a slightly different set of households. Report the drift
      # instead of letting geom_col() hide it.
      comp_sum <- tbl |>
        group_by(.data[[strat]]) |>
        summarise(total = sum(ratio, na.rm = TRUE), .groups = "drop") |>
        filter(abs(total - 100) > 0.5)
      if (nrow(comp_sum) > 0) {
        message(
          "\n WARNING - ",
          estimator,
          " composition of ",
          den,
          ": the components do not sum to 100% in ",
          paste0(comp_sum[[strat]], " (", round(comp_sum$total, 1), "%)",
                 collapse = ", "),
          ".\n   Either the components do not exhaust the denominator, or (micro)",
          " they are not estimated on the same households.\n"
        )
      }

      # --- calcul overall ---
      # Same shortcut as in run_one_analysis(): the pooled reference is already
      # in the ref_* columns of tbl_raw, produced by the same svytotal(). This
      # removes one full estimator call PER COMPONENT (24 components x 3
      # universes x 2 estimators = 144 avoided passes over the design).
      if (all(c("ref", "ref_SE", "ref_IC_low", "ref_IC_high") %in%
                names(tbl_raw))) {
        overall <- tbl_raw |>
          group_by(component) |>
          slice(1) |>
          ungroup() |>
          transmute(
            !!strat := if (is.null(filter)) "Total" else filter,
            ratio = ref,
            SE = ref_SE,
            IC_low = ref_IC_low,
            IC_high = ref_IC_high,
            component = component
          ) |>
          mutate(across(
            c(ratio, SE, IC_low, IC_high),
            ~ round(. * 100, 2)
          ))
      } else {
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
      }

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
      show_plot(plot)
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


### composition analysis of the MEDIAN household ----
#
# Companion of run_composition_analysis(), for get_share_median_overall().
# It deliberately does NOT reuse make_composition_plot(): that function stacks
# the components to 100 % and places its labels with cumsum(), which is only
# valid for an ADDITIVE decomposition. Component medians are not additive, so
# they are drawn side by side, with their Woodruff intervals, and the sum of the
# medians is printed in the caption instead of being silently forced to 100 %.

make_median_composition_plot <- function(
  tbl,
  strat,
  strat_lvl,
  component_labels,
  title,
  subtitle,
  caption,
  col_pal = "cividis",
  direction = 1,
  begin = 0,
  end = 1,
  cv_threshold = 0.3
) {
  plot_data <- tbl |>
    mutate(
      !!strat := factor(.data[[strat]], levels = strat_lvl),
      component = factor(
        component,
        levels = names(component_labels),
        labels = component_labels
      ),
      cv = abs(SE / ratio),
      unreliable = !is.na(cv) & cv > cv_threshold
    )

  sums <- plot_data |>
    group_by(.data[[strat]]) |>
    summarise(s = round(first(sum_of_medians), 1), .groups = "drop")

  ggplot(
    plot_data,
    aes(x = .data[[strat]], y = ratio, fill = component)
  ) +
    geom_col(
      width = 0.8,
      alpha = 0.85,
      colour = "white",
      position = position_dodge(width = 0.8)
    ) +
    geom_errorbar(
      aes(ymin = IC_low, ymax = IC_high),
      width = 0.25,
      colour = "grey30",
      position = position_dodge(width = 0.8)
    ) +
    geom_point(
      data = filter(plot_data, unreliable),
      aes(y = IC_high),
      shape = 8,
      size = 1.6,
      colour = "grey30",
      position = position_dodge(width = 0.8),
      show.legend = FALSE
    ) +
    scale_fill_viridis_d(
      option = col_pal,
      direction = direction,
      begin = begin,
      end = end,
      name = "Component"
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.05)),
      labels = percent_format(scale = 1)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Income decile",
      y = "%",
      caption = str_c(
        caption,
        paste0(
          "Medians are not additive: the component medians sum to ",
          paste0(sums[[strat]], ": ", sums$s, " %", collapse = " | "),
          ", not to 100 %."
        ),
        paste0(
          "* CV > ",
          cv_threshold * 100,
          "%: estimate unreliable, interpret with caution."
        ),
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
}

# Same call signature as run_composition_analysis(), minus the estimator loop.
run_median_composition_analysis <- function(
  design,
  d,
  components,
  component_labels,
  den,
  strat,
  universes, # list of list(universe = , filter = )
  basename,
  title_median = NULL,
  caption_median = NULL,
  col_pal = "cividis",
  direction = 1,
  begin = 0,
  end = 1,
  cv_threshold = 0.3,
  level = 0.99,
  interval_type = "beta",
  positive_denominator = TRUE
) {
  if (is.null(title_median)) {
    title_median <- str_c("Composition of ", den, " for the median household")
  }
  if (is.null(caption_median)) {
    caption_median <- paste(
      "Each bar is the MEDIAN, across the households of the decile, of the share",
      "of that component in the household's own total.",
      "Error bars are 99% Woodruff intervals and are asymmetric by construction.",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  }

  strat_lvl <- levels(as.factor(d[[strat]]))

  for (u in universes) {
    universe <- u$universe
    filter <- u$filter
    suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
    subtitle <- str_c(
      "Universe: ",
      if (is.null(filter)) "total population" else filter
    )

    tbl <- get_share_median_overall(
      design = design,
      components = components,
      den = den,
      strat_var = strat,
      filter_var = universe,
      filter_value = filter,
      level = level,
      interval_type = interval_type,
      positive_denominator = positive_denominator
    ) |>
      mutate(across(
        c(ratio, SE, IC_low, IC_high, sum_of_medians),
        ~ round(. * 100, 2)
      )) |>
      mutate(across(
        any_of(c(
          "ref", "diff", "share_below_ref", "share_below_SE",
          "share_below_IC_low", "share_below_IC_high"
        )),
        ~ round(. * 100, 2)
      )) |>
      mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
      arrange(.data[[strat]], component)

    name <- str_c("median_", basename)
    custom_save(tbl, str_c(name, "_", suffix))

    plot <- make_median_composition_plot(
      tbl = tbl,
      strat = strat,
      strat_lvl = strat_lvl,
      component_labels = component_labels,
      title = title_median,
      subtitle = subtitle,
      caption = caption_median,
      col_pal = col_pal,
      direction = direction,
      begin = begin,
      end = end,
      cv_threshold = cv_threshold
    )

    show_plot(plot)
    custom_save(plot, str_c("plot_", name, "_", suffix), type = "fig")
  }
}


### COMPOSITION OF THE MEDIAN HOUSEHOLD (band around the median) ----
#
# WHAT QUESTION THIS ANSWERS
#   "In the total support received by the MEDIAN agricultural family of decile g,
#    what part comes from each programme?"
#
#   That is NOT what get_share_median_overall() computes. There, the median of
#   each component's share is taken separately, so each median is read off a
#   DIFFERENT ordering of the households: the household that is median for
#   Sembrando Vida is not the one that is median for Nacional de Fertilizantes.
#   That is exactly why those medians do not sum to 100 % - they are nobody's
#   actual budget.
#
#   Here we instead look at ACTUAL households sitting around the median, and
#   describe THEIR budget. The composition therefore sums to exactly 100 %.
#
# WHY A BAND AND NOT THE SINGLE MEDIAN HOUSEHOLD
#   Taking literally the one household at the 50th percentile gives an exactly
#   additive composition, but it is a single observation: no meaningful variance,
#   and it moves entirely if one unit changes. Taking a narrow band around the
#   median (P45-P55 by default) keeps the interpretation - "the households around
#   the median" - while giving a proper design-based estimate with a confidence
#   interval, computed as an ordinary macro composition INSIDE the band.
#
# MEDIAN ACCORDING TO WHAT
#   `rank_var` sets the ordering. The default is the denominator itself (e.g. the
#   median household in terms of total support received), which is the reading of
#   "the median agricultural family" when the sentence is about the composition of
#   its support. Pass rank_var = "n_ing_equivaled" to define the median household
#   by income instead. The band is computed SEPARATELY WITHIN EACH LEVEL of
#   strat_var, using the weighted quantile rule of the rest of the project.
#
# WHO IS IN
#   Households with den <= 0 are excluded: they have no composition to describe.
#   n_obs reports how many households the band actually contains - check it, a
#   10-point band on a small decile can be thin.
get_share_median_band <- function(
  design,
  components,
  den,
  strat_var,
  filter_var = NULL,
  filter_value = NULL,
  band = c(0.45, 0.55),
  rank_var = NULL,
  level = 0.99,
  verbose = TRUE
) {
  stopifnot(length(band) == 2, band[1] < band[2], band[1] >= 0, band[2] <= 1)
  if (is.null(names(components))) names(components) <- unlist(components)
  z <- stats::qnorm((1 + level) / 2)

  v <- design$variables
  den_v <- as.numeric(v[[den]])
  rank_v <- if (is.null(rank_var)) den_v else as.numeric(v[[rank_var]])

  # eligible units: positive denominator, usable ranking variable
  ok <- is.finite(den_v) & den_v > 0 & is.finite(rank_v)

  ds <- .domain_setup(design, strat_var, filter_var, filter_value, ok = ok)
  w <- ds$w
  G <- length(ds$levels)

  # band membership, computed within each level
  in_band <- rep(FALSE, ds$n)
  cuts <- vector("list", G)
  for (k in seq_len(G)) {
    s <- ds$sel[[k]]
    lo <- .wq_math(rank_v[s], w[s], band[1])
    hi <- .wq_math(rank_v[s], w[s], band[2])
    cuts[[k]] <- c(lo, hi)
    in_band[s] <- rank_v[s] >= lo & rank_v[s] <= hi
  }

  # macro composition INSIDE the band: one svytotal for every component + den
  vars <- c(
    stats::setNames(
      lapply(names(components), function(cn) as.numeric(v[[components[[cn]]]])),
      names(components)
    ),
    stats::setNames(list(den_v), ".den")
  )
  dt <- .domain_totals(
    design,
    vars = vars,
    strat_var = strat_var,
    filter_var = filter_var,
    filter_value = filter_value,
    ds = .domain_setup(
      design, strat_var, filter_var, filter_value,
      ok = ok & in_band & Reduce(`&`, lapply(vars, is.finite))
    )
  )

  nc <- length(components)
  out <- purrr::map_dfr(seq_along(dt$levels), function(k) {
    j_den <- dt$pos(k, nc + 1L)
    purrr::map_dfr(seq_len(nc), function(c_i) {
      est <- .delta_ratio(dt$coef, dt$vcov, dt$pos(k, c_i), j_den, z)
      tibble::tibble(
        strat_var = dt$levels[k],
        ratio = est[["est"]],
        SE = est[["SE"]],
        IC_low = est[["IC_low"]],
        IC_high = est[["IC_high"]],
        n_obs = dt$n_obs[k],
        n_pop = dt$n_pop[k],
        component = names(components)[c_i]
      )
    })
  })

  out <- out |>
    dplyr::group_by(.data$strat_var) |>
    dplyr::mutate(sum_of_shares = sum(ratio, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (isTRUE(verbose)) {
    chk <- out |> dplyr::distinct(.data$strat_var, .data$n_obs, .data$sum_of_shares)
    message(
      "\n--- get_share_median_band(): composition of the households around the ",
      "median of ", if (is.null(rank_var)) den else rank_var,
      " (band P", band[1] * 100, "-P", band[2] * 100, ") ---\n",
      paste0(
        "  ", chk$strat_var, " : ", chk$n_obs, " household(s) in the band, ",
        "shares sum to ", round(chk$sum_of_shares * 100, 1), " %",
        collapse = "\n"
      ),
      "\n(the sum is 100 % by construction: this is a real macro composition ",
      "computed inside the band)\n"
    )
  }

  out |> dplyr::rename(!!strat_var := strat_var)
}

# Same call signature as run_composition_analysis(); reuses make_composition_plot()
# because a band composition IS additive and can legitimately be stacked to 100 %.
run_median_band_composition_analysis <- function(
  design,
  d,
  components,
  component_labels,
  den,
  strat,
  universes,
  basename,
  title = NULL,
  caption = NULL,
  band = c(0.45, 0.55),
  rank_var = NULL,
  col_pal = "cividis",
  direction = 1,
  begin = 0,
  end = 1,
  cv_threshold = 0.3,
  level = 0.99
) {
  if (is.null(title)) {
    title <- str_c("Composition of ", den, " for households around the median")
  }
  if (is.null(caption)) {
    caption <- paste(
      str_c(
        "Households between the ", band[1] * 100, "th and ", band[2] * 100,
        "th percentile of ", if (is.null(rank_var)) den else rank_var,
        ", computed separately within each decile."
      ),
      "Unlike a median of shares, this is a real composition: it sums to 100 %.",
      "Source: Based on ENIGH data.",
      sep = "\n"
    )
  }

  strat_lvl <- levels(as.factor(d[[strat]]))
  strat_lvl_with_total <- c(strat_lvl, "Total")

  for (u in universes) {
    universe <- u$universe
    filter <- u$filter
    suffix <- if (is.null(filter)) "total" else str_remove(filter, ".*_")
    subtitle <- str_c(
      "Universe: ",
      if (is.null(filter)) "total population" else filter
    )

    tbl_raw <- get_share_median_band(
      design = design, components = components, den = den, strat_var = strat,
      filter_var = universe, filter_value = filter,
      band = band, rank_var = rank_var, level = level
    )
    tbl <- tbl_raw |>
      mutate(across(
        c(ratio, SE, IC_low, IC_high, sum_of_shares),
        ~ round(. * 100, 2)
      )) |>
      mutate(!!strat := factor(.data[[strat]], levels = strat_lvl)) |>
      arrange(.data[[strat]], component)

    # "Total" bar: the band of the whole universe, not of a decile
    overall <- get_share_median_band(
      design = design, components = components, den = den,
      strat_var = if (is.null(universe)) strat else universe,
      filter_var = universe, filter_value = filter,
      band = band, rank_var = rank_var, level = level, verbose = FALSE
    ) |>
      mutate(across(
        c(ratio, SE, IC_low, IC_high),
        ~ round(. * 100, 2)
      ))
    # make_composition_plot() renames column 1 to the strat variable
    colnames(overall)[1] <- strat

    name <- str_c("medianband_", basename)
    custom_save(bind_rows(tbl, overall), str_c(name, "_", suffix))

    plot <- make_composition_plot(
      tbl = tbl,
      overall = overall,
      strat = strat,
      strat_lvl_with_total = strat_lvl_with_total,
      component_labels = component_labels,
      title = title,
      subtitle = subtitle,
      caption = caption,
      col_pal = col_pal,
      direction = direction,
      begin = begin,
      end = end,
      cv_threshold = cv_threshold
    )
    show_plot(plot)
    custom_save(plot, str_c("plot_", name, "_", suffix), type = "fig")
  }
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

  # scientific notation only when the WHOLE series is tiny (micro shares are
  # of order 1e-6); the previous test used min() and switched a perfectly
  # readable percentage plot to scientific as soon as one decile was small.
  use_scientific <- max(abs(tbl[[value_col]]), na.rm = TRUE) < 0.1

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
        xmax = nlevels(droplevels(tbl[[strat_var]])) + 0.5,
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
    # deciles NOT significantly different from the reference (see the companion
    # plot_signif_* figure for the actual test)
    (if ("signif" %in% names(tbl)) {
      geom_text(
        data = filter(tbl, !signif),
        aes(x = .data[[strat_var]], y = IC_high, label = "ns"),
        vjust = -0.8,
        color = "grey45",
        size = 3.2,
        inherit.aes = FALSE
      )
    } else {
      list()
    }) +
    # LOESS trend. Purely indicative: it is an UNWEIGHTED local fit on the ten
    # decile point estimates, so it ignores both the survey weights and the very
    # unequal precision of those ten points, and it has no confidence band. Read
    # it as visual guidance, never as evidence of a gradient - the companion
    # plot_signif_* figure is what tests anything. Skipped when the series is
    # degenerate (constant or fewer than 4 distinct values), where loess either
    # errors or draws noise.
    (if (sum(is.finite(tbl[[value_col]])) >= 4 &&
         stats::var(tbl[[value_col]], na.rm = TRUE) > 0) {
      geom_smooth(
        aes(group = 1, fill = NULL),
        method = "loess",
        se = FALSE,
        color = "black",
        linewidth = 0.8,
        linetype = "dashed"
      )
    } else {
      list()
    }) +
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
            x = {
              lv <- levels(droplevels(tbl[[strat_var]]))
              lv[max(1L, length(lv) - 1L)]
            },
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


### significance plot: is this decile really different from the reference? ----
#
# A dot-and-whisker plot of the DIFFERENCE to the reference, with a zero line.
# Reading rule: a decile is significantly different from the reference when its
# interval does not cross zero - which is a proper test, unlike eyeballing
# whether the decile interval and the reference band overlap on the main plot.
#
# Two flavours, chosen automatically:
#  * mean-type estimators (macro/micro ratio, macro/micro share): the quantity
#    plotted is  estimate - reference, with its delta-method interval.
#  * medians: there is no usable interval for a difference of medians, so the
#    plot shows the SHARE OF HOUSEHOLDS OF THE DECILE BELOW THE OVERALL MEDIAN,
#    centred on the 50 % that the null hypothesis implies (survey version of
#    Mood's median test).
make_significance_plot <- function(
  tbl,
  strat_var,
  value_col,
  title,
  subtitle,
  caption,
  col_above,
  col_below,
  level = 0.99,
  y_label = NULL
) {
  is_median <- all(is.na(tbl$diff_SE)) && "share_below_ref" %in% names(tbl)

  dat <- if (is_median) {
    tbl |>
      mutate(
        .est = share_below_ref - 50,
        .lo = share_below_IC_low - 50,
        .hi = share_below_IC_high - 50
      )
  } else {
    tbl |>
      mutate(.est = diff, .lo = diff_IC_low, .hi = diff_IC_high)
  }

  dat <- dat |>
    mutate(
      status = case_when(
        is.na(signif) | !signif | !is.finite(.est) ~ "Not significant",
        .est >= 0 ~ "Above reference",
        TRUE ~ "Below reference"
      ),
      status = factor(
        status,
        levels = c("Above reference", "Below reference", "Not significant")
      ),
      # the star grade must agree with the colour: "ns" as soon as the decile
      # is not significant AT THE LEVEL OF THE PLOT (1% here), whatever the
      # conventional 5% thresholds would say
      stars = case_when(
        !is.finite(p_value) ~ "",
        !signif ~ "ns",
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        TRUE ~ "*"
      )
    )

  n_holm <- sum(dat$signif_adj, na.rm = TRUE)
  n_raw <- sum(dat$signif, na.rm = TRUE)

  # design-based linear trend of the gap across the deciles
  slope <- if ("trend_slope" %in% names(dat)) dat$trend_slope[1] else NA_real_
  trend_p <- if ("trend_p" %in% names(dat)) dat$trend_p[1] else NA_real_
  has_trend <- is.finite(slope) && is.finite(trend_p)
  if (has_trend) {
    rank <- as.integer(dat[[strat_var]])
    dat$.fit <- mean(dat$.est, na.rm = TRUE) + slope * (rank - mean(rank))
  }
  trend_txt <- if (has_trend) {
    paste0(
      "Linear trend across deciles: ",
      sprintf("%+.3f", slope), " point per decile step (p = ",
      format.pval(trend_p, digits = 2, eps = 1e-4), ") - ",
      if (trend_p >= 1 - level) {
        "no significant gradient"
      } else if (slope > 0) {
        "the gap WIDENS with income"
      } else {
        "the gap NARROWS with income"
      }
    )
  } else {
    NULL
  }

  ylab <- if (!is.null(y_label)) {
    y_label
  } else if (is_median) {
    "Share of households below the overall median, minus 50 points"
  } else {
    "Difference to the reference (percentage points)"
  }

  method_note <- if (is_median) {
    paste(
      "Survey version of Mood's median test: under the null, half of the",
      "households of a decile lie below the overall median.",
      "The overall median is treated as fixed.",
      sep = "\n"
    )
  } else {
    paste(
      "Difference to the reference and its delta-method interval, computed from",
      "the FULL covariance matrix of the decile estimates: Var(d) = Var(decile)",
      "+ Var(reference) - 2 Cov(.,.). Overlapping intervals on the main figure",
      "are NOT a test - shares are negatively correlated, so a decile can differ",
      "significantly even when the two intervals overlap.",
      sep = "\n"
    )
  }

  ggplot(dat, aes(x = .data[[strat_var]], y = .est, colour = status)) +
    geom_hline(yintercept = 0, linewidth = 0.8, colour = "grey30") +
    (if (has_trend) {
      geom_line(
        data = dat,
        mapping = aes(x = .data[[strat_var]], y = .fit, group = 1),
        colour = "grey25",
        linetype = "dashed",
        linewidth = 0.7,
        inherit.aes = FALSE
      )
    } else {
      list()
    }) +
    geom_linerange(aes(ymin = .lo, ymax = .hi), linewidth = 1.1) +
    geom_point(size = 3.4) +
    geom_text(
      aes(y = .hi, label = stars),
      vjust = -0.6,
      size = 4,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = c(
        "Above reference" = col_above,
        "Below reference" = col_below,
        "Not significant" = "grey65"
      ),
      drop = FALSE,
      name = str_c("Test at ", round(level * 100), "%")
    ) +
    labs(
      title = title,
      subtitle = if (is.null(trend_txt)) subtitle else {
        str_c(subtitle, "\n", trend_txt)
      },
      x = "Income decile",
      y = ylab,
      caption = str_c(
        method_note,
        paste0(
          "*** p < 0.001, ** p < 0.01, * significant at ",
          round((1 - level) * 100, 1),
          "% only, ns = not significant at ",
          round((1 - level) * 100, 1),
          "%. ",
          n_raw,
          " decile(s) significant at ",
          round((1 - level) * 100, 1),
          "%, ",
          n_holm,
          " after Holm correction for the ",
          nrow(dat),
          " comparisons."
        ),
        caption,
        sep = "\n"
      )
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(size = 9)
    )
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
    # The "equal distribution" line is the benchmark get_share_macro() already
    # tests each decile against (column ref = N_g / sum N), computed in the same
    # svytotal(). Using it here rather than calling get_share_macro_overall()
    # again saves a pass over the design AND guarantees that the line drawn on
    # the figure is exactly the one the test compares to - previously the two
    # could differ slightly, because get_share_macro_overall() counts every
    # household of the universe whereas the test counts only those whose
    # target_var is observed.
    ref_tbl <- tbl |>
      transmute(
        !!strat := factor(.data[[strat]], levels = strat_lvl),
        ref_share = ref * 100
      )

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
    # The "overall" reference is the estimator POOLED over the levels of `strat`,
    # and the one-pass engine already produced it (columns ref / ref_SE /
    # ref_IC_*) from the very same svytotal() and covariance matrix that gave
    # `tbl`. Re-calling the estimator on the whole universe would cost a second
    # full pass over the design for the same number - the only possible
    # difference being households whose `strat` is missing, which are counted
    # below. This halves the cost of every analysis.
    has_ref <- all(
      c("ref", "ref_SE", "ref_IC_low", "ref_IC_high") %in% names(tbl)
    )
    if (has_ref) {
      in_univ <- if (is.null(universe)) {
        rep(TRUE, nrow(d))
      } else {
        !is.na(d[[universe]]) & as.character(d[[universe]]) == filter
      }
      n_na_strat <- sum(in_univ & is.na(d[[strat]]))
      if (n_na_strat > 0) {
        message(
          "\n NOTE - ", basename, ": ", n_na_strat, " household(s) of this ",
          "universe have a missing ", strat, " and are therefore excluded from ",
          "the pooled reference.\n"
        )
      }
      overall <- tibble::tibble(
        !!strat := if (is.null(filter)) "Total" else filter,
        !!value_col := tbl$ref[1],
        SE = tbl$ref_SE[1],
        IC_low = tbl$ref_IC_low[1],
        IC_high = tbl$ref_IC_high[1]
      )
    } else {
      overall <- call_estimator(
        strat_var_arg = universe,
        filter_var_arg = universe
      )
      colnames(overall)[1] <- colnames(tbl)[1]
    }
  }

  # arrondi
  # number of decimals: micro shares are ~1e-6, so 2 decimals would print 0.0
  # everywhere. Guard against a zero minimum (a median share can legitimately be
  # exactly 0), which used to give round(x, Inf).
  round_digits <- if (is_share_plot && is.null(ref_tbl)) {
    smallest <- suppressWarnings(min(
      abs(tbl[[value_col]])[is.finite(tbl[[value_col]]) & tbl[[value_col]] != 0],
      na.rm = TRUE
    ))
    if (is.finite(smallest)) {
      min(10, max(0, ceiling(-log10(smallest)) + 2))
    } else {
      2
    }
  } else {
    2
  }

  tbl <- tbl |>
    mutate(across(
      c(all_of(value_col), SE, IC_low, IC_high),
      ~ round(. * 100, round_digits)
    )) |>
    # the test columns are on the same scale as the estimate
    mutate(across(
      any_of(c("ref", "diff", "diff_SE", "diff_IC_low", "diff_IC_high")),
      ~ round(. * 100, round_digits)
    )) |>
    mutate(across(
      any_of(c(
        "share_below_ref", "share_below_SE",
        "share_below_IC_low", "share_below_IC_high",
        "trend_slope", "trend_SE"
      )),
      ~ round(. * 100, 2)
    ))
  overall <- overall |>
    mutate(across(
      any_of(c(value_col, "SE", "IC_low", "IC_high")),
      ~ round(. * 100, round_digits)
    )) |>
    mutate(across(
      any_of(c("ref", "diff", "diff_SE", "diff_IC_low", "diff_IC_high")),
      ~ round(. * 100, round_digits)
    )) |>
    mutate(across(
      any_of(c(
        "share_below_ref", "share_below_SE",
        "share_below_IC_low", "share_below_IC_high"
      )),
      ~ round(. * 100, 2)
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

  show_plot(plot)
  custom_save(plot, str_c("plot_", name))

  # additional figure: formal test of each decile against the reference
  if (all(c("diff", "signif", "p_value") %in% names(tbl))) {
    plot_sig <- make_significance_plot(
      tbl = tbl,
      strat_var = strat,
      value_col = value_col,
      title = str_c("Test: ", title),
      subtitle = subtitle,
      caption = caption,
      col_above = col_above,
      col_below = col_below
    )
    show_plot(plot_sig)
    custom_save(plot_sig, str_c("plot_signif_", name))
  }
}

# Default wording for the "median" estimator, so that the ~25 call sites in
# 2C/2D/2E do not all have to declare a title_median / extra_text_median.
.median_title <- function(title_macro) {
  str_c(
    "Median household ",
    str_to_lower(str_sub(title_macro, 1, 1)),
    str_sub(title_macro, 2)
  )
}
.median_extra_text <- function() {
  paste(
    "Bars represent the MEDIAN of the household-level values within each decile:",
    "the value of the household that splits the decile in two, not an average.",
    "Error bars are Woodruff intervals (confidence interval on the share of",
    "households below the median, mapped back through the quantile function);",
    "they are asymmetric by construction.",
    "Medians are not additive: component medians do not sum to the total.",
    sep = "\n"
  )
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
  title_median = NULL,
  caption_base,
  extra_text,
  extra_text_median = NULL,
  col_above,
  col_below,
  col_overall,
  is_share_plot = FALSE,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  caption_macro <- caption_base
  caption_micro <- paste(extra_text, caption_base, sep = "\n")
  if (is.null(title_median)) title_median <- .median_title(title_macro)
  if (is.null(extra_text_median)) extra_text_median <- .median_extra_text()
  caption_median <- paste(extra_text_median, caption_base, sep = "\n")

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
    if ("median" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_ratio_median,
        value_col = "ratio",
        num = num,
        den = den,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("median_", basename),
        title = title_median,
        caption = caption_median,
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
  title_median = NULL,
  caption_base,
  extra_text,
  extra_text_median = NULL,
  col_above,
  col_below,
  col_overall,
  is_share_plot = TRUE,
  debug_outer = FALSE,
  debug_inner = FALSE
) {
  caption_macro <- caption_base
  caption_micro <- paste(extra_text, caption_base, sep = "\n")
  if (is.null(title_median)) title_median <- .median_title(title_macro)
  if (is.null(extra_text_median)) extra_text_median <- .median_extra_text()
  caption_median <- paste(extra_text_median, caption_base, sep = "\n")

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
    if ("median" %in% estimators) {
      run_one_analysis(
        design = design,
        d = d,
        estimator_fn = get_share_median,
        value_col = "share",
        target_var = target_var,
        strat = strat,
        universe = u$universe,
        filter = u$filter,
        basename = str_c("median_", basename),
        title = title_median,
        caption = caption_median,
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
