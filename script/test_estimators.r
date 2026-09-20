# REGRESSION TEST for the survey estimators of script/0_utils.r
#
# The estimators in 0_utils.r were rewritten as ONE-PASS DOMAIN ESTIMATORS:
# instead of one subset() + svytotal()/svymean() per level of the stratifying
# variable, a single svytotal() is computed on the matrix of the
# indicator-expanded variables y_i * 1[i in domain g].
#
# This file re-implements the PREVIOUS (subset-based) versions in a compact
# form and checks that the new ones reproduce them exactly - estimates AND
# standard errors - on a synthetic design that has the same structure as
# ENIGH 2022 (stratified, one-stage cluster, unequal weights) and the same
# awkward cases (exact zeros, zero denominators, missing values, missing
# strata). It also checks get_ratio_median() against survey::svyquantile().
#
# Run with:   Rscript script/test_estimators.r
# Exit status is 0 when every check passes, 1 otherwise.

suppressMessages({
  library(survey)
  library(srvyr)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(stringr)
})
options(survey.lonely.psu = "adjust")

# ---------------------------------------------------------------- reference
# Previous implementations, kept here as the reference to reproduce.

ref_ratio_macro <- function(design, numerator, denominator, strat_var,
                            filter_var = NULL, filter_value = NULL,
                            level = 0.99) {
  z <- qnorm((1 + level) / 2)
  df <- if (!is.null(filter_var)) {
    subset(design, !is.na(design$variables[[filter_var]]) &
             design$variables[[filter_var]] == filter_value)
  } else design
  lv <- unique(as.character(df$variables[[strat_var]]))
  lv <- lv[!is.na(lv)]
  map_dfr(lv, function(l) {
    ds <- subset(df, !is.na(df$variables[[strat_var]]) &
                   as.character(df$variables[[strat_var]]) == l)
    t2 <- svytotal(as.formula(paste0("~cbind(num = ", numerator,
                                     ", den = ", denominator, ")")),
                   design = ds, na.rm = TRUE)
    co <- coef(t2); vc <- vcov(t2); X <- co[1]; Y <- co[2]
    SE <- sqrt(vc[1, 1] / Y^2 + (X^2 * vc[2, 2]) / Y^4 - 2 * X * vc[1, 2] / Y^3)
    tibble(strat = l, ratio = X / Y, SE = SE)
  })
}

ref_ratio_micro <- function(design, numerator, denominator, strat_var,
                            filter_var = NULL, filter_value = NULL) {
  df <- if (!is.null(filter_var)) {
    subset(design, !is.na(design$variables[[filter_var]]) &
             design$variables[[filter_var]] == filter_value)
  } else design
  df <- update(df, .ratio = df$variables[[numerator]] /
                 df$variables[[denominator]])
  df$variables$.ratio <- ifelse(is.finite(df$variables$.ratio),
                                df$variables$.ratio, NA_real_)
  lv <- unique(as.character(df$variables[[strat_var]]))
  lv <- lv[!is.na(lv)]
  map_dfr(lv, function(l) {
    ds <- subset(df, !is.na(df$variables[[strat_var]]) &
                   as.character(df$variables[[strat_var]]) == l)
    m <- svymean(~.ratio, ds, na.rm = TRUE)
    tibble(strat = l, ratio = coef(m)[[1]], SE = SE(m)[[1]])
  })
}

ref_share_macro <- function(design, target_var, strat_var,
                            filter_var = NULL, filter_value = NULL) {
  df <- if (!is.null(filter_var)) {
    subset(design, !is.na(design$variables[[filter_var]]) &
             design$variables[[filter_var]] == filter_value)
  } else design
  lv <- unique(as.character(df$variables[[strat_var]]))
  lv <- lv[!is.na(lv)]
  map_dfr(lv, function(l) {
    f <- as.formula(paste0("~cbind(part = ", target_var, " * (", strat_var,
                           " == '", l, "'), total = ", target_var, ")"))
    t2 <- svytotal(f, design = df, na.rm = TRUE)
    co <- coef(t2); vc <- vcov(t2); X <- co[1]; Y <- co[2]
    SE <- sqrt(vc[1, 1] / Y^2 + (X^2 * vc[2, 2]) / Y^4 - 2 * X * vc[1, 2] / Y^3)
    tibble(strat = l, share = X / Y, SE = SE)
  })
}

ref_share_macro_overall <- function(design, strat_var,
                                    filter_var = NULL, filter_value = NULL) {
  df <- if (!is.null(filter_var)) {
    subset(design, !is.na(design$variables[[filter_var]]) &
             design$variables[[filter_var]] == filter_value)
  } else design
  lv <- unique(as.character(df$variables[[strat_var]]))
  lv <- lv[!is.na(lv)]
  n <- map_dfr(lv, function(l) {
    ds <- subset(df, as.character(df$variables[[strat_var]]) == l)
    ds <- update(ds, .one = 1)
    tibble(strat = l, n = as.numeric(coef(svytotal(~.one, ds, na.rm = TRUE))))
  })
  n |> mutate(ref_share = n / sum(n) * 100) |> select(-n)
}

ref_share_micro <- function(design, target_var, strat_var,
                            filter_var = NULL, filter_value = NULL) {
  df <- if (!is.null(filter_var)) {
    subset(design, !is.na(design$variables[[filter_var]]) &
             design$variables[[filter_var]] == filter_value)
  } else design
  tot <- as.numeric(coef(svytotal(as.formula(paste0("~", target_var)),
                                  design = df, na.rm = TRUE)))
  df <- update(df, .share = df$variables[[target_var]] / tot)
  df$variables$.share <- ifelse(is.finite(df$variables$.share),
                                df$variables$.share, NA_real_)
  lv <- unique(as.character(df$variables[[strat_var]]))
  lv <- lv[!is.na(lv)]
  map_dfr(lv, function(l) {
    ds <- subset(df, !is.na(df$variables[[strat_var]]) &
                   as.character(df$variables[[strat_var]]) == l)
    m <- svymean(~.share, ds, na.rm = TRUE)
    tibble(strat = l, share = coef(m)[[1]], SE = SE(m)[[1]])
  })
}

ref_proportion <- function(design, strat_var, target_var,
                           filter_var = NULL, filter_value = NULL,
                           level = 0.99) {
  x <- design$variables[[strat_var]]
  y <- design$variables[[target_var]]
  params <- crossing(strat = unique(as.character(x[!is.na(x)])),
                     target = unique(as.character(y[!is.na(y)])))
  pmap_dfr(params, function(strat, target) {
    ds <- if (!is.null(filter_var)) {
      subset(design, !is.na(design$variables[[strat_var]]) &
               as.character(design$variables[[strat_var]]) == strat &
               !is.na(design$variables[[filter_var]]) &
               as.character(design$variables[[filter_var]]) == filter_value)
    } else {
      subset(design, !is.na(design$variables[[strat_var]]) &
               as.character(design$variables[[strat_var]]) == strat)
    }
    ind <- as.character(ds$variables[[target_var]]) == target
    ind[is.na(ind)] <- FALSE
    ds <- update(ds, indicator = ind)
    p <- svyciprop(~indicator, ds, method = "beta", level = level)
    ic <- as.numeric(confint(p))
    tibble(strat = strat, target = target, prop = as.numeric(coef(p)),
           IC_low = ic[1], IC_high = ic[2])
  })
}

# ------------------------------------------------------------------ fixture
set.seed(42)
n <- 15000
nstrat <- 120
strata <- sort(sample(seq_len(nstrat), n, TRUE))
psu <- ave(strata, strata, FUN = function(z)
  sample(seq_along(z) %% max(2, floor(length(z) / 9)) + 1))
dat <- data.frame(strata = strata, upm = paste0(strata, "_", psu),
                  w = runif(n, 50, 900))
dat$num <- rgamma(n, 0.6, 1) * 1000
dat$den <- rgamma(n, 2, 1) * 5000
dat$num2 <- rgamma(n, 1.2, 1) * 800
dat$num[sample(n, n * 0.45)] <- 0   # many households receive nothing
dat$den[sample(n, 40)] <- 0         # zero denominators
dat$num2[sample(n, 30)] <- NA       # missing values
dat$dec <- factor(paste0("D", sample(1:10, n, TRUE)),
                  levels = paste0("D", 1:10))
dat$univ <- sample(c("agri_broad", "not_agri"), n, TRUE, prob = c(.35, .65))
dat$size <- factor(sample(c("s1", "s2", "s3", "s4"), n, TRUE))
dat$size[sample(n, 200)] <- NA
dat$dec[sample(n, 25)] <- NA        # households outside any decile

des <- svydesign(ids = ~upm, strata = ~strata, weights = ~w, data = dat)

# ---------------------------------------------- load the functions under test
src <- readLines(here::here("script", "0_utils.r"))
i0 <- grep("^## Custom survey functions ----", src)
i1 <- grep("^## Wrapper functions for polagri.r ----", src)
stopifnot(length(i0) == 1, length(i1) == 1)
fun <- new.env()
eval(parse(text = paste(src[i0:(i1 - 1)], collapse = "\n")), fun)

ok <- TRUE
chk <- function(label, a, b, tol = 1e-10) {
  a <- as.numeric(a); b <- as.numeric(b)
  both <- is.finite(a) & is.finite(b)
  same_na <- identical(is.finite(a), is.finite(b))
  d <- if (any(both)) {
    max(abs(a[both] - b[both]) / pmax(abs(b[both]), 1e-12))
  } else 0
  pass <- same_na && d < tol
  ok <<- ok && pass
  cat(sprintf("%-48s %s  max rel diff = %.3e%s\n", label,
              if (pass) "PASS" else "FAIL", d,
              if (same_na) "" else "   [NA pattern differs]"))
}

FV <- "univ"
FL <- "agri_broad"

# 1. macro ratio
a <- ref_ratio_macro(des, "num", "den", "dec", FV, FL)
b <- fun$get_ratio_macro(des, "num", "den", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_ratio_macro : estimate", m$ratio.y, m$ratio.x)
chk("get_ratio_macro : SE", m$SE.y, m$SE.x)

# 2. micro ratio
a <- ref_ratio_micro(des, "num", "den", "dec", FV, FL)
b <- fun$get_ratio_micro(des, "num", "den", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_ratio_micro : estimate", m$ratio.y, m$ratio.x)
chk("get_ratio_micro : SE", m$SE.y, m$SE.x)

a <- ref_ratio_micro(des, "num2", "den", "dec", FV, FL)
b <- fun$get_ratio_micro(des, "num2", "den", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_ratio_micro (variable with NA) : estimate", m$ratio.y, m$ratio.x)
chk("get_ratio_micro (variable with NA) : SE", m$SE.y, m$SE.x)

# 3. macro share
a <- ref_share_macro(des, "num", "dec", FV, FL)
b <- fun$get_share_macro(des, "num", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_share_macro : estimate", m$share.y, m$share.x)
chk("get_share_macro : SE", m$SE.y, m$SE.x)
cat(sprintf("%-48s %s  sum = %.12f\n", "get_share_macro : shares sum to 1",
            if (abs(sum(b$share) - 1) < 1e-10) "PASS" else "FAIL",
            sum(b$share)))

a <- ref_share_macro(des, "num2", "dec", FV, FL)
b <- fun$get_share_macro(des, "num2", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_share_macro (variable with NA) : estimate", m$share.y, m$share.x)
chk("get_share_macro (variable with NA) : SE", m$SE.y, m$SE.x)

# 4. reference line
a <- ref_share_macro_overall(des, "dec", FV, FL)
b <- fun$get_share_macro_overall(des, "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_share_macro_overall : ref_share", m$ref_share.y, m$ref_share.x)

# 5. micro share
a <- ref_share_micro(des, "num", "dec", FV, FL)
b <- fun$get_share_micro(des, "num", "dec", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec"))
chk("get_share_micro : estimate", m$share.y, m$share.x)
chk("get_share_micro : SE", m$SE.y, m$SE.x)

# 6. proportions with Korn-Graubard beta intervals
a <- ref_proportion(des, "dec", "size", FV, FL)
b <- fun$get_proportion(des, "dec", "size", FV, FL)
m <- left_join(a, as_tibble(b), by = c("strat" = "dec", "target" = "size"))
chk("get_proportion : estimate", m$prop.y, m$prop.x)
chk("get_proportion : IC_low", m$IC_low.y, m$IC_low.x)
chk("get_proportion : IC_high", m$IC_high.y, m$IC_high.x)

# 7. weighted quantile rule
r <- dat$num / dat$den
r[!is.finite(r)] <- NA
sub <- which(dat$dec == "D3" & dat$univ == "agri_broad" & is.finite(r))
chk("wq_math vs survey qrule_math (p = .5)",
    fun$.wq_math(r[sub], dat$w[sub], .5),
    survey:::qrule_math(r[sub], dat$w[sub], .5))
chk("wq_math vs survey qrule_math (p = .17)",
    fun$.wq_math(r[sub], dat$w[sub], .17),
    survey:::qrule_math(r[sub], dat$w[sub], .17))

# 8. median ratio and its Woodruff interval, against survey::svyquantile()
desr <- update(des, .r = ifelse(is.finite(num / den) & den > 0, num / den, NA))
new_med <- fun$get_ratio_median(des, "num", "den", "dec", FV, FL, level = 0.99)
ref <- map_dfr(levels(dat$dec), function(l) {
  ds <- subset(desr, as.character(desr$variables$dec) == l &
                 desr$variables$univ == FL)
  qm <- unclass(svyquantile(~.r, ds, quantiles = 0.5, ci = TRUE, alpha = 0.01,
                            interval.type = "beta", qrule = "math",
                            na.rm = TRUE))[[1]]
  tibble(dec = l, q = qm[1, 1], lo = qm[1, 2], hi = qm[1, 3], se = qm[1, 4])
})
m <- left_join(as_tibble(new_med), ref, by = "dec")
chk("get_ratio_median : estimate vs svyquantile", m$ratio, m$q)
chk("get_ratio_median : IC_low vs svyquantile", m$IC_low, m$lo)
chk("get_ratio_median : IC_high vs svyquantile", m$IC_high, m$hi)
chk("get_ratio_median : SE vs svyquantile", m$SE, m$se)

# 8b. the difference-to-the-reference test, against survey::svycontrast().
#     survey computes the delta-method variance of an arbitrary non-linear
#     contrast of estimated totals by its own (numerical) differentiation, so
#     this is a fully independent check of .test_contrast().
build_M <- function(num, den, g, keep) {
  lv <- levels(dat$dec)
  lv <- lv[lv %in% unique(as.character(g[keep]))]
  M <- matrix(0, nrow = nrow(dat), ncol = 2L * length(lv))
  colnames(M) <- as.vector(rbind(paste0("n", seq_along(lv)),
                                 paste0("d", seq_along(lv))))
  for (k in seq_along(lv)) {
    sel <- keep & !is.na(g) & as.character(g) == lv[k]
    M[sel, 2L * k - 1L] <- num[sel]
    M[sel, 2L * k] <- den[sel]
  }
  list(M = M, lv = lv)
}
keep <- dat$univ == FL & !is.na(dat$dec) &
  is.finite(dat$num) & is.finite(dat$den)
bm <- build_M(dat$num, dat$den, dat$dec, keep)
tt <- svytotal(bm$M, des)
G <- length(bm$lv)
sum_n <- paste(paste0("n", seq_len(G)), collapse = " + ")
sum_d <- paste(paste0("d", seq_len(G)), collapse = " + ")
ref_sv <- map_dfr(seq_len(G), function(k) {
  e <- str2lang(sprintf("n%d/d%d - (%s)/(%s)", k, k, sum_n, sum_d))
  ct <- svycontrast(tt, list(d = e))
  tibble(dec = bm$lv[k], diff = as.numeric(coef(ct)), se = as.numeric(SE(ct)))
})
new_rm <- fun$get_ratio_macro(des, "num", "den", "dec", FV, FL)
m <- left_join(as_tibble(new_rm), ref_sv, by = c("dec" = "dec"))
chk("test vs reference : diff vs svycontrast", m$diff.x, m$diff.y)
chk("test vs reference : SE   vs svycontrast", m$diff_SE, m$se, tol = 1e-6)

# 8c. same for the paired test of get_share_macro (decile share vs its own
#     demographic weight): share_g = x_g / sum(x)  vs  ref_g = N_g / sum(N)
keep2 <- dat$univ == FL & !is.na(dat$dec) & is.finite(dat$num)
bm2 <- build_M(dat$num, rep(1, nrow(dat)), dat$dec, keep2)
tt2 <- svytotal(bm2$M, des)
G2 <- length(bm2$lv)
sum_x <- paste(paste0("n", seq_len(G2)), collapse = " + ")
sum_N <- paste(paste0("d", seq_len(G2)), collapse = " + ")
ref_sv2 <- map_dfr(seq_len(G2), function(k) {
  e <- str2lang(sprintf("n%d/(%s) - d%d/(%s)", k, sum_x, k, sum_N))
  ct <- svycontrast(tt2, list(d = e))
  tibble(dec = bm2$lv[k], diff = as.numeric(coef(ct)), se = as.numeric(SE(ct)))
})
new_sm <- fun$get_share_macro(des, "num", "dec", FV, FL)
m <- left_join(as_tibble(new_sm), ref_sv2, by = c("dec" = "dec"))
chk("paired test (share vs weight) : diff", m$diff.x, m$diff.y)
chk("paired test (share vs weight) : SE", m$diff_SE, m$se, tol = 1e-6)

# 8d. the pooled reference (columns ref / ref_SE / ref_IC_*) must reproduce the
#     estimator applied to the whole universe. run_one_analysis() now uses it
#     instead of a second svytotal(), so this equality is load-bearing.
#     Checked on a design WITHOUT missing strata, since the pooled reference
#     legitimately excludes households that belong to no decile.
dat2 <- dat[!is.na(dat$dec), ]
des2 <- svydesign(ids = ~upm, strata = ~strata, weights = ~w, data = dat2)
for (nm in c("get_ratio_macro", "get_ratio_micro")) {
  a <- fun[[nm]](des2, "num", "den", "dec", FV, FL)
  b <- fun[[nm]](des2, "num", "den", "univ", FV, FL)
  chk(paste0(nm, " : pooled ref vs universe call"), a$ref[1], b$ratio[1])
  chk(paste0(nm, " : pooled ref SE vs universe SE"), a$ref_SE[1], b$SE[1])
}
a <- fun$get_share_micro(des2, "num", "dec", FV, FL)
b <- fun$get_share_micro(des2, "num", "univ", FV, FL)
chk("get_share_micro : pooled ref vs universe call", a$ref[1], b$share[1])
chk("get_share_micro : pooled ref SE vs universe SE", a$ref_SE[1], b$SE[1])
a <- fun$get_ratio_median(des2, "num", "den", "dec", FV, FL)
b <- fun$get_ratio_median(des2, "num", "den", "univ", FV, FL)
chk("get_ratio_median : pooled ref vs universe call", a$ref[1], b$ratio[1])
chk("get_ratio_median : pooled ref IC_low", a$ref_IC_low[1], b$IC_low[1])
chk("get_ratio_median : pooled ref IC_high", a$ref_IC_high[1], b$IC_high[1])

# 8e. the design-based LINEAR TREND across deciles, again against
#     survey::svycontrast(). Since the OLS slope weights sum to zero, the
#     reference term cancels and the slope is sum_g c_g * (n_g / d_g).
cx <- (seq_len(G) - mean(seq_len(G))) / sum((seq_len(G) - mean(seq_len(G)))^2)
e_trend <- str2lang(paste(
  sprintf("(%.15g)*n%d/d%d", cx, seq_len(G), seq_len(G)), collapse = " + "))
ct <- svycontrast(tt, list(slope = e_trend))
chk("linear trend : slope vs svycontrast",
    new_rm$trend_slope[1], as.numeric(coef(ct)))
chk("linear trend : SE    vs svycontrast",
    new_rm$trend_SE[1], as.numeric(SE(ct)), tol = 1e-6)

# 9. timing (the real design has 560 strata, where the gain is larger)
cat("\n--- timing, 10 deciles, 15 000 units, 120 strata ---\n")
t_old <- system.time(ref_ratio_macro(des, "num", "den", "dec", FV, FL))[["elapsed"]]
t_new <- system.time(fun$get_ratio_macro(des, "num", "den", "dec", FV, FL))[["elapsed"]]
cat(sprintf("get_ratio_macro   old %5.2fs   new %5.2fs   (x%.1f)\n",
            t_old, t_new, t_old / t_new))
t_old <- system.time(ref_proportion(des, "dec", "size", FV, FL))[["elapsed"]]
t_new <- system.time(fun$get_proportion(des, "dec", "size", FV, FL))[["elapsed"]]
cat(sprintf("get_proportion    old %5.2fs   new %5.2fs   (x%.1f)\n",
            t_old, t_new, t_old / t_new))

cat("\n", if (ok) "ALL CHECKS PASSED\n" else "*** SOME CHECKS FAILED ***\n")
quit(status = if (ok) 0L else 1L)
