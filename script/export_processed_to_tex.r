# =====================================================================
# export_processed_to_tex.r — output/processed/*.csv -> output/tex/*.tex
# ---------------------------------------------------------------------
# Exporte chaque tableau de output/processed/ en bloc LaTeX tabularray
# (tblr, ou longtblr au-dela de `long_rows` lignes), produit par tinytable,
# pret a copier-coller dans l'article ou a \input{}.
#
#   Rscript script/export_processed_to_tex.r     # depuis la racine du projet
#
# Preambule de l'article : \usepackage{tabularray}
# tinytable doit etre enregistre dans renv.lock (renv::snapshot()).
#
# IMPORTANT : tinytable passe le contenu des cellules en LaTeX BRUT (il
# n'echappe rien : "x_y & co" sort tel quel dans le .tex). On formate donc
# ET on echappe nous-memes chaque cellule avant tt() ; les cellules en
# $...$ (math) et les libelles de PRETTY (deja en LaTeX) passent intacts.
# =====================================================================

suppressMessages({
  library(readr)
  library(tinytable)
})

# tblr "nu" (sans float \begin{table}) : on veut des blocs a copier-coller
options(tinytable_latex_environment_table = FALSE)

IN_DIR  <- file.path("output", "processed")
OUT_DIR <- file.path("output", "tex")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## En-tetes "paper" (les noms absents de la liste sont echappes tels quels) ----
## Ce sont des libelles LaTeX : ils partent tels quels en en-tete du tblr.
PRETTY <- c(
  "n_deciles_total" = "Decile", "decile" = "Decile",
  "ratio" = "Estimate", "share" = "Share", "pct" = "\\%",
  "pct_low" = "CI low", "pct_upp" = "CI upp",
  "SE" = "SE", "IC_low" = "CI low", "IC_high" = "CI high",
  "n_obs" = "$n$", "n_pop" = "$N$ (w.)", "degf" = "df", "p_zero" = "$P(0)$",
  "ref" = "Ref.", "ref_SE" = "Ref. SE",
  "ref_IC_low" = "Ref. CI low", "ref_IC_high" = "Ref. CI high",
  "diff" = "$\\Delta$", "diff_SE" = "$\\Delta$ SE",
  "diff_IC_low" = "$\\Delta$ CI low", "diff_IC_high" = "$\\Delta$ CI high",
  "share_below_ref" = "\\% below ref.", "share_below_SE" = "\\% below SE",
  "share_below_IC_low" = "\\% below low", "share_below_IC_high" = "\\% below upp",
  "p_value" = "$p$", "p_value_adj" = "$p$ (Holm)",
  "signif" = "sig.", "signif_adj" = "sig. (Holm)",
  "test" = "Test", "trend_slope" = "Trend", "trend_SE" = "Trend SE",
  "trend_p" = "Trend $p$",
  "component" = "Component", "sum_of_medians" = "Sum of med.",
  "sum_of_shares" = "Sum of shares",
  "target_var" = "Modality", "overall_pct" = "Overall \\%",
  "n_acc_alim1" = "Food access", "n_etnia" = "Ethnicity",
  "n_size_class_agro" = "Farm size", "n_tipo_prod_agro" = "Production type",
  "total" = "Total", "total_low" = "CI low", "total_upp" = "CI upp",
  "Group" = "Group", "Gini coefficient" = "Gini",
  "Lower bound (CI 99%)" = "CI 99\\% low", "Upper bound (CI 99%)" = "CI 99\\% upp",
  "Treshold" = "Threshold", "Standard error" = "SE",
  "Lower bound (CI : 99%)" = "CI 99\\% low", "Higher bound (CI : 99%)" = "CI 99\\% upp",
  "Type" = "Type", "prop. (%)" = "prop. (\\%)", "n (millions of ind.)" = "$n$ (millions)",
  "**Characteristic**" = "Characteristic",
  "**Total current income (thousands of MXN)**" = "Total current income (thousands of MXN)"
)
## Colonnes a supprimer pour une version "article" peu large, ex. :
##   drop = c("test", "trend_slope", "trend_SE", "trend_p", "ref_SE",
##            "ref_IC_low", "ref_IC_high")

## -- Echappement LaTeX (tinytable ne le fait PAS) -------------------------
latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("**", "\001", x, fixed = TRUE)  # protege les marqueurs gtsummary
  x <- gsub("__", "\002", x, fixed = TRUE)
  for (sp in c("&", "%", "#", "_", "{", "}")) {
    # fixed = TRUE -> le remplacement est literal : on veut \% \_ \& ...
    x <- gsub(sp, paste0("\\", sp), x, fixed = TRUE)
  }
  x <- gsub("\001", "**", x, fixed = TRUE)
  x <- gsub("\002", "__", x, fixed = TRUE)
  x <- gsub("\\*\\*(.+?)\\*\\*", "\\\\textbf{\\1}", x, perl = TRUE)
  x <- gsub("__(.+?)__", "\\\\textit{\\1}", x, perl = TRUE)
  x
}
# en-tete multi-ligne (gtsummary) : saut de ligne -> \\ entre accolades
header_cell <- function(x) {
  x <- latex_escape(x)
  if (grepl("\n", x, fixed = TRUE)) {
    # fixed = TRUE -> remplacement literal : on veut " \\ " (saut de ligne LaTeX)
    x <- gsub("\n", " \\\\ ", x, fixed = TRUE)
    x <- paste0("{", x, "}")
  }
  x
}

## -- Formatage des nombres -------------------------------------------------
fmt_sci <- function(v) {                       # $a.b\times10^{k}$
  e <- floor(log10(abs(v)))
  m <- v / 10^e
  if (abs(m) >= 9.995) { m <- m / 10; e <- e + 1 }
  sprintf("$%.2f\\times10^{%d}$", m, e)
}
fmt_cell <- function(v, kind) {
  vapply(v, function(x) {
    if (is.na(x) || x == "") return("")
    if (kind == "log") {
      return(if (x == "TRUE") "yes" else if (x == "FALSE") "no" else latex_escape(x))
    }
    xn <- suppressWarnings(as.numeric(x))
    if (is.na(xn)) return(latex_escape(x))     # colonne texte
    if (kind == "p") {
      if (xn == 0) return("0")
      if (abs(xn) < 0.001) return(fmt_sci(xn))
      return(sprintf("%.3f", xn))
    }
    if (kind == "int") {                       # effectif : 3\,269
      s <- formatC(round(xn), format = "f", digits = 0, big.mark = ",")
      return(gsub(",", "\\,", s, fixed = TRUE))   # 3\,269 (espace fine)
    }
    if (xn == 0) return("0")
    if (abs(xn) < 0.01) return(fmt_sci(xn))    # micro shares ~1e-6
    sprintf("%.2f", xn)
  }, character(1), USE.NAMES = FALSE)
}

classify_col <- function(nm, vals) {
  x <- vals[!is.na(vals) & vals != ""]
  if (length(x) == 0) return("text")
  if (all(x %in% c("TRUE", "FALSE"))) return("log")
  if (!any(is.na(suppressWarnings(as.numeric(x))))) {
    if (nm %in% c("p_value", "p_value_adj", "trend_p")) return("p")
    if (nm %in% c("n_obs", "n_pop", "degf", "n", "sum",
                  "total", "total_low", "total_upp")) return("int")
    return("num")
  }
  "text"
}

## -- Coeur : un CSV -> un bloc tblr/longtblr (tinytable) --------------------
csv_to_tex <- function(path, out_path = NULL, rename = PRETTY,
                       long_rows = 25, drop = NULL) {
  name <- tools::file_path_sans_ext(basename(path))
  name <- sub("\\.csv$", "", name)              # gere les "*.csv.csv"
  df <- read_csv(path, show_col_types = FALSE)
  if (!is.null(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]

  nms   <- names(df)
  kinds <- vapply(nms, function(nm) classify_col(nm, as.character(df[[nm]])),
                  character(1))

  # corps : chaque cellule pre-formatee en LaTeX (tinytable passe en brut)
  cells <- lapply(seq_along(nms), function(j) fmt_cell(as.character(df[[j]]), kinds[j]))
  body <- data.frame(cells, stringsAsFactors = FALSE)
  names(body) <- nms

  # en-tetes : libelles PRETTY = deja LaTeX, les autres sont echappes
  lab <- ifelse(
    nms %in% names(rename),
    unname(rename[nms]),
    vapply(nms, header_cell, character(1))
  )
  names(body) <- lab

  colspec <- paste0(
    ifelse(kinds == "text", "Q[l]", ifelse(kinds == "log", "Q[c]", "Q[r]")),
    collapse = " "
  )
  env   <- if (nrow(body) > long_rows) "longtblr" else "tblr"
  inner <- paste0(
    "colspec={", colspec, "}, rowhead=1, ",
    "row{1}={font=\\bfseries}, colsep=4pt, rowsep=2pt"
  )

  if (is.null(out_path)) out_path <- file.path(OUT_DIR, paste0(name, ".tex"))
  options(tinytable_latex_environment = env)   # tblr ou longtblr selon la taille
  save_tt(tt(body) |> theme_latex(inner = inner), out_path)

  # commentaire de provenance en tete du fichier
  txt <- readLines(out_path)
  writeLines(
    c(
      paste0("% Table: ", name, "  (from output/processed/", name,
             ".csv; estimate columns are in percent)"),
      "% Generated by script/export_processed_to_tex.r (tinytable -> tabularray). Requires: \\usepackage{tabularray}",
      txt
    ),
    out_path
  )
  message("✅ ", out_path, "  (", nrow(body), " rows, ", env, ")")
  invisible(out_path)
}

## -- Boucle sur tous les CSV ----------------------------------------------
files <- list.files(IN_DIR, pattern = "\\.csv$", full.names = TRUE)
index <- character(length(files))
for (f in files) {
  p <- csv_to_tex(f)
  index[basename(p)] <- paste0(format(nrow(read_csv(f, show_col_types = FALSE))), " rows")
}
writeLines(paste0(names(index), "  ", index), file.path(OUT_DIR, "INDEX.txt"))
message("\n🎉 ", length(files), " tables exportees dans ", OUT_DIR)
