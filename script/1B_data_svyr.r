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
#WARN : Il nostro ing-cor è SICURAMENTE diverso da quello ENIGH perchè noi consideriamo le perdite delle attività produttive non come una uscita famigliare ma come una perdita aziendale.
#
d <- d |>
  # remove 0 before the code for educa_jefe, est_dis, and num
  mutate(
    upm = as.numeric(upm),
    est_dis = as.numeric(est_dis),
    educa_jefe = as.numeric(educa_jefe)
  ) |>
  # correcting negative income from autonomous (agri and not agri) employment
  mutate(
    #NOTE: do we have to do that on the part ? Could we in fact no do that on n_ing_cor directly ?
    # in ENIGH they have balance in CONCENTRADOHOGAR, corresponding to business losses
    # this means that business loss may be reduced by other income of the household
    # replace_negative() on n_ing_cor would mean less negative to be replaced
    # HOWEVER: we could not in that case construct precent_agro to classify households

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
    # farm total ressources is : (sales + self-consumption + non monetary exchanges) + subsidies
    n_ftr1_agro = n_support_agro + n_size_val1_agro,

    n_is_loc_rural = if_else(tam_loc == 4, "rural", "not_rural"),
    # create a new group for other sources of income
    n_otros_ing_bundled = n_rentas + n_estim_alqu + n_otros_ing,
    # same for n_trabajo and otros_trab (secondary labour income) in order to avoid non significant decile
    n_trabajo_bundled = n_trabajo + n_otros_trab,
    n_nvo_pago_agro = n_nvo_tot_agro - n_nvo_npago_agro, # in support there are other stuff than only NEW
    n_nvo_pago_noagro = n_nvo_tot_noagro - n_support_noagro, # there is only NVO as npago support to non agriculture
    n_nvo_pago = n_nvo_tot - n_nvo_npago # (n_support_noagro + n_nvo_npago_agro)
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

custom_save(edge_cases, type = "diagnostics")

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

custom_save(edge_cases_after, type = "diagnostics")

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
custom_save(d, "0_main_database_for_analysis", type = "data")

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

# ---------------------
# CHECKING the consistency of our INCOME variable ----
#NOTE: there are a bit TOO many households with strong divergence btw our reconstruction and enigh's income variable (in particular a 50000 MXN/year that becomes 0)
# Could that be the case that this is because of normalized trimestrialisation at differing components of ing_cor ? 

message("Checking consistency of income variable reconstruction:")

# nombre de revenus négatifs corrigés
n_neg <- sum(d$n_ing_cor < 0, na.rm = TRUE)
message(n_neg, " negative incomes corrected from n_ing_cor to n_ing_cor_clean")

# seuil d'écart acceptable : 5% de la moyenne
mean_ref <- mean(d$n_ing_cor_enigh, na.rm = TRUE)
threshold <- 0.05

consistency_check <- d |>
  mutate(
    diff_abs = abs(n_ing_cor_clean - n_ing_cor_enigh),
    diff_rel = diff_abs / n_ing_cor_enigh,
    flag = !is.na(diff_rel) & diff_rel > threshold & n_ing_cor_enigh > 0
  )

n_flagged <- sum(consistency_check$flag, na.rm = TRUE)
pct_flagged <- round(n_flagged / nrow(d) * 100, 2)
mean_diff <- mean(
  consistency_check$diff_rel[
    !consistency_check$flag & is.finite(consistency_check$diff_rel)
  ],
  na.rm = TRUE
)
n_zero_enigh <- sum(d$n_ing_cor_enigh == 0, na.rm = TRUE)
message(
  n_zero_enigh,
  " households with n_ing_cor_enigh == 0 (excluded from relative diff)"
)

message(
  n_flagged,
  " households (",
  pct_flagged,
  "%) exceed the ",
  threshold * 100,
  "% relative difference threshold — these differ meaningfully"
)
message(
  "Mean relative difference among consistent households (<= threshold): ",
  round(mean_diff * 100, 3),
  "%"
)
if (n_flagged == 0) {
  message(
    "✅ n_ing_cor_clean and n_ing_cor_enigh are consistent within threshold"
  )
} else {
  message(
    "⚠️ Some households show non-trivial differences — inspect consistency_check$flag"
  )
}

consistency_check |>
  filter(flag) |>
  summarise(
    n = n(),
    median_diff_rel = median(diff_rel, na.rm = TRUE),
    median_ing_enigh = median(n_ing_cor_enigh, na.rm = TRUE),
    n_was_negative = sum(n_ing_cor < 0, na.rm = TRUE)
  )

#TODO: #Les 2004 - 113 = 1891 flaggés non-négatifs sont plus intéressants — ce sont des ménages où n_ing_cor_clean et n_ing_cor_enigh divergent pour une autre raison.  :

flag_not_negative <- consistency_check |>
  filter(flag & n_ing_cor >= 0) |>
  select(factor,n_is_agri, n_ing_cor, n_ing_cor_clean, n_ing_cor_enigh, diff_rel) |>
  arrange(desc(diff_rel)) |>
  head(20)
flag_not_negative
flag_not_negative |> group_by(n_is_agri) |> summarise(n = n())

consistency_check |>
  filter(flag) |>
  count(n_deciles_total) |>
  arrange(n_deciles_total)


#NOTE: 63% of households with >5% divergence between n_ing_cor_clean and n_ing_cor_enigh
# are in D1-D3. Micro ratio estimates for these deciles should be interpreted with caution,
# as individual ratios num_i / n_ing_cor_clean may be affected by income reconstruction
# differences, particularly for households with originally negative incomes.
