# PREPARING THE DATABASE FOR THE PILOT STUDY ON MEXICO

#INFO: n_tipo_prod based on AGROPRODUCTO. But household in AGRO not in AGROPRODUCTOS will be classified as not agri because n_tipo_act will be NA. The difficulty is overcome in part 2 in which we explicitely assign a production type to household in AGRO but not in AGROPRODUCTO
#NOTE: we had issue with the key hogar : now solved, by setting its type explicitely to characer(). all hogares are in concentradohogar.
#NOTE: tipoact_agro is unused : we use instead our own is_agri income based definition

# AGROPRODUCTOS ----

agroproductos_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_agroproductos_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_agroproductos_enigh2022_ns.csv"
  ),
  col_types = cols(
    folioviv = col_character(),
    foliohog = col_character()
  )
)
spec(agroproductos_raw)

## Clean + classify products ----
agroproductos_clean <- agroproductos_raw |>
  select(
    folioviv,
    foliohog,
    tipoact,
    codigo,
    cosecha,
    cantidad, #Total de la cosecha en kilogramos
    cant_venta,
    valor,
    preciokg,
    val_venta
  ) |>
  mutate(
    hogar = stringr::str_c(folioviv, foliohog, sep = "_"),
    #NOTE: some codes are missing for instance 473 does not exist in original database
    tipo = case_when(
      codigo < 230 ~ "agr", # 1 = crops
      codigo < 278 ~ "ani", # 2 = livestock
      codigo < 296 ~ "prodani", # 3 = animal products
      codigo < 474 ~ "for", # 4 = forest products
      codigo < 601 ~ "fis", # 5 = fishing products
      codigo >= 601 ~ "hun" # 6 = hunting products
    ),

    count_prod = 1
  )

## Household totals by product type ----
agroproductos_summary <- agroproductos_clean |>
  filter(!is.na(valor)) |>
  group_by(hogar, tipo) |>
  summarise(
    n = sum(count_prod, na.rm = TRUE), # number of subtypes by production type
    cant = sum(cantidad, na.rm = TRUE), # volume of production, in kg
    cven = sum(cant_venta, na.rm = TRUE), # production sold
    val = sum(valor, na.rm = TRUE), # value of production
    ven = sum(val_venta, na.rm = TRUE), # value of sold production
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = tipo,
    values_from = c(n, cant, cven, val, ven),
    names_glue = "{.value}_{tipo}",
    values_fill = 0
  )

## Add total number of products ----
agroproductos <- agroproductos_clean |>
  group_by(hogar) |>
  summarise(
    n_prod = n(),
    .groups = "drop"
  ) |>
  left_join(agroproductos_summary, by = "hogar")

## Replace remaining NA with 0 ----
agroproductos <- agroproductos |>
  mutate(
    across(where(is.numeric), ~ coalesce(.x, 0))
  )

## Totals ----
agroproductos <- agroproductos |>
  mutate(
    ven_tot = ven_agr +
      ven_ani +
      ven_prodani +
      ven_for +
      ven_fis +
      ven_hun,

    val_tot = val_agr +
      val_ani +
      val_prodani +
      val_for +
      val_fis +
      val_hun
  )

## Market orientation ----
agroproductos <- agroproductos |>
  mutate(
    tipo_market = if_else(ven_tot > 0, 1, 0)
  )

## Farm typology ----
tres_agri_spe <- 2 / 3

agroproductos <- agroproductos |>
  mutate(
    n_tipo_prod = case_when(
      # no production
      val_tot == 0 ~ 0,

      # crop farms
      val_agr >= val_tot * tres_agri_spe ~ 1,

      # livestock farms
      (val_ani + val_prodani) >= val_tot * tres_agri_spe ~ 2,

      # mixed crop-livestock
      (val_agr > 0 &
        (val_ani + val_prodani) > 0 &
        (val_agr + val_ani + val_prodani) > val_tot * tres_agri_spe) ~ 3,

      # mixed farm-primary
      ((val_agr > 0 |
        (val_ani + val_prodani) > 0) &
        (val_agr + val_ani + val_prodani) < val_tot * tres_agri_spe) ~ 4,

      # primary non-farm
      (val_agr + val_ani + val_prodani == 0 &
        val_tot > 0) ~ 5,

      TRUE ~ NA_real_
    )
  )

## Diagnostics ----

## display distrbution agroproductos |> group_by(n_tipo_prod) |> summarise(n=n())
hogar_product_but_no_production <- agroproductos |>
  filter(n_tipo_prod == 0) |>
  select(hogar) |>
  pull()

test_product_but_no_production <- agroproductos_clean |>
  filter(hogar %in% hogar_product_but_no_production) |>
  distinct(cosecha) |>
  pull()

if (test_product_but_no_production == 2) {
  message(paste0(
    "\n\n",
    length(hogar_product_but_no_production),
    " households with declared products but no actual production : they did not harvest! 🌾"
  ))
} else {
  stop(
    "check households with product (n_ =/= 0) but no production (cant_ == 0) : some did harvest! Where has this production all gone?"
  )
}

n_na <- sum(is.na(agroproductos$n_tipo_prod))

if (n_na > 0) {
  stop(paste0(
    "\n\n🐞 ERROR in AGROPRODUCTO: ",
    n_na,
    " missing types"
  ))
} else {
  message(
    "\n\n🆗 AGROPRODUCTO IS CORRECT: all farmers have a production type"
  )
}

## Save ----
output_file <- here("output", "data", "agroproductos")
saveRDS(agroproductos, paste0(output_file, ".rds"))

# AGROCONSUMO ----

agroconsumo_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_agroconsumo_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_agroconsumo_enigh2022_ns.csv"
  ),
  col_types = cols(
    folioviv = col_character(),
    foliohog = col_character()
  )
)

spec(agroconsumo_raw)

## Clean + filter + classify ----

agroconsumo_clean <- agroconsumo_raw |>
  select(
    folioviv,
    foliohog,
    codigo,
    destino,
    valestim
    #TODO: we could add cantitad (to get volume of self-consumption in kg or L o piezas and UNIT PRICES (il faudra prendre en % pour éliminer l'unité)
  ) |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_"),
    count_prod = 1,

    tipo = case_when(
      codigo < 230 ~ "agr",
      codigo < 278 ~ "ani",
      codigo < 296 ~ "prodani",
      codigo < 474 ~ "for",
      codigo < 601 ~ "fis",
      codigo >= 601 ~ "hun"
    )
  ) |>
  # keep only self-consumption
  # left out : 2,Utilizado en el negocio ; 4,Pago deudas del negocio
  filter(
    destino == 1 | # household's consumption
      destino == 3 | # household's debt repayment
      destino > 5 # household's gifts and products exchanges
  )

## Household-level totals by type ----

agroconsumo <- agroconsumo_clean |>
  group_by(hogar, tipo) |>
  summarise(
    n = sum(count_prod, na.rm = TRUE),
    val = sum(valestim, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = tipo,
    values_from = c(n, val),
    names_glue = "{.value}_{tipo}",
    values_fill = 0
  ) |>
  mutate(
    # number of different types of self-consumed products
    n_tot = n_agr + n_ani + n_prodani + n_for + n_fis + n_hun,
    # estimated value of total self-consumption
    #NOTE : valestim is annual value
    valestim = val_agr + val_ani + val_prodani + val_for + val_fis + val_hun
  )

## Safety check NA (normally unnecessary after fill = 0) ----
n_na_n <- sum(is.na(agroconsumo$n_tot))
n_na_val <- sum(is.na(agroconsumo$valestim))

if (n_na_n > 0 | n_na_val > 0) {
  stop(paste0(
    "\n\n🐞 ERROR in AGROCONSUMO: missing values -> n_tot (number of different types of self-consumed products): ",
    n_na_n,
    ", valestim (estimated value of self-consumption): ",
    n_na_val
  ))
} else {
  message(
    "\n\n 🆗 AGROCONSUMO IS CORRECT: no missing values in n_tot (number of different types of self-consumption) or valestim (estimated value of self-consumption)"
  )
}

agroconsumo <- agroconsumo |>
  mutate(across(where(is.numeric), ~ coalesce(.x, 0)))

## Save ----

saveRDS(
  agroconsumo,
  here("output", "data", "agroconsumo.rds")
)
# AGRO ----

agro_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_agro_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_agro_enigh2022_ns.csv"
  ),
  col_types = cols(
    .default = col_double(),
    folioviv = col_character(),
    foliohog = col_character()
  )
)

spec(agro_raw)
# agro_raw |> select(tipoact) |> distinct()

## Clean + feature engineering ----

### This is by product types and support
agro_clean <- agro_raw |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_"),
    count_member = 1,

    # actividad agricola y ganadera
    # NOTE: tipoact_agro = if_else(tipoact < 6, 1, 0) Il n'y a que de 4 à 9 comme activités dans AGRO (industries et services ne sont pas dedans)
    tipoact_agro = if_else(tipoact %in% c(4, 5), 1, 0),

    # cf. spec: some colunms are wrongly interpreted as lgl because only NA
    across(starts_with("apoyo"), ~ replace_na(.x, 0)),
    across(
      c(
        proagro,
        progan,
        starts_with("nvo_"),
        ventas_tri,
        auto_tri,
        otros_tri,
        ing_tri,
        ero_tri
      ),
      ~ replace_na(.x, 0)
    ),

    # NOTE: SUPPORT from MONTHLY to YEARLY data

    # Apoyo con pago (con necesidad de devolver la ayuda de vuelta)
    # resp : Apoyo de gobierno federal, estatal, municipal, no gubernamental con pago
    apoyo_pago = (apoyo_1 + apoyo_2 + apoyo_3 + apoyo_7) * 12,
    # Apoyo sin pago (sin terner que devolverlo)
    # resp : Apoyo de gobierno federal, estatal, municipal, no gubernamental sin pago
    apoyo_npago = (apoyo_4 + apoyo_5 + apoyo_6 + apoyo_8) * 12,

    #Apoyo Procampu y Progan
    ## proagro: PROCAMPO / ProAgro / Bienestar
    ## PROCAMPO se dirige al sector agrícola con pagos por hectárea de superficie elegible (basada en cultivos de 1993-1995),
    ## progan: PROGAN
    ## PROGAN apoya al sector pecuario mediante estímulos por unidad animal bajo condiciones de sustentabilidad y ordenamiento. Strangely, only NA in this column in the data
    pro_agrogan = (proagro + progan) * 12,
    #Total support from new social programmes
    ## households could answer three diff kind of programs
    #TODO: by comparing nvo_tot and nvo_toto_npago we could assess the distribution of direct subsidies. Do lower income households receive more credits as a % of total nvo?
    nvo_tot = (nvo_cant1 + nvo_cant2 + nvo_cant3) * 12,
    # Sembrando vida : 2001, 2002
    nvo1 = case_when(nvo_prog1 %in% c(2001, 2002) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2001, 2002) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2001, 2002) ~ nvo_cant3, TRUE ~ 0),
    sembr_vida = (nvo1 + nvo2 + nvo3) * 12,
    #Tandas para el Bienestar (Microcréditos para el Bienestar)
    nvo1 = case_when(nvo_prog1 %in% c(2003, 2004) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2003, 2004) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2003, 2004) ~ nvo_cant3, TRUE ~ 0),
    tand_bien = (nvo1 + nvo2 + nvo3) * 12,
    # Agromercados Sociales y Sustentables
    nvo1 = case_when(nvo_prog1 %in% c(2005, 2006) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2005, 2006) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2005, 2006) ~ nvo_cant3, TRUE ~ 0),
    agromercados = (nvo1 + nvo2 + nvo3) * 12,
    # Precios de garantia
    nvo1 = case_when(nvo_prog1 %in% c(2007, 2008) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2007, 2008) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2007, 2008) ~ nvo_cant3, TRUE ~ 0),
    precios_gar = (nvo1 + nvo2 + nvo3) * 12,
    # Credito Ganadero a la Palabra (creditos)
    nvo1 = case_when(nvo_prog1 %in% c(2009, 2010) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2009, 2010) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2009, 2010) ~ nvo_cant3, TRUE ~ 0),
    credito_gan = (nvo1 + nvo2 + nvo3) * 12,
    # Programa Nacional de Fertilizantes
    nvo1 = case_when(nvo_prog1 %in% c(2011, 2012) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2011, 2012) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2011, 2012) ~ nvo_cant3, TRUE ~ 0),
    nacion_fer = (nvo1 + nvo2 + nvo3) * 12,
    # Desarollo rural
    nvo1 = case_when(nvo_prog1 %in% c(2013, 2014) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2013, 2014) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2013, 2014) ~ nvo_cant3, TRUE ~ 0),
    desarollo_rur = (nvo1 + nvo2 + nvo3) * 12,
    # Otros programas
    nvo1 = case_when(nvo_prog1 %in% c(2015, 2016) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2015, 2016) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2015, 2016) ~ nvo_cant3, TRUE ~ 0),
    otros_prog = (nvo1 + nvo2 + nvo3) * 12,

    #direct payments new social programmes (sin pago : sin necesidad de devolverlo)
    nvo_tot_npago = sembr_vida +
      agromercados +
      precios_gar +
      nacion_fer +
      desarollo_rur +
      otros_prog,

    # NOTE: QUADRIMESTRIAL TO YEARLY data

    # farm gross output, net income, direct payments
    # Ingreso trimestral por ventas
    # Autoconsumo trimestral
    # Otros montos trimestral
    # INFO: ventas_tri is ing - ero but negative values are set to zero, hence we reconstruct ventas by taking the difference and later applying replace_negatives() in part2
    # INFO: turnover inclut autoconsommation y Otros montos no monetarios trimestrales (pago de trabajadores, deudas del negocio, deudas del hogar e intercambios)
    size_val = (ventas_tri + auto_tri + otros_tri) * 4,

    support = apoyo_npago + pro_agrogan + nvo_tot_npago,

    fni_year = (ing_tri - ero_tri) * 4 + support
  )

## Household aggregation ----

agro <- agro_clean |>
  group_by(hogar) |>
  summarise(
    # TODO: we could compute the number of different activities agro and noagro
    n_act = sum(count_member), # nomber of different activities
    n_fni = sum(fni_year),
    n_autoconsumo1 = sum(auto_tri) * 4, # value of self consumed production from AGRO
    n_size_val1 = sum(size_val),

    n_support = sum(support),
    n_pro_agrogan = sum(pro_agrogan), # old support programs
    n_nvo_tot = sum(nvo_tot_npago), # new support programs
    n_apoyo_npago = sum(apoyo_npago), # other kind of social programs

    n_sembr_vida = sum(sembr_vida),
    n_tand_bien = sum(tand_bien),
    n_agromercados = sum(agromercados),
    n_precios_gar = sum(precios_gar),
    n_credito_gan = sum(credito_gan),
    n_nacion_fert = sum(nacion_fer),
    n_desarollo_rur = sum(desarollo_rur),
    n_otros_prog = sum(otros_prog),
    .groups = "drop"
  ) |>
  mutate(
    n_size_class = case_when(
      n_size_val1 < 250001 ~ 1,
      n_size_val1 < 500001 ~ 2,
      n_size_val1 < 1000001 ~ 3,
      n_size_val1 < 2000001 ~ 4,
      n_size_val1 < 5000001 ~ 5,
      n_size_val1 < 10000001 ~ 6,
      TRUE ~ 7
    )
  )

## Add farm type + market orientation ----

# avoid silent explosions through joining operations
stopifnot(
  agroproductos %>% count(hogar) %>% pull(n) %>% max() == 1
)
stopifnot(
  agroconsumo %>% count(hogar) %>% pull(n) %>% max() == 1
)

agro <- agro |>
  #importing farm type and market orientation from the agroproductos dataset
  left_join(
    agroproductos |>
      select(hogar, n_tipo_prod, tipo_market),
    by = "hogar"
  ) |>
  #importing selfconsumption from the agroconsumo dataset
  left_join(
    agroconsumo |>
      select(hogar, valestim),
    by = "hogar"
  ) |>
  mutate(
    n_autoconsumo2 = coalesce(valestim, 0),
    # total output corrected with agroconsumo self-consumption
    n_size_val2 = n_size_val1 - n_autoconsumo1 + n_autoconsumo2
  )
# |>
# # add provenance flags
# mutate(
#   in_agro = 1,
#   in_producto = if_else(!is.na(n_tipo_prod), 1, 0),
#   in_consumo = if_else(!is.na(valestim), 1, 0)
# )

## Diagnostics ----
message("\n\n❗️Size of joined datasets")
message("number of households in agroproductos : ", nrow(agroproductos))
message("number of households in agroconsumo : ", nrow(agroconsumo))
message("number of households in agro : ", nrow(agro))

hogar_agro <- agro %>% distinct(hogar)
hogar_producto <- agroproductos %>% distinct(hogar)
hogar_consumo <- agroconsumo %>% distinct(hogar)

# 1. agro only
agro_only <- hogar_agro %>%
  anti_join(hogar_producto, by = "hogar") %>%
  anti_join(hogar_consumo, by = "hogar")

# 2. agro + producto only
agro_producto_only <- hogar_agro %>%
  inner_join(hogar_producto, by = "hogar") %>%
  anti_join(hogar_consumo, by = "hogar")

# 3. agro + consumo only
agro_consumo_only <- hogar_agro %>%
  inner_join(hogar_consumo, by = "hogar") %>%
  anti_join(hogar_producto, by = "hogar")

# 4. agro + producto + consumo
all_three <- hogar_agro %>%
  inner_join(hogar_producto, by = "hogar") %>%
  inner_join(hogar_consumo, by = "hogar")

# 5. producto only
producto_only <- hogar_producto %>%
  anti_join(hogar_agro, by = "hogar") %>%
  anti_join(hogar_consumo, by = "hogar")

# 6. consumo only
consumo_only <- hogar_consumo %>%
  anti_join(hogar_agro, by = "hogar") %>%
  anti_join(hogar_producto, by = "hogar")

# 7. producto + consumo only
producto_consumo_only <- hogar_producto %>%
  inner_join(hogar_consumo, by = "hogar") %>%
  anti_join(hogar_agro, by = "hogar")

# Summary table
agro_overlap_diagnostics <- tibble(
  category = c(
    "agro only",
    "agro + producto only",
    "agro + consumo only",
    "agro + producto + consumo",
    "producto only",
    "consumo only",
    "producto + consumo only"
  ),
  n_households = c(
    nrow(agro_only),
    nrow(agro_producto_only),
    nrow(agro_consumo_only),
    nrow(all_three),
    nrow(producto_only),
    nrow(consumo_only),
    nrow(producto_consumo_only)
  )
)

print(agro_overlap_diagnostics)

saveRDS(
  agro_overlap_diagnostics,
  here(
    "output",
    "diagnostics",
    "agro_overlap_diagnostics.rds"
  )
)

readr::write_csv(
  agro_overlap_diagnostics,
  here(
    "output",
    "diagnostics",
    "agro_overlap_diagnostics.csv"
  )
)

message(
  "\n\n😀 AGRO now integrates valestim from AGROCONSUMO and n_tipo_prod/tipo_market from AGROPRODUCTO\n We have:\n 
    * two measures for self-consumption, n_autoconsumo1 = auto_tri*4 and n_autoconsumo2 = valestim, \n 
    * and hence two measures for total farm output, n_size_val1 and n_size_val2\n\n
    ❗️ There are ",
  nrow(agroconsumo),
  " households only in agroconsumo.\n
    This is ",
  nrow(agro) - nrow(agroconsumo),
  " less households than in agro.\n
    The measure of self-consumption n_autoconsumo2 is likely to be less relevant than n_autoconsumo1."
)

## Save ----

saveRDS(
  agro,
  here("output", "data", "agro.rds")
)
# ETNIA ----

etnia_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_poblacion_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_poblacion_enigh2022_ns.csv"
  ),
  col_types = cols(
    .default = col_character(),
    folioviv = col_character(),
    foliohog = col_character()
  )
)
spec(etnia_raw)

etnia <- etnia_raw |>
  select(
    folioviv,
    foliohog,
    parentesco,
    etnia
  ) |>
  # keep only household head (jefe)
  filter(parentesco == "101") |>
  mutate(
    #Variable to identify households
    hogar = str_c(folioviv, foliohog, sep = "_")
  ) |>
  rename(
    n_etnia = etnia
  )
# |>
# # add provenance flags
# mutate(
#   in_etnia = 1
# )

## Save ----

saveRDS(
  etnia,
  here("output", "data", "etnia.rds")
)

message("\n\nETNIA now available 🎉")

# CONCERN FOR FOOD ----

alim_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_hogares_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_hogares_enigh2022_ns.csv"
  ),
  col_types = cols(
    folioviv = col_character(),
    foliohog = col_character()
  )
)
spec(alim_raw)

alim <- alim_raw |>
  select(
    folioviv,
    foliohog,
    acc_alim1
  ) |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_")
  ) |>
  rename(
    n_acc_alim1 = acc_alim1
  )
# |> # add provenance flags
# mutate(
#   in_alim = 1
# )

# DEFINICION : Alguna vez por falta de dinero o recursos, se vio en la preocupación que la comida se acabara.
# PREGUNTA En los últimos tres meses, por falta de dinero o  recursos ¿alguna vez usted se preocupó de que la comida se acabara?

## Save ----

saveRDS(
  alim,
  here("output", "data", "alim.rds")
)
message("\n\nALIM now available 🎉")
# NOAGRO ----

noagro_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_noagro_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_noagro_enigh2022_ns.csv"
  ),
  col_types = cols(
    .default = col_double(),
    folioviv = col_character(),
    foliohog = col_character()
  )
)

spec(noagro_raw)
noagro_raw |> select(ventas_tri)
# noagro_raw |> select(nvo_prog1, nvo_prog2, nvo_prog3) |> distinct()
# noagro_raw |> select(starts_with("nvo_")) |> distinct()
# noagro_raw |> select(tipoact, starts_with("nvo_")) |> distinct()

## Clean + household identifier ----

noagro_clean <- noagro_raw |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_"),
    count_member = 1,

    across(starts_with("nvo"), ~ replace_na(.x, 0)),
    across(
      c(ventas_tri, auto_tri, otros_tri, ing_tri, ero_tri),
      ~ replace_na(.x, 0)
    ),

    #NOTE: SUPPORT from monthly to yearly data

    # no old social program, only the new one
    # NOTE: in NOAGRO, some activities received support from new programs. the activity for whcih support is received is specificied by the surveyed person, but the latter appear in NOAGRO because they are registered as either industrial (1) or commercail (2) or else services (3)

    #Total support from new social programmes
    ## households could answer three diff kind of programs
    nvo_tot = (nvo_cant1 + nvo_cant2 + nvo_cant3) * 12,
    # Sembrando vida : 2001, 2002
    nvo1 = case_when(nvo_prog1 %in% c(2001, 2002) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2001, 2002) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2001, 2002) ~ nvo_cant3, TRUE ~ 0),
    sembr_vida = (nvo1 + nvo2 + nvo3) * 12,
    #Tandas para el Bienestar (Microcréditos para el Bienestar)
    nvo1 = case_when(nvo_prog1 %in% c(2003, 2004) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2003, 2004) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2003, 2004) ~ nvo_cant3, TRUE ~ 0),
    tand_bien = (nvo1 + nvo2 + nvo3) * 12,
    # Agromercados Sociales y Sustentables
    nvo1 = case_when(nvo_prog1 %in% c(2005, 2006) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2005, 2006) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2005, 2006) ~ nvo_cant3, TRUE ~ 0),
    agromercados = (nvo1 + nvo2 + nvo3) * 12,
    # Precios de garantia
    nvo1 = case_when(nvo_prog1 %in% c(2007, 2008) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2007, 2008) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2007, 2008) ~ nvo_cant3, TRUE ~ 0),
    precios_gar = (nvo1 + nvo2 + nvo3) * 12,
    # Credito Ganadero a la Palabra (creditos)
    nvo1 = case_when(nvo_prog1 %in% c(2009, 2010) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2009, 2010) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2009, 2010) ~ nvo_cant3, TRUE ~ 0),
    credito_gan = (nvo1 + nvo2 + nvo3) * 12,
    # Programa Nacional de Fertilizantes
    nvo1 = case_when(nvo_prog1 %in% c(2011, 2012) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2011, 2012) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2011, 2012) ~ nvo_cant3, TRUE ~ 0),
    nacion_fer = (nvo1 + nvo2 + nvo3) * 12,
    # Desarollo rural
    nvo1 = case_when(nvo_prog1 %in% c(2013, 2014) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2013, 2014) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2013, 2014) ~ nvo_cant3, TRUE ~ 0),
    desarollo_rur = (nvo1 + nvo2 + nvo3) * 12,
    # Otros programas
    nvo1 = case_when(nvo_prog1 %in% c(2015, 2016) ~ nvo_cant1, TRUE ~ 0),
    nvo2 = case_when(nvo_prog2 %in% c(2015, 2016) ~ nvo_cant2, TRUE ~ 0),
    nvo3 = case_when(nvo_prog3 %in% c(2015, 2016) ~ nvo_cant3, TRUE ~ 0),
    otros_prog = (nvo1 + nvo2 + nvo3) * 12,

    #direct payments new social programmes (sin pago : sin necesidad de devolverlo)
    nvo_tot_npago = sembr_vida +
      agromercados +
      precios_gar +
      nacion_fer +
      desarollo_rur +
      otros_prog,

    # note: quadrimestrial to yearly data
    size_val = (ventas_tri + auto_tri + otros_tri) * 4,
    support = nvo_tot_npago,
    # annualized non-agricultural self-employed income
    n_ingr_noagr = (ing_tri - ero_tri) * 4 + support
  )

## Household-level aggregation ----

noagro <- noagro_clean |>
  group_by(hogar) |>
  summarise(
    n_act = sum(count_member),
    n_ingr_noagr = sum(n_ingr_noagr, na.rm = TRUE),
    n_autoconsumo1 = sum(auto_tri) * 4,
    n_size_val1 = sum(size_val),

    n_support = sum(support),
    n_nvo_tot = sum(nvo_tot_npago),

    n_sembr_vida = sum(sembr_vida),
    n_tand_bien = sum(tand_bien),
    n_agromercados = sum(agromercados),
    n_precios_gar = sum(precios_gar),
    n_credito_gan = sum(credito_gan),
    n_nacion_fert = sum(nacion_fer),
    n_desarollo_rur = sum(desarollo_rur),
    n_otros_prog = sum(otros_prog),

    .groups = "drop"
  )
#TODO: add n_size_class as in AGRO ?

# noagro |>
#   select(
#     n_sembr_vida,
#     n_tand_bien,
#     n_agromercados,
#     n_precios_gar,
#     n_credito_gan,
#     n_nacion_fert,
#     n_desarollo_rur,
#     n_otros_prog
#   ) |>
#   summarise(
#     across(
#       everything(),
#       ~ sum(.x, na.rm = TRUE)
#     )
#   )

## Save ----

saveRDS(
  noagro,
  here("output", "data", "noagro.rds")
)
message("📋 self-employed NOAGRO is done !")
# CONCENTRADOHOGAR ----

concentradohogar_raw <- readr::read_csv(
  here(
    "src",
    "conjunto_de_datos_enigh_ns_2022_csv",
    "conjunto_de_datos_concentradohogar_enigh2022_ns",
    "conjunto_de_datos",
    "conjunto_de_datos_concentradohogar_enigh2022_ns.csv"
  ),
  col_types = cols(
    # .default = col_double(),
    folioviv = col_character(),
    foliohog = col_character()
  )
)
spec(concentradohogar_raw)

## Select + household identifier ----

concentradohogar_clean <- concentradohogar_raw |>
  select(
    folioviv,
    foliohog,
    trabajo,
    rentas,
    transfer,
    estim_alqu,
    otros_ing
  ) |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_")
  )

## PROVENANCE FLAGS ----

hogar_concentrado <- concentradohogar_clean %>%
  distinct(hogar)

hogar_agro <- agro %>%
  distinct(hogar)

hogar_producto <- agroproductos %>%
  distinct(hogar)

hogar_consumo <- agroconsumo %>%
  distinct(hogar)

hogar_noagro <- noagro %>%
  distinct(hogar)

hogar_etnia <- etnia %>%
  distinct(hogar)

hogar_alim <- alim %>%
  distinct(hogar)

concentradohogar_flag <- concentradohogar_clean |>
  mutate(
    in_producto = if_else(
      hogar %in% hogar_producto$hogar,
      1,
      0
    ),
    in_consumo = if_else(
      hogar %in% hogar_consumo$hogar,
      1,
      0
    ),
    in_agro = if_else(
      hogar %in% hogar_agro$hogar,
      1,
      0
    ),
    in_etnia = if_else(
      hogar %in% hogar_etnia$hogar,
      1,
      0
    ),
    in_alim = if_else(
      hogar %in% hogar_alim$hogar,
      1,
      0
    ),
    in_noagro = if_else(
      hogar %in% hogar_noagro$hogar,
      1,
      0
    )
  )

## Merge external datasets ----

# Avoid silent explosion through joining operations
stopifnot(
  agro %>% count(hogar) %>% pull(n) %>% max() == 1
)
stopifnot(
  noagro %>% count(hogar) %>% pull(n) %>% max() == 1
)
stopifnot(
  etnia %>% count(hogar) %>% pull(n) %>% max() == 1
)
stopifnot(
  alim %>% count(hogar) %>% pull(n) %>% max() == 1
)

concentradohogar_enriched <- concentradohogar_flag |>
  left_join(
    agro |> rename_with(~ paste0(.x, "_agro"), starts_with("n_")),
    by = "hogar"
  ) |>
  left_join(
    noagro |>
      rename(n_ingr = n_ingr_noagr) |>
      rename_with(~ paste0(.x, "_noagro"), starts_with("n_")),
    by = "hogar",
  ) |>
  left_join(
    etnia |>
      select(hogar, n_etnia),
    by = "hogar"
  ) |>
  left_join(
    alim |>
      select(hogar, n_acc_alim1),
    by = "hogar"
  ) |>
  mutate(
    across(
      c(n_fni_agro, n_ingr_noagro),
      ~ coalesce(.x, 0) # avoid NA for non agri household
    )
  )

## Income and support variables ----

concentradohogar_features <- concentradohogar_enriched |>
  mutate(
    # annual income components
    n_trabajo = trabajo * 4, #annual income from employed labour
    n_rentas = rentas * 4, #annual income from owned assets
    n_transfer = transfer * 4, #annual income from social and private transfers
    n_estim_alqu = estim_alqu * 4, #annual implicit rent from property dwelling
    n_otros_ing = otros_ing * 4, #other annual sources of income

    # total current income
    n_ing_cor = n_fni_agro + # containts support from apoyo, nvo, and agrogan
      n_ingr_noagro + # contains support from nvo
      n_trabajo +
      n_rentas +
      n_transfer +
      n_estim_alqu +
      n_otros_ing,

    n_autoconsumo1 = n_autoconsumo1_agro + n_autoconsumo1_noagro,

    n_nvo_tot = n_nvo_tot_agro + n_nvo_tot_noagro,
    n_support = n_support_agro + n_support_noagro,
    n_agromercados = n_agromercados_agro + n_agromercados_noagro,
    n_credito_gan = n_credito_gan_agro + n_credito_gan_noagro,
    n_desarollo_rur = n_desarollo_rur_agro + n_desarollo_rur_noagro,
    n_nacion_fert = n_nacion_fert_agro + n_nacion_fert_noagro,
    n_otros_prog = n_otros_prog_agro + n_otros_prog_noagro,
    n_precios_gar = n_precios_gar_agro + n_precios_gar_noagro,
    n_sembr_vida = n_sembr_vida_agro + n_sembr_vida_noagro,
    n_tand_bien = n_tand_bien_agro + n_tand_bien_noagro,

    # households with farming activities
    n_tipo_act_agro = case_when(
      is.na(n_tipo_prod_agro) ~ NA_real_,
      n_tipo_prod_agro %in% 1:4 ~ 1, # yes
      n_tipo_prod_agro == 5 ~ 2, # no
      n_tipo_prod_agro == 0 ~ 0, # no production yet (no harvest !)
      TRUE ~ NA_real_,
    )
  ) |>
  select(
    -folioviv,
    -foliohog,
    -trabajo,
    -rentas,
    -transfer,
    -estim_alqu,
    -otros_ing
  )

## Merge back with original dataset ----

# Avoid silent explosion through joining operations
stopifnot(
  concentradohogar_features %>% count(hogar) %>% pull(n) %>% max() == 1
)

concentradohogar <- concentradohogar_raw |>
  mutate(
    hogar = str_c(folioviv, foliohog, sep = "_")
  ) |>
  left_join(
    concentradohogar_features,
    by = "hogar"
  )

## Save ----

rev_nb <- "_rev8"

saveRDS(
  concentradohogar,
  here(
    "output",
    "data",
    paste0("concentradohogar", rev_nb, ".rds")
  )
)

message("\n\n𝍃 is it over? Concentradohogar served.")
# CONSISTENCY CHECK ----

if (
  nrow(agro |> count(hogar) |> filter(n > 1)) == 0 &
    nrow(noagro |> count(hogar) |> filter(n > 1)) == 0 &
    nrow(etnia |> count(hogar) |> filter(n > 1)) == 0 &
    nrow(alim |> count(hogar) |> filter(n > 1)) == 0
) {
  message("\n\nno duplicated households in source databases 👌")
} else {
  stop("\n\nOne of agro, noagro, etnia or alim has duplicated households")
}
# DIAGNOSE UNIVERSES ----

message("\nHouseholds in concentradohogar: ", nrow(concentradohogar))
message("\nHouseholds in alim: ", nrow(alim))
message("\nHouseholds in etnia: ", nrow(etnia))
message("\nHouseholds in noagro: ", nrow(noagro))
message("\nHouseholds in agro: ", nrow(agro))
message("\nHouseholds in agroproductos: ", nrow(agroproductos))
message("\nHouseholds in agroconsumo: ", nrow(agroconsumo))

message("\n-----------------------------------\n")
print(agro_overlap_diagnostics)
message("\n-----------------------------------\n")

diagnostic_universe <- function(
  data_hogar,
  data_name,
  ref_hogar = hogar_concentrado
) {
  # households outside concentradohogar
  outside_ref <- data_hogar %>%
    anti_join(ref_hogar, by = "hogar")

  n_outside <- nrow(outside_ref)

  # fatal error
  if (n_outside > 0) {
    stop(
      paste0(
        "\n\n🚨 CRITICAL ERROR: ",
        n_outside,
        " households from ",
        data_name,
        " are absent from CONCENTRADOHOGAR."
      )
    )
  }

  # households included in concentradohogar
  n_included <- data_hogar %>%
    semi_join(ref_hogar, by = "hogar") %>%
    nrow()

  tibble(
    dataset = data_name,
    n_households = nrow(data_hogar),
    n_in_concentrado = n_included,
    share_concentrado = round(
      100 * n_included / nrow(ref_hogar),
      2
    ),
    n_outside_concentrado = n_outside
  )
}

universe_diagnostics <- bind_rows(
  diagnostic_universe(
    hogar_agro,
    "agro"
  ),

  diagnostic_universe(
    hogar_producto,
    "agroproductos"
  ),

  diagnostic_universe(
    hogar_consumo,
    "agroconsumo"
  ),

  diagnostic_universe(
    hogar_noagro,
    "noagro"
  ),

  diagnostic_universe(
    hogar_etnia,
    "etnia"
  ),

  diagnostic_universe(
    hogar_alim,
    "alim"
  )
)

universe_diagnostics <- bind_rows(
  tibble(
    dataset = "concentradohogar",
    n_households = nrow(hogar_concentrado),
    n_in_concentrado = nrow(hogar_concentrado),
    share_concentrado = 100,
    n_outside_concentrado = 0
  ),
  universe_diagnostics
)

message("\n\n📋 Universe diagnostics")
print(universe_diagnostics, width = Inf)

saveRDS(
  universe_diagnostics,
  here(
    "output",
    "diagnostics",
    "universe_diagnostics.rds"
  )
)

readr::write_csv(
  universe_diagnostics,
  here(
    "output",
    "diagnostics",
    "universe_diagnostics.csv"
  )
)
# CORRELATION BETWEEN the two measures of self-consumption ----

test_cor <- agro |>
  select(n_autoconsumo1, n_autoconsumo2) |>
  rename_with(~ paste0(.x, "_agro"), everything())

message("\nComparison of n_autoconsumo1_agro and n_autoconsumo2_agro\n")
summary <- as_tibble(
  test_cor |> summary(n_autoconsumo1_agro - n_autoconsumo2_agro),
  .name_repair = "unique"
)

print(summary)

cor <- test_cor |>
  mutate(
    diff = n_autoconsumo1_agro - n_autoconsumo2_agro
  ) |>
  summarise(
    corr = cor(
      n_autoconsumo1_agro,
      n_autoconsumo2_agro,
      use = "complete.obs"
    ),
    mean_diff = mean(diff, na.rm = TRUE),
    median_diff = median(diff, na.rm = TRUE),
    p90_diff = quantile(diff, .9, na.rm = TRUE)
  )

print(cor)

saveRDS(
  summary,
  here(
    "output",
    "diagnostics",
    "summary_autoconsumo1_2_agro.rds"
  )
)
readr::write_csv(
  summary,
  here(
    "output",
    "diagnostics",
    "summary_autoconsumo1_2_agro.csv"
  )
)

saveRDS(
  cor,
  here(
    "output",
    "diagnostics",
    "cor_autoconsumo1_2_agro.rds"
  )
)
readr::write_csv(
  summary,
  here(
    "output",
    "diagnostics",
    "cor_autoconsumo1_2_agro.csv"
  )
)
# CREATE NEW VARIABLE DICTIONARY (append new variables if needed) ----

vars_n <- sort(names(concentradohogar)[str_starts(
  names(concentradohogar),
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
    "n_size_class_agro",
    "n_tipo_prod_agro",
    "n_etnia",
    "n_acc_alim1",
    "n_tipo_act_agro"
  )

  new_vars <- vars_n[test_new_var]

  dict_new <- purrr::map_dfr(new_vars, function(v) {
    vals <- concentradohogar |> pull(all_of(v))

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

# THE END ----

