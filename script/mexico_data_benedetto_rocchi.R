######################################################
#PREPARING THE DATABASE FOR THE PILOT STUDY ON MEXICO#
#April 2026                                          #
######################################################

#AGROPRODUCTO
#############
wd <- "conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_agroproductos_enigh2022_ns/conjunto_de_datos/"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_agroproductos_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
data <- table[c(
  "folioviv",
  "foliohog",
  "tipoact",
  "codigo",
  "cosecha",
  "cantidad",
  "cant_venta",
  "valor",
  "preciokg",
  "val_venta"
)]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)
data$count_prod <- 1

#classifying products
data$tipo[data$codigo < 230] <- 1 # 1 = crops
data$tipo[data$codigo > 229 & data$codigo < 278] <- 2 # 2 = livestock
data$tipo[data$codigo > 277 & data$codigo < 296] <- 3 # 3 = animal products
data$tipo[data$codigo > 295 & data$codigo < 474] <- 4 # 4 = forest products
data$tipo[data$codigo > 474 & data$codigo < 601] <- 5 # 5 = fishing products
data$tipo[data$codigo > 600] <- 6 # 6 = hunting products

#households' dataset
agroproductos <- data |>
  group_by(hogar) |>
  summarise(n_prod = sum(count_prod))

#datasets with households' total values for different primary activities
data_agr <- data[data$tipo == 1 & !is.na(data$valor), ] #crops
agroproductos1 <- data_agr |>
  group_by(hogar) |>
  summarise(
    n_agr = sum(count_prod),
    cant_agr = sum(cantidad),
    cven_agr = sum(cant_venta),
    val_agr = sum(valor),
    ven_agr = sum(val_venta)
  )
rm(data_agr)
data_ani <- data[data$tipo == 2 & !is.na(data$valor), ] #livestock
agroproductos2 <- data_ani |>
  group_by(hogar) |>
  summarise(
    n_ani = sum(count_prod),
    cant_ani = sum(cantidad),
    cven_ani = sum(cant_venta),
    val_ani = sum(valor),
    ven_ani = sum(val_venta)
  )
rm(data_ani)
data_prodani <- data[data$tipo == 3 & !is.na(data$valor), ] #animal products
agroproductos3 <- data_prodani |>
  group_by(hogar) |>
  summarise(
    n_prodani = sum(count_prod),
    cant_prodani = sum(cantidad),
    cven_prodani = sum(cant_venta),
    val_prodani = sum(valor),
    ven_prodani = sum(val_venta)
  )
rm(data_prodani)
data_for <- data[data$tipo == 4 & !is.na(data$valor), ] #forest products
agroproductos4 <- data_for |>
  group_by(hogar) |>
  summarise(
    n_for = sum(count_prod),
    cant_for = sum(cantidad),
    cven_for = sum(cant_venta),
    val_for = sum(valor),
    ven_for = sum(val_venta)
  )
rm(data_for)
data_fis <- data[data$tipo == 5 & !is.na(data$valor), ] #fishing products
agroproductos5 <- data_fis |>
  group_by(hogar) |>
  summarise(
    n_fis = sum(count_prod),
    cant_fis = sum(cantidad),
    cven_fis = sum(cant_venta),
    val_fis = sum(valor),
    ven_fis = sum(val_venta)
  )
rm(data_fis)
data_hun <- data[data$tipo == 6 & !is.na(data$valor), ] #hunting products
agroproductos6 <- data_hun |>
  group_by(hogar) |>
  summarise(
    n_hun = sum(count_prod),
    cant_hun = sum(cantidad),
    cven_hun = sum(cant_venta),
    val_hun = sum(valor),
    ven_hun = sum(val_venta)
  )
rm(data_hun)

#merging new variables in the households dataset - setting NA to 0
agroproductos <- left_join(agroproductos, agroproductos1, by = "hogar")
agroproductos$n_agr <- ifelse(
  is.na(agroproductos$n_agr),
  0,
  agroproductos$n_agr
)
agroproductos$cant_agr <- ifelse(
  is.na(agroproductos$cant_agr),
  0,
  agroproductos$cant_agr
)
agroproductos$cven_agr <- ifelse(
  is.na(agroproductos$cven_agr),
  0,
  agroproductos$cven_agr
)
agroproductos$val_agr <- ifelse(
  is.na(agroproductos$val_agr),
  0,
  agroproductos$val_agr
)
agroproductos$ven_agr <- ifelse(
  is.na(agroproductos$ven_agr),
  0,
  agroproductos$ven_agr
)

agroproductos <- left_join(agroproductos, agroproductos2, by = "hogar")
agroproductos$n_ani <- ifelse(
  is.na(agroproductos$n_ani),
  0,
  agroproductos$n_ani
)
agroproductos$cant_ani <- ifelse(
  is.na(agroproductos$cant_ani),
  0,
  agroproductos$cant_ani
)
agroproductos$cven_ani <- ifelse(
  is.na(agroproductos$cven_ani),
  0,
  agroproductos$cven_ani
)
agroproductos$val_ani <- ifelse(
  is.na(agroproductos$val_ani),
  0,
  agroproductos$val_ani
)
agroproductos$ven_ani <- ifelse(
  is.na(agroproductos$ven_ani),
  0,
  agroproductos$ven_ani
)

agroproductos <- left_join(agroproductos, agroproductos3, by = "hogar")
agroproductos$n_prodani <- ifelse(
  is.na(agroproductos$n_prodani),
  0,
  agroproductos$n_prodani
)
agroproductos$cant_prodani <- ifelse(
  is.na(agroproductos$cant_prodani),
  0,
  agroproductos$cant_prodani
)
agroproductos$cven_prodani <- ifelse(
  is.na(agroproductos$cven_prodani),
  0,
  agroproductos$cven_prodani
)
agroproductos$val_prodani <- ifelse(
  is.na(agroproductos$val_prodani),
  0,
  agroproductos$val_prodani
)
agroproductos$ven_prodani <- ifelse(
  is.na(agroproductos$ven_prodani),
  0,
  agroproductos$ven_prodani
)

agroproductos <- left_join(agroproductos, agroproductos4, by = "hogar")
agroproductos$n_for <- ifelse(
  is.na(agroproductos$n_for),
  0,
  agroproductos$n_for
)
agroproductos$cant_for <- ifelse(
  is.na(agroproductos$cant_for),
  0,
  agroproductos$cant_for
)
agroproductos$cven_for <- ifelse(
  is.na(agroproductos$cven_for),
  0,
  agroproductos$cven_for
)
agroproductos$val_for <- ifelse(
  is.na(agroproductos$val_for),
  0,
  agroproductos$val_for
)
agroproductos$ven_for <- ifelse(
  is.na(agroproductos$ven_for),
  0,
  agroproductos$ven_for
)

agroproductos <- left_join(agroproductos, agroproductos5, by = "hogar")
agroproductos$n_fis <- ifelse(
  is.na(agroproductos$n_fis),
  0,
  agroproductos$n_fis
)
agroproductos$cant_fis <- ifelse(
  is.na(agroproductos$cant_fis),
  0,
  agroproductos$cant_fis
)
agroproductos$cven_fis <- ifelse(
  is.na(agroproductos$cven_fis),
  0,
  agroproductos$cven_fis
)
agroproductos$val_fis <- ifelse(
  is.na(agroproductos$val_fis),
  0,
  agroproductos$val_fis
)
agroproductos$ven_fis <- ifelse(
  is.na(agroproductos$ven_fis),
  0,
  agroproductos$ven_fis
)

agroproductos <- left_join(agroproductos, agroproductos6, by = "hogar")
agroproductos$n_hun <- ifelse(
  is.na(agroproductos$n_hun),
  0,
  agroproductos$n_hun
)
agroproductos$cant_hun <- ifelse(
  is.na(agroproductos$cant_hun),
  0,
  agroproductos$cant_hun
)
agroproductos$cven_hun <- ifelse(
  is.na(agroproductos$cven_hun),
  0,
  agroproductos$cven_hun
)
agroproductos$val_hun <- ifelse(
  is.na(agroproductos$val_hun),
  0,
  agroproductos$val_hun
)
agroproductos$ven_hun <- ifelse(
  is.na(agroproductos$ven_hun),
  0,
  agroproductos$ven_hun
)

rm(
  agroproductos1,
  agroproductos2,
  agroproductos3,
  agroproductos4,
  agroproductos5,
  agroproductos6
)

#total turnover and output
attach(agroproductos)
agroproductos$ven_tot <- ven_agr +
  ven_ani +
  ven_prodani +
  ven_for +
  ven_fis +
  ven_hun
agroproductos$val_tot <- val_agr +
  val_ani +
  val_prodani +
  val_for +
  val_fis +
  val_hun
detach(agroproductos)

#farm type and market orientation
agroproductos$tipo_market <- ifelse(agroproductos$ven_tot > 0, 1, 0)

attach(agroproductos)
agroproductos$n_tipo_prod[val_tot == 0] <- 0 #no production
agroproductos$n_tipo_prod[val_agr >= val_tot * (2 / 3)] <- 1 #crop farms
agroproductos$n_tipo_prod[val_ani + val_prodani >= val_tot * (2 / 3)] <- 2 #livestock farms
agroproductos$n_tipo_prod[
  (val_agr > 0 & val_ani + val_prodani > 0) &
    val_agr + val_ani + val_prodani > val_tot * (2 / 3)
] <- 3 #mixed crops-livestock farms
agroproductos$n_tipo_prod[
  (val_agr > 0 | val_ani + val_prodani > 0) &
    val_agr + val_ani + val_prodani < val_tot * (2 / 3)
] <- 4 #mixed farm-primary
agroproductos$n_tipo_prod[
  (val_agr + val_ani + val_prodani == 0) & val_tot > 0
] <- 5 #primary non-farm
detach(agroproductos)

#saving data
# paste0(output_file, ".Rdata")
# paste0(output_file, ".csv")
output_file <- here("output", "data", "agroproductos")
save(agroproductos, file = paste0(output_file, ".Rdata"))
write.table(
  agroproductos,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#AGROCONSUMO
############

#working directory
wd <- "conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_agroconsumo_enigh2022_ns/conjunto_de_datos/"

table <- read.csv(
  here("src", wd, "conjunto_de_datos_agroconsumo_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)

#loading original data and selecting variables
# table <- read.csv("conjunto_de_datos_agroconsumo_enigh2022_ns.csv", header = TRUE, sep =",")
data <- table[c("folioviv", "foliohog", "codigo", "destino", "valestim")]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)
data$count_prod <- 1

#classifying products
data$tipo[data$codigo < 230] <- 1 # 1 = crops
data$tipo[data$codigo > 229 & data$codigo < 278] <- 2 # 2 = livestock
data$tipo[data$codigo > 277 & data$codigo < 296] <- 3 # 3 = animal products
data$tipo[data$codigo > 295 & data$codigo < 474] <- 4 # 4 = forest products
data$tipo[data$codigo > 474 & data$codigo < 601] <- 5 # 5 = fishing products
data$tipo[data$codigo > 600] <- 6 # 6 = hunting products

#selecting self-consumption use only
data <- data[
  data$destino == 1 | #households' consumption
    data$destino == 3 | #household's debt repayment
    data$destino > 5,
] #household's gifts and products exchanges

#households' dataset
agroconsumo <- data |>
  group_by(hogar) |>
  summarise(n_prod = sum(count_prod))

#datasets with households' total consumption for different products
data_agr <- data[data$tipo == 1, ] #crops
agroconsumo1 <- data_agr |>
  group_by(hogar) |>
  summarise(n_agr = sum(count_prod), val_agr = sum(valestim))
rm(data_agr)
data_ani <- data[data$tipo == 2, ] #livestock
agroconsumo2 <- data_ani |>
  group_by(hogar) |>
  summarise(n_ani = sum(count_prod), val_ani = sum(valestim))
rm(data_ani)
data_prodani <- data[data$tipo == 3, ] #animal products
agroconsumo3 <- data_prodani |>
  group_by(hogar) |>
  summarise(n_prodani = sum(count_prod), val_prodani = sum(valestim))
rm(data_prodani)
data_for <- data[data$tipo == 4, ] #forest products
agroconsumo4 <- data_for |>
  group_by(hogar) |>
  summarise(n_for = sum(count_prod), val_for = sum(valestim))
rm(data_for)
data_fis <- data[data$tipo == 5, ] #fishing products
agroconsumo5 <- data_fis |>
  group_by(hogar) |>
  summarise(n_fis = sum(count_prod), val_fis = sum(valestim))
rm(data_fis)
data_hun <- data[data$tipo == 6, ] #hunting products
agroconsumo6 <- data_hun |>
  group_by(hogar) |>
  summarise(n_hun = sum(count_prod), val_hun = sum(valestim))
rm(data_hun)

#merging new variables in the households dataset - setting NA to 0
agroconsumo <- left_join(agroconsumo, agroconsumo1, by = "hogar")
agroconsumo$n_agr <- ifelse(is.na(agroconsumo$n_agr), 0, agroconsumo$n_agr)
agroconsumo$val_agr <- ifelse(
  is.na(agroconsumo$val_agr),
  0,
  agroconsumo$val_agr
)

agroconsumo <- left_join(agroconsumo, agroconsumo2, by = "hogar")
agroconsumo$n_ani <- ifelse(is.na(agroconsumo$n_ani), 0, agroconsumo$n_ani)
agroconsumo$val_ani <- ifelse(
  is.na(agroconsumo$val_ani),
  0,
  agroconsumo$val_ani
)

agroconsumo <- left_join(agroconsumo, agroconsumo3, by = "hogar")
agroconsumo$n_prodani <- ifelse(
  is.na(agroconsumo$n_prodani),
  0,
  agroconsumo$n_prodani
)
agroconsumo$val_prodani <- ifelse(
  is.na(agroconsumo$val_prodani),
  0,
  agroconsumo$val_prodani
)

agroconsumo <- left_join(agroconsumo, agroconsumo4, by = "hogar")
agroconsumo$n_for <- ifelse(is.na(agroconsumo$n_for), 0, agroconsumo$n_for)
agroconsumo$val_for <- ifelse(
  is.na(agroconsumo$val_for),
  0,
  agroconsumo$val_for
)

agroconsumo <- left_join(agroconsumo, agroconsumo5, by = "hogar")
agroconsumo$n_fis <- ifelse(is.na(agroconsumo$n_fis), 0, agroconsumo$n_fis)
agroconsumo$val_fis <- ifelse(
  is.na(agroconsumo$val_fis),
  0,
  agroconsumo$val_fis
)

agroconsumo <- left_join(agroconsumo, agroconsumo6, by = "hogar")
agroconsumo$n_hun <- ifelse(is.na(agroconsumo$n_hun), 0, agroconsumo$n_hun)
agroconsumo$val_hun <- ifelse(
  is.na(agroconsumo$val_hun),
  0,
  agroconsumo$val_hun
)

rm(
  agroconsumo1,
  agroconsumo2,
  agroconsumo3,
  agroconsumo4,
  agroconsumo5,
  agroconsumo6
)

#total self-consumption
attach(agroconsumo)
agroconsumo$n_tot <- n_agr + n_ani + n_prodani + n_for + n_fis + n_hun
agroconsumo$valestim <- val_agr +
  val_ani +
  val_prodani +
  val_for +
  val_fis +
  val_hun
detach(agroconsumo)

#saving data
output_file <- here("output", "data", "agroconsumo")
save(agroconsumo, file = paste0(output_file, ".Rdata"))
write.table(
  agroconsumo,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#AGRO
#####

#working directory
wd <- "/conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_agro_enigh2022_ns/conjunto_de_datos"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_agro_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
data <- table #when  using the original file
#data <- table[c("folioviv","foliohog","codigo","destino","valestim")]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)
data$count_member <- 1

#new variables
data$tipoact_agro <- ifelse(data$tipoact < 6, 1, 0) #actividad agricola y ganadera

data$apoyo_1 <- ifelse(is.na(data$apoyo_1), 0, data$apoyo_1) #Apoyo de gobierno federal con pago
data$apoyo_2 <- ifelse(is.na(data$apoyo_2), 0, data$apoyo_2) #Apoyo de gobierno estatal con pago
data$apoyo_3 <- ifelse(is.na(data$apoyo_3), 0, data$apoyo_3) #Apoyo de gobierno municipal con pago
data$apoyo_7 <- ifelse(is.na(data$apoyo_7), 0, data$apoyo_7) #Apoyo no gubernamental con pago
data$apoyo_pago <- (data$apoyo_1 + data$apoyo_2 + data$apoyo_3 + data$apoyo_7) *
  12 #Apoyo con pago

data$apoyo_4 <- ifelse(is.na(data$apoyo_4), 0, data$apoyo_4) #Apoyo de gobierno federal sin pago
data$apoyo_5 <- ifelse(is.na(data$apoyo_5), 0, data$apoyo_5) #Apoyo de gobierno estatal sin pago
data$apoyo_6 <- ifelse(is.na(data$apoyo_6), 0, data$apoyo_6) #Apoyo de gobierno municipal sin pago
data$apoyo_8 <- ifelse(is.na(data$apoyo_8), 0, data$apoyo_8) #Apoyo no gubernamental sin pago
data$apoyo_npago <- (data$apoyo_4 +
  data$apoyo_5 +
  data$apoyo_6 +
  data$apoyo_8) *
  12 #Apoyo sin pago

data$proagro <- ifelse(is.na(data$proagro), 0, data$proagro) #Apoyo PROCAMPO / ProAgro / Bienestar
data$progan <- ifelse(is.na(data$progan), 0, data$progan) #Apoyo del PROGAN
data$pro_agrogan <- (data$proagro + data$progan) * 12 #Apoyo Procampo y Progan

data$nvo_cant1 <- ifelse(is.na(data$nvo_cant1), 0, data$nvo_cant1)
data$nvo_cant2 <- ifelse(is.na(data$nvo_cant2), 0, data$nvo_cant2)
data$nvo_cant3 <- ifelse(is.na(data$nvo_cant3), 0, data$nvo_cant3)
data$nvo_tot <- (data$nvo_cant1 + data$nvo_cant2 + data$nvo_cant3) * 12 #Total support from new social programmes

data$nvo_prog1 <- ifelse(is.na(data$nvo_prog1), 0, data$nvo_prog1)
data$nvo_prog2 <- ifelse(is.na(data$nvo_prog2), 0, data$nvo_prog2)
data$nvo_prog3 <- ifelse(is.na(data$nvo_prog3), 0, data$nvo_prog3)
data$nvo1 <- ifelse(
  data$nvo_prog1 == 2001 | data$nvo_prog1 == 2002,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2001 | data$nvo_prog2 == 2002,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2001 | data$nvo_prog3 == 2002,
  data$nvo_cant3,
  0
)
data$sembr_vida <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Sembrando Vida

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2003 | data$nvo_prog1 == 2004,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2003 | data$nvo_prog2 == 2004,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2003 | data$nvo_prog3 == 2004,
  data$nvo_cant3,
  0
)
data$tand_bien <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Tandas para el Bienestar (Microcréditos para el Bienestar)

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2005 | data$nvo_prog1 == 2006,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2005 | data$nvo_prog2 == 2006,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2005 | data$nvo_prog3 == 2006,
  data$nvo_cant3,
  0
)
data$agromercados <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Agromercados Sociales y Sustentables

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2007 | data$nvo_prog1 == 2008,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2007 | data$nvo_prog2 == 2008,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2007 | data$nvo_prog3 == 2008,
  data$nvo_cant3,
  0
)
data$precios_gar <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Precios de Garantía a Productos Alimentarios Básicos

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2009 | data$nvo_prog1 == 2010,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2009 | data$nvo_prog2 == 2010,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2009 | data$nvo_prog3 == 2010,
  data$nvo_cant3,
  0
)
data$credito_gan <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Crédito Ganadero a la Palabra

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2011 | data$nvo_prog1 == 2012,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2011 | data$nvo_prog2 == 2012,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2011 | data$nvo_prog3 == 2012,
  data$nvo_cant3,
  0
)
data$nacion_fer <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Nacional de Fertilizantes

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2013 | data$nvo_prog1 == 2014,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2013 | data$nvo_prog2 == 2014,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2013 | data$nvo_prog3 == 2014,
  data$nvo_cant3,
  0
)
data$desarollo_rur <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Desarrollo Rural

data$nvo1 <- ifelse(
  data$nvo_prog1 == 2015 | data$nvo_prog1 == 2016,
  data$nvo_cant1,
  0
)
data$nvo2 <- ifelse(
  data$nvo_prog2 == 2015 | data$nvo_prog2 == 2016,
  data$nvo_cant2,
  0
)
data$nvo3 <- ifelse(
  data$nvo_prog3 == 2015 | data$nvo_prog3 == 2016,
  data$nvo_cant3,
  0
)
data$otros_prog <- (data$nvo1 + data$nvo2 + data$nvo3) * 12 #Otros programas sociales

#direct payments new social programmes
data$nvo_tot_npago <- data$sembr_vida +
  data$agromercados +
  data$precios_gar +
  data$nacion_fer +
  data$desarollo_rur +
  data$otros_prog

#farm gross output, net income, direct payments
data$size_val <- (data$ventas_tri + data$auto_tri + data$otros_tri) * 4
data$fni_year <- (data$ing_tri - data$ero_tri) *
  4 +
  data$apoyo_npago +
  data$pro_agrogan +
  data$nvo_tot_npago
data$support <- data$apoyo_npago + data$pro_agrogan + data$nvo_tot_npago

#dataset with new agro variables at the household level
agro <- data |>
  group_by(hogar) |>
  summarise(
    n_act = sum(count_member),
    n_fni = sum(fni_year),
    auto_tri = sum(auto_tri),
    n_support = sum(support),
    n_size_val = sum(size_val),
    n_apoyo_npago = sum(apoyo_npago),
    n_pro_agrogan = sum(pro_agrogan),
    n_nvo_tot = sum(nvo_tot_npago),
    n_sembr_vida = sum(sembr_vida),
    n_tand_bien = sum(tand_bien),
    n_agromercados = sum(agromercados),
    n_precios_gar = sum(precios_gar),
    n_credito_gan = sum(credito_gan),
    n_nacion_fert = sum(nacion_fer),
    n_desarollo_rur = sum(desarollo_rur),
    n_otros_prog = sum(otros_prog)
  )

#share of self consumption on total output
agro$n_autoconsumo <- ifelse(
  agro$n_size_val > 0,
  (agro$auto_tri * 4) / agro$n_size_val,
  0
)

#share of support on total revenues
agro$n_apoyo <- 0
agro$n_apoyo <- ifelse(
  agro$n_size_val + agro$n_support > 0 & agro$n_size_val > 0,
  agro$n_support / (agro$n_size_val + agro$n_support),
  agro$n_apoyo
)
agro$n_apoyo <- ifelse(
  agro$n_size_val + agro$n_support > 0 & agro$n_size_val == 0,
  1,
  agro$n_apoyo
)

agro$n_size_class[agro$n_size_val < 250001] <- 1
agro$n_size_class[agro$n_size_val > 250000 & agro$n_size_val < 500001] <- 2
agro$n_size_class[agro$n_size_val > 500000 & agro$n_size_val < 1000001] <- 3
agro$n_size_class[agro$n_size_val > 1000000 & agro$n_size_val < 2000001] <- 4
agro$n_size_class[agro$n_size_val > 2000000 & agro$n_size_val < 5000001] <- 5
agro$n_size_class[agro$n_size_val > 5000000 & agro$n_size_val < 10000001] <- 6
agro$n_size_class[agro$n_size_val > 10000000] <- 7

#importing farm type and market orientation from the agroproductos dataset
farmtype <- agroproductos[c("hogar", "n_tipo_prod", "tipo_market")]
agro <- left_join(agro, farmtype, by = "hogar")
rm(farmtype)

#importing selfconsumption from the agroconsumo dataset
autocons <- agroconsumo[c("hogar", "valestim")]
agro <- left_join(agro, autocons, by = "hogar")
rm(autocons)

#share of self consumption on total output with selfconsumption values from the agroconsumo dataset
agro$n_size_val2 <- agro$n_size_val - agro$auto_tri * 4 + agro$valestim
agro$n_autoconsumo2 <- agro$valestim / agro$n_size_val2

#saving data
output_file <- here("output", "data", "agro")
save(agro, file = paste0(output_file, ".Rdata"))
write.table(
  agro,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#ETNIA
######
#working directory
wd <- "conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_poblacion_enigh2022_ns/conjunto_de_datos/"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_poblacion_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
data <- table[c("folioviv", "foliohog", "parentesco", "etnia")]
data <- data[which(data$parentesco == "101"), ] # including only Jefe

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)
etnia <- data
colnames(etnia)[4] <- "n_etnia"

#saving data
output_file <- here("output", "data", "etnia")
save(etnia, file = paste0(output_file, ".Rdata"))
write.table(
  etnia,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#CONCERN FOR FOOD
#################
#working directory
wd <- "conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_hogares_enigh2022_ns/conjunto_de_datos"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_hogares_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
data <- table[c("folioviv", "foliohog", "acc_alim1")]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)
alim <- data

colnames(alim)[3] <- "n_acc_alim1"

#saving data
output_file <- here("output", "data", "alim")
save(alim, file = paste0(output_file, ".Rdata"))
write.table(
  alim,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#NOAGRO
#######

#working directory
wd <- "conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_noagro_enigh2022_ns/conjunto_de_datos"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_noagro_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
#loading original data and selecting variables
data <- table[c("folioviv", "foliohog", "ing_tri", "ero_tri")]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)

#non agricultural self employed incomes
data$n_ingr_noagr <- (data$ing_tri - data$ero_tri) * 4

#dataset with new noagro variables at the household level
noagro <- data |>
  group_by(hogar) |>
  summarise(n_ingr_noagr = sum(n_ingr_noagr))

#saving data
output_file <- here("output", "data", "noagro")
save(noagro, file = paste0(output_file, ".Rdata"))
write.table(
  noagro,
  file = paste0(output_file, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
rm(table, data)

#CONCENTRADOHOGAR
#################
#working directory
wd <- "/conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_concentradohogar_enigh2022_ns/conjunto_de_datos"
#loading original data and selecting variables
table <- read.csv(
  here("src", wd, "conjunto_de_datos_concentradohogar_enigh2022_ns.csv"),
  header = TRUE,
  sep = ","
)
data <- table[c(
  "folioviv",
  "foliohog",
  "trabajo",
  "rentas",
  "transfer",
  "estim_alqu",
  "otros_ing"
)]

#Variable to identify households
data$hogar <- str_c(data$folioviv, data$foliohog)

#importing additional variables from other datasets
#agro
imp <- agro[c(
  "hogar",
  "n_fni",
  "n_size_val",
  "n_size_class",
  "n_tipo_prod",
  "n_autoconsumo",
  "n_autoconsumo2",
  "n_apoyo",
  "n_apoyo_npago",
  "n_pro_agrogan",
  "n_nvo_tot",
  "n_support",
  "n_sembr_vida",
  "n_tand_bien",
  "n_agromercados",
  "n_precios_gar",
  "n_credito_gan",
  "n_nacion_fert",
  "n_desarollo_rur",
  "n_otros_prog"
)]
data <- left_join(data, imp, by = "hogar")
data$n_fni <- ifelse(is.na(data$n_fni), 0, data$n_fni)
rm(imp)
#noagro
imp <- noagro[c("hogar", "n_ingr_noagr")]
data <- left_join(data, imp, by = "hogar")
data$n_ingr_noagr <- ifelse(is.na(data$n_ingr_noagr), 0, data$n_ingr_noagr)
rm(imp)
#etnia
imp <- etnia[c("hogar", "n_etnia")]
data <- left_join(data, imp, by = "hogar")
rm(imp)
#alim
imp <- alim[c("hogar", "n_acc_alim1")]
data <- left_join(data, imp, by = "hogar")
rm(imp)

#creating new variables
data$n_trabajo <- data$trabajo * 4 #annual income from employed labour
data$n_rentas <- data$rentas * 4 #annual income from owned assets
data$n_transfer <- data$transfer * 4 #annual income from social and private transfers
data$n_estim_alqu <- data$estim_alqu * 4 #annual implicit rent from property dwelling
data$n_otros_ing <- data$otros_ing * 4 #other annual sources of income
data$n_ing_cor <- data$n_fni +
  data$n_ingr_noagr +
  data$n_trabajo +
  data$n_rentas +
  data$n_transfer +
  data$n_estim_alqu +
  data$n_otros_ing
data$n_tipo_act <- ifelse(data$n_tipo_prod < 5, 1, 0) #households with farming activities

exclude <- names(data) %in%
  c(
    "folioviv",
    "foliohog",
    "trabajo",
    "rentas",
    "transfer",
    "estim_alqu",
    "otros_ing"
  )
data <- data[!exclude]
rm(exclude)

#merging the new variables with the original dataset
table$hogar <- str_c(table$folioviv, table$foliohog)
concentradohogar <- left_join(table, data, by = "hogar")
rm(data, table)

#INCOME DECILES AND QUINTILES (we will do that in part 2)
#############################

# #selecting variables
# data <- concentradohogar[c("hogar","factor","tot_integ",
#                            "n_ing_cor","n_fni","n_ingr_noagr","n_trabajo","n_rentas","n_transfer","n_estim_alqu","n_otros_ing")]
# #replacing negative income values
# quantiles <- Hmisc::wtd.quantile(data$n_fni[data$n_fni > 0], weights = data$factor, na.rm = TRUE)
# lower_quartile_fni <- quantiles[2]
# quantiles <- wtd.quantile(data$n_ingr_noagr[data$n_ingr_noagr > 0], weights = data$factor, na.rm = TRUE)
# lower_quartile_ingr_noagr <-quantiles[2]
# rm(quantiles)
# data$neg_idx <- ifelse(data$n_fni < 0,1,0)
# data$nn_fni <- ifelse(data$neg_idx == 1,runif(n=sum(!is.na(data$n_fni)&data$n_fni<0), min = 0, max = lower_quartile_fni[1]),data$n_fni)
# data$neg_idx <- ifelse(data$n_ingr_noagr < 0,1,0)
# data$nn_ingr_noagr <- ifelse(data$neg_idx == 1 , runif(n=sum(!is.na(data$n_ingr_noagr)&data$n_ingr_noagr<0), min = 0, max = lower_quartile_ingr_noagr[1]),data$n_ingr_noagr)
# data$nn_fni <- ifelse(is.na(data$n_fni),0,data$nn_fni)
# data$nn_ingr_noagr <- ifelse(is.na(data$n_ingr_noagr),0,data$nn_ingr_noagr)
# data$neg_idx <- NULL
# summary(data$nn_fni)
# summary(data$nn_ingr_noagr)
#
# data$nn_ingr_corr <- data$nn_fni+data$nn_ingr_noagr+data$n_trabajo+data$n_rentas+data$n_transfer+data$n_estim_alqu+data$n_otros_ing
# summary(data$nn_ingr_corr)
# data$equiv_integ <- sqrt(data$tot_integ)#equivalence scale square root
# data$epc_ingr_cor <- data$nn_ingr_corr/data$equiv_integ#per capita equivalent income using the square root equivalence scale
# summary(data$epc_ingr_cor)
# summary(data$equiv_integ)
#
# #income deciles
# dec <- as.vector(weightedQuantile(data$epc_ingr_cor,weights = data$factor, probs = seq(0,1,0.1), sorted = FALSE,na.rm = TRUE))
# data$decile <- 0
# data$decile <- ifelse(data$epc_ingr_cor < dec[2],1,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[2]& data$epc_ingr_cor<dec[3],2,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[3]& data$epc_ingr_cor<dec[4],3,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[4]& data$epc_ingr_cor<dec[5],4,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[5]& data$epc_ingr_cor<dec[6],5,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[6]& data$epc_ingr_cor<dec[7],6,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[7]& data$epc_ingr_cor<dec[8],7,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[8]& data$epc_ingr_cor<dec[9],8,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[9]& data$epc_ingr_cor<dec[10],9,data$decile)
# data$decile <- ifelse(data$epc_ingr_cor>= dec[10],10,data$decile)
#
# #income quintile
# data$quintile <- 0
# data$quintile <- ifelse(data$decile==1 | data$decile==2,1,data$quintile)
# data$quintile <- ifelse(data$decile==3 | data$decile==4,2,data$quintile)
# data$quintile <- ifelse(data$decile==5 | data$decile==6,3,data$quintile)
# data$quintile <- ifelse(data$decile==7 | data$decile==8,4,data$quintile)
# data$quintile <- ifelse(data$decile==9 | data$decile==10,5,data$quintile)
#
# data <- data[c("hogar","decile","quintile")]
# concentradohogar <- left_join(concentradohogar,data,by="hogar")
#
# #saving data
rev_nb <- "_rev8"
output_file <- here("output", "data", "concentradohogar")
save(concentradohogar, file = paste0(output_file, rev_nb, ".Rdata"))
write.table(
  concentradohogar,
  file = paste0(output_file, rev_nb, ".csv"),
  sep = ";",
  col.names = TRUE,
  row.names = FALSE
)
# rm(data)
