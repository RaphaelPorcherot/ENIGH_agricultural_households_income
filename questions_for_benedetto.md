## n_apoyo et n_autoconsum1 ou 2 

on calculera les shares correspondants dans un second temps et on exportera le .csv
## In AGRO

### tipoact
tipoact,descripcion
1,Industrial
2,Comercial
3,De servicios
4,Actividades agrícolas
5,Actividades de cría y explotación de animales
6,Actividades de recolección
7,Reforestación y tala de arboles
8,Actividades de caza y captura de animales
9,Actividades de pesca


Nel codice, facciamo : 
data$tipoact_agro <- ifelse(data$tipoact < 6, 1, 0) #actividad agricola y ganadera

E corretto ? Se vogliamo solo activdad agriola e ganadera dovrebbe essere : 

data$tipoact %in% c(4,5)

O volevi integrare anche le attivita on farm pero non agricola ? 

AGRO es la tavella dei negozi agropastorali delle famiglie.


### share of n apoyo 

en fait n_size_val

      # farm gross output, net income, direct payments
      # Ingreso trimestral por ventas 
      # Autoconsumo trimestral
      # Otros montos trimestral
      # TODO: pour préciser dans présentation : turnover inclut autoconsommation y Otros montos no monetarios trimestrales (pago de trabajadores, deudas del negocio, deudas del hogar e intercambios)

size_val = (ventas_tri + auto_tri + otros_tri) * 4,


#share of support on total revenues
agro$n_apoyo <- 0
agro$n_apoyo <- ifelse(
  agro$n_size_val + agro$n_support > 0 & agro$n_size_val > 0,
  agro$n_support / (agro$n_size_val + agro$n_support),
  agro$n_apoyo
)

## In NOAGRO 

      # annualized non-agricultural self-employed income
      n_ingr_noagr = (ing_tri - ero_tri) * 4

On devrait mettre aussi dedans les valeurs des programmes sociaux non ? elles ne sont pas nulles 

      fni_year = (ing_tri - ero_tri) *
        4 +
        apoyo_npago +
        pro_agrogan +
        nvo_tot_npago,

## IN EVERYWHERE 

there is an issue with annualization 
