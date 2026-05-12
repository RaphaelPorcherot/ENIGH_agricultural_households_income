
## In AGRO

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


