## FINALLY

Caro benedetto, 

il diagnostico va cambiando, credo che adesso va stabilizato.

d7 e il database che abbiamo utilizzato per il ocse

d8 e il database che mi hai mandato con il tuo script

d e il database che ho generato basandome sulla correzione del tuo script, pasandolo al universo "tidyverse)

n_ing_cor e la variabile sospetta 

d7

d8

d

Qua vedi le distribuzione di n_ing_cor in ogni dataset. Il punto e decidere se ce n'e una che e la corretta e si e cosi, quale.

Possiamo leggere y verificare di nuovo il codice, possiamo anche compare con il valore annuale della variabilie smg che computano nel INEGHI. smg = minimum wage trimestrializato. 

Mi sembra che possiamo descartare d8 : i valori sono absurdi.

Comparando smg*12 con d7 e d, mi sembra che smg*12 e consistente con il valore media di d : 239 861 invece d7 e anch'assurdo. 

Se e cosi, d7 (elaborato dal excel) e sbagliato e abbiamo mostrato numeri falsi al ocse.

Dall'altra parte, se e cosi, non avremo solo 3% della popolazione totale sotto la linea de poverta calcolata sulla base de smg*12 equivaled income. Mi sembra piu correto.

Che ne dici ? 

E possibile che anche d sia per il momento ancore sbagliato : vado a verificare linea per linea e, per necesita, anche i unit of measures di ogni variabilie nella doc del survey.

saluti

## mail 20260515 Ciao benedetto, 

quindi le questione : 

1. la piu importante e che c'e qualcosa di strano con i redditi. un problema di annualizaione dei valori sicuro.: 

il primo valore sempre e dal vecchio database (_rev7). il seguente e della nuova. 

Sono cambiate tanto il ordine di grandezza che il valore. 

* il valore puo essere che viene di che hai cambiato la forma di calcolare n_fni (adesso senza tandas y creditos a la palabra)

* il ordine di grandezza e piu problematico : significa che c'e un problema nella forma di passare dai valori mensuali o trimestriali a annuali. 

Fin'a ora non ho potuto capire di dove viene. Magari puoi vederci 

Agri/INEGI_Mexico/project/script/mexico_data_part1.R

nel nostro drive e chequeare ? 

2. support program for non agro activities 

il reddito dal lavoro autonomo non agri e 

      n_ingr_noagr = (ing_tri - ero_tri) * 4

il reddito dal lavoro autonomo agri e : 

fni_year = (ing_tri - ero_tri) *
        4 +
        apoyo_npago +
        pro_agrogan +
        nvo_tot_npago,

in NOAGRO ci sono anche nuovi programi soziali con valori non NA 

Non dovremmo incluirle in ing_noagro ? 

Mi sembra cmq strano che attivita non agricole ricevano apoggio per le attivate agricole, ma e cio che sembra essere il caso nel database

4. Minor questions

* sai perche ci sono due codici per ogni programa soziale ? 2001 e 2002 per sembrando vida per esempio. 

* in AGRO, usi tipoact per calcolare tipoact_agro 

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

avevi scritto

data$tipoact_agro <- ifelse(data$tipoact < 6, 1, 0) #actividad agricola y ganadera

Ho clarificato la intenzione con 

      tipoact_agro = if_else(tipoact %in% c(4, 5), 1, 0),

Ho visto che in AGRO non ci sono i livelli 1 a 3. 

e corretto con la tua intenzione ? 


Per ultimo, alcune informazione : 

* ci sono variabile nuove nel database finale e ho togliato le variabile di share (n_apoyo n_autoconsumo1 e n_autoconsumo2) : necesito calcolarle dopo, con il pkg survey, per avre i intervalli di fiducia correti

Cmq mi sembra che c'eranno cose stranne

#share of support on total revenues
agro$n_apoyo <- 0
agro$n_apoyo <- ifelse(
  agro$n_size_val + agro$n_support > 0 & agro$n_size_val > 0,
  agro$n_support / (agro$n_size_val + agro$n_support),
  agro$n_apoyo
)

perche n_support anche nel denominatore ? E in realite n_support non e una part of totla revenus : 

      size_val = (ventas_tri + auto_tri + otros_tri) * 4,

      fni_year = (ing_tri - ero_tri) *
        4 +
        apoyo_npago +
        pro_agrogan +
        nvo_tot_npago,

      support = apoyo_npago + pro_agrogan + nvo_tot_npago

* ho creato un nuovo diccionario delle variabile : dict_new_variables.csv. C'e una parte del script in part1 che verifica se c'e bisogno ampliarlo o no dependiendo dei cambi che sono stati fatti in part1

## sur les liens entre n_fni et n_size_class 

```r 

nvo_tot_npago = sembr_vida + agromercados + precios_gar + nacion_fer + desarollo_rur + otros_prog  

size_val = (ventas_tri + auto_tri + otros_tri) * 4

support = apoyo_npago + pro_agrogan + nvo_tot_npago 

fni_year = (ing_tri - ero_tri) * 4 + support

```

ing_tri c'est quoi par rapport à ventas_tri ? 

Il faut vérifier les unités car soit les deux sont en valeurs et alors ca doit être égal, soit c'est en volume et alors il y a quelque chose de bizarre

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
