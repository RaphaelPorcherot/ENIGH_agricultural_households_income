# CURRENT
| Column1 | Column2 | Column3 |
| ------------- | -------------- | -------------- |
| Item1 | Item1 | Item1 |

ing_cor est la sommme ingtra + ce qu'on a 
ingtra c'est negocio (agrope et noagrope) + trabajo + otro_trab 
-> il faut qu'on rajoute otros_trab dans trabajo 

dans transf_hog il y a bene_gob mais est ce que apoyo est dans bene_gob ? 

ingreos.ing_tri : dans cette variabel de la table INGRESO, comment est reporté l'ing_tri de AGRO ? c'est ing_tri - ero_tri normalisé par zéro ? ing_tri et ventas_tri, c'est quoi la différence ? 

alcune domande sulla survey :

c'e una variabile ing_cor in CONCENTRADOHOGAR

ing_cor = ingtra + rentas + estim_alqui + transfer + otros_ing

ingtra = trabajo + otros_trab + negocio

negocio = agrope + noagrope

agrope e la suma de ing_tri nella tabella INGRESOS quando e una attivita agricola

Noi riconstruiamo ing_cor.

la nostra e 

n_ing_cor = trabajo + rentas + estim_alqui + transfer + otros_ing + fni + ingr_noagr

fni = (ing_tri - ero_tri) + support

ingr_noagr = (ing_tri - ero_tri) + support


Le domande:

1. non abbiamo incluido otros_tra, dovremmo aggiungerla, vero? O era a proposito che la hai lasciato fuori ? 

2. in transfer c'e transf_hog, in transf_hog c'e bene_gob per esempio : solo per essere sicuro, non sono incluso qua i programi sociali ? 

3. In AGRO valori dei programi soziali non possono essere mensuale (se no : assurdita in ratio di support/entrate aziendale) : ti ricordi perche avevi considerato che erano mensuali ? Mi sembra che sono annuali, sei di accordo ? 

4. in AGRO c'e ing_tri, che dopo pasa a INGRESOS.ing_tri y di la a negocio in CONCENTRADOHOGAR. Non capisco perche i appogi dai programmi soziali non erano incluiti in ing_tri da AGRO. Mi sembra incoerente con il resto del survey. Sei sicuro che non vanno registrati e che dobbiamo incluirli in fni e ingr_nogr ? 

5. in AGRO c'e venta_tri al lato di ing_tri. Cual e la differenzia ? Mi hai spiegato qualcosa su una variabile di cui loro gia transfornano i valori negativi in valore zero. Era ventas_tri ? 

6. Hai capito come fanno per pasare i redditi dalle attivita agricole da AGRO a INGRESOS ? Che cos'e la cosa che passano da una tabella all'altra ? ing_tri - ero_tri ? 

Mi sembra illogico che fosse ing_tri - ero_tri gia che in INGRESO la variabile si chiama ing_tri.

Pero si è ing_tri che passano da una tabella all'altra, allora dove passano ero_tri dalle attivita del negozio ? c'e balance, che sono le perdite di un negozio, ma non c'e ero_tri.

Cambia il senso di ing_cor da ENIGH verso n_ing_cor da noi : noi avremmo ing_cor - ero_tri_agro * 4 - ero_tri_noagro * 4, quindi una variabile di reddito fra reddito corriente e reddito disponibile, ma comunque concettualmente distinto da ing_cor.

mi sembra di ricordarmi che ing_tri era la varibilie di cui loro normizavano i valori negativi a zero.

in questo caso, e sbagliato di usare come facciamo fin'ad ora

    fni_year = (ing_tri - ero_tri) * 4 + support

e dovremmo usare :

    fni_year = (ventas_tri - ero_tri) * 4 + support

In questo caso, in INGRESO c'e solo redditi corrienti, il nostro n_ing_cor e un reddito corriente totale como il ing_cor dal ENIGH. 

* [ ] il faudrait vérifier la construction de notre variable de revenu en regardant les totaux
* [ ] ing_tri is not used we reconstruit agrope et no a partir de (ing - ero) * 4 + support (l'ingreso est net des erogaciones), le n_ing_cor_clean est nette des erogaciones liés au travail indépendant
* [ ] annnual turnover factor levels needs to be corrected : we need to check whether they make sense (since minimum wage c'est 60000 par an, la on a des fermes avec moins de 2000 pesos de turnover)


r$> agro |> select(n_size_val1) |> skim()
── Data Summary ────────────────────────
                           Values                   
Name                       select(agro, n_size_val1)
Number of rows             11697                    
Number of columns          1                        
_______________________                             
Column type frequency:                              
  numeric                  1                        
________________________                            
Group variables            None                     

── Variable type: numeric ──────────────────────────────────────────────────────────────────────────────────
  skim_variable n_missing complete_rate   mean      sd p0   p25    p50    p75      p100 hist 
1 n_size_val1           0             1 64408. 425823.  0 3585. 12921. 43533. 40304348. ▇▁▁▁▁

* [ ] farm specialisation : big change, no mixed 
* [ ] sahre of fni much more unequal for some reasons (Gini also i assume)
* [ ] some poor agri hosuehods have farms with > 50 000 anual turnover and richest have < 2000 turnover
* [ ] reprendre les TODO et il lui lister 
* [ ] reprendre les notes sur FLAN + rajouter celles sur AFEP : probablement des TODO, genre MFA()
* [ ] elf consumption has been corrected (on calculait la part de l'auotconsommation du décile dans la prod total agri lol)

# PAST 

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
