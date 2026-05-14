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
