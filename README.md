# Workflow GitHub del progetto

## 1. Clonare il progetto in locale

Una volta che sei stato aggiunto come collaboratore su GitHub:

```bash
git clone https://github.com/RaphaelPorcherot/INEGI_Mexico.git
cd INEGI_Mexico
```

Poi apri il progetto in RStudio (o VSCode).

---

## 2. Attivare l’ambiente `renv`

Il progetto usa `renv` per gestire le dipendenze R.

Solo la prima volta:

```r
install.packages("renv")
renv::restore()
```

Questo installerà automaticamente le versioni corrette dei package.

Dopo, normalmente basta aprire il progetto: `renv` si attiva automaticamente.

Importante:

* evitare `install.packages()` manuali salvo necessità;
* se aggiungi un package utile al progetto, avvisami prima di fare `renv::snapshot()`.

---

## 3. Lavorare su un branch dedicato

Per evitare di lavorare direttamente sul branch principale (`main`), crea un branch personale:

```bash
git checkout -b nome-feature
```

Esempio:

```bash
git checkout -b benedetto-verif-data
```

Da quel momento lavori solo su quel branch.

Per verificare il branch attivo:

```bash
git branch
```

L’asterisco indica il branch corrente.

---

## 4. Salvare il proprio lavoro

Quando vuoi registrare le modifiche:

```bash
git add .
git commit -m "descrizione chiara delle modifiche"
```

Poi invia il branch su GitHub:

```bash
git push origin nome-del-branch
```

Esempio:

```bash
git push origin benedetto-verif-data
```

---

## 5. Proporre una fusione verso `main`

Quando le modifiche sono pronte:

1. Vai su GitHub;
2. GitHub proporrà automaticamente di aprire una Pull Request;
3. Crea la Pull Request verso `main`.

Controllerò poi le modifiche prima del merge.

L’idea è:

* `main` resta stabile;
* ognuno sviluppa sul proprio branch;
* le modifiche passano tramite Pull Request prima di essere integrate.

---

## 6. Aggiornare il proprio branch con le ultime modifiche di `main`

Prima di iniziare una nuova sessione di lavoro:

```bash
git checkout main
git pull
```

Poi torna sul tuo branch:

```bash
git checkout nome-del-branch
```

E recupera gli aggiornamenti di `main`:

```bash
git merge main
```

Così si evitano conflitti all’ultimo momento.

---

## 7. Cose da evitare

* non lavorare mai direttamente su `main`;
* evitare di modificare `renv.lock` senza motivo;
* evitare commit troppo grandi e confusi;
* fare commit piccoli e chiari.

Esempio di buon messaggio di commit:

```bash
git commit -m "corregge la pulizia delle date in import_data.R"
```

---

## 8. Aprire una Pull Request (PR)

Dopo aver fatto il push del tuo branch su GitHub:

1. Vai sul repository su GitHub
2. Se appare il pulsante **“Compare & pull request”**, cliccalo
   (altrimenti vai su **Pull requests → New pull request**)
3. Controlla che:
   - base = `main`
   - compare = il tuo branch

---

## 9. Descrivere la PR

Nella descrizione spiega brevemente:

- cosa hai fatto
- quali file sono stati modificati
- eventuali punti da verificare

Esempio:

> Aggiunta pulizia dei dati in import_data.R  
> Correzione del formato delle date  
> Miglioramento della funzione di filtro

---

## 10. Creare la PR

Clicca su **Create pull request**.

Da quel momento io potrò:
- rivedere le modifiche
- chiedere eventuali correzioni
- fare il merge in `main` quando tutto è ok

---

## 11. Dopo la PR

Se vengono richieste modifiche:
- continui a lavorare sullo stesso branch
- fai nuovi commit e push
- la Pull Request si aggiorna automaticamente

Non è necessario crearne una nuova.
