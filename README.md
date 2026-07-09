# Data source 

Go to [https://www.inegi.org.mx/datosabiertos/](https://www.inegi.org.mx/datosabiertos/), Encuestas/regulares/Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH)/2022.

Put the data in a /src/ folder and unzip it. The scripts will go and target the necessary csv.

# Release on Zenodo

L'obiettivo è pubblicare una versione stabile del codice su Zenodo, che genera un DOI citabile. La versione pubblicata sarà pubblica e priva dei commenti di lavoro interni (#TODO, #WARN, #NOTE, #INFO) — che restano invece visibili su main per il nostro uso quotidiano.

Il processo usa un branch temporaneo `zenodo` che esiste solo il tempo della release, poi viene eliminato. Il branch main non viene mai toccato.

`clean_comments.py` non è nel repository — esiste solo sul computer di Raphael e va lanciato manualmente prima della release.

```bash
git checkout -b zenodo
rm README.md && rm notes_and_questions.md # aussi docs/ et les scripts z_ dans scripts/
python clean_comments.py
git add .
git commit -m "clean : remove todo comments for release"
git push -u origin zenodo

```
A questo punto:

* Si va su GitHub.com → Releases → Create a new release
* Si sceglie il branch zenodo (non main)
* Si assegna un tag di versione (es. v1.0.0)
* Si pubblica la release → Zenodo la rileva automaticamente e genera il DOI

(optionale) Poi si torna su main e si elimina il branch temporaneo:

```bash
git checkout main 
git branche -d zenodo
git push origin --delete zenodo
```

# Working with renv

Il progetto usa `renv` per bloccare le versioni di R e dei package, cosi che il codice funzioni allo stesso modo su ogni computer (il mio, il tuo, e quello di chiunque scarichi il repository da Zenodo).

**Versioni usate in questo progetto:**

- R: `4.5.3`
- renv: `1.2.2`

⚠️ Se hai una versione di R molto diversa (es. 4.3.x), alcuni package potrebbero non installarsi correttamente. In quel caso avvisami prima di forzare qualcosa.

## Installazione (solo la prima volta)

Come già scritto nella sezione 2, dopo aver clonato il progetto e aperto il file `.Rproj`, nella console di RStudio:

```r
install.packages("renv")
renv::restore()
```

`renv::restore()` legge il file `renv.lock` (che è nel repository) e installa **esattamente** le stesse versioni dei package che uso io. Puo volerci qualche minuto la prima volta.

## Come lavorare con renv (in breve)

**Installare un nuovo package:**

Non usare `install.packages("nome_package")`. Usa invece:

```r
renv::install("nome_package")
```

Fa la stessa cosa, ma lo registra correttamente nell'ambiente renv del progetto (isolato dagli altri progetti R sul tuo computer).

**`renv::snapshot()` — cosa fa:**

Aggiorna il file `renv.lock` con le versioni dei package attualmente installati nel progetto. In pratica "fotografa" lo stato attuale delle librerie e lo salva, cosi che chiunque faccia `renv::restore()` dopo di te ottenga esattamente quelle versioni.

⚠️ **Non farlo mai senza avvisarmi** — se lo fai tu, io mi ritrovo con `renv::restore()` che cambia le mie versioni dei package, e puo rompere cose che stavo usando.

**`renv::status()` — utile per controllare:**

Ti dice se ci sono package installati ma non ancora "fotografati" nel `renv.lock`, o viceversa package nel lockfile che non hai installato. Comodo da lanciare ogni tanto per capire se sei allineato.

```r
renv::status()
```

Per tutto il resto (come funziona renv internamente, gestione delle librerie, uso con Docker, ecc.), la guida ufficiale è qui: [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

# Workflow GitHub del progetto

## Strumenti: GitHub Desktop vs riga di comando

Esistono due modi per usare Git: tramite interfaccia grafica (versione GUI, **GitHub Desktop**, che usi tu) o tramite **riga di comando nel terminale** (versione CLI, che uso io). Le operazioni sono identiche — è solo la forma che cambia. Ecco una tabla di correspondenzia. Qui sotto scrivo il workflow GUI e sempre anche su equivalente CLI

| Operazione | Riga di comando (Raphael) | GitHub Desktop (Benedetto) |
|---|---|---|
| Vedere il branch attivo | `git branch` | Nome visibile in alto al centro della finestra |
| Creare un branch | `git checkout -b nome` | Current Branch → New Branch |
| Cambiare branch | `git checkout nome` | Current Branch → seleziona dalla lista |
| Salvare modifiche | `git add . && git commit -m "..."` | Scrivi messaggio in basso a sinistra → "Commit to [branch]" |
| Inviare su GitHub | `git push origin nome-branch` | Pulsante "Push origin" in alto a destra |
| Aggiornare da main | `git checkout main && git pull` poi `git merge main` | Branch → Update from main |
| Aprire una PR | GitHub.com | Pulsante "Create Pull Request" dopo il push |

---

## 1. Clonare il progetto in locale

**GitHub Desktop:** File → Clone repository → scegli il repository → scegli la cartella locale → Clone.

Poi apri il progetto in RStudio facendo **doppio clic sul file `.Rproj`** nella cartella clonata. Questo è essenziale: apre RStudio nel contesto corretto del progetto.

---

**Equivalente CLI:**

```bash
git clone https://github.com/RaphaelPorcherot/ENIGH_agricultural_households_income.git
cd INEGI_Mexico
```

---

## 2. Attivare l'ambiente `renv`

Il progetto usa `renv` per gestire le versioni dei package R. **Solo la prima volta**, nella console di RStudio:

```r
install.packages("renv")
renv::restore()
```

Questo installerà automaticamente le versioni corrette dei package. Dopo, `renv` si attiva da solo ogni volta che apri il progetto tramite il `.Rproj`.

⚠️ **Importante:**
- Evita `install.packages()` manuali senza avvisarmi
- Non fare mai `renv::snapshot()` senza prima chiedermelo — sovrascriverebbe la lista dei package del progetto

---

## 3. Lavorare su un branch dedicato

Non si lavora mai direttamente su `main`. Ogni modifica va fatta su un branch personale.

**Creare il tuo branch (solo la prima volta):**

In GitHub Desktop: Current Branch → New Branch → chiama il branch `benedetto` → Create Branch.

**Controllare sempre il branch attivo prima di lavorare:**

In GitHub Desktop il branch attivo è visibile in alto al centro. **Se vedi `main`, fermati** e seleziona `benedetto` dalla lista Current Branch.

--- 

**Equivalente CLI:**

Creare un branch personale:

```bash
git checkout -b nome-feature
```

Esempio:

```bash
git checkout -b benedetto
```

Assicurarsi di essere sul tuo branch:

```bash
git checkout benedetto # senza -b : ti pasa a una branch gia esistente ; con -b : crea una nuova branch
```

Per verificare il branch attivo:

```bash
git branch
```

L’asterisco indica il branch corrente.

---

## 4. Iniziare una sessione di lavoro

Ad ogni nuova sessione, prima di modificare qualsiasi file:

1. In GitHub Desktop: clicca **"Fetch origin"** per scaricare gli aggiornamenti
2. Poi: **Branch → Update from main** — per recuperare le mie eventuali modifiche

Questo evita conflitti alla fine.

--- 

**Equivalente CLI:**

Scaricare l'ultima versione da main sulla tua branch:

```bash
git pull origin main # assicura che hai gli ultimi cambi dalla branch prinzipale
```

O di forma piu dettagliata:

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

---

## 5. Salvare il proprio lavoro (commit + push)

Quando vuoi registrare le tue modifiche:

1. In GitHub Desktop, i file modificati appaiono automaticamente nella colonna sinistra
2. Scrivi un **messaggio chiaro** in basso a sinistra (es. *"verifica date nel file import_data.R"*). Prova a fare piccoli commit: uno per cambio unitario che fai. Dopo al etapa 4. invirai tutti i distinti commits da una volta. Fare piccoli commits aiuta a identificare rapido i problemi. 
3. Clicca **"Commit to benedetto"**
4. Clicca **"Push origin"** in alto a destra per inviare su GitHub

⚠️ **Regola fondamentale:** invia sempre sul tuo branch personale, mai su `main`.

Per proteggere il branch principale con una obligazione di usare il mecanismo chiamato "Pull request", dovremmo passare il repo in public invece di private o pagare. Non vogliamo fare ne il uno ne l'altro. 

**Quindi dobbiamo avere disciplina:**

- **Non fare mai** `git push origin main` — invia sempre sul tuo branch personale
- **Non fare mai** `git merge main` o `git checkout main` per modificare direttamente il branch principale
- Quando hai finito una parte del lavoro, **avvisami su WhatsApp/email e creo io il merge** (o una volta che ti senti piu comodo, vedi sezione 5.)

In caso di dubbio, chiedi prima di fare qualsiasi operazione — è sempre meglio una domanda in più che un errore difficile da correggere.

---

**Equivalente CLI:**

Registrare le modifiche:

```bash
git add .
git commit -m "descrizione chiara delle modifiche"
```

Inviare il branch su GitHub:

```bash
git push -u origin nome-del-branch # la prima volta, per publiccare la nuova local branch
git push origin nome-del-branch # le altre volte
```

Esempio:

```bash
git push origin benedetto
```

Per verificare che non stai lavorando su `main` per sbaglio, controlla sempre prima di fare `git add .` :
```bash
git branch
```
Se vedi `* main`, fermati e torna sul tuo branch :
```bash
git checkout benedetto
```

---

## 6. Avvisarmi e proporre le modifiche (Pull Request)

Fusionar il tuo lavoro con la branch principale sono due etapi:

* creare un pull request 

* assicurarsi che tutto va bene e fare il merge

Quando hai finito una parte del lavoro, **avvisami su WhatsApp o email**: faro entrambi operazione.

**Come gestirò io le tue modifiche all'inizio:**

Quando mi avvisi che hai finito una parte del lavoro, ecco cosa farò io :

Vado su GitHub.com nel nostro repository, scheda Pull requests. Se non hai ancora creato una PR, apparirà un banner giallo con il pulsante "Compare & pull request" — lo clicco io. Controllo le tue modifiche, e se tutto è ok clicco "Merge pull request" per integrare il tuo lavoro nel branch principale main.

Da parte tua non devi fare nulla di più che avvisarmi. Gestisco io la fusione.

In caso di conflitto (cioè se abbiamo modificato gli stessi file nello stesso punto), ti contatto prima di procedere.

**Una volta che ti senti più a tuo agio, farai la prima operazione:**

Puoi anche creare tu stesso la Pull Request: dopo il push, in GitHub Desktop apparirà un pulsante blu **"Create Pull Request"** — cliccalo, si aprirà GitHub nel browser. Aggiungi un breve messaggio per descrivere le modifiche e clicca **"Create pull request"**.

Si puo fare anche diretto da la pagina web 

Dopo aver fatto il push del tuo branch su GitHub:

1. Vai sul repository su GitHub
2. Se appare il pulsante **“Compare & pull request”**, cliccalo
   (altrimenti vai su **Pull requests → New pull request**)
3. Controlla che:
   - base = `main`
   - compare = il tuo branch

**Nella descrizione spiega brevemente:**

- cosa hai fatto
- quali file hai modificato
- eventuali dubbi o punti da verificare

Esempio:

> Aggiunta pulizia dei dati in import_data.R  
> Correzione del formato delle date  
> Miglioramento della funzione di filtro

Dopodiché gestisco io il merge in `main`. Piu avanti, farai anche la seconda operazione. 

---
**Equivalente CLI:**

C'è, ma da Github, non da Git. Le pull request sono una feature da Github, c'è bisogno installare `gh`.

```bash
gh pr create --base main --head benedetto-verif-data --title "verifica dati" --body "descrizione"
```

---


## 7. In caso di conflitto

Se abbiamo modificato gli stessi punti degli stessi file, Git segnalerà un conflitto. **Non fare nulla** — contattami e lo risolviamo insieme.

---

## 8. Cose da evitare

- Non lavorare mai direttamente su `main`
- Non fare `renv::snapshot()` senza avvisarmi
- Evitare commit troppo grandi e generici — meglio commit piccoli e descrittivi
- In caso di dubbio su qualsiasi operazione: **chiedi prima**. È sempre meglio una domanda in più che un errore difficile da correggere.

