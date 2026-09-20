# Data source 

Go to [https://www.inegi.org.mx/datosabiertos/](https://www.inegi.org.mx/datosabiertos/), Encuestas/regulares/Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH)/2022.

Put the data in the /src/ folder and unzip it. The scripts will go and target the necessary csv.

# Working with renv

The project uses `renv` to lock the versions of R and its packages, so that the code runs the same way on every computer.

**Versions used in this project:**
- R: `4.6.1`
- renv: `1.2.2`

⚠️ A significantly different version of R (e.g. 4.3.x) may cause some packages not to install correctly.

## Installation (first time only)

After cloning the project, (optionally) create a `.Rproj` file (if you work with RStudio), then run the following in the R console:

```r
install.packages("renv")
renv::restore()
```

`renv::restore()` reads the `renv.lock` file (included in the repository) and installs **exactly** the same package versions that were used in the repo. This may take a few minutes the first time.

## Upgrading from R 4.5.x (if you cloned this repo before September 2026)

The project moved from **R 4.5.3 to R 4.6.1**. This matters because `renv` keeps a
**separate library per R minor version**: the packages you installed under 4.5 live in
`renv/library/macos/R-4.5/…` and are simply invisible to R 4.6. Nothing is broken —
the new library just has to be built once.

1. **Install R 4.6.1** from [cran.r-project.org](https://cran.r-project.org/) and make
   sure your editor is using it. Check with:

   ```r
   R.version.string   # should print "R version 4.6.1 ..."
   ```

2. **Pull the latest `renv.lock`**, which now records R 4.6.1 and the matching package
   versions:

   ```bash
   git pull
   ```

3. **Rebuild the library** from the R console, at the project root:

   ```r
   renv::restore()
   ```

   This installs all 152 packages into `renv/library/macos/R-4.6/…`. Expect 10–30
   minutes depending on your connection. Binary packages install in seconds; any
   package with no binary for your platform is compiled, which is what takes the time.

4. **Check** that everything is in order:

   ```r
   renv::status()   # should print "No issues found -- the project is in a consistent state."
   ```

5. (Optional) The old `renv/library/macos/R-4.5/` folder is no longer used and can be
   deleted to reclaim disk space. It is git-ignored, so removing it changes nothing in
   the repository.

**If `renv::restore()` stalls on a package.** One package in the lockfile, `V8`,
downloads an external library during its installation and can hang on a slow or
filtered connection. Nothing in the pipeline uses it, so it is safe to skip:
interrupt with <kbd>Esc</kbd>, then

```r
renv::restore(exclude = "V8")
```

**If `renv::snapshot()` refuses to run** with a message about a package required by
another one but not installed, install the missing package and snapshot again:

```r
renv::install("<the missing package>")
renv::snapshot()
```

## Working with renv (in brief)

**Installing a new package:**

Do not use `install.packages("package_name")`. Use instead:

```r
renv::install("package_name")
```

It does the same thing, but registers the package correctly within the project's renv environment (isolated from other R projets).

**`renv::snapshot()` — what it does:**

Updates the `renv.lock` file with the versions of the packages currently installed in the project. It essentially takes a "snapshot" of the current state of the libraries and saves it, so that anyone who runs `renv::restore()` afterwards gets exactly those versions.

**`renv::status()` — useful for checking:**

Tells whether there are packages installed but not yet captured in `renv.lock`, or conversely packages in the lockfile that are not installed. Handy to run occasionally to check whether in sync.

```r
renv::status()
```

For everything else (how renv works internally, library management, use with Docker, etc.), the official guide is here: [https://rstudio.github.io/renv/articles/renv.html](https://rstudio.github.io/renv/articles/renv.html)

# Description

## What this code does

The pipeline takes the raw **ENIGH 2022** micro-data published by INEGI and produces,
for Mexican **agricultural households**, a description of where their income comes
from, how agricultural support policies are distributed across the income
distribution, and how unequal that distribution is.

Everything is estimated on the **complex survey design** (stratified, one-stage
cluster sampling with unequal weights) using the `survey` / `srvyr` packages, so every
estimate comes with a design-based confidence interval — never a naive one.

The unit of analysis is the **household**, ranked into **deciles of equivalised
current income** for the whole population (square-root equivalence scale). All
agricultural results are then read against those population-wide deciles, which is
what makes statements such as "the poorest decile of the overall population" possible.

## How to run it

From the project root, with the `renv` environment active:

```r
source("script/main_script.r")
```

or from a terminal:

```bash
Rscript script/main_script.r
```

A full run takes about **35 minutes** and writes ~320 figures and ~200 tables. The raw
INEGI data must be in `src/` first (see *Data source* above).

Useful options:

```r
options(enigh.show_plots = TRUE)   # also render every figure on screen (slower)
options(enigh.fig_device = "png")  # write PNG instead of vector PDF
```

## Pipeline structure

The scripts are numbered in execution order and sourced by `main_script.r`.

| script | what it does |
|---|---|
| `0_utils.r` | All the functions: survey estimators, significance tests, plotting, saving. Nothing runs here. |
| `1A_data_prep.r` | Reads the raw INEGI tables (`AGRO`, `AGROPRODUCTOS`, `AGROCONSUMO`, `NOAGRO`, `CONCENTRADOHOGAR`, `POBLACION`…), aggregates them to household level and builds the project's own variables, all prefixed `n_`. |
| `1B_data_svyr.r` | Builds the survey design, handles negative incomes, classifies households as agricultural or not, resolves edge cases, computes the income deciles, and updates `dict_new_variables.csv`. |
| `2A_stat_basic.r` | Decile cut-off points and basic descriptive tables. |
| `2B_stat_ineq.r` | Inequality: Gini coefficients, Lorenz curves, farm size and ethnicity distributions, food-access indicators. |
| `2C_stat_comp_analysis.r` | **Composition analysis** — what a total is made of, decile by decile (100 % stacked bars). |
| `2D_stat_share_analysis.r` | **Share analysis** — how an aggregate is distributed across deciles. |
| `2E_stat_ratio_analysis.r` | **Ratio analysis** — how large one variable is relative to another. |
| `main_script.r` | Loads packages, sources everything in order, and builds the colour dictionary that keeps a variable the same colour across all figures. |
| `test_estimators.r` | Regression test for the estimators. Not part of the pipeline; run it separately. |

## The three estimators, and how to choose between them

The same question — *"how big is X relative to Y in this decile?"* — has three
different answers, and the project computes all three because they say different
things. This is the single most important thing to understand when reading the
figures.

**Macro** — *ratio of the aggregates*

> `T(X | decile) / T(Y | decile)`

"Out of every peso of Y received by the decile **as a whole**, how much comes from X?"
Households contribute in proportion to their size, so one very large farm can drive
the whole decile. This is the only estimator that is **exactly additive**: the
components of a total sum to 100 %, which is why the composition figures use it.

**Micro** — *mean of the individual ratios*

> `mean over households of (X_i / Y_i)`

"What does the **average household** look like?" Every household counts for one,
whatever its size. Beware: households with `Y_i = 0` drop out of this computation
entirely, so the estimate is conditional on `Y_i ≠ 0`; the `n_obs` column tells you
how many households actually contributed.

**Median** — *median of the individual ratios*

> `weighted median over households of (X_i / Y_i)`

"What does the **median household** look like?" Far more readable than the micro mean,
because `X_i / Y_i` is unbounded and heavy-tailed and its mean is driven by a handful
of households. Households with a null or negative denominator are excluded by default.
Its confidence interval is a **Woodruff interval** — asymmetric by construction, and
obtained by inverting an interval for the share of households below the median.

Medians are **not additive**: the medians of the components of a total do not sum to
the median of the total, and generally not to 100 %. The median composition figures
therefore draw the components side by side, never stacked, and print the real sum in
the caption.

A large gap between the macro and the micro/median values is itself a result: it
signals strong heterogeneity in farm size and a correlation between scale and the
ratio being measured.

## Significance testing

Each ratio/share analysis produces **two** figures:

- `plot_<name>.pdf` — the estimates by decile, with their confidence intervals and the
  overall reference line;
- `plot_signif_<name>.pdf` — a dot-and-whisker plot of the **difference to that
  reference**, with a zero line.

The second one exists because **comparing confidence intervals by eye is not a test**.
It is conservative when the two estimates are independent, and it can be badly wrong
when they are correlated — which is the case here, since shares sum to 1 and are
therefore strongly negatively correlated. The difference is estimated directly, with
its own variance:

> `Var(θ_g − θ_ref) = Var(θ_g) + Var(θ_ref) − 2·Cov(θ_g, θ_ref)`

using the full joint covariance matrix of the decile estimates. Reading rule:
**significant when the interval does not cross zero**. The caption also reports how
many deciles survive a Holm correction for the ten comparisons.

The subtitle of that figure gives a **design-based linear trend** across the deciles —
the slope of the gap against decile rank, with its p-value. That is the progressivity
question in one number: a positive slope means the gap widens with income (regressive),
a negative one that it narrows (progressive).

## Output

Everything lands in `output/`, which is git-ignored.

| folder | content |
|---|---|
| `output/data/` | The prepared household-level database (`.rds` and `.csv`). |
| `output/diagnostics/` | Edge-case counts and consistency checks from `1B`. |
| `output/processed/` | One `.csv` per analysis: estimates, standard errors, confidence intervals, `n_obs`, and the test columns. |
| `output/fig/` | One vector `.pdf` per figure. |

Every result table carries, besides the estimate and its interval:

- `n_obs` — unweighted number of households that actually contributed to the decile
  (compare it with the decile size before interpreting);
- `n_pop`, `p_zero` — for medians: weighted population count, and weighted share of
  households whose numerator is exactly zero (if `p_zero > 0.5`, the median *is* zero,
  and that is the correct answer);
- `ref`, `diff`, `diff_SE`, `diff_IC_low`, `diff_IC_high`, `p_value`, `p_value_adj`,
  `signif`, `signif_adj` — the test against the reference;
- `trend_slope`, `trend_SE`, `trend_p` — the linear trend across deciles.

## Testing the estimators

```bash
Rscript script/test_estimators.r
```

Runs 38 checks on a synthetic design with the same structure as ENIGH 2022 and the same
awkward cases (exact zeros, zero denominators, missing values, households outside any
decile). It verifies the estimators against a reference implementation and the
inferential code against `survey::svycontrast()` and `survey::svyquantile()`. Exit
status 0 when everything passes. Run it after touching anything in `0_utils.r`.

## Methodological choices worth knowing

Documented in full in `report.md`. Three points that affect interpretation.

**Negative self-employment income is bottom-coded at 0** before the deciles are
computed, which is the standard treatment (LIS, OECD, Eurostat). Negative values
otherwise break the Gini, the Lorenz curve and any log transform. The floor is applied
to the *component* (farm and non-farm self-employment income), not to total income, so
a household with 10 000 of wages and a 2 000 farm loss is credited with 10 000 — this
is required for `percent_farm`, which classifies households as agricultural.

Two alternative treatments are available for sensitivity analysis:

```r
options(enigh.negative_income = "draw")  # random draw from the bottom quartile of positives
options(enigh.negative_income = "keep")  # no treatment (will break the Gini)
```

The `"draw"` option was the project's original treatment. It is kept only for
comparison: it ignores the household's own scale and produced 498 households (4.3 %)
whose farm net income exceeded the farm's total resources. Running the pipeline under
both settings and reporting how far the headline numbers move is a recommended check.

**Support amounts are on two different time bases.** The `nvo_cant*` variables
(nuevos programas sociales) are already 12-month totals — the questionnaire asks about
the period "between [month] of last year and [month] of this year" — and are used as
they are. The `apoyo_*` variables come from a different question and are reported
monthly, so they are multiplied by 12 (`apoyo_to_year` in `1A_data_prep.r`). Getting
this wrong in either direction moves support by a factor of twelve; `report.md` II.16
gives the evidence for each.

**Micro estimators exclude households whose denominator is zero.** They are therefore
conditional on a positive denominator. This is mild for production ratios (5–9 % of the
universe) but decisive for the support ratios, where 86 % of agricultural households
receive nothing from new-policy programmes: there, the micro mean and the median
describe *recipients*. Always read `n_obs` alongside the estimate.

**Composition of the median household — two different objects.** The median of each
component's share (`get_share_median_overall`) is taken component by component, on
different orderings of the households, so the shares do not sum to 100 % — it is
nobody's actual budget. The composition of the households *around* the median
(`get_share_median_band`, P45–P55 by default) describes real households and does sum to
100 %. Both are produced; pick according to the sentence you want to write.

# Release on Zenodo

L'obiettivo è pubblicare una versione stabile del codice su Zenodo, che genera un DOI citabile. La versione pubblicata sarà pubblica e priva dei commenti di lavoro interni (#TODO, #WARN, #NOTE, #INFO) — che restano invece visibili su main per il nostro uso quotidiano.

Il processo usa un branch temporaneo `zenodo` che esiste solo il tempo della release, poi viene eliminato. Il branch main non viene mai toccato.

`clean_comments.py` fa due cose:

1. rimuove dai file `.R` i blocchi `#TODO` / `#NOTE` / `#INFO` / `#WARN`;
2. **tronca `README.md`** appena prima di questa sezione (`# Release on Zenodo`,
   inclusa). Tutto quello che segue è interno al progetto — questa procedura di
   release e il workflow GitHub in italiano — e non va pubblicato. Le sezioni
   precedenti (*Data source*, *Working with renv*, *Description*) restano, e sono
   quelle che descrivono il codice a chi lo scarica da Zenodo.

Il README quindi **non va più cancellato a mano**: ci pensa lo script.

Prima di lanciarlo, conviene verificare cosa verrebbe rimosso:

```bash
python clean_comments.py --dry-run          # non modifica niente
python clean_comments.py --dry-run --log    # scrive anche clean_comments_log.md
```

```bash
git checkout -b zenodo
rm notes_and_questions.md report.md   # e anche docs/ e gli script z_ in script/
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

