# Data source 

Go to [https://www.inegi.org.mx/datosabiertos/](https://www.inegi.org.mx/datosabiertos/), Encuestas/regulares/Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH)/2022.

Put the data in the /src/ folder and unzip it. The scripts will go and target the necessary csv.

# Working with renv

The project uses `renv` to lock the versions of R and its packages, so that the code runs the same way on every computer.

**Versions used in this project:**
- R: `4.5.3`
- renv: `1.2.2`

⚠️ Significantly different version of R (e.g. 4.3.x) may cause that some packages not install correctly.

## Installation (first time only)

After cloning the project, (optionnaly) created a `.Rproj` file (if you work with RStudio), run the following in the R console:

```r
install.packages("renv")
renv::restore()
```

`renv::restore()` reads the `renv.lock` file (included in the repository) and installs **exactly** the same package versions that were used in the repo. This may take a few minutes the first time.

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
