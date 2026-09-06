# SGMT 431: Lectures 4–6

Open [index.html](index.html) to access all three lectures. Each includes seven tutorials, a matching Reveal.js slide deck, local sports datasets, and editable Quarto and R sources in the format of Lectures 1–3.

| Lecture | Tutorials | Slides | Guided R lab |
|---|---|---|---|
| 4: Player Performance Evaluation | [Open](lecture%204/_site/index.html) | [87 slides](lecture%204/slides/_site/lecture-4-slides.html) | [R script](lecture%204/R/guided_lab.R) |
| 5: Team Performance Evaluation | [Open](lecture%205/_site/index.html) | [70 slides](lecture%205/slides/_site/lecture-5-slides.html) | [R script](lecture%205/R/guided_lab.R) |
| 6: Clustering Methods for Sport | [Open](lecture%206/_site/index.html) | [75 slides](lecture%206/slides/_site/lecture-6-slides.html) | [R script](lecture%206/R/guided_lab.R) |

See [TEACHING-GUIDE.md](TEACHING-GUIDE.md) for prerequisites, a core classroom route, optional extensions, and a grading rubric. The complete decks support multiple class meetings per unit.

## Contents

**Player performance:** noisy averages, partial pooling, varying intercepts and slopes, crossed effects, conjugate models, empirical Bayes versus full Bayes, posterior computation, ridge priors, RAPM, and player comparisons with uncertainty.

**Team performance:** schedule-adjusted margins, the ridge-prior connection, Bayesian strength, Bradley–Terry, ordered outcomes, Poisson attack and defense models, Elo, state-space models, and chronological forecast evaluation.

**Clustering:** feature engineering and distances, k-means and PAM, hierarchical methods, GMM and EM, DBSCAN/HDBSCAN, spectral clustering, Gower distance, NMF, fuzzy c-means, compositional shot profiles, validation, and consensus under measurement uncertainty.

## Opening and editing

The rendered HTML files are ready to read and present. Keep the supporting folders with them. Equations use browser-native MathML. The materials use American English and explicit mathematical definitions.

Edit the tutorial `.qmd` files for explanations, equations, code, and exercises. Edit `slides/lecture-N-slides.qmd` for the slides. Styling is in `styles.css` and `slides/slides.scss`. Each `data/README.md` records data sources and filters.

## Running R

Run `install-packages.R` once if dependencies are missing. It installs only missing tutorial packages. Viewing the rendered HTML requires no R installation.

From this folder, in R:

```r
source("install-packages.R")
setwd("lecture 4")
source("R/guided_lab.R")
```

Set the working directory to the relevant lecture before running its examples. Each tutorial loads its own data. Scripts run from the lecture root, not from inside `R/`. The guided scripts were extracted from Tutorials 4.7, 5.7, and 6.7. After editing a tutorial, regenerate its script with `knitr::purl()`.

## Rendering

Run `./render-all.ps1` in PowerShell to rebuild all tutorials and decks. The script accepts `-RBin` if your R installation differs from the local default. Alternatively, from a lecture folder:

```powershell
quarto render
Set-Location slides
quarto render
```

If Quarto cannot locate R on Windows, set `QUARTO_R` to the R `bin` directory. The current build uses R 4.3.3. Each lecture's `R/session-info.txt` records package versions used for the new guided lab.

## Bayesian computation and stored results

The new player and team labs fit complete conjugate hierarchies in R without Stan, using four chains and modern diagnostics. The inverse-gamma variance priors are explicit teaching assumptions. The player lab includes prior sensitivity.

More elaborate logistic, Poisson, and dynamic examples use stored posterior results in `data/`. Rendering these examples reads the existing results rather than refitting those models. Rebuilding them requires the packages listed in `R/build_*.R` and, where indicated, CmdStan and a compiler. The stored fits came with the local lecture drafts; their preparation and fitting scripts accompany them.

## Sources

The player and team sequence is informed by [Ron Yurko's course overview](https://statthinksportsanalytics.substack.com/p/statistical-thinking-in-sports-analytics). Technical references include [lme4](https://lme4.github.io/lme4/), [Bayes Rules!](https://www.bayesrulesbook.com/), [Stan](https://mc-stan.org/), [mclust](https://mclust-org.github.io/mclust/), and the R package manuals. Clustering topics extend beyond the overview. NFL, WNBA, NBA, and Premier League data provenance appears in each lecture's data folder.
