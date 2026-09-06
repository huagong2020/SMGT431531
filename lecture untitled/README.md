# SGMT 431: three new lectures

This folder holds three lectures built in the same format as Lectures 1 to 3:
each is a self-contained Quarto website of tutorials plus a reveal.js slide
deck. The numbering (4, 5, 6) is provisional; renumbering a lecture means
renaming its folder, the tutorial files, the headings inside the slide deck,
and the `lecture_number` in `slides/section-footer.lua`.

| Folder | Lecture | Data | Precomputed fits |
|---|---|---|---|
| `lecture 4` | Player Performance Evaluation: multilevel models, Bayesian inference, and the connection between them | NFL 2023 pass attempts (nflreadr); WNBA 2024 shooting (wehoop) | `lme4` bootstrap and a CmdStan multilevel logistic model |
| `lecture 5` | Team Performance Evaluation: regression, Bradley-Terry, Poisson, Elo, and state-space ratings | NFL 2022 to 2024 games with betting lines (nflreadr); Premier League 2021-22 to 2023-24 (football-data.co.uk) | `rstanarm` Poisson model and a CmdStan state-space model |
| `lecture 6` | Clustering Methods for Sport: k-means, hierarchical, Gaussian mixtures, density, spectral, and NMF | NBA 2023-24 player profiles and shot locations (hoopR) | none |

## Rendering

From inside a lecture folder:

```powershell
quarto render            # tutorials -> _site/index.html
cd slides
quarto render            # slides -> _site/lecture-N-slides.html
```

Each lecture's `README.md` lists the required R packages. All data and slow
model fits are stored under `data/`, so rendering works offline. The scripts
under `R/` rebuild the data (`prepare_*.R`) and the fits (`build_*.R`), and
`slides/build_slide_images.R` regenerates the figures used in each deck.

## Where the design came from

The lectures follow the course outline in Ron Yurko's "Statistical Thinking in
Sports Analytics" post: player and team evaluation through multilevel models,
empirical Bayes and full Bayes, Metropolis-Hastings coded from scratch, Stan,
regularized adjusted plus-minus, Bradley-Terry, Elo, and the Glickman-Stern
state-space model. The clustering lecture adds the validity, stability, and
dissimilarity-design emphasis of his "Careful Clustering of Soccer Players"
post to the classical methods.
