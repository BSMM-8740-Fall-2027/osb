# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this project is

Course website for **BSMM-8740: Data Analytic Methods & Algorithms** at the University of Windsor (Fall 2027), published at <https://bsmm-8740-fall-2027.github.io/osb/>. Built with **Quarto** as a website project, rendered to `_site/`, and deployed to GitHub Pages via the `gh-pages` branch.

## Commands

```bash
# Render the entire site locally
quarto render

# Preview with live reload
quarto preview

# Render a single file (fast, avoids full site rebuild)
quarto render slides/BSMM_8740_lec_03.qmd
quarto render hw/hw-3.qmd

# Render from R (matches CI exactly)
R -e 'quarto::quarto_render(".", as_job = FALSE)'
```

`freeze: auto` is set in `_quarto.yml`, so unchanged `.qmd` files use cached outputs from `_freeze/`. Delete the relevant `_freeze/` subfolder to force a re-render of a specific file.

## Architecture

| Directory | Purpose |
|---|---|
| `slides/` | RevealJS lecture slides (`BSMM_8740_lec_NN.qmd`). Each imports shared R utilities from `slides/R/setup.R`. |
| `weeks/` | Weekly landing pages (`BSMM_8740_week_NN.qmd`) that link out to slides, labs, AEs. |
| `labs/` | Student lab documents (`BSMM_8740_lab_N.qmd`); solutions live in `labs/solutions/`. |
| `hw/` | Homework assignments. |
| `ae/` | Application exercise (in-class) documents. |
| `supplemental/` | Reference notes linked from the sidebar (bias-variance, PCA, collinearity, causal inference, etc.). `supplemental/R/` holds standalone R scripts backing those notes. |
| `exams/` | Exam and quiz solution files. |
| `documents/` | Course PDFs (syllabus, rubrics, reference papers). |
| `misc/` | Scratch/draft `.qmd` and `.R` files — not rendered as part of the site. |
| `scratch/` | Local scratch files not rendered as part of the site. |

**Navigation** is fully defined in `_quarto.yml` sidebar. Adding a new page requires an entry there to appear in the site nav.

**Theming**: `theme.scss` (light) and `theme-dark.scss` (dark) extend the Cosmo Quarto theme. Brand colours are `#D9E3E4` (background) and `#5B888C` (headings). Slides use their own `slides/slides.scss`.

**Extensions**: custom Quarto extensions are in `_extensions/` (RevealJS chalkboard, etc.).

**Freeze cache**: `_freeze/` is committed to the repo so CI can build without re-executing all R chunks. When modifying code chunks in a file, delete or update the corresponding `_freeze/` entry so CI picks up the change.

**CI/CD**: `.github/workflows/build-website.yaml` renders the site with `quarto::quarto_render(".")` on every push to `main` and deploys `_site/` to the `gh-pages` branch via the JamesIves deploy action.

## Slide format

Slides are RevealJS with `multiplex: true`, `chalkboard: true`, and `freeze: auto`. The first chunk is always a setup block:

```r
```{r setup}
#| include: false
library(countdown)
knitr::opts_chunk$set(fig.width = 6, fig.asp = 0.618, fig.align = "center", out.width = "90%")
```
```

## R environment

Dependencies are managed with **renv** (lockfile at `renv.lock`). The `.Rprofile` has `renv/activate.R` commented out — renv is restored by CI via `r-lib/actions/setup-renv@v2`. Locally, run `renv::restore()` to sync packages. A full package list is also in `packages.csv`.
