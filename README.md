# biomarkerqc

<!-- badges: start -->
[![R-CMD-check](https://github.com/elitay152/biomarkerqc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elitay152/biomarkerqc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Quality control for clinical biomarker lab files, written as an R package.

> **Status:** in development. All data in this repository is synthetic.

## What it does

Labs often receive the same kinds of results from more than one vendor, each
with its own column names, units and quirks. `biomarkerqc` turns those files
into one clean, auditable dataset:

| Stage | What happens |
|---|---|
| Ingest | Read each vendor's file and map it to one standard schema |
| Normalize | Convert every result to its approved unit, and reject unapproved units |
| Reconcile | Match records that appear in more than one source and compare their values |
| Flag | Mark duplicates, missing values and out-of-range results |
| Report | Produce a QC report that traces every issue back to its source file and row |

## Roadmap

- [x] Package skeleton
- [x] Synthetic vendor files (`data-raw/make_vendor_files.R` → `inst/extdata/`)
- [x] Ingest and standard schema
- [x] Unit normalization
- [x] Duplicate, missing-value and plausibility flags
- [x] Cross-vendor reconciliation
- [ ] Traceable QC report
- [x] Continuous integration (R CMD check on GitHub Actions)
