# biomarkerqc

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
- [ ] Synthetic vendor files
- [ ] Ingest and standard schema
- [ ] Unit normalization
- [ ] Duplicate and missing-value flags
- [ ] Cross-vendor reconciliation
- [ ] Traceable QC report
- [ ] Continuous integration (R CMD check on GitHub Actions)
