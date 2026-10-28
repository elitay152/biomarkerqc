# biomarkerqc

<!-- badges: start -->
[![R-CMD-check](https://github.com/elitay152/biomarkerqc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elitay152/biomarkerqc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Quality control for clinical biomarker lab files, written as an R package.

> All data in this repository is synthetic.

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

## Installation

```r
# install.packages("remotes")
remotes::install_github("elitay152/biomarkerqc")
```

## Usage

Run the whole pipeline on the example files shipped with the package:

```r
library(biomarkerqc)

run <- run_qc(c(
  A = biomarkerqc_example("vendor_a_results.csv"),
  B = biomarkerqc_example("vendor_b_results.csv")
))
run
#> ── biomarkerqc run ──────────────────────────────────────
#> 2 files, 3608 rows
#> ✖ 12 error findings
#> ! 15 warning findings
#> ℹ 3 info findings
#> Reconciliation: 596 of 600 shared samples agree, 4 need review

qc_report(run, "qc_report.html")
```

Every finding records the source file and line it came from, and the report
lists each input file's MD5 checksum, so any issue can be traced back to the
exact delivery that caused it.

Each stage can also be run on its own:

| Function | Stage |
|---|---|
| `ingest_lab_files()` | Read vendor files into the standard schema (`standard_columns`) |
| `normalize_units()` | Convert results to approved units (`unit_rules()`, `unit_synonyms()`) |
| `flag_issues()` | Run the QC checks (`qc_checks()`, `plausibility_limits()`) |
| `reconcile_vendors()` | Compare results that two vendors reported for the same samples |
| `run_qc()`, `qc_report()` | Run everything and write the HTML report |

### CDISC SDTM lab data

The built-in `SDTM` spec reads the SDTM laboratory (LB) domain, the standard
format for clinical trial lab data. Pass a data frame, for example one read
with `haven::read_xpt()`:

```r
run <- run_qc(list(SDTM = pharmaversesdtm::lb))   # CDISC pilot study data
run
#> 1 file, 59580 rows
#> 42 tests without unit rules were not value-checked (52304 rows)
#> ℹ 1 info finding
```

On the CDISC pilot data, results normalized by `biomarkerqc` agree with the
sponsor's own SI-standardized results (`LBSTRESN`) to within 0.02% across
more than 7,000 glucose, creatinine, cholesterol and potassium results.

### Your own formats

Vendor formats, unit rules and limits are all data, so supporting a new vendor
or approving a new unit means editing a table, not the code. Describe a new
format with `vendor_spec()`:

```r
my_lab <- vendor_spec(
  "C",
  columns = c(subject_id = "PatientID", visit = "Visit", collection_date = "Drawn",
              test_code = "Test", result_raw = "Value", unit_raw = "Units"),
  tests = c(Glucose = "GLUC", Potassium = "K"),
  date_format = c("%Y-%m-%d", "%m/%d/%Y"),
  delim = "\t"
)
run <- run_qc(c(C = "c.tsv"), specs = c(vendor_specs(), list(C = my_lab)))
```

Visit and test mappings are optional (values are kept as delivered), and
`ingest_lab_files()` and `run_qc()` accept data frames as well as file paths.

## Roadmap

- [x] Package skeleton
- [x] Synthetic vendor files (`data-raw/make_vendor_files.R` → `inst/extdata/`)
- [x] Ingest and standard schema
- [x] Unit normalization
- [x] Duplicate, missing-value and plausibility flags
- [x] Cross-vendor reconciliation
- [x] Traceable QC report
- [x] CDISC SDTM LB support, custom vendor specs and data frame input
- [x] Continuous integration (R CMD check on GitHub Actions)
