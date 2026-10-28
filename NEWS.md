# biomarkerqc (development version)

# biomarkerqc 0.1.0

First release.

* `ingest_lab_files()`, `read_vendor_file()` and `read_vendor_data()` read
  vendor files or data frames into one standard schema.
* `vendor_spec()` defines new vendor formats; built-in specs cover two example
  vendors and the CDISC SDTM LB domain.
* `normalize_units()` converts results to approved units.
* `flag_issues()` runs 10 QC checks; `reconcile_vendors()` compares vendors.
* `run_qc()` runs the pipeline and `qc_report()` writes a traceable HTML report.