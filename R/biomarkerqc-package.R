#' biomarkerqc: Quality Control for Clinical Biomarker Lab Files
#'
#' The package runs a lab-data QC pipeline in five stages:
#'
#' 1. **Ingest** vendor files into one standard schema.
#' 2. **Normalize** every result to its approved unit.
#' 3. **Reconcile** records that appear in more than one source.
#' 4. **Flag** duplicates, missing values and out-of-range results.
#' 5. **Report** the findings with full traceability back to the source rows.
#'
#' @keywords internal
#' @importFrom rlang abort
"_PACKAGE"
# rlang is imported because cli_abort() and cli_warn() need it at runtime.
