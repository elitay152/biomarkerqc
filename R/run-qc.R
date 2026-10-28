#' Run the full QC pipeline
#'
#' Ingests vendor files, normalizes units, flags issues and reconciles vendors
#' in one call. The result keeps every setting that was used alongside the
#' outputs, so a report built from it always describes the run that produced
#' it.
#'
#' @inheritParams ingest_lab_files
#' @param rules Unit conversion rules. Defaults to [unit_rules()].
#' @param synonyms Unit spelling synonyms. Defaults to [unit_synonyms()].
#' @param limits Plausibility limits. Defaults to [plausibility_limits()].
#' @param tolerance Reconciliation tolerance, passed to [reconcile_vendors()].
#'
#' @return A `biomarkerqc_run` object: a list with
#'   * `labs`: normalized lab results ([normalize_units()]).
#'   * `findings`: QC findings ([flag_issues()]).
#'   * `reconciliation`: cross-vendor comparison ([reconcile_vendors()]), or
#'     `NULL` when fewer than two vendors were ingested.
#'   * `ingest_log`: one row per file, with its MD5 checksum.
#'   * `settings`: the rules, synonyms, limits, checks and tolerance used.
#'   * `run_info`: package version, R version and run time.
#' @export
#' @examples
#' run <- run_qc(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' ))
#' run
#' head(run$findings)
run_qc <- function(files,
                   specs = vendor_specs(),
                   rules = unit_rules(),
                   synonyms = unit_synonyms(),
                   limits = plausibility_limits(),
                   tolerance = 0.05) {
  labs <- ingest_lab_files(files, specs)
  ingest_log <- attr(labs, "ingest_log")
  labs <- normalize_units(labs, rules, synonyms)

  reconciliation <- if (length(unique(labs$vendor)) >= 2) {
    reconcile_vendors(labs, tolerance = tolerance)
  }

  structure(
    list(
      labs = labs,
      findings = flag_issues(labs, limits),
      reconciliation = reconciliation,
      ingest_log = ingest_log,
      settings = list(
        rules = rules, synonyms = synonyms, limits = limits,
        checks = qc_checks(), tolerance = tolerance
      ),
      run_info = list(
        package_version = as.character(utils::packageVersion("biomarkerqc")),
        r_version = R.version.string,
        run_at = Sys.time()
      )
    ),
    class = "biomarkerqc_run"
  )
}

#' @export
print.biomarkerqc_run <- function(x, ...) {
  severity <- table(factor(x$findings$severity, levels = c("error", "warning", "info")))
  cli::cli_h1("biomarkerqc run")
  cli::cli_text("{nrow(x$ingest_log)} file{?s}, {nrow(x$labs)} rows")
  cli::cli_bullets(c(
    "x" = "{severity[['error']]} error finding{?s}",
    "!" = "{severity[['warning']]} warning finding{?s}",
    "i" = "{severity[['info']]} info finding{?s}"
  ))
  if (!is.null(x$reconciliation)) {
    rec <- x$reconciliation
    cli::cli_text(
      "Reconciliation: {sum(rec$status == 'agree')} of {nrow(rec)} shared sample{?s} agree, ",
      "{sum(rec$status != 'agree')} need review"
    )
  }
  invisible(x)
}

#' Write an HTML QC report
#'
#' Renders a self-contained HTML report from a [run_qc()] result: input files
#' and checksums, a summary by severity, every finding and every
#' reconciliation problem with its source file and line, and the settings
#' that were used.
#'
#' Requires the rmarkdown and knitr packages and pandoc.
#'
#' @param run A `biomarkerqc_run` from [run_qc()].
#' @param output_file Path of the HTML file to write.
#' @param title Report title.
#'
#' @return The path to the report, invisibly.
#'
#' @section Errors:
#' * `biomarkerqc_error_invalid_argument`: `run` is not a `biomarkerqc_run`,
#'   or the folder for `output_file` does not exist.
#' * `biomarkerqc_error_missing_dependency`: rmarkdown, knitr or pandoc is
#'   not available.
#'
#' @export
#' @examplesIf rlang::is_installed(c("rmarkdown", "knitr")) && rmarkdown::pandoc_available()
#' run <- run_qc(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' ))
#' path <- qc_report(run, tempfile(fileext = ".html"))
qc_report <- function(run, output_file = "qc_report.html", title = "Lab data QC report") {
  if (!inherits(run, "biomarkerqc_run")) {
    cli::cli_abort(
      "{.arg run} must be the result of {.fn run_qc}.",
      class = c("biomarkerqc_error_invalid_argument", "biomarkerqc_error")
    )
  }
  missing_pkgs <- c("rmarkdown", "knitr")[!vapply(c("rmarkdown", "knitr"), requireNamespace,
                                                  logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0 || !rmarkdown::pandoc_available()) {
    cli::cli_abort(
      c(
        "Writing the report needs rmarkdown, knitr and pandoc.",
        "i" = if (length(missing_pkgs) > 0) "Install {.pkg {missing_pkgs}}." else "Install pandoc, or use RStudio, which includes it."
      ),
      class = c("biomarkerqc_error_missing_dependency", "biomarkerqc_error")
    )
  }

  # Render from a temporary copy: the installed package folder may be read-only
  work_dir <- tempfile("biomarkerqc-report-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  template <- file.path(work_dir, "qc_report.Rmd")
  file.copy(system.file("rmarkdown", "qc_report.Rmd", package = "biomarkerqc"), template)

  # Resolve the folder, which exists, rather than the file, which does not
  # yet: only existing paths get their canonical spelling on every OS
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    cli::cli_abort(
      "Folder {.file {output_dir}} does not exist.",
      class = c("biomarkerqc_error_invalid_argument", "biomarkerqc_error")
    )
  }
  output_file <- file.path(normalizePath(output_dir), basename(output_file))
  rmarkdown::render(
    template,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    intermediates_dir = work_dir,
    params = list(run = run, title = title),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )
  invisible(output_file)
}
