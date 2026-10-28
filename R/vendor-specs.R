# A new vendor needs a new spec, not a new reader.

#' Standard schema produced by ingest
#'
#' Every ingested file or data frame is returned with exactly these columns,
#' in this order.
#'
#' * `vendor`: vendor name from the spec (for example `"A"`).
#' * `subject_id`: standard subject ID, or `NA` if the source value was blank
#'   or could not be mapped.
#' * `visit`, `test_code`: standard codes from the spec's mappings, or the
#'   values as delivered when the spec has no mapping.
#' * `collection_date`: sample collection date (`Date`).
#' * `result_raw`: the result exactly as delivered (character).
#' * `result`: the result parsed as a number, or `NA` if blank or not numeric.
#' * `qualifier`: `"<"` or `">"` for censored results such as `"<0.20"`,
#'   otherwise `NA`.
#' * `unit_raw`: the unit exactly as delivered. Units are normalized in a
#'   later step, not during ingest.
#' * `source_file`: base name of the file the row came from, or a label for a
#'   data frame.
#' * `source_line`: line number in that file (the header is line 1), or the
#'   row number in a data frame, so any row can be traced back to its source.
#'
#' @format A character vector of column names.
#' @export
standard_columns <- c(
  "vendor", "subject_id", "visit", "test_code", "collection_date",
  "result_raw", "result", "qualifier", "unit_raw",
  "source_file", "source_line"
)

#' Define a vendor format
#'
#' Describes how a vendor's lab results map onto the standard schema, so they
#' can be ingested without writing a new reader. The spec is validated when it
#' is created.
#'
#' @param name Vendor name, recorded in the `vendor` column.
#' @param columns Named character vector mapping each standard field to the
#'   vendor's column name. Needs `subject_id`, `visit`, `collection_date`,
#'   `test_code`, `result_raw` and `unit_raw`.
#' @param subject_id Function converting the vendor's subject IDs to standard
#'   IDs, returning `NA` for values it cannot convert. Defaults to using the
#'   IDs as delivered.
#' @param visits,tests Named character vectors mapping vendor values (names)
#'   to standard codes (values). `NULL`, the default, keeps the values as
#'   delivered.
#' @param date_format One or more [strptime()] formats, tried in order until
#'   one parses. Trailing characters are ignored, so `"%Y-%m-%d"` also reads
#'   ISO 8601 date-times.
#' @param delim,decimal_mark How the file is delimited and how decimals are
#'   written. Only used when reading files.
#'
#' @return A `biomarkerqc_vendor_spec` object.
#'
#' @section Errors:
#' `biomarkerqc_error_invalid_spec`: an argument is missing or malformed.
#'
#' @export
#' @examples
#' # A lab that sends a tab-separated file with its own column names
#' my_lab <- vendor_spec(
#'   "C",
#'   columns = c(
#'     subject_id = "PatientID", visit = "Visit", collection_date = "Drawn",
#'     test_code = "Test", result_raw = "Value", unit_raw = "Units"
#'   ),
#'   tests = c(Glucose = "GLUC", Potassium = "K"),
#'   delim = "\t"
#' )
#' my_lab
#'
#' # Use it alongside the built-in specs:
#' # run_qc(c(C = "c.tsv"), specs = c(vendor_specs(), list(C = my_lab)))
vendor_spec <- function(name,
                        columns,
                        subject_id = NULL,
                        visits = NULL,
                        tests = NULL,
                        date_format = "%Y-%m-%d",
                        delim = ",",
                        decimal_mark = ".") {
  bad <- function(msg) {
    cli::cli_abort(
      paste("Invalid vendor spec:", msg),
      class = c("biomarkerqc_error_invalid_spec", "biomarkerqc_error"),
      call = rlang::caller_env()
    )
  }
  is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  is_mapping <- function(x) {
    is.null(x) || (is.character(x) && !is.null(names(x)) && all(nzchar(names(x))))
  }

  if (missing(name) || !is_string(name)) bad("{.arg name} must be a single non-empty string.")
  needed <- setdiff(standard_columns, c("vendor", "result", "qualifier", "source_file", "source_line"))
  if (missing(columns) || !is.character(columns) || is.null(names(columns))) {
    bad("{.arg columns} must be a named character vector.")
  }
  missing_fields <- setdiff(needed, names(columns))
  if (length(missing_fields) > 0) {
    bad("{.arg columns} has no entry for {.val {missing_fields}}.")
  }
  if (!is.null(subject_id) && !is.function(subject_id)) bad("{.arg subject_id} must be a function.")
  if (!is_mapping(visits)) bad("{.arg visits} must be NULL or a named character vector.")
  if (!is_mapping(tests)) bad("{.arg tests} must be NULL or a named character vector.")
  if (!is.character(date_format) || length(date_format) == 0) {
    bad("{.arg date_format} must be one or more format strings.")
  }
  if (!is_string(delim) || nchar(delim) != 1) bad("{.arg delim} must be a single character.")
  if (!decimal_mark %in% c(".", ",")) bad('{.arg decimal_mark} must be "." or ",".')

  structure(
    list(
      name = name,
      columns = columns[needed],
      subject_id = subject_id %||% function(x) x,
      visits = visits,
      tests = tests,
      date_format = date_format,
      delim = delim,
      decimal_mark = decimal_mark
    ),
    class = "biomarkerqc_vendor_spec"
  )
}

#' @export
print.biomarkerqc_vendor_spec <- function(x, ...) {
  mapping <- function(m) if (is.null(m)) "kept as delivered" else paste(length(m), "mapped")
  cli::cli_text("Vendor spec {.val {x$name}}")
  cli::cli_bullets(c(
    "*" = "Columns: {paste(names(x$columns), '=', x$columns, collapse = ', ')}",
    "*" = "Visits: {mapping(x$visits)}; tests: {mapping(x$tests)}",
    "*" = "Dates: {.val {x$date_format}}",
    "*" = "Files: delimiter {.val {x$delim}}, decimal mark {.val {x$decimal_mark}}"
  ))
  invisible(x)
}

#' Built-in vendor specifications
#'
#' Returns the specs shipped with the package:
#'
#' * `A`, `B`: the two synthetic example vendors (see [biomarkerqc_example()]).
#' * `SDTM`: the CDISC SDTM laboratory (LB) domain, the standard format for
#'   clinical trial lab data. It reads original results (`LBORRES`,
#'   `LBORRESU`) and keeps `VISIT` and `LBTESTCD` as delivered. SDTM data
#'   usually arrives as SAS transport files: read them with
#'   `haven::read_xpt()` and pass the data frame to [ingest_lab_files()].
#'
#' See [vendor_spec()] to define your own.
#'
#' @return A named list of `biomarkerqc_vendor_spec` objects.
#' @export
#' @examples
#' specs <- vendor_specs()
#' names(specs)
#' specs$SDTM
vendor_specs <- function() {
  list(
    A = vendor_spec(
      "A",
      columns = c(
        subject_id = "SUBJID", visit = "VISIT", collection_date = "COLL_DT",
        test_code = "TESTCD", result_raw = "RESULT", unit_raw = "UNIT"
      ),
      subject_id = function(x) ifelse(grepl("^SUBJ-[0-9]{4}$", x), x, NA_character_),
      visits = c(SCR = "SCR", WK4 = "WK4", WK12 = "WK12"),
      tests = c(GLUC = "GLUC", CREAT = "CREAT", HBA1C = "HBA1C", CHOL = "CHOL", K = "K"),
      date_format = "%Y-%m-%d %H:%M"
    ),
    B = vendor_spec(
      "B",
      columns = c(
        subject_id = "patient", visit = "visit_name", collection_date = "sample_date",
        test_code = "analyte", result_raw = "value", unit_raw = "units"
      ),
      # Vendor B sends bare numbers ("7" rather than "SUBJ-0007")
      subject_id = function(x) {
        out <- rep(NA_character_, length(x))
        ok <- !is.na(x) & grepl("^[0-9]+$", x)
        out[ok] <- sprintf("SUBJ-%04d", as.integer(x[ok]))
        out
      },
      visits = c("Screening" = "SCR", "Week 4" = "WK4", "Week 12" = "WK12"),
      tests = c(
        "Glucose" = "GLUC", "Creatinine" = "CREAT", "HbA1c" = "HBA1C",
        "Total cholesterol" = "CHOL", "Potassium" = "K"
      ),
      date_format = "%d/%m/%Y",
      delim = ";",
      decimal_mark = ","
    ),
    SDTM = vendor_spec(
      "SDTM",
      columns = c(
        subject_id = "USUBJID", visit = "VISIT", collection_date = "LBDTC",
        test_code = "LBTESTCD", result_raw = "LBORRES", unit_raw = "LBORRESU"
      ),
      date_format = "%Y-%m-%d"
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
