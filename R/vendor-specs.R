# A new vendor needs a new spec here, not a new reader.

#' Standard schema produced by ingest
#'
#' Every ingested file is returned with exactly these columns, in this order.
#'
#' * `vendor`: vendor name from the spec (for example `"A"`).
#' * `subject_id`: standard subject ID (for example `"SUBJ-0001"`), or `NA`
#'   if the source value was blank or could not be mapped.
#' * `visit`: standard visit code (`"SCR"`, `"WK4"`, `"WK12"`).
#' * `test_code`: standard test code (`"GLUC"`, `"CREAT"`, `"HBA1C"`,
#'   `"CHOL"`, `"K"`).
#' * `collection_date`: sample collection date (`Date`).
#' * `result_raw`: the result exactly as delivered (character).
#' * `result`: the result parsed as a number, or `NA` if blank or not numeric.
#' * `qualifier`: `"<"` or `">"` for censored results such as `"<0.20"`,
#'   otherwise `NA`.
#' * `unit_raw`: the unit exactly as delivered. Units are normalized in a
#'   later step, not during ingest.
#' * `source_file`: base name of the file the row came from.
#' * `source_line`: line number in that file (the header is line 1), so any
#'   row can be traced back to the original delivery.
#'
#' @format A character vector of column names.
#' @export
standard_columns <- c(
  "vendor", "subject_id", "visit", "test_code", "collection_date",
  "result_raw", "result", "qualifier", "unit_raw",
  "source_file", "source_line"
)

#' Built-in vendor specifications
#'
#' Returns the specs for the two example vendors shipped with the package.
#' A spec is a list with:
#'
#' * `name`: vendor name.
#' * `delim`, `decimal_mark`: how the file is delimited and how decimals
#'   are written.
#' * `columns`: named character vector mapping each standard field to the
#'   vendor's column name.
#' * `subject_id`: function converting the vendor's subject IDs to standard
#'   IDs, returning `NA` for values it cannot convert.
#' * `visits`, `tests`: named character vectors mapping vendor values (names)
#'   to standard codes (values).
#' * `date_format`: format string for [strptime()].
#'
#' @return A named list of vendor specs.
#' @export
#' @examples
#' specs <- vendor_specs()
#' names(specs)
#' specs$B$tests
vendor_specs <- function() {
  list(
    A = list(
      name = "A",
      delim = ",",
      decimal_mark = ".",
      columns = c(
        subject_id = "SUBJID", visit = "VISIT", collection_date = "COLL_DT",
        test_code = "TESTCD", result_raw = "RESULT", unit_raw = "UNIT"
      ),
      subject_id = function(x) ifelse(grepl("^SUBJ-[0-9]{4}$", x), x, NA_character_),
      visits = c(SCR = "SCR", WK4 = "WK4", WK12 = "WK12"),
      tests = c(GLUC = "GLUC", CREAT = "CREAT", HBA1C = "HBA1C", CHOL = "CHOL", K = "K"),
      date_format = "%Y-%m-%d %H:%M"
    ),
    B = list(
      name = "B",
      delim = ";",
      decimal_mark = ",",
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
      date_format = "%d/%m/%Y"
    )
  )
}
