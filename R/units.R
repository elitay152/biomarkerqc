# Unit rules use \u00b5 escapes because R CMD check rejects non-ASCII
# characters in package code.

#' Unit conversion rules
#'
#' Lists every unit the package accepts for each test and how to convert it to
#' that test's approved unit:
#' `approved value = value * multiplier + offset`.
#' Most conversions are a plain ratio (`offset = 0`). HbA1c from mmol/mol
#' (IFCC) to % (NGSP) also needs an offset.
#'
#' A unit that is not listed for its test is *unapproved*: it is flagged, not
#' converted, even if a conversion exists in principle.
#'
#' @return A data frame with columns `test_code`, `unit`, `approved_unit`,
#'   `multiplier` and `offset`.
#' @export
#' @examples
#' unit_rules()
unit_rules <- function() {
  data.frame(
    test_code = c("GLUC", "GLUC", "CREAT", "CREAT", "HBA1C", "HBA1C", "CHOL", "CHOL", "K", "K"),
    unit = c("mg/dL", "mmol/L", "mg/dL", "\u00b5mol/L", "%", "mmol/mol", "mg/dL", "mmol/L", "mmol/L", "mEq/L"),
    approved_unit = c("mg/dL", "mg/dL", "mg/dL", "mg/dL", "%", "%", "mg/dL", "mg/dL", "mmol/L", "mmol/L"),
    multiplier = c(1, 18.016, 1, 1 / 88.42, 1, 0.09148, 1, 38.67, 1, 1),
    offset = c(0, 0, 0, 0, 0, 2.152, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

#' Unit spelling synonyms
#'
#' Maps alternative spellings to the canonical spelling used in
#' [unit_rules()]. All unit matching is case-insensitive, so case variants
#' such as `"MG/DL"` or `"UMOL/L"` need no entry.
#'
#' @return A named character vector: names are synonyms, values are canonical
#'   spellings.
#' @export
#' @examples
#' unit_synonyms()
unit_synonyms <- function() {
  c(
    "umol/L" = "\u00b5mol/L",
    "\u03bcmol/L" = "\u00b5mol/L" # Greek mu, which looks identical to the micro sign
  )
}

#' Normalize lab results to approved units
#'
#' Converts every result to its test's approved unit, without dropping rows.
#' Each row gets a `unit_status` explaining what happened:
#'
#' * `"ok"`: already in the approved unit.
#' * `"converted"`: converted from another accepted unit.
#' * `"unapproved_unit"`: the unit is not accepted for this test.
#' * `"missing_unit"`: no unit was delivered.
#' * `"no_result"`: the unit is fine but there is no numeric result.
#' * `"unknown_test"`: the test code could not be identified at ingest.
#'
#' Censored results (such as `"<0.20"`) are converted too and keep their
#' qualifier, which stays valid because every conversion is increasing.
#'
#' @param labs A data frame in the standard schema, from [ingest_lab_files()]
#'   or [read_vendor_file()].
#' @param rules Conversion rules. Defaults to [unit_rules()].
#' @param synonyms Unit spelling synonyms. Defaults to [unit_synonyms()].
#'
#' @return `labs` with four columns added: `unit` (canonical spelling of
#'   `unit_raw`), `unit_status`, `result_std` and `unit_std`. `result_std` is
#'   `NA` unless the status is `"ok"` or `"converted"`.
#'
#' @section Errors:
#' * `biomarkerqc_error_not_standard`: `labs` is missing standard columns.
#' * `biomarkerqc_error_invalid_rules`: `rules` is malformed or lists the same
#'   test and unit twice.
#'
#' @export
#' @examples
#' labs <- ingest_lab_files(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' ))
#' labs <- normalize_units(labs)
#' table(labs$unit_status)
normalize_units <- function(labs, rules = unit_rules(), synonyms = unit_synonyms()) {
  check_standard(labs)
  check_rules(rules)

  labs$unit <- canonical_unit(labs$unit_raw, c(rules$unit, unname(synonyms)), synonyms)

  rule <- match(paste(labs$test_code, labs$unit), paste(rules$test_code, rules$unit))
  multiplier <- rules$multiplier[rule]
  offset <- rules$offset[rule]

  # Later assignments win, so the most fundamental problem is the one reported
  status <- ifelse(labs$unit == rules$approved_unit[rule], "ok", "converted")
  status[is.na(labs$result)] <- "no_result"
  status[is.na(rule)] <- "unapproved_unit"
  status[is.na(labs$unit)] <- "missing_unit"
  status[is.na(labs$test_code)] <- "unknown_test"
  labs$unit_status <- status

  usable <- labs$unit_status %in% c("ok", "converted")
  labs$result_std <- ifelse(usable, labs$result * multiplier + offset, NA_real_)
  labs$unit_std <- ifelse(usable, rules$approved_unit[rule], NA_character_)
  labs
}

# Internal helpers -----------------------------------------------------------

# Unknown units are returned trimmed but otherwise unchanged.
canonical_unit <- function(x, known, synonyms) {
  x <- trimws(x)
  hit <- match(tolower(x), tolower(names(synonyms)))
  x[!is.na(hit)] <- synonyms[hit[!is.na(hit)]]

  known <- unique(known)
  hit <- match(tolower(x), tolower(known))
  x[!is.na(hit)] <- known[hit[!is.na(hit)]]
  x
}

check_standard <- function(labs) {
  missing_cols <- setdiff(standard_columns, names(labs))
  if (!is.data.frame(labs) || length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "{.arg labs} must be a data frame in the standard schema.",
        "x" = "Missing column{?s}: {.val {missing_cols}}",
        "i" = "Create it with {.fn ingest_lab_files} or {.fn read_vendor_file}."
      ),
      class = c("biomarkerqc_error_not_standard", "biomarkerqc_error")
    )
  }
}

check_rules <- function(rules) {
  needed <- c("test_code", "unit", "approved_unit", "multiplier", "offset")
  missing_cols <- setdiff(needed, names(rules))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg rules} is missing column{?s}: {.val {missing_cols}}",
      class = c("biomarkerqc_error_invalid_rules", "biomarkerqc_error")
    )
  }
  key <- paste(rules$test_code, rules$unit)
  if (anyDuplicated(key)) {
    cli::cli_abort(
      "{.arg rules} lists the same test and unit more than once: {.val {unique(key[duplicated(key)])}}",
      class = c("biomarkerqc_error_invalid_rules", "biomarkerqc_error")
    )
  }
}
