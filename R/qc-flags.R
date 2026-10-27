#' QC checks performed by [flag_issues()]
#'
#' @return A data frame with one row per check: `check`, `severity`
#'   (`"error"`, `"warning"` or `"info"`) and `description`.
#' @export
#' @examples
#' qc_checks()
qc_checks <- function() {
  data.frame(
    check = c(
      "missing_subject_id", "unknown_test", "missing_unit", "unapproved_unit",
      "conflicting_duplicate", "implausible_value",
      "exact_duplicate", "missing_result", "non_numeric_result",
      "censored_result"
    ),
    severity = c(rep("error", 6), rep("warning", 3), "info"),
    description = c(
      "Subject ID is blank or could not be mapped",
      "Test could not be identified",
      "No unit was delivered",
      "Unit is not accepted for this test",
      "Same sample and test delivered more than once with different results",
      "Result is outside the physiologically plausible range",
      "Row delivered more than once, identical",
      "No result was delivered",
      "Result is not a number (for example a comment such as 'HEMOLYZED')",
      "Result is below or above a reporting limit (for example '<0.20')"
    ),
    stringsAsFactors = FALSE
  )
}

#' Plausibility limits
#'
#' Results outside these limits are flagged as `implausible_value`. They are
#' deliberately wide: they catch keying and unit errors, not abnormal
#' results. Limits are in each test's approved unit.
#'
#' @return A data frame with columns `test_code`, `low` and `high`.
#' @export
#' @examples
#' plausibility_limits()
plausibility_limits <- function() {
  data.frame(
    test_code = c("GLUC", "CREAT", "HBA1C", "CHOL", "K"),
    low = c(10, 0.1, 3, 40, 1.5),
    high = c(1500, 25, 20, 1000, 10),
    stringsAsFactors = FALSE
  )
}

#' Flag data quality issues
#'
#' Runs every check in [qc_checks()] over normalized lab results and returns
#' one finding per problem. A row with two problems produces two findings.
#' The lab data itself is not changed.
#'
#' Duplicates are judged within each vendor: the same sample tested by two
#' vendors is a reconciliation question, not a duplicate.
#'
#' @param labs Lab results from [normalize_units()].
#' @param limits Plausibility limits. Defaults to [plausibility_limits()].
#'
#' @return A data frame of findings with columns `check`, `severity`,
#'   `vendor`, `subject_id`, `visit`, `test_code`, `result_raw`, `unit_raw`,
#'   `detail`, `source_file` and `source_line`, sorted by severity then
#'   source location. It has zero rows if nothing is found.
#'
#' @section Errors:
#' `biomarkerqc_error_not_normalized`: `labs` has not been through
#' [normalize_units()].
#'
#' @export
#' @examples
#' labs <- ingest_lab_files(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' ))
#' findings <- flag_issues(normalize_units(labs))
#' table(findings$check)
flag_issues <- function(labs, limits = plausibility_limits()) {
  check_normalized(labs)

  found <- list()
  add <- function(check, rows, detail = "") {
    if (any(rows)) found[[length(found) + 1]] <<- make_findings(labs, check, rows, detail)
  }

  # Unit problems come straight from normalize_units()
  for (status in c("unknown_test", "missing_unit", "unapproved_unit")) {
    add(status, labs$unit_status == status)
  }

  add("missing_subject_id", is.na(labs$subject_id))
  add("missing_result", is.na(labs$result_raw))
  add("non_numeric_result", !is.na(labs$result_raw) & is.na(labs$result))
  add("censored_result", !is.na(labs$qualifier),
      paste0("reported as '", labs$result_raw, "'"))

  # Exact duplicates: every delivered field matches an earlier row
  content <- paste(labs$vendor, labs$subject_id, labs$visit, labs$test_code,
                   labs$collection_date, labs$result_raw, labs$unit_raw, sep = "|")
  first <- match(content, content)
  add("exact_duplicate", duplicated(content),
      paste("same as line", labs$source_line[first]))

  # Conflicting duplicates: one sample and test, more than one distinct result
  sample_key <- paste(labs$vendor, labs$subject_id, labs$visit, labs$test_code, sep = "|")
  distinct <- !duplicated(paste(sample_key, labs$result_raw))
  n_results <- as.vector(table(sample_key[distinct])[sample_key])
  conflicting <- !is.na(labs$subject_id) & n_results > 1
  add("conflicting_duplicate", conflicting,
      paste(n_results, "different results for this sample"))

  rule <- match(labs$test_code, limits$test_code)
  low <- limits$low[rule]
  high <- limits$high[rule]
  outside <- !is.na(labs$result_std) & !is.na(rule) &
    (labs$result_std < low | labs$result_std > high)
  add("implausible_value", outside,
      sprintf("%s %s is outside %s-%s", signif(labs$result_std, 4),
              labs$unit_std, low, high))

  findings <- if (length(found) > 0) {
    do.call(rbind, found)
  } else {
    make_findings(labs, "missing_result", rep(FALSE, nrow(labs)), "")
  }
  severity_rank <- match(findings$severity, c("error", "warning", "info"))
  findings <- findings[order(severity_rank, findings$check, findings$source_file, findings$source_line), ]
  rownames(findings) <- NULL
  findings
}

# Internal helpers -----------------------------------------------------------

make_findings <- function(labs, check, rows, detail) {
  detail <- rep_len(detail, nrow(labs))
  data.frame(
    check = rep(check, sum(rows)),
    severity = rep(qc_checks()$severity[match(check, qc_checks()$check)], sum(rows)),
    vendor = labs$vendor[rows],
    subject_id = labs$subject_id[rows],
    visit = labs$visit[rows],
    test_code = labs$test_code[rows],
    result_raw = labs$result_raw[rows],
    unit_raw = labs$unit_raw[rows],
    detail = detail[rows],
    source_file = labs$source_file[rows],
    source_line = labs$source_line[rows],
    stringsAsFactors = FALSE
  )
}

check_normalized <- function(labs) {
  check_standard(labs)
  missing_cols <- setdiff(c("unit", "unit_status", "result_std", "unit_std"), names(labs))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "{.arg labs} has not been normalized.",
        "i" = "Run {.fn normalize_units} first."
      ),
      class = c("biomarkerqc_error_not_normalized", "biomarkerqc_error")
    )
  }
}
