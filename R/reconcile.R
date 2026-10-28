#' Reconcile results that two vendors reported for the same samples
#'
#' Matches samples (subject, visit and test) that both vendors reported and
#' compares their normalized results. Samples reported by only one vendor are
#' not part of reconciliation.
#'
#' The difference is relative to the mean of the two results,
#' `|a - b| / mean(|a|, |b|)`, so it does not matter which vendor is "first".
#' The default tolerance of 5% sits well above the rounding differences
#' expected when vendors report in different units (under 2% in the example
#' data) and well below a real disagreement.
#'
#' Each matched sample gets a `status`:
#'
#' * `"agree"`: results differ by no more than `tolerance`.
#' * `"discrepant"`: results differ by more than `tolerance`.
#' * `"censored"`: at least one result is censored (such as `"<0.20"`), so
#'   the values cannot be compared directly.
#' * `"date_mismatch"`: the collection dates differ, so the results may not be
#'   from the same sample.
#' * `"ambiguous"`: a vendor reported the sample more than once with
#'   different results (see `conflicting_duplicate` in [flag_issues()]).
#' * `"unusable"`: at least one vendor has no usable normalized result.
#'
#' A sample matching several statuses gets the first in this order:
#' unusable, ambiguous, date_mismatch, censored, then discrepant or agree.
#'
#' @param labs Lab results from [normalize_units()].
#' @param vendors The two vendors to compare. Defaults to the first two
#'   vendors in `labs`.
#' @param tolerance Largest relative difference counted as agreement.
#'
#' @return A data frame with one row per matched sample: `subject_id`,
#'   `visit`, `test_code`, `unit_std`, `result_a`, `result_b`, `rel_diff`,
#'   `status`, and `source_file_a`, `source_line_a`, `source_file_b`,
#'   `source_line_b`. Columns ending in `_a` refer to `vendors[1]` and `_b` to
#'   `vendors[2]`, which are also stored in the `"vendors"` attribute.
#'
#' @section Errors:
#' * `biomarkerqc_error_not_normalized`: `labs` has not been normalized.
#' * `biomarkerqc_error_invalid_argument`: fewer than two vendors are
#'   available, or `tolerance` is not a single number between 0 and 1.
#'
#' @export
#' @examples
#' labs <- normalize_units(ingest_lab_files(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' )))
#' rec <- reconcile_vendors(labs)
#' table(rec$status)
#' rec[rec$status == "discrepant", ]
reconcile_vendors <- function(labs, vendors = NULL, tolerance = 0.05) {
  check_normalized(labs)
  if (is.null(vendors)) vendors <- utils::head(unique(labs$vendor), 2)
  if (length(vendors) != 2 || !all(vendors %in% labs$vendor) || vendors[1] == vendors[2]) {
    cli::cli_abort(
      c(
        "Reconciliation needs two different vendors present in {.arg labs}.",
        "i" = "Vendors found: {.val {unique(labs$vendor)}}"
      ),
      class = c("biomarkerqc_error_invalid_argument", "biomarkerqc_error")
    )
  }
  if (!is.numeric(tolerance) || length(tolerance) != 1 || is.na(tolerance) ||
      tolerance < 0 || tolerance > 1) {
    cli::cli_abort(
      "{.arg tolerance} must be a single number between 0 and 1, not {.val {tolerance}}.",
      class = c("biomarkerqc_error_invalid_argument", "biomarkerqc_error")
    )
  }

  a <- one_per_sample(labs[labs$vendor == vendors[1], ])
  b <- one_per_sample(labs[labs$vendor == vendors[2], ])
  shared <- intersect(a$key, b$key)
  a <- a[match(shared, a$key), ]
  b <- b[match(shared, b$key), ]

  rel_diff <- abs(a$result_std - b$result_std) / ((abs(a$result_std) + abs(b$result_std)) / 2)
  rel_diff[which(a$result_std == b$result_std)] <- 0 # avoids 0/0 when both are zero

  # Later assignments win, so the most fundamental problem is reported
  status <- ifelse(rel_diff > tolerance, "discrepant", "agree")
  status[!is.na(a$qualifier) | !is.na(b$qualifier)] <- "censored"
  status[which(a$collection_date != b$collection_date)] <- "date_mismatch"
  status[a$ambiguous | b$ambiguous] <- "ambiguous"
  status[is.na(a$result_std) | is.na(b$result_std)] <- "unusable"

  out <- data.frame(
    subject_id = a$subject_id,
    visit = a$visit,
    test_code = a$test_code,
    unit_std = ifelse(is.na(a$unit_std), b$unit_std, a$unit_std),
    result_a = a$result_std,
    result_b = b$result_std,
    rel_diff = rel_diff,
    status = status,
    source_file_a = a$source_file,
    source_line_a = a$source_line,
    source_file_b = b$source_file,
    source_line_b = b$source_line,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$subject_id, match(out$visit, c("SCR", "WK4", "WK12")), out$test_code), ]
  rownames(out) <- NULL
  attr(out, "vendors") <- vendors
  out
}

# Internal helpers -----------------------------------------------------------

# Keeps the first row for each sample and marks samples whose rows disagree.
# Rows without a subject ID cannot be matched and are dropped.
one_per_sample <- function(rows) {
  rows <- rows[!is.na(rows$subject_id), ]
  rows$key <- paste(rows$subject_id, rows$visit, rows$test_code, sep = "|")
  distinct <- !duplicated(paste(rows$key, rows$result_raw))
  n_results <- as.vector(table(rows$key[distinct])[rows$key])
  rows$ambiguous <- n_results > 1
  rows[!duplicated(rows$key), ]
}
