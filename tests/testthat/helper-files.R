# Helpers shared by the test files (testthat loads helper-*.R automatically)

path_a <- function() biomarkerqc_example("vendor_a_results.csv")
path_b <- function() biomarkerqc_example("vendor_b_results.csv")

planted <- function(type, vendor = NULL) {
  p <- utils::read.csv(biomarkerqc_example("planted_issues.csv"), stringsAsFactors = FALSE)
  p <- p[p$issue_type == type, ]
  if (!is.null(vendor)) p <- p[p$vendor == vendor, ]
  p
}

# Small lab table in the standard schema, one row per result, for targeted cases
lab_row <- function(test_code, result, unit_raw) {
  n <- length(result)
  data.frame(
    vendor = "X", subject_id = "SUBJ-0001", visit = "SCR",
    test_code = test_code, collection_date = as.Date("2025-01-01"),
    result_raw = as.character(result), result = result, qualifier = NA_character_,
    unit_raw = unit_raw, source_file = "x.csv", source_line = seq_len(n) + 1L,
    stringsAsFactors = FALSE
  )
}

# Writes lines to a temporary file that is deleted when the test finishes
temp_lab_file <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, path, useBytes = TRUE)
  path
}
