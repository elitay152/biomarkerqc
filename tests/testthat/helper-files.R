# Helpers shared by the test files (testthat loads helper-*.R automatically)

path_a <- function() biomarkerqc_example("vendor_a_results.csv")
path_b <- function() biomarkerqc_example("vendor_b_results.csv")

planted <- function(type, vendor = NULL) {
  p <- utils::read.csv(biomarkerqc_example("planted_issues.csv"), stringsAsFactors = FALSE)
  p <- p[p$issue_type == type, ]
  if (!is.null(vendor)) p <- p[p$vendor == vendor, ]
  p
}

# Writes lines to a temporary file that is deleted when the test finishes
temp_lab_file <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(lines, path, useBytes = TRUE)
  path
}
