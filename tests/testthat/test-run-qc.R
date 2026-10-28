run_example <- function(...) run_qc(c(A = path_a(), B = path_b()), ...)

# run_qc() -------------------------------------------------------------------

test_that("run_qc() runs every stage and keeps its settings", {
  run <- run_example(tolerance = 0.1)

  expect_s3_class(run, "biomarkerqc_run")
  expect_equal(nrow(run$labs), 2105 + 1503)
  expect_equal(nrow(run$ingest_log), 2)
  expect_equal(nrow(run$reconciliation), 600)
  expect_equal(run$settings$tolerance, 0.1)
  expect_equal(run$settings$rules, unit_rules())
  expect_equal(run$run_info$package_version, as.character(utils::packageVersion("biomarkerqc")))
})

test_that("run_qc() finds every planted problem", {
  run <- run_example()
  p <- utils::read.csv(biomarkerqc_example("planted_issues.csv"))

  # conflicting duplicates flag both copies; synonyms are not problems
  expected <- nrow(p) - sum(p$issue_type %in% c("unit_synonym", "cross_vendor_discrepancy")) +
    sum(p$issue_type == "conflicting_duplicate")
  expect_equal(nrow(run$findings), expected)
  expect_equal(sum(run$reconciliation$status == "discrepant"),
               sum(p$issue_type == "cross_vendor_discrepancy"))
})

test_that("a single vendor runs without reconciliation", {
  run <- run_qc(c(A = path_a()))
  expect_null(run$reconciliation)
  expect_gt(nrow(run$findings), 0)
})

test_that("print() summarizes the run", {
  output <- paste(capture_messages(print(run_example())), collapse = "")
  expect_match(output, "2 files, 3608 rows")
  expect_match(output, "12 error findings")
  expect_match(output, "596 of 600 shared samples agree")
})

# qc_report() ----------------------------------------------------------------

test_that("qc_report() writes a traceable HTML report", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- run_example()
  path <- withr::local_tempfile(fileext = ".html")
  written <- qc_report(run, path)
  # Compare resolved paths: the same file can be spelled differently
  # (macOS /var vs /private/var, Windows 8.3 short names)
  expect_equal(normalizePath(written), normalizePath(path))
  html <- paste(readLines(path, encoding = "UTF-8"), collapse = "\n")

  # Inputs are identified by checksum
  expect_match(html, run$ingest_log$md5[1], fixed = TRUE)
  # Findings and discrepancies carry their source location
  implausible <- run$findings[run$findings$check == "implausible_value", ][1, ]
  expect_match(html, implausible$subject_id, fixed = TRUE)
  expect_match(html, as.character(implausible$source_line), fixed = TRUE)
  discrepant <- run$reconciliation[run$reconciliation$status == "discrepant", ][1, ]
  expect_match(html, discrepant$subject_id, fixed = TRUE)
  # Delivered values are escaped, not interpreted as HTML
  expect_match(html, "&lt;0.20", fixed = TRUE)
})

test_that("qc_report() rejects bad arguments before rendering", {
  expect_error(qc_report(list()), class = "biomarkerqc_error_invalid_argument")

  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())
  missing_dir <- file.path(withr::local_tempdir(), "no-such-folder", "report.html")
  expect_error(qc_report(run_example(), missing_dir),
               class = "biomarkerqc_error_invalid_argument")
})
