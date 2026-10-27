findings_example <- function() {
  flag_issues(normalize_units(ingest_lab_files(c(A = path_a(), B = path_b()))))
}

# Matches findings to the answer key, which names its test column `test`
key <- function(df) {
  test <- if ("test_code" %in% names(df)) df[["test_code"]] else df[["test"]]
  paste(df$vendor, df$subject_id, df$visit, test)
}

# Answer key ----------------------------------------------------------------

test_that("every planted problem is found, at the planted rows", {
  f <- findings_example()

  for (check in c("exact_duplicate", "missing_result", "implausible_value", "unapproved_unit")) {
    expect_setequal(key(f[f$check == check, ]), key(planted(check)))
  }
  expect_setequal(key(f[f$check == "censored_result", ]), key(planted("below_detection_limit")))

  # Both copies of each conflicting duplicate are flagged
  conflicts <- f[f$check == "conflicting_duplicate", ]
  expect_equal(nrow(conflicts), 2 * nrow(planted("conflicting_duplicate")))
  expect_setequal(unique(key(conflicts)), key(planted("conflicting_duplicate")))

  # The subject ID is gone from these rows, so match on count and vendor
  missing_ids <- f[f$check == "missing_subject_id", ]
  expect_equal(nrow(missing_ids), nrow(planted("missing_subject_id")))
  expect_true(all(missing_ids$vendor == "B"))
})

test_that("nothing is flagged that was not planted", {
  f <- findings_example()
  expected <- c("exact_duplicate", "conflicting_duplicate", "missing_result",
                "missing_subject_id", "implausible_value", "unapproved_unit",
                "censored_result")
  expect_setequal(unique(f$check), expected)

  # Unit synonyms are normalized, not flagged
  synonyms <- planted("unit_synonym")
  expect_false(any(key(f) %in% key(synonyms)))
})

test_that("findings point back to real lines in the source files", {
  f <- findings_example()
  dup <- f[f$check == "exact_duplicate", ][1, ]
  lines <- readLines(biomarkerqc_example(dup$source_file))
  original_line <- as.integer(sub("same as line ", "", dup$detail))

  expect_identical(lines[dup$source_line], lines[original_line])
  expect_false(dup$source_line == original_line)
})

# Individual checks ----------------------------------------------------------

test_that("non-numeric results are flagged separately from blanks", {
  labs <- normalize_units(lab_row(c("GLUC", "GLUC"), c(NA, NA), c("mg/dL", "mg/dL")))
  labs$result_raw <- c(NA, "HEMOLYZED")
  labs$visit <- c("SCR", "WK4") # different samples, so not duplicates
  f <- flag_issues(labs)
  expect_equal(f$check[order(f$source_line)], c("missing_result", "non_numeric_result"))
})

test_that("identical rows from different vendors are not duplicates", {
  labs <- lab_row(c("GLUC", "GLUC"), c(95, 95), c("mg/dL", "mg/dL"))
  labs$vendor <- c("A", "B")
  expect_equal(nrow(flag_issues(normalize_units(labs))), 0)
})

test_that("a clean table gives zero findings with the usual columns", {
  f <- flag_issues(normalize_units(lab_row("GLUC", 95, "mg/dL")))
  expect_equal(nrow(f), 0)
  expect_named(f, c("check", "severity", "vendor", "subject_id", "visit", "test_code",
                    "result_raw", "unit_raw", "detail", "source_file", "source_line"))
})

test_that("plausibility limits use the approved unit", {
  # 30 mmol/L glucose is 540 mg/dL: high, but plausible
  f <- flag_issues(normalize_units(lab_row("GLUC", 30, "mmol/L")))
  expect_equal(nrow(f), 0)
  f <- flag_issues(normalize_units(lab_row("K", 25, "mmol/L")))
  expect_equal(f$check, "implausible_value")
})

test_that("un-normalized input raises a classed error", {
  expect_error(flag_issues(lab_row("GLUC", 95, "mg/dL")),
               class = "biomarkerqc_error_not_normalized")
})
