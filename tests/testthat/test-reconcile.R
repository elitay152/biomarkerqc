rec_example <- function(...) {
  labs <- normalize_units(ingest_lab_files(c(A = path_a(), B = path_b())))
  reconcile_vendors(labs, ...)
}

# Two-vendor table of the same samples, for targeted cases
two_vendors <- function(result_a, result_b, unit = "mg/dL", test_code = "GLUC") {
  n <- length(result_a)
  a <- lab_row(rep(test_code, n), result_a, rep(unit, n))
  b <- lab_row(rep(test_code, n), result_b, rep(unit, n))
  a$vendor <- "A"
  b$vendor <- "B"
  a$visit <- b$visit <- c("SCR", "WK4", "WK12")[seq_len(n)]
  normalize_units(rbind(a, b))
}

# Example data ---------------------------------------------------------------

test_that("example data: every overlap sample is matched", {
  rec <- rec_example()
  # Subjects 101-140 x 3 visits x 5 tests
  expect_equal(nrow(rec), 40 * 3 * 5)
  expect_equal(attr(rec, "vendors"), c("A", "B"))
})

test_that("example data: exactly the planted discrepancies are discrepant", {
  rec <- rec_example()
  planted_keys <- with(planted("cross_vendor_discrepancy"), paste(subject_id, visit, test))
  found_keys <- with(rec[rec$status == "discrepant", ], paste(subject_id, visit, test_code))

  expect_setequal(found_keys, planted_keys)
  expect_true(all(rec$status %in% c("agree", "discrepant")))
  # Agreeing samples differ only by rounding
  expect_lt(max(rec$rel_diff[rec$status == "agree"]), 0.02)
})

test_that("both source references point at the matching rows", {
  rec <- rec_example()
  d <- rec[rec$status == "discrepant", ][1, ]
  a_line <- readLines(biomarkerqc_example(d$source_file_a))[d$source_line_a]
  b_line <- readLines(biomarkerqc_example(d$source_file_b), encoding = "UTF-8")[d$source_line_b]

  expect_match(a_line, d$subject_id, fixed = TRUE)
  expect_match(b_line, paste0("^", as.integer(sub("SUBJ-", "", d$subject_id)), ";"))
})

test_that("tolerance controls what counts as agreement", {
  strict <- rec_example(tolerance = 0.001)
  expect_gt(sum(strict$status == "discrepant"), nrow(planted("cross_vendor_discrepancy")))
})

test_that("results do not depend on vendor order", {
  ab <- rec_example(vendors = c("A", "B"))
  ba <- rec_example(vendors = c("B", "A"))
  expect_equal(ab$status, ba$status)
  expect_equal(ab$rel_diff, ba$rel_diff)
  expect_equal(ab$result_a, ba$result_b)
})

# Statuses -------------------------------------------------------------------

test_that("each status is assigned for its own reason", {
  labs <- two_vendors(c(100, 100, 100), c(103, 120, NA))
  rec <- reconcile_vendors(labs)
  expect_equal(rec$status, c("agree", "discrepant", "unusable"))
  expect_equal(rec$rel_diff[1:2], c(3 / 101.5, 20 / 110))
})

test_that("censored, date-mismatched and ambiguous samples are not compared", {
  labs <- two_vendors(c(100, 100, 100), c(100, 100, 100))
  labs$qualifier[labs$vendor == "A" & labs$visit == "SCR"] <- "<"
  labs$collection_date[labs$vendor == "B" & labs$visit == "WK4"] <- as.Date("2025-02-01")

  extra <- labs[labs$vendor == "B" & labs$visit == "WK12", ]
  extra$result_raw <- "150"
  extra$result <- extra$result_std <- 150
  labs <- rbind(labs, extra)

  rec <- reconcile_vendors(labs)
  expect_equal(rec$status[match(c("SCR", "WK4", "WK12"), rec$visit)],
               c("censored", "date_mismatch", "ambiguous"))
})

test_that("two zero results agree instead of producing 0/0", {
  rec <- reconcile_vendors(two_vendors(0, 0))
  expect_equal(rec$rel_diff, 0)
  expect_equal(rec$status, "agree")
})

# Errors ---------------------------------------------------------------------

test_that("bad arguments raise classed errors", {
  labs <- two_vendors(100, 100)
  expect_error(reconcile_vendors(labs[labs$vendor == "A", ]),
               class = "biomarkerqc_error_invalid_argument")
  expect_error(reconcile_vendors(labs, vendors = c("A", "Z")),
               class = "biomarkerqc_error_invalid_argument")
  expect_error(reconcile_vendors(labs, tolerance = 5),
               class = "biomarkerqc_error_invalid_argument")
  expect_error(reconcile_vendors(lab_row("GLUC", 95, "mg/dL")),
               class = "biomarkerqc_error_not_normalized")
})
