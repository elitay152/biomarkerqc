labs_example <- function() {
  normalize_units(ingest_lab_files(c(A = path_a(), B = path_b())))
}

# Conversions ----------------------------------------------------------------

test_that("conversions match published factors, including HbA1c's offset", {
  out <- normalize_units(lab_row(
    c("GLUC", "CREAT", "HBA1C", "CHOL", "K", "GLUC"),
    c(5.6, 88.42, 39, 5.0, 4.1, 95),
    c("mmol/L", "\u00b5mol/L", "mmol/mol", "mmol/L", "mEq/L", "mg/dL")
  ))

  expect_equal(out$result_std, c(5.6 * 18.016, 1, 39 * 0.09148 + 2.152, 5.0 * 38.67, 4.1, 95))
  expect_equal(out$unit_std, c("mg/dL", "mg/dL", "%", "mg/dL", "mmol/L", "mg/dL"))
  expect_equal(out$unit_status, c(rep("converted", 5), "ok"))
})

test_that("unit spellings are canonicalized before matching", {
  out <- normalize_units(lab_row(
    rep("CREAT", 4), rep(88.42, 4),
    c("umol/L", "\u03bcmol/L", " \u00b5mol/L ", "UMOL/L")
  ))
  expect_equal(out$unit, rep("\u00b5mol/L", 4))
  expect_equal(out$result_std, rep(1, 4))
})

test_that("censored results are converted and keep their qualifier", {
  row <- lab_row("GLUC", 2.8, "mmol/L")
  row$qualifier <- "<"
  out <- normalize_units(row)
  expect_equal(out$qualifier, "<")
  expect_equal(out$result_std, 2.8 * 18.016)
})

# Statuses -------------------------------------------------------------------

test_that("each problem gets its own status and no converted value", {
  out <- normalize_units(lab_row(
    c(NA, "GLUC", "GLUC", "GLUC", "GLUC"),
    c(95, 95, 0.95, NA, 95),
    c("mg/dL", NA, "g/L", "mg/dL", "mg/dL")
  ))
  expect_equal(out$unit_status,
               c("unknown_test", "missing_unit", "unapproved_unit", "no_result", "ok"))
  expect_equal(is.na(out$result_std), c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("tests without unit rules are marked, not treated as unit errors", {
  labs <- lab_row(c("ALT", "ANISO"), c(30, NA), c("U/L", "NO UNITS"))
  labs$result_raw[2] <- "N" # qualitative result
  labs$visit <- c("SCR", "WK4")
  out <- normalize_units(labs)
  expect_equal(out$unit_status, c("no_unit_rule", "no_unit_rule"))
  expect_true(all(is.na(out$result_std)))
  expect_equal(nrow(flag_issues(out)), 0)
})

test_that("the most fundamental problem wins when a row has several", {
  # Unapproved unit and no result: the unit problem is reported
  out <- normalize_units(lab_row("GLUC", NA_real_, "g/L"))
  expect_equal(out$unit_status, "unapproved_unit")
})

# Example data ---------------------------------------------------------------

test_that("example data: only planted problems fail to normalize", {
  labs <- labs_example()
  status <- table(labs$unit_status)

  expect_equal(nrow(labs), 2105 + 1503)
  expect_equal(unname(status["unapproved_unit"]), nrow(planted("unapproved_unit")))
  expect_equal(unname(status["no_result"]), nrow(planted("missing_result")))
  expect_false("missing_unit" %in% names(status))
  expect_false("unknown_test" %in% names(status))
})

test_that("example data: each test ends up in exactly one approved unit", {
  labs <- labs_example()
  units_per_test <- tapply(labs$unit_std, labs$test_code, function(u) length(unique(stats::na.omit(u))))
  expect_true(all(units_per_test == 1))
})

test_that("example data: both vendors agree on shared samples after normalizing", {
  labs <- labs_example()
  key <- paste(labs$subject_id, labs$visit, labs$test_code)
  a <- labs[labs$vendor == "A", ]
  b <- labs[labs$vendor == "B", ]
  ka <- key[labs$vendor == "A"]
  kb <- key[labs$vendor == "B"]

  shared <- intersect(ka[!is.na(a$result_std)], kb[!is.na(b$result_std)])
  rel_diff <- abs(b$result_std[match(shared, kb)] - a$result_std[match(shared, ka)]) /
    a$result_std[match(shared, ka)]

  # Rounding differences stay under 2%; only planted discrepancies exceed 5%
  expect_equal(sum(rel_diff > 0.05), nrow(planted("cross_vendor_discrepancy")))
  expect_true(all(rel_diff[rel_diff <= 0.05] < 0.02))
})

# Errors ---------------------------------------------------------------------

test_that("bad inputs raise classed errors", {
  expect_error(normalize_units(data.frame(x = 1)),
               class = "biomarkerqc_error_not_standard")

  dup_rules <- rbind(unit_rules(), unit_rules()[1, ])
  expect_error(normalize_units(lab_row("GLUC", 95, "mg/dL"), rules = dup_rules),
               class = "biomarkerqc_error_invalid_rules")

  expect_error(normalize_units(lab_row("GLUC", 95, "mg/dL"), rules = unit_rules()[, -5]),
               class = "biomarkerqc_error_invalid_rules")
})
