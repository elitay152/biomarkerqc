# Reading the example files --------------------------------------------------

test_that("vendor A is read into the standard schema, one row per data row", {
  a <- read_vendor_file(path_a(), "A")

  expect_named(a, standard_columns)
  expect_equal(nrow(a), 2105)
  expect_s3_class(a$collection_date, "Date")
  expect_type(a$result, "double")
  expect_equal(a$source_line, seq_len(nrow(a)) + 1L)
  expect_true(all(a$vendor == "A"))
})

test_that("vendor B's IDs, names and decimal commas are standardized", {
  b <- read_vendor_file(path_b(), "B")

  expect_named(b, standard_columns)
  expect_equal(nrow(b), 1503)
  expect_true(all(grepl("^SUBJ-[0-9]{4}$", b$subject_id[!is.na(b$subject_id)])))
  expect_setequal(unique(b$visit), c("SCR", "WK4", "WK12"))
  expect_setequal(unique(b$test_code), c("GLUC", "CREAT", "HBA1C", "CHOL", "K"))
  # "4,6" must become 4.6, not NA: the only NA results are the planted blanks
  expect_equal(sum(is.na(b$result)), nrow(planted("missing_result", "B")))
  expect_false(anyNA(b$collection_date))
})

test_that("ingest preserves every planted problem instead of fixing it", {
  # Blank fields are QC findings, so reading the files must not warn
  expect_no_warning(a <- read_vendor_file(path_a(), "A"))
  expect_no_warning(b <- read_vendor_file(path_b(), "B"))

  expect_equal(sum(is.na(a$result_raw)), nrow(planted("missing_result", "A")))
  expect_equal(sum(is.na(b$result_raw)), nrow(planted("missing_result", "B")))

  censored <- a[!is.na(a$qualifier), ]
  expect_equal(nrow(censored), nrow(planted("below_detection_limit")))
  expect_true(all(censored$qualifier == "<" & censored$result == 0.2))

  expect_equal(sum(a$unit_raw == "g/L"), nrow(planted("unapproved_unit")))
  expect_equal(sum(b$unit_raw == "umol/L"), nrow(planted("unit_synonym")))

  expect_equal(sum(is.na(b$subject_id)), nrow(planted("missing_subject_id")))
})

test_that("ingest_lab_files() combines vendors and logs each file", {
  labs <- ingest_lab_files(c(A = path_a(), B = path_b()))
  log <- attr(labs, "ingest_log")

  expect_equal(nrow(labs), 2105 + 1503)
  expect_equal(log$vendor, c("A", "B"))
  expect_equal(log$rows, c(2105, 1503))
  expect_match(log$md5, "^[0-9a-f]{32}$")
})

# Result parsing -------------------------------------------------------------

test_that("parse_result() handles qualifiers, decimal commas and junk", {
  p <- parse_result(c("5.6", "<0.20", "> 400", NA, "HEMOLYZED"))
  expect_equal(p$value, c(5.6, 0.2, 400, NA, NA))
  expect_equal(p$qualifier, c(NA, "<", ">", NA, NA))

  p <- parse_result(c("4,6", "<0,20"), decimal_mark = ",")
  expect_equal(p$value, c(4.6, 0.2))
})

# Errors and warnings --------------------------------------------------------

test_that("unusable inputs raise classed errors", {
  expect_error(read_vendor_file("no_such_file.csv", "A"),
               class = "biomarkerqc_error_file_not_found")
  expect_error(read_vendor_file(path_a(), "Z"),
               class = "biomarkerqc_error_unknown_vendor")
  expect_error(ingest_lab_files(c(path_a(), path_b())),
               class = "biomarkerqc_error_unnamed_files")

  no_unit <- temp_lab_file(c(
    "SUBJID,VISIT,COLL_DT,TESTCD,RESULT",
    "SUBJ-0001,SCR,2025-01-01 08:00,GLUC,95"
  ))
  expect_error(read_vendor_file(no_unit, "A"),
               class = "biomarkerqc_error_missing_columns")
})

test_that("unmappable values warn, keep the row, and become NA", {
  odd <- temp_lab_file(c(
    "SUBJID,VISIT,COLL_DT,TESTCD,RESULT,UNIT",
    "SUBJ-0001,SCR,2025-01-01 08:00,GLUC,95,mg/dL",
    "SUBJ-0001,UNSCHEDULED,2025-01-09 08:00,GLUC,101,mg/dL"
  ))

  expect_warning(
    out <- read_vendor_file(odd, "A"),
    class = "biomarkerqc_warning_unmapped"
  )
  expect_equal(nrow(out), 2)
  expect_equal(out$visit, c("SCR", NA))
  expect_equal(out$result, c(95, 101))
})

test_that("bad dates warn instead of failing the whole file", {
  bad_date <- temp_lab_file(c(
    "SUBJID,VISIT,COLL_DT,TESTCD,RESULT,UNIT",
    "SUBJ-0001,SCR,31/01/2025,GLUC,95,mg/dL"
  ))
  expect_warning(out <- read_vendor_file(bad_date, "A"),
                 class = "biomarkerqc_warning_unmapped")
  expect_true(is.na(out$collection_date))
})
