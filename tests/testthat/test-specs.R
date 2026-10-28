tsv_columns <- c(
  subject_id = "PatientID", visit = "Visit", collection_date = "Drawn",
  test_code = "Test", result_raw = "Value", unit_raw = "Units"
)

# vendor_spec() --------------------------------------------------------------

test_that("vendor_spec() fills in defaults", {
  spec <- vendor_spec("C", columns = tsv_columns)
  expect_s3_class(spec, "biomarkerqc_vendor_spec")
  expect_null(spec$visits)
  expect_null(spec$tests)
  expect_equal(spec$subject_id(c("x", NA)), c("x", NA))
  expect_equal(spec$delim, ",")
})

test_that("vendor_spec() rejects malformed specs with a classed error", {
  expect_error(vendor_spec("", tsv_columns), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", tsv_columns[-1]), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", unname(tsv_columns)), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", tsv_columns, tests = c("GLUC")), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", tsv_columns, subject_id = "x"), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", tsv_columns, delim = ";;"), class = "biomarkerqc_error_invalid_spec")
  expect_error(vendor_spec("C", tsv_columns, decimal_mark = "'"), class = "biomarkerqc_error_invalid_spec")
})

test_that("plain lists are validated when used as specs", {
  expect_error(read_vendor_file(path_a(), list(name = "C")),
               class = "biomarkerqc_error_invalid_spec")
})

test_that("the built-in specs are valid vendor_spec objects", {
  specs <- vendor_specs()
  expect_named(specs, c("A", "B", "SDTM"))
  for (s in specs) expect_s3_class(s, "biomarkerqc_vendor_spec")
})

test_that("print() describes a spec", {
  output <- paste(capture_messages(print(vendor_specs()$B)), collapse = "")
  expect_match(output, "5 mapped")
  expect_match(output, "decimal mark")
})

# Custom specs end to end ----------------------------------------------------

test_that("a custom spec reads a new format with pass-through codes and several date formats", {
  path <- temp_lab_file(c(
    "PatientID\tVisit\tDrawn\tTest\tValue\tUnits",
    "P1\tBaseline\t2025-03-01T08:30\tGLUC\t5.4\tmmol/L",
    "P1\tFollow-up\t03/15/2025\tGLUC\t99\tmg/dL"
  ))
  spec <- vendor_spec("C", columns = tsv_columns, delim = "\t",
                      date_format = c("%Y-%m-%d", "%m/%d/%Y"))

  run <- run_qc(c(C = path), specs = list(C = spec))
  labs <- run$labs
  expect_equal(labs$visit, c("Baseline", "Follow-up"))
  expect_equal(labs$collection_date, as.Date(c("2025-03-01", "2025-03-15")))
  expect_equal(labs$result_std, c(5.4 * 18.016, 99))
  expect_null(run$reconciliation)
})

# Data frames ----------------------------------------------------------------

test_that("data frames are ingested like files, with row numbers as source lines", {
  df <- utils::read.csv(path_a(), colClasses = "character")
  from_df <- read_vendor_data(df, "A", source = "vendor A as data")
  from_file <- read_vendor_file(path_a(), "A")

  cols <- setdiff(standard_columns, c("source_file", "source_line"))
  expect_equal(from_df[cols], from_file[cols])
  expect_equal(from_df$source_line, seq_len(nrow(df)))
  expect_equal(unique(from_df$source_file), "vendor A as data")
  expect_match(attr(from_df, "ingest_log")$md5, "^[0-9a-f]{32}$")
})

test_that("numeric and factor columns are converted to text first", {
  df <- data.frame(
    PatientID = factor("P1"), Visit = "V1", Drawn = "2025-01-01",
    Test = "K", Value = 4.25, Units = "mmol/L"
  )
  out <- read_vendor_data(df, vendor_spec("C", tsv_columns))
  expect_equal(out$result_raw, "4.25")
  expect_equal(out$result, 4.25)
  expect_equal(out$subject_id, "P1")
})

test_that("ingest_lab_files() mixes file paths and data frames", {
  df <- utils::read.csv2(path_b(), colClasses = "character", encoding = "UTF-8")
  labs <- ingest_lab_files(list(A = path_a(), B = df))
  expect_equal(nrow(labs), 2105 + 1503)
  expect_equal(attr(labs, "ingest_log")$source_file,
               c("vendor_a_results.csv", "B (data frame)"))
})

test_that("a data frame checksum changes when its contents do", {
  df <- data.frame(PatientID = "P1", Visit = "V1", Drawn = "2025-01-01",
                   Test = "K", Value = "4.2", Units = "mmol/L")
  spec <- vendor_spec("C", tsv_columns)
  md5_1 <- attr(read_vendor_data(df, spec), "ingest_log")$md5
  df$Value <- "4.3"
  md5_2 <- attr(read_vendor_data(df, spec), "ingest_log")$md5
  expect_false(md5_1 == md5_2)
})

test_that("read_vendor_data() rejects anything that is not a data frame", {
  expect_error(read_vendor_data(list(a = 1), "A"), class = "biomarkerqc_error_invalid_argument")
})
