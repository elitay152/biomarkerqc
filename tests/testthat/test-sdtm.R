# Real-format data: the CDISC SDTM pilot study LB domain from pharmaversesdtm

sdtm_run <- function() run_qc(list(SDTM = pharmaversesdtm::lb))

test_that("the SDTM LB domain runs through the whole pipeline without warnings", {
  skip_if_not_installed("pharmaversesdtm")
  lb <- pharmaversesdtm::lb

  expect_no_warning(run <- sdtm_run())
  expect_equal(nrow(run$labs), nrow(lb))
  # as.character() drops SDTM's variable labels, which are metadata, not values
  expect_equal(run$labs$visit, as.character(lb$VISIT))
  expect_equal(run$labs$test_code, as.character(lb$LBTESTCD))
  expect_false(anyNA(run$labs$collection_date))
})

test_that("in-scope tests normalize cleanly; the rest are counted, not flagged", {
  skip_if_not_installed("pharmaversesdtm")
  run <- sdtm_run()

  in_scope <- run$labs$test_code %in% unit_rules()$test_code
  expect_true(all(run$labs$unit_status[in_scope] %in% c("ok", "converted")))
  expect_true(all(run$labs$unit_status[!in_scope] == "no_unit_rule"))
  expect_equal(sum(run$unchecked_tests$rows), sum(!in_scope))

  # Qualitative results such as "N" in out-of-scope tests are not findings
  expect_false(any(run$findings$check %in% c("non_numeric_result", "unapproved_unit")))
})

test_that("conversions agree with the sponsor's own SI results (LBSTRESN)", {
  skip_if_not_installed("pharmaversesdtm")
  lb <- pharmaversesdtm::lb
  ours <- sdtm_run()$labs

  # Normalize the sponsor's standardized results with the same rules
  sponsor <- ours
  sponsor$result <- lb$LBSTRESN
  sponsor$unit_raw <- lb$LBSTRESU
  sponsor <- normalize_units(sponsor[standard_columns])

  compared <- ours$unit_status %in% c("ok", "converted") &
    sponsor$unit_status %in% c("ok", "converted")
  rel_diff <- abs(ours$result_std - sponsor$result_std) / ours$result_std

  expect_gt(sum(compared), 7000)
  expect_setequal(unique(ours$test_code[compared]), c("GLUC", "CREAT", "CHOL", "K"))
  expect_lt(max(rel_diff[compared]), 0.001)
})
