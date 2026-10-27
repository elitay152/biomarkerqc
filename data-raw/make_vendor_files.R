# Generate synthetic vendor lab files -------------------------------------
#
# Creates the example data shipped in inst/extdata/:
#   vendor_a_results.csv  Vendor A: US conventional units, ISO dates
#   vendor_b_results.csv  Vendor B: SI units, European CSV (; and decimal comma)
#   planted_issues.csv    ground truth: every problem deliberately planted
#
# Scenario: a 200-subject study switches lab vendors partway through.
# Vendor A tested subjects 1-140 and Vendor B tested 101-200, so samples from
# subjects 101-140 were tested by BOTH labs. That overlap is what
# reconciliation compares.
#
# To keep the ground truth unambiguous, each kind of problem is planted in its
# own subject range:
#   subjects   1-100  Vendor A problems
#   subjects 101-140  cross-vendor discrepancies only
#   subjects 141-200  Vendor B problems
#
# All data is synthetic.

set.seed(20261027)

# 1. The "true" results ---------------------------------------------------
# One row per subject x visit x test, in the approved (US conventional) units.

subjects <- sprintf("SUBJ-%04d", 1:200)
subject_num <- 1:200
visit_days <- c(SCR = 0, WK4 = 28, WK12 = 84)
tests <- c("GLUC", "CREAT", "HBA1C", "CHOL", "K")

truth <- expand.grid(
  test = tests,
  visit = names(visit_days),
  subject_id = subjects,
  stringsAsFactors = FALSE
)[, c("subject_id", "visit", "test")]
truth$subject_num <- match(truth$subject_id, subjects)

# Each subject enrolls on a random day in 2025; visits follow the schedule,
# with collection between 07:00 and 11:00. All tests at a visit share one
# sample, so they share one collection time.
enrolled <- as.POSIXct("2025-01-06 07:00", tz = "UTC") + sample(0:300, 200, replace = TRUE) * 86400
draw_minutes <- sample(0:240, 200 * 3, replace = TRUE)
names(draw_minutes) <- paste(rep(subjects, each = 3), names(visit_days))
truth$collected <- enrolled[truth$subject_num] +
  visit_days[truth$visit] * 86400 +
  draw_minutes[paste(truth$subject_id, truth$visit)] * 60

generators <- list(
  GLUC  = function(n) rlnorm(n, log(100), 0.18),  # mg/dL
  CREAT = function(n) rlnorm(n, log(0.9), 0.25),  # mg/dL
  HBA1C = function(n) pmax(4.2, rnorm(n, 5.7, 0.7)), # % (NGSP)
  CHOL  = function(n) rnorm(n, 190, 35),          # mg/dL
  K     = function(n) rnorm(n, 4.2, 0.35)         # mmol/L
)
truth$value <- NA_real_
for (t in tests) {
  rows <- truth$test == t
  truth$value[rows] <- generators[[t]](sum(rows))
}

# 2. Render each vendor's file format --------------------------------------
# Each rendered table keeps hidden key columns (.subject_id, .visit, .test) so
# planted issues can be recorded in standard form. They are dropped before
# the files are written.

fmt <- function(x, digits) sprintf("%.*f", as.integer(digits), x)

# Vendor A: conventional units, test codes, ISO date-times
a_digits <- c(GLUC = 0, CREAT = 2, HBA1C = 1, CHOL = 0, K = 1)
a_units  <- c(GLUC = "mg/dL", CREAT = "mg/dL", HBA1C = "%", CHOL = "mg/dL", K = "mmol/L")

ta <- truth[truth$subject_num <= 140, ]
vendor_a <- data.frame(
  SUBJID = ta$subject_id,
  VISIT = ta$visit,
  COLL_DT = format(ta$collected, "%Y-%m-%d %H:%M"),
  TESTCD = ta$test,
  RESULT = fmt(ta$value, a_digits[ta$test]),
  UNIT = a_units[ta$test],
  .subject_id = ta$subject_id, .visit = ta$visit, .test = ta$test,
  stringsAsFactors = FALSE
)

# Vendor B: SI units, full analyte names, numeric patient IDs, day/month/year
to_si <- list(
  GLUC  = function(x) x / 18.016,          # mg/dL  -> mmol/L
  CREAT = function(x) x * 88.42,           # mg/dL  -> umol/L
  HBA1C = function(x) (x - 2.152) / 0.09148, # %    -> mmol/mol (IFCC); not a simple ratio
  CHOL  = function(x) x / 38.67,           # mg/dL  -> mmol/L
  K     = function(x) x                    # mmol/L -> mmol/L
)
b_digits <- c(GLUC = 1, CREAT = 0, HBA1C = 0, CHOL = 2, K = 1)
b_units  <- c(GLUC = "mmol/L", CREAT = "µmol/L", HBA1C = "mmol/mol", CHOL = "mmol/L", K = "mmol/L")
b_names  <- c(GLUC = "Glucose", CREAT = "Creatinine", HBA1C = "HbA1c",
              CHOL = "Total cholesterol", K = "Potassium")
b_visits <- c(SCR = "Screening", WK4 = "Week 4", WK12 = "Week 12")

tb <- truth[truth$subject_num >= 101, ]
si_value <- numeric(nrow(tb))
for (t in tests) {
  rows <- tb$test == t
  si_value[rows] <- to_si[[t]](tb$value[rows])
}
vendor_b <- data.frame(
  patient = as.character(tb$subject_num),  # vendor B drops the prefix and zeros
  visit_name = b_visits[tb$visit],
  sample_date = format(tb$collected, "%d/%m/%Y"),
  analyte = b_names[tb$test],
  value = fmt(si_value, b_digits[tb$test]),
  units = b_units[tb$test],
  .subject_id = tb$subject_id, .visit = tb$visit, .test = tb$test,
  stringsAsFactors = FALSE
)

# 3. Plant the problems ------------------------------------------------------

issues <- list()
record <- function(issue_type, vendor, df, rows, detail) {
  issues[[length(issues) + 1]] <<- data.frame(
    issue_type = issue_type,
    vendor = vendor,
    subject_id = df$.subject_id[rows],
    visit = df$.visit[rows],
    test = df$.test[rows],
    detail = detail,
    stringsAsFactors = FALSE
  )
}

# Draws rows without replacement from a pool, so no row gets two problems.
# `filter` is an optional logical vector over all rows (e.g. only GLUC rows).
take <- function(pool_env, n, filter = NULL) {
  candidates <- if (is.null(filter)) pool_env$rows else pool_env$rows[filter[pool_env$rows]]
  picked <- candidates[sample.int(length(candidates), n)]
  pool_env$rows <- setdiff(pool_env$rows, picked)
  picked
}

# Vendor A problems (subjects 1-100)
pool_a <- new.env()
pool_a$rows <- which(as.integer(sub("SUBJ-", "", vendor_a$.subject_id)) <= 100)

rows <- take(pool_a, 5)
record("exact_duplicate", "A", vendor_a, rows, "row delivered twice, identical")
dup_a <- vendor_a[rows, ]

rows <- take(pool_a, 6)
record("missing_result", "A", vendor_a, rows, "RESULT is blank")
vendor_a$RESULT[rows] <- ""

rows <- take(pool_a, 3, vendor_a$TESTCD == "CREAT")
record("below_detection_limit", "A", vendor_a, rows, "RESULT reported as '<0.20'")
vendor_a$RESULT[rows] <- "<0.20"

rows <- take(pool_a, 2, vendor_a$TESTCD == "GLUC")
record("unapproved_unit", "A", vendor_a, rows, "glucose reported in g/L")
vendor_a$RESULT[rows] <- fmt(as.numeric(vendor_a$RESULT[rows]) / 100, 2)
vendor_a$UNIT[rows] <- "g/L"

rows <- take(pool_a, 2, vendor_a$TESTCD == "K")
record("implausible_value", "A", vendor_a, rows, "potassium of 25.0 mmol/L (likely a keying error)")
vendor_a$RESULT[rows] <- "25.0"

vendor_a <- rbind(vendor_a, dup_a)
vendor_a <- vendor_a[order(vendor_a$.subject_id, match(vendor_a$.visit, names(visit_days)),
                           match(vendor_a$.test, tests)), ]

# Vendor B problems (subjects 141-200)
pool_b <- new.env()
pool_b$rows <- which(as.integer(vendor_b$patient) >= 141)

rows <- take(pool_b, 3)
record("conflicting_duplicate", "B", vendor_b, rows,
       "row delivered twice; second copy's value is 15% higher")
dup_b <- vendor_b[rows, ]
dup_value <- as.numeric(dup_b$value) * 1.15
dup_b$value <- fmt(dup_value, b_digits[dup_b$.test])

rows <- take(pool_b, 4)
record("missing_result", "B", vendor_b, rows, "value is blank")
vendor_b$value[rows] <- ""

rows <- take(pool_b, 2)
record("missing_subject_id", "B", vendor_b, rows, "patient is blank")
vendor_b$patient[rows] <- ""

# Not an error: an alternative spelling of an approved unit, which the
# package should normalize rather than reject.
rows <- take(pool_b, 10, vendor_b$.test == "CREAT")
record("unit_synonym", "B", vendor_b, rows, "creatinine unit spelled 'umol/L' instead of 'µmol/L'")
vendor_b$units[rows] <- "umol/L"

# Cross-vendor discrepancies (overlap subjects 101-140): Vendor B's result
# for the same sample differs far beyond rounding.
overlap <- which(match(vendor_b$.subject_id, subjects) %in% 101:140)
rows <- overlap[sample.int(length(overlap), 4)]
record("cross_vendor_discrepancy", "B", vendor_b, rows,
       "Vendor B result is 30% higher than Vendor A for the same sample")
vendor_b$value[rows] <- fmt(as.numeric(vendor_b$value[rows]) * 1.3, b_digits[vendor_b$.test[rows]])

vendor_b <- rbind(vendor_b, dup_b)
vendor_b <- vendor_b[sample.int(nrow(vendor_b)), ]  # vendor B's feed arrives unsorted

# Vendor B uses a decimal comma (blank values stay blank)
vendor_b$value <- sub(".", ",", vendor_b$value, fixed = TRUE)

# 4. Write the files --------------------------------------------------------

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

drop_keys <- function(df) df[, !startsWith(names(df), ".")]

write.csv(drop_keys(vendor_a), file.path(out_dir, "vendor_a_results.csv"),
          row.names = FALSE, na = "")
write.table(drop_keys(vendor_b), file.path(out_dir, "vendor_b_results.csv"),
            sep = ";", row.names = FALSE, quote = FALSE, na = "", fileEncoding = "UTF-8")

planted <- do.call(rbind, issues)
planted <- planted[order(planted$issue_type, planted$vendor, planted$subject_id), ]
write.csv(planted, file.path(out_dir, "planted_issues.csv"), row.names = FALSE)

message(sprintf("Vendor A: %d rows | Vendor B: %d rows | planted issues: %d",
                nrow(vendor_a), nrow(vendor_b), nrow(planted)))
