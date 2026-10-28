# Ingest translates formats; it never fixes or drops rows. An unusable source
# is an error; an unmappable value is a warning, and the row is kept with NA.

#' Read one vendor lab file into the standard schema
#'
#' @param path Path to the vendor file.
#' @param vendor A vendor name from [vendor_specs()] (such as `"A"`), or a
#'   spec from [vendor_spec()].
#'
#' @return A data frame with the columns in [standard_columns], one row per
#'   data row in the file. It carries an `"ingest_log"` attribute recording
#'   the source, its MD5 checksum, the row count and the time of ingest.
#'
#' @section Errors:
#' * `biomarkerqc_error_unknown_vendor`: `vendor` is not a known spec.
#' * `biomarkerqc_error_invalid_spec`: `vendor` is a malformed spec.
#' * `biomarkerqc_error_file_not_found`: `path` does not exist.
#' * `biomarkerqc_error_read`: the file could not be parsed.
#' * `biomarkerqc_error_missing_columns`: required columns are absent.
#'
#' @section Warnings:
#' `biomarkerqc_warning_unmapped` is raised when non-blank values cannot be
#' mapped to a standard subject ID, visit, test or date. Those rows are kept,
#' with `NA` in the affected field.
#'
#' @seealso [read_vendor_data()] for data that is already in R.
#' @export
#' @examples
#' a <- read_vendor_file(biomarkerqc_example("vendor_a_results.csv"), "A")
#' head(a)
read_vendor_file <- function(path, vendor) {
  spec <- resolve_spec(vendor)

  if (!file.exists(path)) {
    cli::cli_abort(
      "Lab file not found: {.file {path}}",
      class = c("biomarkerqc_error_file_not_found", "biomarkerqc_error")
    )
  }

  raw <- tryCatch(
    utils::read.table(
      path,
      sep = spec$delim, header = TRUE, quote = "\"", comment.char = "",
      colClasses = "character", na.strings = c("", "NA"),
      strip.white = TRUE, check.names = FALSE, fileEncoding = "UTF-8"
    ),
    error = function(e) {
      cli::cli_abort(
        "Could not read {.file {path}} as a vendor {spec$name} file.",
        parent = e,
        class = c("biomarkerqc_error_read", "biomarkerqc_error")
      )
    }
  )

  standardize(raw, spec, source = basename(path), md5 = unname(tools::md5sum(path)),
              first_line = 2L)
}

#' Read lab data that is already in R into the standard schema
#'
#' Works like [read_vendor_file()] for a data frame, such as an SDTM LB
#' domain read with `haven::read_xpt()`. `source_line` records the row number
#' in the data frame, and the ingest log records a checksum of its contents.
#'
#' @param data A data frame with the columns named in the spec.
#' @param vendor A vendor name from [vendor_specs()] (such as `"SDTM"`), or a
#'   spec from [vendor_spec()].
#' @param source Label recorded in `source_file`. Defaults to the expression
#'   passed as `data`.
#'
#' @inherit read_vendor_file return
#' @section Errors:
#' * `biomarkerqc_error_invalid_argument`: `data` is not a data frame.
#' * `biomarkerqc_error_unknown_vendor`, `biomarkerqc_error_invalid_spec`,
#'   `biomarkerqc_error_missing_columns`: as for [read_vendor_file()].
#' @export
#' @examplesIf rlang::is_installed("pharmaversesdtm")
#' lb <- read_vendor_data(pharmaversesdtm::lb, "SDTM")
#' table(lb$test_code)[c("GLUC", "CREAT", "K")]
read_vendor_data <- function(data, vendor, source = NULL) {
  source <- source %||% paste(deparse(substitute(data)), collapse = "")
  spec <- resolve_spec(vendor)
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, not {.obj_type_friendly {data}}.",
      class = c("biomarkerqc_error_invalid_argument", "biomarkerqc_error")
    )
  }

  # Match what reading a file produces: text columns, blanks as NA
  raw <- as.data.frame(lapply(data, function(x) {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    x
  }), stringsAsFactors = FALSE, check.names = FALSE)

  standardize(raw, spec, source = source, md5 = data_md5(raw), first_line = 1L)
}

#' Ingest several vendor sources into one table
#'
#' @param files Lab data named by vendor: a named character vector of file
#'   paths (`c(A = "a.csv", B = "b.csv")`), or a named list that can also
#'   contain data frames (`list(A = "a.csv", SDTM = lb)`).
#' @param specs A named list of vendor specs. Defaults to [vendor_specs()].
#'   Add your own with `c(vendor_specs(), list(C = vendor_spec(...)))`.
#'
#' @return One data frame in the standard schema, with an `"ingest_log"`
#'   attribute holding one row per source.
#' @export
#' @examples
#' labs <- ingest_lab_files(c(
#'   A = biomarkerqc_example("vendor_a_results.csv"),
#'   B = biomarkerqc_example("vendor_b_results.csv")
#' ))
#' table(labs$vendor)
#' attr(labs, "ingest_log")
ingest_lab_files <- function(files, specs = vendor_specs()) {
  if (is.null(names(files)) || any(names(files) == "")) {
    cli::cli_abort(
      c(
        "Every source must be named with its vendor.",
        "i" = 'For example: {.code ingest_lab_files(c(A = "a.csv", B = "b.csv"))}'
      ),
      class = c("biomarkerqc_error_unnamed_files", "biomarkerqc_error")
    )
  }

  parts <- lapply(names(files), function(v) {
    spec <- specs[[v]]
    if (is.null(spec)) spec <- v # let resolve_spec() raise the standard error
    x <- files[[v]]
    if (is.data.frame(x)) {
      read_vendor_data(x, spec, source = paste0(v, " (data frame)"))
    } else {
      read_vendor_file(x, spec)
    }
  })

  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  attr(out, "ingest_log") <- do.call(rbind, lapply(parts, attr, "ingest_log"))
  out
}

#' Path to an example file shipped with the package
#'
#' @param file File name. Leave as `NULL` to list the available files.
#' @return A file path, or a character vector of file names.
#' @export
#' @examples
#' biomarkerqc_example()
#' biomarkerqc_example("planted_issues.csv")
biomarkerqc_example <- function(file = NULL) {
  if (is.null(file)) {
    return(dir(system.file("extdata", package = "biomarkerqc")))
  }
  system.file("extdata", file, package = "biomarkerqc", mustWork = TRUE)
}

# Internal helpers -----------------------------------------------------------

# Maps raw text columns onto the standard schema. Shared by files and data
# frames so both follow exactly the same rules.
standardize <- function(raw, spec, source, md5, first_line) {
  missing_cols <- setdiff(spec$columns, names(raw))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "{.file {source}} is missing required vendor {spec$name} column{?s}: {.val {missing_cols}}",
        "i" = "Columns found: {.val {names(raw)}}"
      ),
      class = c("biomarkerqc_error_missing_columns", "biomarkerqc_error")
    )
  }

  col <- function(field) raw[[spec$columns[[field]]]]
  map_codes <- function(field, mapping, what) {
    if (is.null(mapping)) return(col(field))
    check_mapped(col(field), unname(mapping[col(field)]), what, source)
  }
  parsed <- parse_result(col("result_raw"), spec$decimal_mark)

  out <- data.frame(
    vendor = rep(spec$name, nrow(raw)),
    subject_id = check_mapped(col("subject_id"), spec$subject_id(col("subject_id")), "subject ID", source),
    visit = map_codes("visit", spec$visits, "visit"),
    test_code = map_codes("test_code", spec$tests, "test"),
    collection_date = check_mapped(
      col("collection_date"), parse_dates(col("collection_date"), spec$date_format),
      "collection date", source
    ),
    result_raw = col("result_raw"),
    result = parsed$value,
    qualifier = parsed$qualifier,
    unit_raw = col("unit_raw"),
    source_file = rep(source, nrow(raw)),
    source_line = seq_len(nrow(raw)) + first_line - 1L,
    stringsAsFactors = FALSE
  )

  attr(out, "ingest_log") <- data.frame(
    vendor = spec$name,
    source_file = source,
    md5 = md5,
    rows = nrow(out),
    ingested_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  out
}

resolve_spec <- function(vendor) {
  if (inherits(vendor, "biomarkerqc_vendor_spec")) return(vendor)
  if (is.list(vendor)) return(do.call(vendor_spec, vendor)) # validates a plain list
  specs <- vendor_specs()
  if (!is.character(vendor) || length(vendor) != 1 || !vendor %in% names(specs)) {
    cli::cli_abort(
      c(
        "Unknown vendor {.val {vendor}}.",
        "i" = "Known vendors: {.val {names(specs)}}. Or pass a spec from {.fn vendor_spec}."
      ),
      class = c("biomarkerqc_error_unknown_vendor", "biomarkerqc_error")
    )
  }
  specs[[vendor]]
}

# Tries each format in turn; a value keeps the first format that parses it
parse_dates <- function(x, formats) {
  out <- as.Date(rep(NA_character_, length(x)))
  for (fmt in formats) {
    todo <- is.na(out) & !is.na(x)
    out[todo] <- as.Date(strptime(x[todo], fmt, tz = "UTC"))
  }
  out
}

# Checksum of a data frame's contents, so it can be identified like a file
data_md5 <- function(data) {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(data, path, row.names = FALSE)
  unname(tools::md5sum(path))
}

# Blank source values are not warned about: missing data is a QC finding.
check_mapped <- function(source, mapped, what, source_name) {
  failed <- !is.na(source) & is.na(mapped)
  if (any(failed)) {
    bad <- unique(source[failed])
    shown <- utils::head(bad, 5)
    more <- if (length(bad) > 5) sprintf(" and %d more", length(bad) - 5) else ""
    cli::cli_warn(
      c(
        "{sum(failed)} row{?s} in {.file {source_name}} had {what} values that could not be mapped; set to NA.",
        "i" = "Unmapped: {.val {shown}}{more}"
      ),
      class = c("biomarkerqc_warning_unmapped", "biomarkerqc_warning")
    )
  }
  mapped
}

# "<0.20" -> qualifier "<", value 0.20
parse_result <- function(x, decimal_mark = ".") {
  qualifier <- ifelse(grepl("^[<>]", x), substr(x, 1, 1), NA_character_)
  number <- sub("^[<>]=?\\s*", "", x)
  if (decimal_mark != ".") {
    number <- gsub(decimal_mark, ".", number, fixed = TRUE)
  }
  list(
    value = suppressWarnings(as.numeric(number)),
    qualifier = qualifier
  )
}
