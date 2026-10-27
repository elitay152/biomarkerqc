# Ingest translates formats; it never fixes or drops rows. An unusable file is
# an error; an unmappable value is a warning, and the row is kept with NA.

#' Read one vendor lab file into the standard schema
#'
#' @param path Path to the vendor file.
#' @param vendor A vendor name from [vendor_specs()] (such as `"A"`), or a
#'   spec list with the same structure.
#'
#' @return A data frame with the columns in [standard_columns], one row per
#'   data row in the file. It carries an `"ingest_log"` attribute recording
#'   the file name, its MD5 checksum, the row count and the time of ingest.
#'
#' @section Errors:
#' * `biomarkerqc_error_unknown_vendor`: `vendor` is not a known spec.
#' * `biomarkerqc_error_file_not_found`: `path` does not exist.
#' * `biomarkerqc_error_read`: the file could not be parsed.
#' * `biomarkerqc_error_missing_columns`: required columns are absent.
#'
#' @section Warnings:
#' `biomarkerqc_warning_unmapped` is raised when non-blank values cannot be
#' mapped to a standard subject ID, visit, test or date. Those rows are kept,
#' with `NA` in the affected field.
#'
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

  missing_cols <- setdiff(spec$columns, names(raw))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "{.file {basename(path)}} is missing required vendor {spec$name} column{?s}: {.val {missing_cols}}",
        "i" = "Columns found: {.val {names(raw)}}"
      ),
      class = c("biomarkerqc_error_missing_columns", "biomarkerqc_error")
    )
  }

  col <- function(field) raw[[spec$columns[[field]]]]
  file_name <- basename(path)
  parsed <- parse_result(col("result_raw"), spec$decimal_mark)

  out <- data.frame(
    vendor = rep(spec$name, nrow(raw)),
    subject_id = check_mapped(col("subject_id"), spec$subject_id(col("subject_id")), "subject ID", file_name),
    visit = check_mapped(col("visit"), unname(spec$visits[col("visit")]), "visit", file_name),
    test_code = check_mapped(col("test_code"), unname(spec$tests[col("test_code")]), "test", file_name),
    collection_date = check_mapped(
      col("collection_date"),
      as.Date(strptime(col("collection_date"), spec$date_format, tz = "UTC")),
      "collection date", file_name
    ),
    result_raw = col("result_raw"),
    result = parsed$value,
    qualifier = parsed$qualifier,
    unit_raw = col("unit_raw"),
    source_file = rep(file_name, nrow(raw)),
    source_line = seq_len(nrow(raw)) + 1L,
    stringsAsFactors = FALSE
  )

  attr(out, "ingest_log") <- data.frame(
    vendor = spec$name,
    source_file = file_name,
    md5 = unname(tools::md5sum(path)),
    rows = nrow(out),
    ingested_at = Sys.time(),
    stringsAsFactors = FALSE
  )
  out
}

#' Ingest several vendor files into one table
#'
#' @param files A named character vector of file paths, where each name is the
#'   vendor (for example `c(A = "a.csv", B = "b.csv")`).
#' @param specs A named list of vendor specs. Defaults to [vendor_specs()].
#'
#' @return One data frame in the standard schema, with an `"ingest_log"`
#'   attribute holding one row per file.
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
        "Every file must be named with its vendor.",
        "i" = 'For example: {.code ingest_lab_files(c(A = "a.csv", B = "b.csv"))}'
      ),
      class = c("biomarkerqc_error_unnamed_files", "biomarkerqc_error")
    )
  }

  parts <- lapply(names(files), function(v) {
    spec <- specs[[v]]
    if (is.null(spec)) spec <- v # let resolve_spec() raise the standard error
    read_vendor_file(files[[v]], spec)
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

resolve_spec <- function(vendor) {
  if (is.list(vendor)) return(vendor)
  specs <- vendor_specs()
  if (!is.character(vendor) || length(vendor) != 1 || !vendor %in% names(specs)) {
    cli::cli_abort(
      c(
        "Unknown vendor {.val {vendor}}.",
        "i" = "Known vendors: {.val {names(specs)}}. Or pass a spec list."
      ),
      class = c("biomarkerqc_error_unknown_vendor", "biomarkerqc_error")
    )
  }
  specs[[vendor]]
}

# Blank source values are not warned about: missing data is a QC finding.
check_mapped <- function(source, mapped, what, file_name) {
  failed <- !is.na(source) & is.na(mapped)
  if (any(failed)) {
    bad <- unique(source[failed])
    shown <- utils::head(bad, 5)
    more <- if (length(bad) > 5) sprintf(" and %d more", length(bad) - 5) else ""
    cli::cli_warn(
      c(
        "{sum(failed)} row{?s} in {.file {file_name}} had {what} values that could not be mapped; set to NA.",
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
