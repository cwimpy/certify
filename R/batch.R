#' Generate certificates from a data frame
#'
#' Produces one PDF per row of `data`, for a class roster, an awards
#' ceremony, or any other list of recipients. Columns whose names match
#' arguments of [make_certificate()] are used row by row; anything constant
#' across the whole batch is passed through `...`.
#'
#' @section Columns:
#' A `recipient` column is required unless `recipient` is supplied through
#' `...`. Any other column named after a [make_certificate()] argument is
#' used as well, so a spreadsheet with `recipient`, `award_name`, and
#' `citation` columns gives each person their own award and wording.
#' Columns that do not match an argument are ignored, and the ignored names
#' are reported so a misspelled heading is easy to spot. Empty cells (`NA`)
#' fall back to whatever was passed in `...`, or to the default.
#'
#' If a column and a `...` argument have the same name, the column wins,
#' since per-row values are the more specific of the two.
#'
#' @param data A data frame with one row per certificate.
#' @param dir Directory for the output PDFs. Created if it does not exist.
#' @param filename Optional character vector of file names, one per row. By
#'   default names are derived from `recipient` (`"Jane A. Doe"` becomes
#'   `"jane-a-doe.pdf"`), or taken from a `filename` column if `data` has
#'   one. Duplicates get a numeric suffix so no file is silently overwritten.
#' @param quiet If `TRUE`, suppress the summary message.
#' @param ... Arguments passed to [make_certificate()] for every row, such
#'   as `logo`, `theme`, `signers`, `paper`, or `title`.
#'
#' @return A character vector of the paths written, invisibly.
#' @export
#'
#' @examples
#' roster <- data.frame(
#'   recipient  = c("Jane A. Doe", "John B. Smith"),
#'   award_name = c("Outstanding Student", "Outstanding Service"),
#'   citation   = c("for exceptional scholarship.",
#'                  "for years of devoted service.")
#' )
#'
#' out <- file.path(tempdir(), "certificates")
#' files <- make_certificates(
#'   roster,
#'   dir          = out,
#'   organization = "University of Somewhere",
#'   signers      = list(
#'     list(name = "Alex Chair, Ph.D.", title = "Department Chair")
#'   ),
#'   device       = "pdf",
#'   quiet        = TRUE
#' )
#' basename(files)
make_certificates <- function(data, dir = ".", filename = NULL,
                              quiet = FALSE, ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame, one row per certificate.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` has no rows.", call. = FALSE)
  }

  dots      <- list(...)
  arg_names <- setdiff(names(formals(make_certificate)), "path")
  per_row   <- intersect(names(data), arg_names)

  if (!"recipient" %in% per_row && !"recipient" %in% names(dots)) {
    stop("`data` needs a `recipient` column ",
         "(or pass recipient = ... for every certificate).\n",
         "  Columns found: ", paste(names(data), collapse = ", "),
         call. = FALSE)
  }

  ignored <- setdiff(names(data), c(per_row, "filename"))
  if (length(ignored) && !quiet) {
    message("Ignoring column(s) not used by make_certificate(): ",
            paste(ignored, collapse = ", "))
  }

  clash <- intersect(per_row, names(dots))
  if (length(clash) && !quiet) {
    message("Using the column rather than the argument for: ",
            paste(clash, collapse = ", "))
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  files <- cert_filenames(data, filename, per_row, dots)

  paths <- character(nrow(data))
  for (i in seq_len(nrow(data))) {
    row_args <- list()
    for (cn in per_row) {
      value <- data[[cn]][[i]]
      if (is.factor(value)) value <- as.character(value)
      # An empty cell means "no opinion", so let ... or the default stand.
      if (length(value) == 1L && is.na(value)) next
      row_args[[cn]] <- value
    }
    args      <- utils::modifyList(dots, row_args)
    args$path <- file.path(dir, files[i])
    do.call(make_certificate, args)
    paths[i] <- args$path
  }

  if (!quiet) {
    message("Wrote ", length(paths), " certificate",
            if (length(paths) == 1L) "" else "s", " to ", normalizePath(dir))
  }
  invisible(paths)
}

# Work out one file name per row, keeping them unique.
cert_filenames <- function(data, filename, per_row, dots) {
  n <- nrow(data)

  if (!is.null(filename)) {
    if (length(filename) != n) {
      stop("`filename` must have one entry per row of `data` (",
           n, " needed, ", length(filename), " given).", call. = FALSE)
    }
    out <- as.character(filename)
  } else if ("filename" %in% names(data)) {
    out <- as.character(data$filename)
  } else if ("recipient" %in% per_row) {
    out <- cert_slug(as.character(data$recipient))
  } else {
    out <- sprintf("certificate-%02d", seq_len(n))
  }

  out[is.na(out) | !nzchar(out)] <- sprintf(
    "certificate-%02d", which(is.na(out) | !nzchar(out))
  )
  out <- ifelse(grepl("\\.pdf$", out, ignore.case = TRUE), out,
                paste0(out, ".pdf"))

  # Two people can share a name; do not let one quietly overwrite the other.
  dup <- duplicated(out)
  while (any(dup)) {
    stem <- sub("\\.pdf$", "", out[dup], ignore.case = TRUE)
    out[dup] <- paste0(stem, "-", seq_len(sum(dup)) + 1L, ".pdf")
    dup <- duplicated(out)
  }
  out
}

# "Jane A. Doe" -> "jane-a-doe"
cert_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-|-$", "", x)
}
