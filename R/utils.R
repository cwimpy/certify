# Internal helpers shared across the package.
# Not exported; not part of the public API.

wrap_text <- function(text, width = 78) {
  paste(strwrap(text, width = width), collapse = "\n")
}

make_rect <- function(xmin, ymin, xmax, ymax, color, fill = NA, lwd = 1) {
  grid::rectGrob(
    x      = grid::unit(xmin, "in"),
    y      = grid::unit(ymin, "in"),
    width  = grid::unit(xmax - xmin, "in"),
    height = grid::unit(ymax - ymin, "in"),
    just   = c("left", "bottom"),
    gp     = grid::gpar(col = color, fill = fill, lwd = lwd)
  )
}

# Open the PDF device, preferring cairo but surviving its absence.
#
# `capabilities("cairo")` can report TRUE on machines where the cairo module
# still fails to load at run time (macOS without XQuartz is the usual case),
# and the failure arrives as a warning rather than an error, so a plain
# tryCatch is not enough. Warnings during the open are treated as failure and
# the base pdf device is used instead.
cert_open_device <- function(path, width, height, device = "auto") {
  open_cairo <- function()
    grDevices::cairo_pdf(path, width = width, height = height)
  open_base <- function()
    grDevices::pdf(path, width = width, height = height)

  if (identical(device, "cairo_pdf")) {
    open_cairo()
    return("cairo_pdf")
  }
  if (identical(device, "pdf")) {
    open_base()
    return("pdf")
  }

  if (!isTRUE(capabilities("cairo"))) {
    open_base()
    return("pdf")
  }

  before <- unname(grDevices::dev.cur())
  failed <- FALSE
  withCallingHandlers(
    tryCatch(open_cairo(), error = function(e) failed <<- TRUE),
    warning = function(w) {
      failed <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  if (!failed) {
    return("cairo_pdf")
  }

  # cairo may have left a half-open device behind; close it before retrying.
  if (unname(grDevices::dev.cur()) != before) grDevices::dev.off()
  open_base()
  "pdf"
}

# Named paper sizes, landscape, in inches.
CERT_PAPER_SIZES <- list(
  letter  = c(11.00,  8.50),
  legal   = c(14.00,  8.50),
  tabloid = c(17.00, 11.00),
  a3      = c(16.54, 11.69),
  a4      = c(11.69,  8.27),
  a5      = c(8.27,   5.83)
)

cert_paper_size <- function(paper) {
  if (!is.character(paper) || length(paper) != 1L) {
    stop("`paper` must be a single string, one of: ",
         paste(names(CERT_PAPER_SIZES), collapse = ", "), call. = FALSE)
  }
  key <- tolower(trimws(paper))
  if (!key %in% names(CERT_PAPER_SIZES)) {
    stop("Unknown paper size: ", paper, "\n",
         "  Choose one of: ", paste(names(CERT_PAPER_SIZES), collapse = ", "),
         "\n  or pass `width` and `height` in inches instead.", call. = FALSE)
  }
  CERT_PAPER_SIZES[[key]]
}

# How much to scale the design, which is drawn for an 11 x 8.5 page. Taking
# the smaller of the two ratios keeps the proportions and guarantees the
# design still fits; exactly 1 at the default size.
cert_scale <- function(width, height) {
  min(width / 11, height / 8.5)
}
