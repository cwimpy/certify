#' Generate a PDF certificate
#'
#' Renders a single landscape PDF certificate with decorative borders,
#' corner ornaments, optional laurels around the recipient name, and one
#' or two signature blocks. The output is drawn entirely with the
#' `grid` graphics system; no LaTeX or Quarto installation is required.
#'
#' @section Layout:
#' The page is US-letter landscape by default (11 x 8.5 inches), which
#' prints as-is on standard paper at 100% scale. Pass `paper` for another
#' named size, or `width` and `height` for an arbitrary one. The header region
#' shows either an image (if `logo` is supplied) or a wordmark
#' constructed from `organization`. Below the header, the certificate
#' shows a divider, the title, an optional award subtitle and program
#' line, the recipient name, the citation, an optional academic year,
#' and the signature block(s).
#'
#' @param path File path for the output PDF. Required.
#' @param recipient Name of the person receiving the certificate.
#' @param title Main heading. Defaults to `"Certificate of Achievement"`.
#' @param award_name Optional subtitle, rendered in upper-case (for
#'   example, `"Outstanding Graduate"`).
#' @param organization Optional name of the issuing organization. Used
#'   in the header when no `logo` is supplied.
#' @param program Optional italic line shown beneath `award_name` (for
#'   example, `"Bachelor of Arts in Political Science"`).
#' @param presented_text Lead-in text immediately above the recipient
#'   name. Defaults to `"This certificate is proudly presented to"`.
#' @param citation Italic citation paragraph displayed beneath the
#'   recipient name. Will be soft-wrapped to fit the page.
#' @param academic_year Optional academic-year line (for example,
#'   `"2025-2026"`).
#' @param date_str Date displayed in the signature block. Defaults to
#'   the current month and year.
#' @param signers A list of one or two signers. Each signer is itself a
#'   list with `name` and `title` character entries. With one signer the
#'   layout shows a date line on the right; with two signers the date
#'   appears centered below the signature row.
#' @param logo Optional path to a PNG logo. When provided, the image is
#'   centered in the header and `organization` is ignored.
#' @param logo_height,logo_width Maximum logo size in inches. The image is
#'   scaled to fit inside this box without distorting its proportions, so
#'   whichever limit it reaches first wins: a tall or squarish logo is
#'   limited by `logo_height` (2 inches by default), while a wide wordmark
#'   is limited by `logo_width` (4.5 inches by default) and renders shorter
#'   than `logo_height`. Raise `logo_width` to let a wide logo run closer to
#'   the border, or lower both to make any logo smaller.
#' @param theme A `cert_theme` object controlling colors. See
#'   [cert_theme()] and the preset variants.
#' @param paper Named paper size, matched case-insensitively: `"letter"`
#'   (the default), `"legal"`, `"tabloid"`, `"a3"`, `"a4"`, or `"a5"`. All
#'   are landscape. The design is drawn for an 11 x 8.5 page and scaled
#'   proportionally to whatever size is chosen, so type and ornament sizes
#'   grow with the paper rather than staying letter-sized on a large sheet.
#' @param width,height Page dimensions in inches, overriding `paper` when
#'   supplied. Leave as `NULL` to use the named size.
#' @param show_laurels Logical; if `TRUE` (the default), draw decorative
#'   laurel sprays flanking the recipient name.
#' @param device Either `"cairo_pdf"` or `"pdf"`. If left unset, cairo is
#'   used when it is actually usable and the base `pdf` device is used
#'   otherwise, so no argument is needed on machines missing Cairo. Cairo
#'   renders Unicode glyphs (en-dashes, em-dashes, smart quotes) cleanly
#'   and is the recommended choice. The base `"pdf"` device is more
#'   portable — it has no system dependency on Cairo / XQuartz — but
#'   silently downgrades non-ASCII characters to hyphens.
#'   Passing `"cairo_pdf"` explicitly skips the fallback and will fail on
#'   systems without Cairo (some macOS installations without XQuartz,
#'   stripped Linux containers, etc.).
#'
#' @return The output `path`, returned invisibly.
#' @export
#'
#' @examples
#' out <- tempfile("certify_", fileext = ".pdf")
#' make_certificate(
#'   path          = out,
#'   recipient     = "Jane A. Doe",
#'   title         = "Certificate of Achievement",
#'   award_name    = "Outstanding Student",
#'   organization  = "University of Somewhere",
#'   citation      = "in recognition of exceptional dedication and scholarship.",
#'   academic_year = "2025-2026",
#'   signers       = list(
#'     list(name = "Alex Chair, Ph.D.", title = "Department Chair")
#'   ),
#'   device        = "pdf"   # omit this to pick the best device available
#' )
#' file.exists(out)
make_certificate <- function(path,
                             recipient,
                             title          = "Certificate of Achievement",
                             award_name     = NULL,
                             organization   = NULL,
                             program        = NULL,
                             presented_text = "This certificate is proudly presented to",
                             citation       = "",
                             academic_year  = NULL,
                             date_str       = format(Sys.Date(), "%B %Y"),
                             signers        = list(list(name = "", title = "")),
                             logo           = NULL,
                             logo_height    = 2,
                             logo_width     = 4.5,
                             theme          = cert_theme_classic(),
                             paper          = "letter",
                             width          = NULL,
                             height         = NULL,
                             show_laurels   = TRUE,
                             device         = c("cairo_pdf", "pdf")) {

  if (missing(path) || !nzchar(path)) {
    stop("`path` is required.", call. = FALSE)
  }
  if (missing(recipient) || !nzchar(recipient)) {
    stop("`recipient` is required.", call. = FALSE)
  }
  if (!inherits(theme, "cert_theme")) {
    stop("`theme` must be a cert_theme object (see ?cert_theme).",
         call. = FALSE)
  }
  if (length(signers) < 1L || length(signers) > 2L) {
    stop("`signers` must be a list of 1 or 2 signers.", call. = FALSE)
  }
  if (!is.numeric(logo_height) || length(logo_height) != 1L ||
      is.na(logo_height) || logo_height <= 0) {
    stop("`logo_height` must be a single positive number (inches).",
         call. = FALSE)
  }
  if (!is.numeric(logo_width) || length(logo_width) != 1L ||
      is.na(logo_width) || logo_width <= 0) {
    stop("`logo_width` must be a single positive number (inches).",
         call. = FALSE)
  }
  device <- if (missing(device)) "auto" else match.arg(device)

  dims   <- cert_paper_size(paper)
  PAGE_W <- if (is.null(width))  dims[1] else width
  PAGE_H <- if (is.null(height)) dims[2] else height

  # Everything below is expressed in inches on an 11 x 8.5 page and then
  # multiplied by `s`, so the design keeps its proportions on other paper
  # sizes. s is exactly 1 at the default size, leaving that output untouched.
  s <- cert_scale(PAGE_W, PAGE_H)

  cert_open_device(path, PAGE_W, PAGE_H, device)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()

  # Page background
  grid::grid.draw(make_rect(0, 0, PAGE_W, PAGE_H,
                            color = NA, fill = theme$background))

  # Borders: outer (primary), middle (accent), inner hairline
  m  <- 0.45 * s
  io <- 0.18 * s
  grid::grid.draw(make_rect(m, m, PAGE_W - m, PAGE_H - m,
                            color = theme$primary, fill = NA, lwd = 4 * s))
  grid::grid.draw(make_rect(m + 0.05 * s, m + 0.05 * s,
                            PAGE_W - m - 0.05 * s, PAGE_H - m - 0.05 * s,
                            color = theme$accent, fill = NA, lwd = 0.8 * s))
  grid::grid.draw(make_rect(m + io, m + io,
                            PAGE_W - m - io, PAGE_H - io - m,
                            color = theme$hairline, fill = NA, lwd = 0.5 * s))

  # Corner ornaments
  corners <- list(
    c(m + io,           m + io,           1,  1),
    c(PAGE_W - m - io,  m + io,          -1,  1),
    c(m + io,           PAGE_H - m - io,  1, -1),
    c(PAGE_W - m - io,  PAGE_H - m - io, -1, -1)
  )
  for (cc in corners) {
    for (g in draw_corner(cc[1], cc[2], cc[3], cc[4], theme = theme, scale = s)) {
      grid::grid.draw(g)
    }
  }

  # Header: logo > organization > nothing
  has_logo <- !is.null(logo) && nzchar(logo)
  has_org  <- is.null(logo) && !is.null(organization) && nzchar(organization)

  if (has_logo) {
    logo_top_y <- PAGE_H - 0.70 * s
    fitted <- draw_logo(logo, PAGE_W / 2, logo_top_y,
                        max_h = logo_height * s, max_w = logo_width * s)
    grid::grid.draw(fitted$grob)
    # Follow the height the logo actually used, so a short wide logo does not
    # leave a gap between itself and the divider.
    div_y <- logo_top_y - fitted$height - 0.18 * s
  } else if (has_org) {
    org_y <- PAGE_H - 1.05 * s
    grid::grid.text(
      toupper(organization),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(org_y, "in"),
      gp = grid::gpar(col = theme$primary, fontface = "bold",
                      fontfamily = "serif", fontsize = 22 * s)
    )
    div_y <- org_y - 0.45 * s
  } else {
    div_y <- PAGE_H - 1.10 * s
  }

  # Ornamental divider
  grid::grid.segments(
    grid::unit(PAGE_W / 2 - 1.85 * s, "in"), grid::unit(div_y, "in"),
    grid::unit(PAGE_W / 2 - 0.14 * s, "in"), grid::unit(div_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 0.9 * s)
  )
  grid::grid.segments(
    grid::unit(PAGE_W / 2 + 0.14 * s, "in"), grid::unit(div_y, "in"),
    grid::unit(PAGE_W / 2 + 1.85 * s, "in"), grid::unit(div_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 0.9 * s)
  )
  grid::grid.polygon(
    x  = grid::unit(PAGE_W / 2 + c(0, 0.055, 0, -0.055) * s, "in"),
    y  = grid::unit(div_y + c(0.055, 0, -0.055, 0) * s, "in"),
    gp = grid::gpar(col = NA, fill = theme$primary)
  )

  # Title
  title_y <- div_y - 0.50 * s
  grid::grid.text(
    title,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(title_y, "in"),
    gp = grid::gpar(col = theme$primary, fontface = "bold",
                    fontfamily = "serif", fontsize = 32 * s)
  )

  # Optional award subtitle (small caps feel)
  cursor_y <- title_y
  if (!is.null(award_name) && nzchar(award_name)) {
    cursor_y <- cursor_y - 0.44 * s
    grid::grid.text(
      toupper(award_name),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(cursor_y, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "bold",
                      fontfamily = "serif", fontsize = 14 * s)
    )
  }

  # Optional program line (italic)
  if (!is.null(program) && nzchar(program)) {
    cursor_y <- cursor_y - 0.30 * s
    grid::grid.text(
      program,
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(cursor_y, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 13.5 * s)
    )
  }

  # Presented-to lead-in
  presented_y <- cursor_y - 0.34 * s
  grid::grid.text(
    presented_text,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(presented_y, "in"),
    gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                    fontfamily = "serif", fontsize = 12 * s)
  )

  # Recipient
  name_y <- presented_y - 0.55 * s
  grid::grid.text(
    recipient,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(name_y, "in"),
    gp = grid::gpar(col = theme$ink, fontface = "bold",
                    fontfamily = "serif", fontsize = 30 * s)
  )

  grid::grid.segments(
    grid::unit(PAGE_W / 2 - 2.8 * s, "in"), grid::unit(name_y - 0.27 * s, "in"),
    grid::unit(PAGE_W / 2 + 2.8 * s, "in"), grid::unit(name_y - 0.27 * s, "in"),
    gp = grid::gpar(col = theme$hairline, lwd = 0.7 * s)
  )

  if (isTRUE(show_laurels)) {
    for (g in draw_laurel(PAGE_W / 2 - 3.15 * s, name_y + 0.08 * s,
                          side = "left", theme = theme, scale = s))
      grid::grid.draw(g)
    for (g in draw_laurel(PAGE_W / 2 + 3.15 * s, name_y + 0.08 * s,
                          side = "right", theme = theme, scale = s))
      grid::grid.draw(g)
  }

  # Citation
  if (nzchar(citation)) {
    grid::grid.text(
      wrap_text(citation, 82),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(name_y - 0.78 * s, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 12.5 * s)
    )
  }

  # Academic year
  if (!is.null(academic_year) && nzchar(academic_year)) {
    grid::grid.text(
      paste0("Academic Year  ", academic_year),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(name_y - 1.40 * s, "in"),
      gp = grid::gpar(col = theme$primary_dark, fontface = "bold",
                      fontfamily = "serif", fontsize = 11 * s)
    )
  }

  # Signatures
  sig_y <- 1.15 * s
  if (length(signers) == 2L) {
    draw_sig(PAGE_W * 0.32, sig_y,
             signers[[1]]$name, signers[[1]]$title, theme = theme, scale = s)
    draw_sig(PAGE_W * 0.68, sig_y,
             signers[[2]]$name, signers[[2]]$title, theme = theme, scale = s)
    grid::grid.text(
      date_str,
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(sig_y - 0.08 * s, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 10.5 * s)
    )
  } else {
    draw_sig(PAGE_W * 0.30, sig_y,
             signers[[1]]$name, signers[[1]]$title, theme = theme, scale = s)
    grid::grid.text(
      date_str,
      x  = grid::unit(PAGE_W * 0.70, "in"),
      y  = grid::unit(sig_y + 0.20 * s, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 11 * s)
    )
    grid::grid.segments(
      grid::unit(PAGE_W * 0.70 - 0.9 * s, "in"),
      grid::unit(sig_y + 0.03 * s, "in"),
      grid::unit(PAGE_W * 0.70 + 0.9 * s, "in"),
      grid::unit(sig_y + 0.03 * s, "in"),
      gp = grid::gpar(col = theme$ink, lwd = 0.6 * s)
    )
    grid::grid.text(
      "Date",
      x  = grid::unit(PAGE_W * 0.70, "in"),
      y  = grid::unit(sig_y - 0.14 * s, "in"),
      gp = grid::gpar(col = theme$soft_ink,
                      fontfamily = "serif", fontsize = 9.5 * s)
    )
  }

  invisible(path)
}

draw_sig <- function(cx, y, name, title, theme, scale = 1) {
  grid::grid.segments(
    grid::unit(cx - 1.2 * scale, "in"), grid::unit(y + 0.03 * scale, "in"),
    grid::unit(cx + 1.2 * scale, "in"), grid::unit(y + 0.03 * scale, "in"),
    gp = grid::gpar(col = theme$ink, lwd = 0.6 * scale)
  )
  if (!is.null(name) && nzchar(name)) {
    grid::grid.text(
      name,
      x  = grid::unit(cx, "in"),
      y  = grid::unit(y - 0.14 * scale, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "bold",
                      fontfamily = "serif", fontsize = 11 * scale)
    )
  }
  if (!is.null(title) && nzchar(title)) {
    grid::grid.text(
      title,
      x  = grid::unit(cx, "in"),
      y  = grid::unit(y - 0.31 * scale, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 9.5 * scale)
    )
  }
}
