#' Generate a PDF certificate
#'
#' Renders a single landscape PDF certificate with decorative borders,
#' corner ornaments, optional laurels around the recipient name, and one
#' or two signature blocks. The output is drawn entirely with the
#' `grid` graphics system; no LaTeX or Quarto installation is required.
#'
#' @section Layout:
#' The page is US-letter landscape by default (11 x 8.5 inches). Pass
#' `width` and `height` to render any other size. The header region
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
#' @param theme A `cert_theme` object controlling colors. See
#'   [cert_theme()] and the preset variants.
#' @param width,height Page dimensions in inches. Defaults give US
#'   letter landscape (11 x 8.5).
#' @param show_laurels Logical; if `TRUE` (the default), draw decorative
#'   laurel sprays flanking the recipient name.
#'
#' @return The output `path`, returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' make_certificate(
#'   path         = "jane_doe.pdf",
#'   recipient    = "Jane A. Doe",
#'   title        = "Certificate of Achievement",
#'   award_name   = "Outstanding Student",
#'   organization = "University of Somewhere",
#'   citation     = "in recognition of exceptional dedication and scholarship.",
#'   academic_year = "2025-2026",
#'   signers      = list(
#'     list(name = "Alex Chair, Ph.D.", title = "Department Chair")
#'   )
#' )
#' }
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
                             theme          = cert_theme_classic(),
                             width          = 11,
                             height         = 8.5,
                             show_laurels   = TRUE) {

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

  PAGE_W <- width
  PAGE_H <- height

  grDevices::cairo_pdf(path, width = PAGE_W, height = PAGE_H)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()

  # Page background
  grid::grid.draw(make_rect(0, 0, PAGE_W, PAGE_H,
                            color = NA, fill = theme$background))

  # Borders: outer (primary), middle (accent), inner hairline
  m  <- 0.45
  io <- 0.18
  grid::grid.draw(make_rect(m, m, PAGE_W - m, PAGE_H - m,
                            color = theme$primary, fill = NA, lwd = 4))
  grid::grid.draw(make_rect(m + 0.05, m + 0.05,
                            PAGE_W - m - 0.05, PAGE_H - m - 0.05,
                            color = theme$accent, fill = NA, lwd = 0.8))
  grid::grid.draw(make_rect(m + io, m + io,
                            PAGE_W - m - io, PAGE_H - io - m,
                            color = theme$hairline, fill = NA, lwd = 0.5))

  # Corner ornaments
  corners <- list(
    c(m + io,           m + io,           1,  1),
    c(PAGE_W - m - io,  m + io,          -1,  1),
    c(m + io,           PAGE_H - m - io,  1, -1),
    c(PAGE_W - m - io,  PAGE_H - m - io, -1, -1)
  )
  for (cc in corners) {
    for (g in draw_corner(cc[1], cc[2], cc[3], cc[4], theme = theme)) {
      grid::grid.draw(g)
    }
  }

  # Header: logo > organization > nothing
  has_logo <- !is.null(logo) && nzchar(logo)
  has_org  <- is.null(logo) && !is.null(organization) && nzchar(organization)

  if (has_logo) {
    logo_h     <- 2.00
    logo_top_y <- PAGE_H - 0.70
    grid::grid.draw(draw_logo(logo, PAGE_W / 2, logo_top_y, logo_h))
    div_y <- logo_top_y - logo_h - 0.18
  } else if (has_org) {
    org_y <- PAGE_H - 1.05
    grid::grid.text(
      toupper(organization),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(org_y, "in"),
      gp = grid::gpar(col = theme$primary, fontface = "bold",
                      fontfamily = "serif", fontsize = 22)
    )
    div_y <- org_y - 0.45
  } else {
    div_y <- PAGE_H - 1.10
  }

  # Ornamental divider
  grid::grid.segments(
    grid::unit(PAGE_W / 2 - 1.85, "in"), grid::unit(div_y, "in"),
    grid::unit(PAGE_W / 2 - 0.14, "in"), grid::unit(div_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 0.9)
  )
  grid::grid.segments(
    grid::unit(PAGE_W / 2 + 0.14, "in"), grid::unit(div_y, "in"),
    grid::unit(PAGE_W / 2 + 1.85, "in"), grid::unit(div_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 0.9)
  )
  grid::grid.polygon(
    x  = grid::unit(PAGE_W / 2 + c(0, 0.055, 0, -0.055), "in"),
    y  = grid::unit(div_y + c(0.055, 0, -0.055, 0), "in"),
    gp = grid::gpar(col = NA, fill = theme$primary)
  )

  # Title
  title_y <- div_y - 0.50
  grid::grid.text(
    title,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(title_y, "in"),
    gp = grid::gpar(col = theme$primary, fontface = "bold",
                    fontfamily = "serif", fontsize = 32)
  )

  # Optional award subtitle (small caps feel)
  cursor_y <- title_y
  if (!is.null(award_name) && nzchar(award_name)) {
    cursor_y <- cursor_y - 0.44
    grid::grid.text(
      toupper(award_name),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(cursor_y, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "bold",
                      fontfamily = "serif", fontsize = 14)
    )
  }

  # Optional program line (italic)
  if (!is.null(program) && nzchar(program)) {
    cursor_y <- cursor_y - 0.30
    grid::grid.text(
      program,
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(cursor_y, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 13.5)
    )
  }

  # Presented-to lead-in
  presented_y <- cursor_y - 0.34
  grid::grid.text(
    presented_text,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(presented_y, "in"),
    gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                    fontfamily = "serif", fontsize = 12)
  )

  # Recipient
  name_y <- presented_y - 0.55
  grid::grid.text(
    recipient,
    x  = grid::unit(PAGE_W / 2, "in"),
    y  = grid::unit(name_y, "in"),
    gp = grid::gpar(col = theme$ink, fontface = "bold",
                    fontfamily = "serif", fontsize = 30)
  )

  grid::grid.segments(
    grid::unit(PAGE_W / 2 - 2.8, "in"), grid::unit(name_y - 0.27, "in"),
    grid::unit(PAGE_W / 2 + 2.8, "in"), grid::unit(name_y - 0.27, "in"),
    gp = grid::gpar(col = theme$hairline, lwd = 0.7)
  )

  if (isTRUE(show_laurels)) {
    for (g in draw_laurel(PAGE_W / 2 - 3.15, name_y + 0.08,
                          side = "left", theme = theme))  grid::grid.draw(g)
    for (g in draw_laurel(PAGE_W / 2 + 3.15, name_y + 0.08,
                          side = "right", theme = theme)) grid::grid.draw(g)
  }

  # Citation
  if (nzchar(citation)) {
    grid::grid.text(
      wrap_text(citation, 82),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(name_y - 0.78, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 12.5)
    )
  }

  # Academic year
  if (!is.null(academic_year) && nzchar(academic_year)) {
    grid::grid.text(
      paste0("Academic Year  ", academic_year),
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(name_y - 1.40, "in"),
      gp = grid::gpar(col = theme$primary_dark, fontface = "bold",
                      fontfamily = "serif", fontsize = 11)
    )
  }

  # Signatures
  sig_y <- 1.15
  if (length(signers) == 2L) {
    draw_sig(PAGE_W * 0.32, sig_y,
             signers[[1]]$name, signers[[1]]$title, theme = theme)
    draw_sig(PAGE_W * 0.68, sig_y,
             signers[[2]]$name, signers[[2]]$title, theme = theme)
    grid::grid.text(
      date_str,
      x  = grid::unit(PAGE_W / 2, "in"),
      y  = grid::unit(sig_y - 0.08, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 10.5)
    )
  } else {
    draw_sig(PAGE_W * 0.30, sig_y,
             signers[[1]]$name, signers[[1]]$title, theme = theme)
    grid::grid.text(
      date_str,
      x  = grid::unit(PAGE_W * 0.70, "in"),
      y  = grid::unit(sig_y + 0.20, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 11)
    )
    grid::grid.segments(
      grid::unit(PAGE_W * 0.70 - 0.9, "in"), grid::unit(sig_y + 0.03, "in"),
      grid::unit(PAGE_W * 0.70 + 0.9, "in"), grid::unit(sig_y + 0.03, "in"),
      gp = grid::gpar(col = theme$ink, lwd = 0.6)
    )
    grid::grid.text(
      "Date",
      x  = grid::unit(PAGE_W * 0.70, "in"),
      y  = grid::unit(sig_y - 0.14, "in"),
      gp = grid::gpar(col = theme$soft_ink,
                      fontfamily = "serif", fontsize = 9.5)
    )
  }

  invisible(path)
}

draw_sig <- function(cx, y, name, title, theme) {
  grid::grid.segments(
    grid::unit(cx - 1.2, "in"), grid::unit(y + 0.03, "in"),
    grid::unit(cx + 1.2, "in"), grid::unit(y + 0.03, "in"),
    gp = grid::gpar(col = theme$ink, lwd = 0.6)
  )
  if (!is.null(name) && nzchar(name)) {
    grid::grid.text(
      name,
      x  = grid::unit(cx, "in"),
      y  = grid::unit(y - 0.14, "in"),
      gp = grid::gpar(col = theme$ink, fontface = "bold",
                      fontfamily = "serif", fontsize = 11)
    )
  }
  if (!is.null(title) && nzchar(title)) {
    grid::grid.text(
      title,
      x  = grid::unit(cx, "in"),
      y  = grid::unit(y - 0.31, "in"),
      gp = grid::gpar(col = theme$soft_ink, fontface = "italic",
                      fontfamily = "serif", fontsize = 9.5)
    )
  }
}
