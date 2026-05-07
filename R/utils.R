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
