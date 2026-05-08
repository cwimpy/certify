# Build the certify hex sticker.
# Run from the package root:
#   Rscript inst/sticker/build-sticker.R

library(hexSticker)
library(showtext)
library(sysfonts)
library(grid)

# Use a serif face that complements the certificates themselves
font_add_google("Cormorant Garamond", "cert_serif")
showtext_auto()

# Palette pulled from cert_theme_classic()
NAVY  <- "#1a3a5c"
NAVY2 <- "#0f2540"
GOLD  <- "#b08d3e"
GOLD2 <- "#d4b26a"
PARCH <- "#fbf8f1"
PARC2 <- "#f0e7d2"

# Subplot: a miniature certificate echoing the real layout
mini_cert <- function() {
  gTree(children = gList(
    # Outer parchment
    rectGrob(x = 0.5, y = 0.5, width = 0.92, height = 0.74,
             gp = gpar(fill = PARC2, col = NA)),
    # Outer gold border
    rectGrob(x = 0.5, y = 0.5, width = 0.92, height = 0.74,
             gp = gpar(fill = NA, col = GOLD, lwd = 2.6)),
    # Inner thin border
    rectGrob(x = 0.5, y = 0.5, width = 0.84, height = 0.66,
             gp = gpar(fill = NA, col = GOLD2, lwd = 0.9)),

    # Two short gold rules with a diamond between (the title divider motif)
    segmentsGrob(x0 = 0.20, y0 = 0.74, x1 = 0.46, y1 = 0.74,
                 gp = gpar(col = GOLD, lwd = 1.4)),
    segmentsGrob(x0 = 0.54, y0 = 0.74, x1 = 0.80, y1 = 0.74,
                 gp = gpar(col = GOLD, lwd = 1.4)),
    polygonGrob(
      x = 0.5 + c(0, 0.020, 0, -0.020),
      y = 0.74 + c(0.022, 0, -0.022, 0),
      gp = gpar(col = NA, fill = NAVY)
    ),

    # Recipient line (bold)
    segmentsGrob(x0 = 0.22, y0 = 0.58, x1 = 0.78, y1 = 0.58,
                 gp = gpar(col = NAVY2, lwd = 1.4)),

    # Soft underline beneath name
    segmentsGrob(x0 = 0.18, y0 = 0.52, x1 = 0.82, y1 = 0.52,
                 gp = gpar(col = GOLD2, lwd = 0.6)),

    # Citation lines (faint)
    segmentsGrob(x0 = 0.24, y0 = 0.43, x1 = 0.76, y1 = 0.43,
                 gp = gpar(col = "#9a9a9a", lwd = 0.5)),
    segmentsGrob(x0 = 0.28, y0 = 0.38, x1 = 0.72, y1 = 0.38,
                 gp = gpar(col = "#9a9a9a", lwd = 0.5)),

    # Signature lines bottom
    segmentsGrob(x0 = 0.18, y0 = 0.27, x1 = 0.36, y1 = 0.27,
                 gp = gpar(col = NAVY2, lwd = 0.6)),
    segmentsGrob(x0 = 0.64, y0 = 0.27, x1 = 0.82, y1 = 0.27,
                 gp = gpar(col = NAVY2, lwd = 0.6))
  ))
}

out_dir   <- "inst/sticker"
logo_path <- "man/figures/logo.png"
dir.create(out_dir,                showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(logo_path),     showWarnings = FALSE, recursive = TRUE)

sticker(
  subplot  = mini_cert(),
  s_x      = 1,    s_y      = 1.10,
  s_width  = 1.35, s_height = 0.95,
  package  = "certify",
  p_x      = 1,    p_y      = 0.55,
  p_color  = NAVY,
  p_family = "cert_serif",
  p_size   = 24,
  p_fontface = "italic",
  h_fill   = PARCH,
  h_color  = NAVY,
  h_size   = 1.8,
  filename = logo_path,
  dpi      = 600
)

# Also drop a copy in inst/sticker for archival
file.copy(logo_path, file.path(out_dir, "certify-hex.png"), overwrite = TRUE)

cat("Wrote:\n  ", logo_path, "\n  ",
    file.path(out_dir, "certify-hex.png"), "\n", sep = "")
