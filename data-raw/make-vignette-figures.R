# Rebuilds the PNG previews used by vignettes/certify.Rmd.
#
# The vignette itself does not evaluate any certificate code: rendering PDFs
# and converting them to images at build time would drag pdftools onto every
# machine that installs the package. Instead the images are baked here, once,
# and checked in. Run this after any change to the layout, then rebuild the
# vignette.
#
#   Rscript data-raw/make-vignette-figures.R
#
# Requires pdftools (dev-only; not a package dependency).

pkgload::load_all(quiet = TRUE)

fig_dir <- "vignettes/figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

tmp <- tempfile("certify-figs-")
dir.create(tmp)

# --- A neutral stand-in logo -------------------------------------------------
# Deliberately generic. Shipping a real institution's mark inside a package
# would be a trademark problem.
placeholder_logo <- file.path(tmp, "northfield.png")
grDevices::png(placeholder_logo, width = 900, height = 900, bg = "transparent")
grid::grid.newpage()
navy <- "#1a3a5c"
gold <- "#b08d3e"
grid::grid.circle(y = 0.60, r = 0.30,
                  gp = grid::gpar(col = navy, fill = NA, lwd = 26))
grid::grid.circle(y = 0.60, r = 0.255,
                  gp = grid::gpar(col = gold, fill = NA, lwd = 6))
grid::grid.text("NC", y = 0.60,
                gp = grid::gpar(col = navy, fontfamily = "serif",
                                fontface = "bold", cex = 11))
grid::grid.text("NORTHFIELD COLLEGE", y = 0.16,
                gp = grid::gpar(col = navy, fontfamily = "serif",
                                fontface = "bold", cex = 5.2))
grDevices::dev.off()

file.copy(placeholder_logo, file.path(fig_dir, "example-logo.png"),
          overwrite = TRUE)

# --- Helper: render a certificate and save it as a PNG preview ---------------
preview <- function(name, dpi = 72, ...) {
  pdf <- file.path(tmp, paste0(name, ".pdf"))
  make_certificate(path = pdf, ...)
  png <- pdftools::pdf_convert(pdf, dpi = dpi,
                               filenames = file.path(tmp, paste0(name, ".png")),
                               verbose = FALSE)
  file.copy(png, file.path(fig_dir, paste0(name, ".png")), overwrite = TRUE)
  invisible(NULL)
}

citation_text <- paste(
  "in recognition of exceptional dedication, scholarship,",
  "and service to the academic community."
)

one_signer <- list(list(name = "Alex Chair, Ph.D.", title = "Department Chair"))

# 1. The plain first certificate, organization name in the header
preview(
  "fig-first",
  recipient     = "Jane A. Doe",
  title         = "Certificate of Achievement",
  award_name    = "Outstanding Student",
  organization  = "Northfield College",
  program       = "Bachelor of Arts in Political Science",
  citation      = citation_text,
  academic_year = "2025-2026",
  signers       = one_signer,
  date_str      = "May 2026"
)

# 2. The same certificate with a logo replacing the organization line
preview(
  "fig-logo",
  recipient     = "Jane A. Doe",
  title         = "Certificate of Achievement",
  award_name    = "Outstanding Student",
  citation      = citation_text,
  academic_year = "2025-2026",
  signers       = one_signer,
  logo          = placeholder_logo,
  date_str      = "May 2026"
)

# 3. Two signers: signatures side by side, date underneath
preview(
  "fig-two-signers",
  recipient = "Jane A. Doe",
  title     = "Certificate of Recognition",
  citation  = "in grateful recognition of years of devoted service.",
  signers   = list(
    list(name = "Dana President", title = "President"),
    list(name = "Sam Secretary",  title = "Secretary")
  ),
  organization = "Northfield College",
  date_str     = "May 2026"
)

# 4. The three preset palettes, at a smaller size since they sit side by side
themes <- list(
  classic = cert_theme_classic(),
  formal  = cert_theme_formal(),
  warm    = cert_theme_warm()
)
for (nm in names(themes)) {
  preview(
    paste0("fig-theme-", nm),
    dpi          = 50,
    recipient    = "Jane A. Doe",
    award_name   = "Outstanding Student",
    organization = "Northfield College",
    citation     = citation_text,
    signers      = one_signer,
    theme        = themes[[nm]],
    date_str     = "May 2026"
  )
}

message("Wrote figures to ", fig_dir, ":")
print(file.info(list.files(fig_dir, full.names = TRUE))["size"])
