# certify 0.1.0

* Initial release.
* `make_certificate()` renders a landscape PDF certificate with
  decorative borders, corner ornaments, optional laurels, an optional
  logo, and one or two signature blocks.
* `cert_theme()` constructs a color palette; convenience presets
  `cert_theme_classic()`, `cert_theme_formal()`, and
  `cert_theme_warm()` cover common looks.
* `device = c("cairo_pdf", "pdf")` argument lets the caller fall back
  to the base `pdf` device on systems without Cairo (e.g. macOS without
  XQuartz, stripped Linux containers).
