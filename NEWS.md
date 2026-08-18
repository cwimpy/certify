# certify 0.2.0

* New `make_certificates()` (plural) builds a whole batch from a data frame,
  one PDF per row. Columns named after `make_certificate()` arguments vary
  per certificate, anything passed through `...` stays constant, file names
  are derived from the recipient, and duplicate names are given a numeric
  suffix rather than overwriting each other. No extra packages required.
* New `paper` argument on `make_certificate()`: `"letter"` (default),
  `"legal"`, `"tabloid"`, `"a3"`, `"a4"`, `"a5"`. The design is scaled
  proportionally to the page rather than staying letter-sized on a larger
  sheet, so type and ornaments grow with the paper. Output at the default
  11 x 8.5 size is byte-for-byte unchanged. `width` and `height` still work
  and override `paper`.

* Logos are now scaled to fit inside a box rather than forced to a fixed
  2-inch height. Wide wordmark logos previously ran past the decorative
  border; they are now limited by width instead and render shorter. Square
  and vertical logos are unaffected.
* New `logo_height` and `logo_width` arguments to `make_certificate()` set
  that box, defaulting to 2 and 4.5 inches.
* The ornamental divider now sits directly beneath the logo's actual
  rendered height, so a short wide logo no longer leaves a gap.
* `make_certificate()` now picks the PDF device automatically when
  `device` is not supplied: cairo when it is genuinely usable, and the base
  `pdf` device otherwise. Machines that reported cairo as available but
  failed to load it at run time (macOS without XQuartz, most often) produced
  a "failed to load cairo DLL" warning and a broken file; they now just
  work. Passing `device` explicitly is unchanged.
* Logo errors now say which of the three things went wrong. `png::readPNG()`
  reports "unable to open" both for a file that is absent and for one that
  exists but cannot be read, which sent people hunting for a typo when the
  real cause was an OS permission or an undownloaded cloud file. certify now
  distinguishes missing, unreadable, and not-actually-a-PNG.
* New "Getting started with certify" vignette, aimed at readers who use R
  occasionally rather than daily.

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
