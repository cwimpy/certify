
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# certify <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/certify)](https://CRAN.R-project.org/package=certify)
[![R-CMD-check](https://github.com/cwimpy/certify/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cwimpy/certify/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`certify` produces formal landscape PDF certificates with configurable
color themes, optional logos, decorative borders, laurels, and corner
ornaments. The package is built on `grid` only, and no LaTeX or Quarto
installation is required.

![](man/figures/README-preview.png)

## Installation

Install the released version from CRAN:

``` r
install.packages("certify")
```

Or install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("cwimpy/certify")
```

## Usage

``` r
library(certify)

make_certificate(
  path          = "jane_doe.pdf",
  recipient     = "Jane A. Doe",
  title         = "Certificate of Achievement",
  award_name    = "Outstanding Student",
  organization  = "University of Somewhere",
  program       = "Bachelor of Arts in Political Science",
  citation      = paste(
    "in recognition of exceptional dedication, scholarship,",
    "and service to the academic community."
  ),
  academic_year = "2025-2026",
  signers       = list(
    list(name = "Alex Chair, Ph.D.", title = "Department Chair")
  )
)
```

## Themes

Three preset palettes are included:

- `cert_theme_classic()` — navy and gold on parchment (default)
- `cert_theme_formal()` — black and silver on cream
- `cert_theme_warm()` — burgundy and gold on warm cream

For full control, build a custom palette with `cert_theme()`:

``` r
my_theme <- cert_theme(
  primary      = "#003366",
  primary_dark = "#001f3f",
  accent       = "#c0a062",
  background   = "#fbf8f1"
)

make_certificate(
  path      = "custom.pdf",
  recipient = "Recipient Name",
  theme     = my_theme
)
```

## Two signers, custom logo, custom page size

``` r
make_certificate(
  path      = "award.pdf",
  recipient = "Recipient Name",
  title     = "Certificate of Recognition",
  citation  = "in grateful recognition of years of devoted service.",
  signers   = list(
    list(name = "President Name", title = "President"),
    list(name = "Secretary Name", title = "Secretary")
  ),
  logo      = "path/to/your_logo.png",
  width     = 11.69,    # A4 landscape
  height    = 8.27,
  theme     = cert_theme_warm()
)
```

## License

MIT © Cameron Wimpy
