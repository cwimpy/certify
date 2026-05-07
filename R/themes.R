#' Build a certificate color theme
#'
#' Constructs a color palette used by [make_certificate()] to render
#' borders, ornaments, and text. Supply your own colors, or use one of
#' the preset themes ([cert_theme_classic()], [cert_theme_formal()],
#' [cert_theme_warm()]).
#'
#' @param primary Border and accent color (any hex or named R color).
#'   Used for the outer border, the title text, and the accent diamond
#'   in the divider.
#' @param primary_dark A darker variant of `primary`, used for the
#'   academic-year line and laurel outlines.
#' @param accent Secondary accent color, typically a metallic such as
#'   gold, bronze, or silver. Used for inner borders and corner arcs.
#' @param accent_light Lighter version of `accent`, used as the leaf
#'   fill in the laurel ornaments.
#' @param ink Main body text color.
#' @param soft_ink Italic / subdued text color (subtitles, dates).
#' @param background Page fill color.
#' @param hairline Thin inner-border and underline color.
#'
#' @return A `cert_theme` object: a named list of color strings.
#' @export
#' @examples
#' my_theme <- cert_theme(primary = "#003366", accent = "#c0a062")
cert_theme <- function(primary      = "#1a3a5c",
                       primary_dark = "#0f2540",
                       accent       = "#b08d3e",
                       accent_light = "#d4b26a",
                       ink          = "#1a1a1a",
                       soft_ink     = "#4a4a4a",
                       background   = "#fbf8f1",
                       hairline     = "#c9a961") {
  structure(
    list(
      primary      = primary,
      primary_dark = primary_dark,
      accent       = accent,
      accent_light = accent_light,
      ink          = ink,
      soft_ink     = soft_ink,
      background   = background,
      hairline     = hairline
    ),
    class = "cert_theme"
  )
}

#' Preset certificate themes
#'
#' Convenience wrappers around [cert_theme()] for common looks.
#'
#' * `cert_theme_classic()` — navy and gold on parchment.
#' * `cert_theme_formal()` — black and silver on cream.
#' * `cert_theme_warm()` — burgundy and gold on warm cream.
#'
#' @return A `cert_theme` object.
#' @name cert_theme_presets
#' @examples
#' th <- cert_theme_classic()
#' th$primary
NULL

#' @rdname cert_theme_presets
#' @export
cert_theme_classic <- function() {
  cert_theme(
    primary      = "#1a3a5c",
    primary_dark = "#0f2540",
    accent       = "#b08d3e",
    accent_light = "#d4b26a",
    ink          = "#1a1a1a",
    soft_ink     = "#4a4a4a",
    background   = "#fbf8f1",
    hairline     = "#c9a961"
  )
}

#' @rdname cert_theme_presets
#' @export
cert_theme_formal <- function() {
  cert_theme(
    primary      = "#1a1a1a",
    primary_dark = "#000000",
    accent       = "#7a7a7a",
    accent_light = "#bcbcbc",
    ink          = "#1a1a1a",
    soft_ink     = "#4a4a4a",
    background   = "#fafafa",
    hairline     = "#9c9c9c"
  )
}

#' @rdname cert_theme_presets
#' @export
cert_theme_warm <- function() {
  cert_theme(
    primary      = "#722f37",
    primary_dark = "#4a1d22",
    accent       = "#a87b3e",
    accent_light = "#d2a96a",
    ink          = "#1a1a1a",
    soft_ink     = "#4a4a4a",
    background   = "#fdf6e9",
    hairline     = "#c9a961"
  )
}
