# Decorative grobs (laurels, corner arcs, dividers).
# All ornaments take a `theme` (a `cert_theme` list) so colors stay in sync
# with the rest of the certificate.

draw_laurel <- function(cx, cy, side = "left", theme, scale = 1) {
  direction <- if (side == "left") -1 else 1
  stem_len  <- 1.10 * scale
  grobs     <- list()

  # Curved stem
  stem_ts <- seq(0, 1, length.out = 30)
  stem_x  <- cx + direction * stem_len * stem_ts
  stem_y  <- cy - 0.02 * scale * sin(pi * stem_ts)
  grobs[[length(grobs) + 1]] <- grid::linesGrob(
    x  = grid::unit(stem_x, "in"),
    y  = grid::unit(stem_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 1.2 * scale)
  )

  # Paired leaves along the stem, tapering toward the tip
  n_pairs <- 6
  for (i in 0:(n_pairs - 1)) {
    t  <- 0.12 + (i / (n_pairs - 1)) * 0.82
    bx <- cx + direction * stem_len * t
    by <- cy - 0.02 * scale * sin(pi * t)
    leaf_len <- (0.32 - i * 0.025) * scale
    leaf_wid <- (0.085 - i * 0.005) * scale
    for (angle_deg in c(42, -42)) {
      theta <- direction * angle_deg * pi / 180
      ts <- seq(0, 2 * pi, length.out = 40)
      lx <- leaf_len * 0.5 * (1 - cos(ts))
      ly <- leaf_wid * sin(ts)
      rx <- lx * cos(theta) - ly * sin(theta)
      ry <- lx * sin(theta) + ly * cos(theta)
      if (direction < 0) rx <- -rx
      grobs[[length(grobs) + 1]] <- grid::polygonGrob(
        x  = grid::unit(bx + rx, "in"),
        y  = grid::unit(by + ry, "in"),
        gp = grid::gpar(col = theme$primary_dark,
                        fill = theme$accent_light, lwd = 0.4 * scale)
      )
      grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
        x0 = grid::unit(bx, "in"),
        y0 = grid::unit(by, "in"),
        x1 = grid::unit(bx + (if (direction < 0) -1 else 1) *
                          leaf_len * cos(theta), "in"),
        y1 = grid::unit(by + leaf_len * sin(theta), "in"),
        gp = grid::gpar(col = theme$primary_dark, lwd = 0.3 * scale)
      )
    }
  }

  # Terminal leaf
  tip_x   <- cx + direction * stem_len
  tip_y   <- cy
  tip_len <- 0.18 * scale
  tip_wid <- 0.05 * scale
  ts <- seq(0, 2 * pi, length.out = 40)
  lx <- tip_len * 0.5 * (1 - cos(ts))
  ly <- tip_wid * sin(ts)
  grobs[[length(grobs) + 1]] <- grid::polygonGrob(
    x  = grid::unit(tip_x + direction * lx, "in"),
    y  = grid::unit(tip_y + ly, "in"),
    gp = grid::gpar(col = theme$primary_dark,
                    fill = theme$accent_light, lwd = 0.4 * scale)
  )

  # Berries clustered at the base
  for (offset in list(c(0, 0),
                      c(direction * 0.06,  0.035) * scale,
                      c(direction * 0.06, -0.035) * scale)) {
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x  = grid::unit(cx + offset[1], "in"),
      y  = grid::unit(cy + offset[2], "in"),
      r  = grid::unit(0.025 * scale, "in"),
      gp = grid::gpar(col = NA, fill = theme$primary)
    )
  }
  grobs
}

draw_corner <- function(cx, cy, sx, sy, theme, scale = 1) {
  size  <- 0.45 * scale
  grobs <- list()

  for (r in c(size, size * 0.72, size * 0.44)) {
    ang <- seq(0, pi / 2, length.out = 40)
    xs  <- cx + sx * r * cos(ang)
    ys  <- cy + sy * r * sin(ang)
    grobs[[length(grobs) + 1]] <- grid::linesGrob(
      x  = grid::unit(xs, "in"),
      y  = grid::unit(ys, "in"),
      gp = grid::gpar(col = theme$accent, lwd = 1.1 * scale)
    )
  }

  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x  = grid::unit(cx + sx * 0.11 * scale, "in"),
    y  = grid::unit(cy + sy * 0.11 * scale, "in"),
    r  = grid::unit(0.035 * scale, "in"),
    gp = grid::gpar(col = NA, fill = theme$primary)
  )
  for (frac in c(0.22, 0.35)) {
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x  = grid::unit(cx + sx * frac * scale, "in"),
      y  = grid::unit(cy + sy * frac * scale, "in"),
      r  = grid::unit(0.022 * scale, "in"),
      gp = grid::gpar(col = NA, fill = theme$accent)
    )
  }
  grobs
}

# Scale a logo to fit inside a max_w x max_h box without distorting it.
# Returns the grob plus the size actually used, so the caller can place the
# divider directly beneath a logo that came out shorter than max_h.
draw_logo <- function(path, cx, cy_top, max_h = 2.0, max_w = 4.5) {
  img <- read_logo(path)
  ih  <- dim(img)[1]
  iw  <- dim(img)[2]

  # Contain-fit: whichever dimension runs out of room first sets the scale.
  # Tall or squarish logos are height-limited (the historical behavior);
  # wide wordmarks are width-limited instead of overrunning the border.
  s <- min(max_h / ih, max_w / iw)
  h <- ih * s
  w <- iw * s

  list(
    grob   = grid::rasterGrob(
      img,
      x      = grid::unit(cx, "in"),
      y      = grid::unit(cy_top - h / 2, "in"),
      width  = grid::unit(w, "in"),
      height = grid::unit(h, "in")
    ),
    width  = w,
    height = h
  )
}

# Read a logo, distinguishing the three ways this goes wrong. png::readPNG
# reports "unable to open" both for a file that is absent and for one the
# process may stat but not open, which sends people hunting for a typo when
# the real cause is a permission or a cloud-storage placeholder.
read_logo <- function(path) {
  if (!file.exists(path)) {
    stop("Logo not found at: ", path, "\n",
         "  Check the spelling of the path, or run file.choose() to get it.",
         call. = FALSE)
  }

  # suppressWarnings: a denied open emits a noisy warning as well as an
  # error, and the error message below is the one worth reading.
  head_bytes <- tryCatch(suppressWarnings(readBin(path, "raw", n = 8L)),
                         error = function(e) e)

  if (inherits(head_bytes, "error")) {
    stop("Logo found but could not be opened: ", path, "\n",
         "  The file exists, so this is not a wrong path. Usually it is\n",
         "  one of: the operating system is denying R access to that\n",
         "  folder (on macOS see Privacy & Security > Files and Folders),\n",
         "  the file lives in cloud storage and has not been downloaded\n",
         "  to this machine yet, or its permissions do not allow reading.",
         call. = FALSE)
  }

  if (!identical(head_bytes,
                 as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))) {
    stop("Logo is not a PNG file: ", path, "\n",
         "  A JPEG, SVG, or PDF renamed to .png will not work. Re-export\n",
         "  the image as a real PNG.",
         call. = FALSE)
  }

  tryCatch(
    png::readPNG(path),
    error = function(e) {
      stop("Could not read the PNG at: ", path, "\n  ",
           conditionMessage(e), call. = FALSE)
    }
  )
}
