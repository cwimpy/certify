# Decorative grobs (laurels, corner arcs, dividers).
# All ornaments take a `theme` (a `cert_theme` list) so colors stay in sync
# with the rest of the certificate.

draw_laurel <- function(cx, cy, side = "left", theme) {
  direction <- if (side == "left") -1 else 1
  stem_len  <- 1.10
  grobs     <- list()

  # Curved stem
  stem_ts <- seq(0, 1, length.out = 30)
  stem_x  <- cx + direction * stem_len * stem_ts
  stem_y  <- cy - 0.02 * sin(pi * stem_ts)
  grobs[[length(grobs) + 1]] <- grid::linesGrob(
    x  = grid::unit(stem_x, "in"),
    y  = grid::unit(stem_y, "in"),
    gp = grid::gpar(col = theme$accent, lwd = 1.2)
  )

  # Paired leaves along the stem, tapering toward the tip
  n_pairs <- 6
  for (i in 0:(n_pairs - 1)) {
    t  <- 0.12 + (i / (n_pairs - 1)) * 0.82
    bx <- cx + direction * stem_len * t
    by <- cy - 0.02 * sin(pi * t)
    leaf_len <- 0.32 - i * 0.025
    leaf_wid <- 0.085 - i * 0.005
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
                        fill = theme$accent_light, lwd = 0.4)
      )
      grobs[[length(grobs) + 1]] <- grid::segmentsGrob(
        x0 = grid::unit(bx, "in"),
        y0 = grid::unit(by, "in"),
        x1 = grid::unit(bx + (if (direction < 0) -1 else 1) *
                          leaf_len * cos(theta), "in"),
        y1 = grid::unit(by + leaf_len * sin(theta), "in"),
        gp = grid::gpar(col = theme$primary_dark, lwd = 0.3)
      )
    }
  }

  # Terminal leaf
  tip_x   <- cx + direction * stem_len
  tip_y   <- cy
  tip_len <- 0.18
  tip_wid <- 0.05
  ts <- seq(0, 2 * pi, length.out = 40)
  lx <- tip_len * 0.5 * (1 - cos(ts))
  ly <- tip_wid * sin(ts)
  grobs[[length(grobs) + 1]] <- grid::polygonGrob(
    x  = grid::unit(tip_x + direction * lx, "in"),
    y  = grid::unit(tip_y + ly, "in"),
    gp = grid::gpar(col = theme$primary_dark,
                    fill = theme$accent_light, lwd = 0.4)
  )

  # Berries clustered at the base
  for (offset in list(c(0, 0),
                      c(direction * 0.06,  0.035),
                      c(direction * 0.06, -0.035))) {
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x  = grid::unit(cx + offset[1], "in"),
      y  = grid::unit(cy + offset[2], "in"),
      r  = grid::unit(0.025, "in"),
      gp = grid::gpar(col = NA, fill = theme$primary)
    )
  }
  grobs
}

draw_corner <- function(cx, cy, sx, sy, theme) {
  size  <- 0.45
  grobs <- list()

  for (r in c(size, size * 0.72, size * 0.44)) {
    ang <- seq(0, pi / 2, length.out = 40)
    xs  <- cx + sx * r * cos(ang)
    ys  <- cy + sy * r * sin(ang)
    grobs[[length(grobs) + 1]] <- grid::linesGrob(
      x  = grid::unit(xs, "in"),
      y  = grid::unit(ys, "in"),
      gp = grid::gpar(col = theme$accent, lwd = 1.1)
    )
  }

  grobs[[length(grobs) + 1]] <- grid::circleGrob(
    x  = grid::unit(cx + sx * 0.11, "in"),
    y  = grid::unit(cy + sy * 0.11, "in"),
    r  = grid::unit(0.035, "in"),
    gp = grid::gpar(col = NA, fill = theme$primary)
  )
  for (frac in c(0.22, 0.35)) {
    grobs[[length(grobs) + 1]] <- grid::circleGrob(
      x  = grid::unit(cx + sx * frac, "in"),
      y  = grid::unit(cy + sy * frac, "in"),
      r  = grid::unit(0.022, "in"),
      gp = grid::gpar(col = NA, fill = theme$accent)
    )
  }
  grobs
}

draw_logo <- function(path, cx, cy_top, target_h = 2.0) {
  if (!file.exists(path)) {
    stop("Logo not found at: ", path, call. = FALSE)
  }
  img <- png::readPNG(path)
  ih  <- dim(img)[1]
  iw  <- dim(img)[2]
  target_w <- target_h * (iw / ih)
  grid::rasterGrob(
    img,
    x      = grid::unit(cx, "in"),
    y      = grid::unit(cy_top - target_h / 2, "in"),
    width  = grid::unit(target_w, "in"),
    height = grid::unit(target_h, "in")
  )
}
