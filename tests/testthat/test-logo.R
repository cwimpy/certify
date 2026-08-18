# A throwaway PNG of a given pixel size, so the fixtures stay out of the repo.
tmp_logo <- function(px_w, px_h) {
  f <- tempfile(fileext = ".png")
  png::writePNG(array(0.5, dim = c(px_h, px_w, 3)), f)
  f
}

test_that("a square logo is limited by height", {
  fit <- draw_logo(tmp_logo(600, 600), cx = 5.5, cy_top = 7.8,
                   max_h = 2, max_w = 4.5)
  expect_equal(fit$height, 2)
  expect_equal(fit$width, 2)
})

test_that("a wide logo is limited by width and keeps its proportions", {
  fit <- draw_logo(tmp_logo(1200, 300), cx = 5.5, cy_top = 7.8,
                   max_h = 2, max_w = 4.5)
  expect_equal(fit$width, 4.5)
  expect_equal(fit$height, 1.125)
  expect_equal(fit$width / fit$height, 4)
})

test_that("a tall logo is limited by height and keeps its proportions", {
  fit <- draw_logo(tmp_logo(300, 1200), cx = 5.5, cy_top = 7.8,
                   max_h = 2, max_w = 4.5)
  expect_equal(fit$height, 2)
  expect_equal(fit$width, 0.5)
})

test_that("a logo never exceeds the box it is given", {
  for (dims in list(c(1200, 300), c(300, 1200), c(600, 600), c(2100, 1639))) {
    fit <- draw_logo(tmp_logo(dims[1], dims[2]), cx = 5.5, cy_top = 7.8,
                     max_h = 2, max_w = 4.5)
    expect_lte(fit$height, 2)
    expect_lte(fit$width, 4.5)
  }
})

test_that("make_certificate renders with a logo", {
  out <- tempfile(fileext = ".pdf")
  make_certificate(
    path      = out,
    recipient = "Test Person",
    citation  = "in recognition of testing.",
    logo      = tmp_logo(1200, 300),
    device    = "pdf"
  )
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000)
})

test_that("a missing logo file is reported clearly", {
  expect_error(
    make_certificate(
      path      = tempfile(fileext = ".pdf"),
      recipient = "Test Person",
      logo      = file.path(tempdir(), "no-such-logo.png"),
      device    = "pdf"
    ),
    "Logo not found"
  )
})

test_that("logo size arguments are validated", {
  args <- list(
    path      = tempfile(fileext = ".pdf"),
    recipient = "Test Person",
    logo      = tmp_logo(600, 600),
    device    = "pdf"
  )
  expect_error(do.call(make_certificate, c(args, list(logo_height = 0))),
               "`logo_height` must be a single positive number")
  expect_error(do.call(make_certificate, c(args, list(logo_height = "big"))),
               "`logo_height` must be a single positive number")
  expect_error(do.call(make_certificate, c(args, list(logo_width = -1))),
               "`logo_width` must be a single positive number")
})

test_that("an unreadable logo is distinguished from a missing one", {
  skip_on_os("windows")             # chmod semantics differ
  f <- tmp_logo(200, 200)
  Sys.chmod(f, "000")
  on.exit(Sys.chmod(f, "600"), add = TRUE)
  skip_if(file.access(f, mode = 4) == 0)   # root ignores the mode bits

  expect_error(read_logo(f), "could not be opened")
  expect_error(read_logo(file.path(tempdir(), "absent.png")), "not found")
})

test_that("a non-PNG file is reported as such", {
  f <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0xff, 0xd8, 0xff, 0xe0, rep(0, 32))), f)   # JPEG header
  expect_error(read_logo(f), "not a PNG file")
})
