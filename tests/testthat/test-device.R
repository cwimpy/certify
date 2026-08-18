test_that("explicit device choices are honored", {
  out <- tempfile(fileext = ".pdf")
  expect_identical(cert_open_device(out, 11, 8.5, "pdf"), "pdf")
  grDevices::dev.off()
  expect_true(file.exists(out))
})

test_that("auto falls back to the base pdf device when cairo errors", {
  out <- tempfile(fileext = ".pdf")
  local_mocked_bindings(
    cairo_pdf = function(...) stop("failed to load cairo DLL"),
    .package  = "grDevices"
  )
  expect_identical(cert_open_device(out, 11, 8.5, "auto"), "pdf")
  grDevices::dev.off()
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("auto falls back when cairo only warns", {
  out <- tempfile(fileext = ".pdf")
  local_mocked_bindings(
    cairo_pdf = function(...) warning("failed to load cairo DLL"),
    .package  = "grDevices"
  )
  expect_identical(cert_open_device(out, 11, 8.5, "auto"), "pdf")
  grDevices::dev.off()
  expect_true(file.exists(out))
})

test_that("a certificate still renders when cairo is broken", {
  out <- tempfile(fileext = ".pdf")
  local_mocked_bindings(
    cairo_pdf = function(...) stop("failed to load cairo DLL"),
    .package  = "grDevices"
  )
  expect_silent(
    make_certificate(
      path         = out,
      recipient    = "Test Person",
      organization = "Test Organization",
      citation     = "in recognition of testing."
    )
  )
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000)
})
