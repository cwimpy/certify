test_that("make_certificate writes a non-empty PDF", {
  out <- tempfile(fileext = ".pdf")
  make_certificate(
    path         = out,
    recipient    = "Test Person",
    organization = "Test Organization",
    citation     = "in recognition of testing.",
    device       = "pdf"
  )
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000)
})

test_that("make_certificate works with two signers", {
  out <- tempfile(fileext = ".pdf")
  make_certificate(
    path         = out,
    recipient    = "Test Person",
    organization = "Test Organization",
    citation     = "in recognition of testing.",
    signers      = list(
      list(name = "First Signer",  title = "Title One"),
      list(name = "Second Signer", title = "Title Two")
    ),
    device       = "pdf"
  )
  expect_true(file.exists(out))
})

test_that("make_certificate accepts cairo_pdf when cairo loads", {
  skip_on_ci()                       # GH Actions macOS lacks XQuartz
  skip_if_not(isTRUE(capabilities("cairo")))
  out <- tempfile(fileext = ".pdf")
  make_certificate(
    path         = out,
    recipient    = "Test Person",
    organization = "Test Organization",
    citation     = "in recognition of testing.",
    device       = "cairo_pdf"
  )
  expect_true(file.exists(out))
})

test_that("make_certificate validates required arguments", {
  expect_error(make_certificate(recipient = "x"), "`path` is required")
  expect_error(make_certificate(path = tempfile(fileext = ".pdf")),
               "`recipient` is required")
})

test_that("make_certificate rejects invalid theme objects", {
  expect_error(
    make_certificate(
      path      = tempfile(fileext = ".pdf"),
      recipient = "x",
      theme     = list(primary = "#000")
    ),
    "cert_theme"
  )
})

test_that("make_certificate rejects more than two signers", {
  expect_error(
    make_certificate(
      path      = tempfile(fileext = ".pdf"),
      recipient = "x",
      signers   = list(
        list(name = "a", title = "a"),
        list(name = "b", title = "b"),
        list(name = "c", title = "c")
      ),
      device    = "pdf"
    ),
    "1 or 2 signers"
  )
})
