test_that("the default page is unscaled", {
  expect_equal(cert_scale(11, 8.5), 1)
  expect_equal(cert_paper_size("letter"), c(11, 8.5))
})

test_that("paper names are matched case-insensitively", {
  expect_equal(cert_paper_size("A4"), cert_paper_size("a4"))
  expect_equal(cert_paper_size("  Tabloid "), c(17, 11))
})

test_that("an unknown paper size is rejected with the valid options", {
  expect_error(cert_paper_size("foolscap"), "Unknown paper size")
  expect_error(cert_paper_size("foolscap"), "letter")
  expect_error(cert_paper_size(c("a4", "letter")), "single string")
})

test_that("larger paper scales the design up, smaller scales it down", {
  expect_gt(cert_scale(17, 11), 1)      # tabloid
  expect_lt(cert_scale(8.27, 5.83), 1)  # a5
  expect_equal(cert_scale(14, 8.5), 1)  # legal: height-limited, same as letter
})

test_that("every named size renders", {
  for (p in c("letter", "legal", "tabloid", "a3", "a4", "a5")) {
    out <- tempfile(fileext = ".pdf")
    make_certificate(path = out, recipient = "Test Person",
                     citation = "in recognition of testing.",
                     paper = p, device = "pdf")
    expect_true(file.exists(out), info = p)
    expect_gt(file.size(out), 1000)
  }
})

test_that("width and height override paper", {
  out <- tempfile(fileext = ".pdf")
  expect_silent(
    make_certificate(path = out, recipient = "Test Person", paper = "a4",
                     width = 12, height = 9, device = "pdf")
  )
  expect_true(file.exists(out))
})
