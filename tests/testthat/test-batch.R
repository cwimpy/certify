roster <- function() {
  data.frame(
    recipient  = c("Jane A. Doe", "John B. Smith"),
    award_name = c("Outstanding Student", "Outstanding Service"),
    citation   = c("for exceptional scholarship.", "for devoted service."),
    stringsAsFactors = FALSE
  )
}

test_that("one certificate is written per row, named after the recipient", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  files <- make_certificates(roster(), dir = d, device = "pdf", quiet = TRUE)

  expect_length(files, 2)
  expect_equal(basename(files), c("jane-a-doe.pdf", "john-b-smith.pdf"))
  expect_true(all(file.exists(files)))
  expect_true(all(file.size(files) > 1000))
})

test_that("constant arguments apply to every row", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  files <- make_certificates(roster(), dir = d, paper = "a4",
                             organization = "Everywhere University",
                             device = "pdf", quiet = TRUE)
  expect_true(all(file.exists(files)))
})

test_that("duplicate names do not overwrite each other", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  twins <- data.frame(recipient = c("Sam Jones", "Sam Jones", "Sam Jones"),
                      stringsAsFactors = FALSE)
  files <- make_certificates(twins, dir = d, device = "pdf", quiet = TRUE)

  expect_length(unique(files), 3)
  expect_equal(basename(files),
               c("sam-jones.pdf", "sam-jones-2.pdf", "sam-jones-3.pdf"))
  expect_true(all(file.exists(files)))
})

test_that("an empty cell falls back to the constant argument", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  partial <- data.frame(recipient  = c("A Person", "B Person"),
                        award_name = c("Specific Award", NA),
                        stringsAsFactors = FALSE)
  files <- make_certificates(partial, dir = d, award_name = "Default Award",
                             device = "pdf", quiet = TRUE)
  expect_true(all(file.exists(files)))
})

test_that("a filename column is honored", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dd <- roster()
  dd$filename <- c("first", "second.pdf")
  files <- make_certificates(dd, dir = d, device = "pdf", quiet = TRUE)
  expect_equal(basename(files), c("first.pdf", "second.pdf"))
})

test_that("unused columns are reported rather than silently dropped", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dd <- roster()
  dd$Recipient_Email <- c("a@x.edu", "b@x.edu")
  expect_message(
    make_certificates(dd, dir = d, device = "pdf"),
    "Ignoring column\\(s\\).*Recipient_Email"
  )
})

test_that("bad input is rejected clearly", {
  expect_error(make_certificates(list(recipient = "x")), "must be a data frame")
  expect_error(make_certificates(data.frame()), "no rows")
  expect_error(
    make_certificates(data.frame(name = "Jane Doe")),
    "needs a `recipient` column"
  )
  expect_error(
    make_certificates(roster(), filename = "only-one"),
    "one entry per row"
  )
})

test_that("factor columns are handled", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dd <- data.frame(recipient = c("Jane A. Doe", "John B. Smith"),
                   stringsAsFactors = TRUE)
  files <- make_certificates(dd, dir = d, device = "pdf", quiet = TRUE)
  expect_equal(basename(files), c("jane-a-doe.pdf", "john-b-smith.pdf"))
})
