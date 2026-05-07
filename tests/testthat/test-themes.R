test_that("cert_theme returns a cert_theme object with expected slots", {
  th <- cert_theme()
  expect_s3_class(th, "cert_theme")
  expect_named(
    th,
    c("primary", "primary_dark", "accent", "accent_light",
      "ink", "soft_ink", "background", "hairline")
  )
})

test_that("preset themes return cert_theme objects", {
  expect_s3_class(cert_theme_classic(), "cert_theme")
  expect_s3_class(cert_theme_formal(),  "cert_theme")
  expect_s3_class(cert_theme_warm(),    "cert_theme")
})

test_that("user can override individual colors", {
  th <- cert_theme(primary = "#000000", accent = "#ffffff")
  expect_equal(th$primary, "#000000")
  expect_equal(th$accent,  "#ffffff")
})
