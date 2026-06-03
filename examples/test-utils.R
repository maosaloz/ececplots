test_that("ECEC_WIDTHS has the correct values", {
  expect_equal(ECEC_WIDTHS[["1/3"]],  6)
  expect_equal(ECEC_WIDTHS[["1/2"]],  9.5)
  expect_equal(ECEC_WIDTHS[["2/3"]],  13.5)
  expect_equal(ECEC_WIDTHS[["1"]], 18.6)
})

test_that("ECEC_HEIGHTS has the correct values", {
  expect_equal(ECEC_HEIGHTS[["1/3"]],  6)
  expect_equal(ECEC_HEIGHTS[["1/2"]], 9.5)
  expect_equal(ECEC_HEIGHTS[["2/3"]], 12)
  expect_equal(ECEC_HEIGHTS[["1"]], 21)
})

test_that("ecec_get_width resolves keys correctly", {
  expect_equal(ecec_get_width("1/3"),  6)
  expect_equal(ecec_get_width("1/2"),  9.5)
  expect_equal(ecec_get_width("2/3"),  13.5)
  expect_equal(ecec_get_width("1"), 18.6)
})

test_that("ecec_get_width accepts numeric input", {
  expect_equal(ecec_get_width(9.5), 9.5)
  expect_equal(ecec_get_width(13.5), 13.5)
})

test_that("ecec_get_width errors on bad input", {
  expect_error(ecec_get_width("bad_key"), "Unknown width key")
  expect_error(ecec_get_width(-1),        "'width' must be a positive number")
  expect_error(ecec_get_width(0),         "'width' must be a positive number")
})

test_that("ecec_get_height resolves keys correctly", {
  expect_equal(ecec_get_height("1/3"),  6)
  expect_equal(ecec_get_height("1/2"), 9.5)
  expect_equal(ecec_get_height("2/3"), 12)
  expect_equal(ecec_get_height("1"), 21)
})

test_that("ecec_get_height accepts numeric input", {
  expect_equal(ecec_get_height(6),   6)
  expect_equal(ecec_get_height(12), 12)
})

test_that("ecec_get_height errors on bad input", {
  expect_error(ecec_get_height("bad_key"), "Unknown height key")
  expect_error(ecec_get_height(-1),        "'height' must be a positive number")
  expect_error(ecec_get_height(0),         "'height' must be a positive number")
})

test_that("ecec_theme returns a ggplot theme", {
  th <- ecec_theme()
  expect_s3_class(th, "theme")
})
