test_that("ecec_base returns an ecec_layout with correct defaults", {
  layout <- ecec_base()
  expect_s3_class(layout, "ecec_layout")
  expect_equal(layout$width,    9.5)   # "1/2"
  expect_equal(layout$height,   6)     # "medium"
  expect_equal(layout$n_panels, 1L)
  expect_equal(layout$n_rows,   1L)
  expect_equal(layout$n_cols,   1L)
})

test_that("ecec_base resolves named width/height keys", {
  layout <- ecec_base(width = "full", height = "large")
  expect_equal(layout$width,  19)
  expect_equal(layout$height,  9)
})

test_that("ecec_base accepts numeric dimensions", {
  layout <- ecec_base(width = 12, height = 8)
  expect_equal(layout$width,  12)
  expect_equal(layout$height,  8)
})

test_that("ecec_base stores panel layout correctly", {
  layout <- ecec_base(n_panels = 6, n_rows = 2, n_cols = 3)
  expect_equal(layout$n_panels, 6L)
  expect_equal(layout$n_rows,   2L)
  expect_equal(layout$n_cols,   3L)
})

test_that("ecec_base errors on invalid arguments", {
  expect_error(ecec_base(n_panels = 0),  "n_panels.*>=.*1")
  expect_error(ecec_base(n_rows   = 0),  "n_rows.*>=.*1")
  expect_error(ecec_base(n_cols   = 0),  "n_cols.*>=.*1")
})

test_that("ecec_base warns when n_rows*n_cols < n_panels", {
  expect_warning(
    ecec_base(n_panels = 6, n_rows = 2, n_cols = 2),
    "n_rows \\* n_cols"
  )
})

test_that("print.ecec_layout works without error", {
  layout <- ecec_base()
  expect_output(print(layout), "ecec_layout")
})
