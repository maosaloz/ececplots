df <- data.frame(
  category = rep(c("A", "B", "C"), each = 2),
  segment  = rep(c("X", "Y"), times = 3),
  value    = c(10, 15, 8, 12, 9, 14)
)

# ── ecec_bar_stacked ─────────────────────────────────────────────────────────

test_that("ecec_bar_stacked returns a ggplot (vertical)", {
  p <- ecec_bar_stacked(df, x = "category", y = "value", fill = "segment")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_bar_stacked returns a ggplot (horizontal)", {
  p <- ecec_bar_stacked(df, x = "category", y = "value", fill = "segment",
                        orientation = "horizontal")
  expect_s3_class(p, "ggplot")
  # coord_flip is present
  expect_true(inherits(p$coordinates, "CoordFlip"))
})

test_that("ecec_bar_stacked errors on bad orientation", {
  expect_error(
    ecec_bar_stacked(df, x = "category", y = "value", fill = "segment",
                     orientation = "diagonal"),
    '"vertical" or "horizontal"'
  )
})

test_that("ecec_bar_stacked errors on missing column", {
  expect_error(
    ecec_bar_stacked(df, x = "category", y = "missing", fill = "segment"),
    "Column\\(s\\) not found"
  )
})

# ── ecec_bar_grouped ─────────────────────────────────────────────────────────

test_that("ecec_bar_grouped returns a ggplot (vertical)", {
  p <- ecec_bar_grouped(df, x = "category", y = "value", fill = "segment")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_bar_grouped returns a ggplot (horizontal)", {
  p <- ecec_bar_grouped(df, x = "category", y = "value", fill = "segment",
                        orientation = "horizontal")
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$coordinates, "CoordFlip"))
})

test_that("ecec_bar_grouped errors on bad orientation", {
  expect_error(
    ecec_bar_grouped(df, x = "category", y = "value", fill = "segment",
                     orientation = "wrong"),
    '"vertical" or "horizontal"'
  )
})

# ── ecec_bar_scatter ─────────────────────────────────────────────────────────

test_that("ecec_bar_scatter returns a ggplot (vertical)", {
  p <- ecec_bar_scatter(mtcars, x = "cyl", y = "mpg")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_bar_scatter returns a ggplot (horizontal)", {
  p <- ecec_bar_scatter(mtcars, x = "cyl", y = "mpg",
                        orientation = "horizontal")
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$coordinates, "CoordFlip"))
})

test_that("ecec_bar_scatter has bar and jitter layers", {
  p <- ecec_bar_scatter(mtcars, x = "cyl", y = "mpg")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomBar"    %in% layer_classes)
  expect_true("GeomJitter" %in% layer_classes)
})

test_that("ecec_bar_scatter errors on missing column", {
  expect_error(
    ecec_bar_scatter(mtcars, x = "cyl", y = "missing_col"),
    "Column\\(s\\) not found"
  )
})
