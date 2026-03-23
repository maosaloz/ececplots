test_that("ecec_scatter returns a ggplot", {
  p <- ecec_scatter(mtcars, x = "wt", y = "mpg")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_scatter accepts a colour column", {
  p <- ecec_scatter(mtcars, x = "wt", y = "mpg", colour = "cyl")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_scatter errors when column is missing", {
  expect_error(
    ecec_scatter(mtcars, x = "wt", y = "nonexistent"),
    "Column\\(s\\) not found"
  )
})

test_that("ecec_scatter_quadrant returns a ggplot with vline and hline", {
  p <- ecec_scatter_quadrant(mtcars, x = "wt", y = "mpg",
                              x_line = 3, y_line = 20)
  expect_s3_class(p, "ggplot")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% layer_classes)
  expect_true("GeomHline" %in% layer_classes)
})

test_that("ecec_scatter_quadrant errors when column is missing", {
  expect_error(
    ecec_scatter_quadrant(mtcars, x = "bad_col", y = "mpg"),
    "Column\\(s\\) not found"
  )
})

test_that("ecec_scatter_connected returns a ggplot with line and point layers", {
  p <- ecec_scatter_connected(mtcars, x = "wt", y = "mpg", group = "cyl")
  expect_s3_class(p, "ggplot")
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine"  %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})

test_that("ecec_scatter_connected works without a group", {
  p <- ecec_scatter_connected(mtcars, x = "wt", y = "mpg")
  expect_s3_class(p, "ggplot")
})

test_that("ecec_scatter_connected errors when column is missing", {
  expect_error(
    ecec_scatter_connected(mtcars, x = "wt", y = "missing_col"),
    "Column\\(s\\) not found"
  )
})
