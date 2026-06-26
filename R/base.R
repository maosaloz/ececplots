#' @title Base visualization frame
#'
#' @description
#' \code{ecec_base()} defines the outer frame of any ececplots visualization:
#' the figure dimensions (resolved from the shared width/height dictionaries)
#' and the panel grid layout (number of panels, rows, and columns).
#'
#' The object it returns is used by \code{\link{ecec_save}} to write the
#' figure to disk at the correct size, and by the plot functions (which accept
#' an optional \code{layout} argument) to arrange multiple panels with
#' \pkg{patchwork}.
#'
#' @param width  Width of the figure.  Either a key from
#'   \code{\link{ECEC_WIDTHS}} (\code{"1/3"}, \code{"1/2"}, \code{"2/3"},
#'   \code{"full"}) or a positive numeric value in inches.
#'   Default: \code{"1/2"} (9.5 in).
#' @param height Height of the figure.  Either a key from
#'   \code{\link{ECEC_HEIGHTS}} (\code{"small"}, \code{"medium"},
#'   \code{"large"}) or a positive numeric value in inches.
#'   Default: \code{"medium"} (6 in).
#' @param n_panels Total number of panels (individual charts) in the
#'   visualization.  Default: \code{1}.
#' @param n_rows  Number of panel rows.  Default: \code{1}.
#' @param n_cols  Number of panel columns.  Default: \code{1}.
#'
#' @return A named list of class \code{"ecec_layout"} with the following
#'   components:
#'   \describe{
#'     \item{\code{width}}{Resolved figure width in inches.}
#'     \item{\code{height}}{Resolved figure height in inches.}
#'     \item{\code{n_panels}}{Number of panels.}
#'     \item{\code{n_rows}}{Number of panel rows.}
#'     \item{\code{n_cols}}{Number of panel columns.}
#'   }
#' @export
#'
#' @examples
#' # Half-page-wide, medium-tall, single panel (default)
#' layout <- ecec_base()
#'
#' # Full-page-wide, large, 2×3 grid of panels
#' layout <- ecec_base(width = "1/2", height = "1/2",
#'                     n_panels = 6, n_rows = 2, n_cols = 3)
#'
#' # Custom numeric dimensions
#' layout <- ecec_base(width = 12, height = 8, n_panels = 4,
#'                     n_rows = 2, n_cols = 2)
ecec_base <- function(width    = "1/2",
                      height   = "1/2",
                      n_panels = 1L,
                      n_rows   = 1L,
                      n_cols   = 1L) {

  w <- ecec_get_width(width)
  h <- ecec_get_height(height)

  n_panels <- as.integer(n_panels)
  n_rows   <- as.integer(n_rows)
  n_cols   <- as.integer(n_cols)

  if (n_panels < 1L) stop("'n_panels' must be >= 1.")
  if (n_rows   < 1L) stop("'n_rows' must be >= 1.")
  if (n_cols   < 1L) stop("'n_cols' must be >= 1.")

  if (n_rows * n_cols < n_panels) {
    warning(
      "n_rows * n_cols (", n_rows * n_cols, ") < n_panels (", n_panels, "). ",
      "Some panels may not be displayed."
    )
  }

  structure(
    list(
      width    = w,
      height   = h,
      n_panels = n_panels,
      n_rows   = n_rows,
      n_cols   = n_cols
    ),
    class = "ecec_layout"
  )
}

#' Print method for ecec_layout
#'
#' @param x An \code{ecec_layout} object.
#' @param ... Unused.
#' @export
print.ecec_layout <- function(x, ...) {
  cat("<ecec_layout>\n")
  cat("  Width   :", x$width,    "inches\n")
  cat("  Height  :", x$height,   "inches\n")
  cat("  Panels  :", x$n_panels, "(", x$n_rows, "row x", x$n_cols, "col )\n")
  invisible(x)
}

#' Combine plots using an ecec_layout
#'
#' Arranges a list of ggplot objects into a multi-panel figure according to
#' the rows and columns specified in an \code{\link{ecec_base}} layout.
#'
#' @param plots A list of ggplot objects.
#' @param layout An \code{ecec_layout} object from \code{\link{ecec_base}}.
#'
#' @return A \pkg{patchwork} composite plot.
#' @importFrom patchwork wrap_plots plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p1 <- ecec_scatter(mtcars, x = "wt", y = "mpg")
#' p2 <- ecec_scatter(mtcars, x = "hp", y = "mpg")
#' layout <- ecec_base(width = "2/3", height = "medium",
#'                     n_panels = 2, n_rows = 1, n_cols = 2)
#' ecec_combine(list(p1, p2), layout)
#' }
ecec_combine <- function(plots, layout) {
  if (!inherits(layout, "ecec_layout")) {
    stop("'layout' must be an ecec_layout object from ecec_base().")
  }
  patchwork::wrap_plots(plots) +
    patchwork::plot_layout(nrow = layout$n_rows, ncol = layout$n_cols)
}

#' Save an ececplots figure to disk
#'
#' Saves a ggplot (or patchwork composite) to a file using the dimensions
#' recorded in an \code{ecec_layout} object.
#'
#' @param plot   A ggplot or patchwork object to save.
#' @param file   Output file path (extension determines the format, e.g.
#'   \code{".png"}, \code{".pdf"}, \code{".svg"}).
#' @param layout An \code{ecec_layout} object from \code{\link{ecec_base}}.
#'   If \code{NULL}, a default half-page medium layout is used.
#' @param dpi    Resolution in dots per inch for raster formats (default 300).
#' @param ...    Additional arguments forwarded to
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @return Invisibly returns \code{file}.
#' @importFrom ggplot2 ggsave
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ecec_scatter(mtcars, x = "wt", y = "mpg")
#' layout <- ecec_base(width = "1/2", height = "medium")
#' ecec_save(p, "my_scatter.png", layout)
#' }
ecec_save <- function(plot, file, layout = NULL, dpi = 300, ...) {
  if (is.null(layout)) layout <- ecec_base()
  if (!inherits(layout, "ecec_layout")) {
    stop("'layout' must be an ecec_layout object from ecec_base().")
  }
  ggplot2::ggsave(
    filename = file,
    plot     = plot,
    width    = layout$width,
    height   = layout$height,
    dpi      = dpi,
    units    = "in",
    ...
  )
  invisible(file)
}
