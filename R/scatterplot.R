#' @title Scatterplot functions
#'
#' @description
#' Three flavours of scatterplot, all styled with \code{\link{ecec_theme}}:
#' \itemize{
#'   \item \code{ecec_scatter}           – simple scatterplot
#'   \item \code{ecec_scatter_quadrant}  – quadrant scatterplot with dashed
#'     reference lines running from each axis
#'   \item \code{ecec_scatter_connected} – scatterplot with lines connecting
#'     points (ordered by the x variable, optionally by a group variable)
#' }

# ── ecec_scatter ──────────────────────────────────────────────────────────────

#' Simple scatterplot
#'
#' Creates a basic scatterplot using the ECEC house style.
#'
#' @param data   A data frame.
#' @param x      Name of the column to map to the x-axis (character or
#'   unquoted name).
#' @param y      Name of the column to map to the y-axis (character or
#'   unquoted name).
#' @param colour Optional name of the column used to colour points.
#' @param size   Point size (default \code{2.5}).
#' @param alpha  Point transparency (default \code{0.8}).
#' @param title  Plot title (default \code{NULL}).
#' @param subtitle Plot subtitle (default \code{NULL}).
#' @param caption  Plot caption (default \code{NULL}).
#' @param x_label  x-axis label; defaults to the column name.
#' @param y_label  y-axis label; defaults to the column name.
#' @param base_size Base font size forwarded to \code{\link{ecec_theme}}.
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' ecec_scatter(mtcars, x = "wt", y = "mpg", colour = "cyl",
#'              title = "Weight vs Fuel Economy")
ecec_scatter <- function(data,
                         x,
                         y,
                         colour    = NULL,
                         size      = 2.5,
                         alpha     = 0.8,
                         title     = NULL,
                         subtitle  = NULL,
                         caption   = NULL,
                         x_label   = NULL,
                         y_label   = NULL,
                         base_size = 11) {
  x <- as.character(rlang::ensym(x))
  y <- as.character(rlang::ensym(y))
  .check_cols(data, x, y, colour)

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  mapping <- if (!is.null(colour)) {
    colour <- as.character(colour)
    ggplot2::aes(x = !!rlang::sym(x),
                 y = !!rlang::sym(y),
                 colour = !!rlang::sym(colour))
  } else {
    ggplot2::aes(x = !!rlang::sym(x),
                 y = !!rlang::sym(y))
  }

  ggplot2::ggplot(data, mapping) +
    ggplot2::geom_point(size = size, alpha = alpha) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(base_size = base_size)
}

# ── ecec_scatter_quadrant ─────────────────────────────────────────────────────

#' Quadrant scatterplot
#'
#' A scatterplot with dashed reference lines running across the full plot
#' from a specified point on each axis, dividing the space into four
#' quadrants.
#'
#' @param data      A data frame.
#' @param x         Column name for the x-axis.
#' @param y         Column name for the y-axis.
#' @param x_line    x-axis intercept of the vertical dashed reference line
#'   (default \code{0}).
#' @param y_line    y-axis intercept of the horizontal dashed reference line
#'   (default \code{0}).
#' @param line_colour Colour of the reference lines (default \code{"#555555"}).
#' @param line_size   Line width of the reference lines (default \code{0.6}).
#' @param colour    Optional column name used to colour points.
#' @param size      Point size (default \code{2.5}).
#' @param alpha     Point transparency (default \code{0.8}).
#' @param title     Plot title.
#' @param subtitle  Plot subtitle.
#' @param caption   Plot caption.
#' @param x_label   x-axis label; defaults to the column name.
#' @param y_label   y-axis label; defaults to the column name.
#' @param base_size Base font size forwarded to \code{\link{ecec_theme}}.
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_vline labs
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' ecec_scatter_quadrant(mtcars, x = "wt", y = "mpg",
#'                       x_line = 3.2, y_line = 20,
#'                       colour = "cyl",
#'                       title  = "Quadrant: Weight vs MPG")
ecec_scatter_quadrant <- function(data,
                                  x,
                                  y,
                                  x_line       = 0,
                                  y_line       = 0,
                                  line_colour  = "#555555",
                                  line_size    = 0.6,
                                  colour       = NULL,
                                  size         = 2.5,
                                  alpha        = 0.8,
                                  title        = NULL,
                                  subtitle     = NULL,
                                  caption      = NULL,
                                  x_label      = NULL,
                                  y_label      = NULL,
                                  base_size    = 11) {
  x <- as.character(rlang::ensym(x))
  y <- as.character(rlang::ensym(y))
  .check_cols(data, x, y, colour)

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  mapping <- if (!is.null(colour)) {
    colour <- as.character(colour)
    ggplot2::aes(x = !!rlang::sym(x),
                 y = !!rlang::sym(y),
                 colour = !!rlang::sym(colour))
  } else {
    ggplot2::aes(x = !!rlang::sym(x),
                 y = !!rlang::sym(y))
  }

  ggplot2::ggplot(data, mapping) +
    ggplot2::geom_vline(xintercept = x_line,
                        linetype   = "dashed",
                        colour     = line_colour,
                        linewidth  = line_size) +
    ggplot2::geom_hline(yintercept = y_line,
                        linetype   = "dashed",
                        colour     = line_colour,
                        linewidth  = line_size) +
    ggplot2::geom_point(size = size, alpha = alpha) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(base_size = base_size)
}

# ── ecec_scatter_connected ────────────────────────────────────────────────────

#' Scatterplot with connected points
#'
#' Plots points and connects them with lines.  When a \code{group} variable is
#' supplied, points within each group are connected separately (ordered by the
#' x variable within each group).
#'
#' @param data    A data frame.
#' @param x       Column name for the x-axis.
#' @param y       Column name for the y-axis.
#' @param group   Optional column name that defines groups of connected points.
#'   If \code{NULL} all points are connected in the order they appear after
#'   sorting by \code{x}.
#' @param colour  Optional column name used to colour both points and lines.
#'   Defaults to \code{group} when \code{group} is supplied.
#' @param point_size  Size of points (default \code{2.5}).
#' @param line_size   Width of connecting lines (default \code{0.8}).
#' @param alpha       Transparency of points and lines (default \code{0.8}).
#' @param title       Plot title.
#' @param subtitle    Plot subtitle.
#' @param caption     Plot caption.
#' @param x_label     x-axis label; defaults to the column name.
#' @param y_label     y-axis label; defaults to the column name.
#' @param base_size   Base font size forwarded to \code{\link{ecec_theme}}.
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Connected over time within gear group
#' ecec_scatter_connected(mtcars, x = "wt", y = "mpg", group = "cyl",
#'                        title = "Connected: Weight vs MPG by Cylinder")
ecec_scatter_connected <- function(data,
                                   x,
                                   y,
                                   group      = NULL,
                                   colour     = NULL,
                                   point_size = 2.5,
                                   line_size  = 0.8,
                                   alpha      = 0.8,
                                   title      = NULL,
                                   subtitle   = NULL,
                                   caption    = NULL,
                                   x_label    = NULL,
                                   y_label    = NULL,
                                   base_size  = 11) {
  x <- as.character(rlang::ensym(x))
  y <- as.character(rlang::ensym(y))

  # Resolve group / colour defaults
  group_col  <- if (!is.null(group))  as.character(group)  else NULL
  colour_col <- if (!is.null(colour)) as.character(colour) else group_col

  .check_cols(data, x, y, group_col, colour_col)

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  # Sort data so lines are drawn in x-order within each group
  sort_vars <- if (!is.null(group_col)) c(group_col, x) else x
  data <- data[do.call(order, data[sort_vars]), , drop = FALSE]

  # Build aesthetic mappings
  build_aes <- function(include_group = TRUE) {
    a <- list(
      x = rlang::sym(x),
      y = rlang::sym(y)
    )
    if (!is.null(colour_col)) a$colour <- rlang::sym(colour_col)
    if (include_group && !is.null(group_col)) {
      a$group <- rlang::sym(group_col)
    } else if (is.null(group_col)) {
      a$group <- 1L  # connect all points
    }
    do.call(ggplot2::aes, a)
  }

  ggplot2::ggplot(data, build_aes(include_group = TRUE)) +
    ggplot2::geom_line(linewidth = line_size, alpha = alpha) +
    ggplot2::geom_point(size = point_size, alpha = alpha) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(base_size = base_size)
}
