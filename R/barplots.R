#' @title Barplot functions
#'
#' @description
#' Three flavours of barplot, each supporting both vertical (the default) and
#' horizontal orientations, all styled with \code{\link{ecec_theme}}:
#' \itemize{
#'   \item \code{ecec_bar_stacked}  – stacked barplot
#'   \item \code{ecec_bar_grouped}  – grouped (non-stacked / side-by-side)
#'     barplot
#'   \item \code{ecec_bar_scatter}  – barplot with individual data points
#'     overlaid as a scatter
#' }

# ── ecec_bar_stacked ──────────────────────────────────────────────────────────

#' Stacked barplot
#'
#' Creates a stacked barplot where each bar segment represents a level of the
#' \code{fill} variable.  Supports both vertical (default) and horizontal
#' orientations.
#'
#' @param data        A data frame.
#' @param x           Column name for the categorical x-axis (or y-axis when
#'   \code{orientation = "horizontal"}).
#' @param y           Column name for the numeric value.
#' @param fill        Column name used to colour/fill the bar segments.
#' @param facet       Optional column name to divide the chart into separate
#'   panels by this grouping variable (default \code{NULL}).
#' @param facet_var   Optional vector specifying which values of the \code{facet}
#'   column to display. If \code{NULL} (default), all facet levels are shown.
#' @param orientation \code{"vertical"} (default) or \code{"horizontal"}.
#' @param position    Stack position: \code{"stack"} (absolute heights,
#'   default) or \code{"fill"} (proportion, 0–1).
#' @param width       Bar width (0–1, default \code{0.7}).
#' @param title       Plot title.
#' @param subtitle    Plot subtitle.
#' @param caption     Plot caption.
#' @param x_label     Axis label for the category axis; defaults to the column
#'   name.
#' @param y_label     Axis label for the value axis; defaults to the column
#'   name.
#' @param x_angle     Rotation angle for x-axis labels in degrees (default \code{0}).
#' @param y_angle     Rotation angle for y-axis labels in degrees (default \code{0}).
#' @param text_scale  Scaling factor for text sizes (default \code{1}).
#' @param legend_nrow Number of rows in the legend (default \code{NULL}: automatic).
#' @param legend_ncol Number of columns in the legend (default \code{NULL}: automatic).
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_bar labs coord_flip facet_wrap
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Vertical stacked bar
#' df <- data.frame(
#'   category = rep(c("A", "B", "C"), each = 3),
#'   segment  = rep(c("X", "Y", "Z"), times = 3),
#'   value    = c(10, 15, 8, 12, 9, 14, 7, 11, 13)
#' )
#' ecec_bar_stacked(df, x = "category", y = "value", fill = "segment")
#'
#' # Horizontal stacked bar
#' ecec_bar_stacked(df, x = "category", y = "value", fill = "segment",
#'                  orientation = "horizontal")
#'
#' # With faceting by group
#' df_facet <- data.frame(
#'   country = rep(c("A", "B", "C"), times = 2),
#'   level   = rep(c("ISCED 02", "Under age 3"), each = 3),
#'   segment = rep(c("X", "Y", "Z"), times = 2),
#'   value   = c(10, 15, 8, 12, 9, 14, 7, 11, 13, 14)
#' )
#' ecec_bar_stacked(df_facet, x = "country", y = "value", fill = "segment",
#'                  facet = "level")
ecec_bar_stacked <- function(data,
                              x,
                              y,
                              fill,
                              facet       = NULL,
                              facet_var   = NULL,
                              orientation = "vertical",
                              position    = "stack",
                              width       = 0.7,
                              title       = NULL,
                              subtitle    = NULL,
                              caption     = NULL,
                              x_label     = NULL,
                              y_label     = NULL,
                              x_angle     = 0,
                              y_angle     = 0,
                              text_scale  = 1,
                              legend_nrow = NULL,
                              legend_ncol = NULL) {
  orientation <- .check_orientation(orientation)
  x    <- as.character(rlang::ensym(x))
  y    <- as.character(rlang::ensym(y))
  fill <- as.character(rlang::ensym(fill))
  facet_col <- if (!is.null(facet)) as.character(facet) else NULL
  .check_cols(data, x, y, fill, facet_col)

  # Remove rows where x variable is NA
  data <- data[!is.na(data[[x]]), ]

  if (!is.null(facet_col)) {
    # Determine which facet levels to process
    facet_levels <- if (!is.null(facet_var)) facet_var else unique(data[[facet_col]])
    
    # Process each facet level separately
    data_list <- lapply(facet_levels, function(level) {
      # Filter to current facet level
      subset_data <- data[data[[facet_col]] == level, ]
      
      # Remove rows where y is NA
      subset_data <- subset_data[!is.na(subset_data[[y]]), ]
      
      # Drop unused x levels for this facet level only
      subset_data[[x]] <- droplevels(factor(subset_data[[x]]))
      subset_data[[facet_col]] <- factor(subset_data[[facet_col]], levels = level)
      
      return(subset_data)
    })
    
    # Combine all processed facet levels
    data <- do.call(rbind, data_list)
    
    # Reset both factors to only include levels present in combined data
    data[[facet_col]] <- droplevels(factor(data[[facet_col]]))
    data[[x]] <- droplevels(factor(data[[x]]))
  } else {
    # Even without faceting, ensure x is a factor with only present levels
    data[[x]] <- droplevels(factor(data[[x]]))
  }

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x    = !!rlang::sym(x),
      y    = !!rlang::sym(y),
      fill = !!rlang::sym(fill)
    )
  ) +
    ggplot2::geom_bar(stat     = "identity",
                      position = position,
                      width    = width) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(text_scale = text_scale, x_angle = x_angle, y_angle = y_angle) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend_nrow, ncol = legend_ncol))

  if (!is.null(facet_col)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_col)))
  }

  if (orientation == "horizontal") p <- p + ggplot2::coord_flip()
  p
}

# ── ecec_bar_grouped ──────────────────────────────────────────────────────────

#' Grouped (non-stacked) barplot
#'
#' Creates a side-by-side barplot where bars within each category are placed
#' next to each other rather than stacked.  Supports both vertical (default)
#' and horizontal orientations.
#'
#' @param data        A data frame.
#' @param x           Column name for the categorical axis.
#' @param y           Column name for the numeric value.
#' @param fill        Column name used to define and colour the bar groups.
#' @param orientation \code{"vertical"} (default) or \code{"horizontal"}.
#' @param width       Bar width (0–1, default \code{0.7}).
#' @param title       Plot title.
#' @param subtitle    Plot subtitle.
#' @param caption     Plot caption.
#' @param x_label     Axis label for the category axis; defaults to the column
#'   name.
#' @param y_label     Axis label for the value axis; defaults to the column
#'   name.
#' @param x_angle     Rotation angle for x-axis labels in degrees (default \code{0}).
#' @param y_angle     Rotation angle for y-axis labels in degrees (default \code{0}).
#' @param text_scale  Scaling factor for text sizes (default \code{1}).
#' @param legend_nrow Number of rows in the legend (default \code{NULL}: automatic).
#' @param legend_ncol Number of columns in the legend (default \code{NULL}: automatic).
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_bar labs coord_flip
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' df <- data.frame(
#'   category = rep(c("A", "B", "C"), each = 2),
#'   group    = rep(c("G1", "G2"), times = 3),
#'   value    = c(10, 15, 8, 12, 9, 14)
#' )
#' ecec_bar_grouped(df, x = "category", y = "value", fill = "group")
#'
#' # Horizontal
#' ecec_bar_grouped(df, x = "category", y = "value", fill = "group",
#'                  orientation = "horizontal")
ecec_bar_grouped <- function(data,
                              x,
                              y,
                              fill,
                              orientation = "vertical",
                              width       = 0.7,
                              title       = NULL,
                              subtitle    = NULL,
                              caption     = NULL,
                              x_label     = NULL,
                              y_label     = NULL,
                              x_angle     = 0,
                              y_angle     = 0,
                              text_scale  = 1,
                              legend_nrow = NULL,
                              legend_ncol = NULL) {
  orientation <- .check_orientation(orientation)
  x    <- as.character(rlang::ensym(x))
  y    <- as.character(rlang::ensym(y))
  fill <- as.character(rlang::ensym(fill))
  .check_cols(data, x, y, fill)

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x    = !!rlang::sym(x),
      y    = !!rlang::sym(y),
      fill = !!rlang::sym(fill)
    )
  ) +
    ggplot2::geom_bar(stat     = "identity",
                      position = "dodge",
                      width    = width) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(text_scale = text_scale, x_angle = x_angle, y_angle = y_angle) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend_nrow, ncol = legend_ncol))

  if (orientation == "horizontal") p <- p + ggplot2::coord_flip()
  p
}

# ── ecec_bar_scatter ──────────────────────────────────────────────────────────

#' Barplot with overlaid scatter points
#'
#' Creates a barplot (showing e.g. a group mean or total) with individual
#' data points overlaid as a jittered scatter.  Useful for showing both
#' summary statistics and the underlying distribution.  Supports both vertical
#' (default) and horizontal orientations.
#'
#' @param data         A data frame.
#' @param x            Column name for the categorical axis.
#' @param y            Column name for the numeric value used for both the
#'   bars and the scatter points.
#' @param fill         Optional column name used to fill the bars (default
#'   \code{NULL}: a single fill colour).
#' @param bar_stat     Statistic for the bars: \code{"identity"} uses the raw
#'   \code{y} values (one row per bar), \code{"summary"} computes the mean.
#'   Default \code{"summary"}.
#' @param bar_fun      Summary function passed to
#'   \code{geom_bar(fun = )} when \code{bar_stat = "summary"}.  Default
#'   \code{"mean"}.
#' @param bar_colour   Fill colour for bars when \code{fill} is \code{NULL}
#'   (default \code{"#4472C4"}).
#' @param orientation  \code{"vertical"} (default) or \code{"horizontal"}.
#' @param width        Bar width (0–1, default \code{0.6}).
#' @param jitter_width  Horizontal spread of jittered points (default
#'   \code{0.15}).
#' @param point_size   Size of scatter points (default \code{2}).
#' @param point_colour Colour of scatter points (default \code{"#222222"}).
#' @param point_alpha  Transparency of scatter points (default \code{0.6}).
#' @param title        Plot title.
#' @param subtitle     Plot subtitle.
#' @param caption      Plot caption.
#' @param x_label      Axis label for the category axis; defaults to the
#'   column name.
#' @param y_label      Axis label for the value axis; defaults to the column
#'   name.
#' @param x_angle      Rotation angle for x-axis labels in degrees (default \code{0}).
#' @param y_angle      Rotation angle for y-axis labels in degrees (default \code{0}).
#' @param text_scale   Scaling factor for text sizes (default \code{1}).
#' @param legend_nrow Number of rows in the legend (default \code{NULL}: automatic).
#' @param legend_ncol Number of columns in the legend (default \code{NULL}: automatic).
#'
#' @return A \code{ggplot} object.
#' @importFrom ggplot2 ggplot aes geom_bar geom_jitter labs coord_flip
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' ecec_bar_scatter(mtcars, x = "cyl", y = "mpg",
#'                  title = "MPG by Number of Cylinders")
#'
#' # Horizontal with fill
#' ecec_bar_scatter(mtcars, x = "cyl", y = "mpg", fill = "cyl",
#'                  orientation = "horizontal",
#'                  title = "Horizontal: MPG by Cylinders")
ecec_bar_scatter <- function(data,
                              x,
                              y,
                              fill          = NULL,
                              bar_stat      = "summary",
                              bar_fun       = "mean",
                              bar_colour    = "#4472C4",
                              orientation   = "vertical",
                              width         = 0.7,
                              jitter_width  = 0.15,
                              point_size    = 2,
                              point_colour  = "#222222",
                              point_alpha   = 0.6,
                              title         = NULL,
                              subtitle      = NULL,
                              caption       = NULL,
                              x_label       = NULL,
                              y_label       = NULL,
                              x_angle       = 0,
                              y_angle       = 0,
                              text_scale    = 1,
                              legend_nrow   = NULL,
                              legend_ncol   = NULL) {
  orientation <- .check_orientation(orientation)
  x <- as.character(rlang::ensym(x))
  y <- as.character(rlang::ensym(y))

  fill_col <- if (!is.null(fill)) as.character(fill) else NULL
  .check_cols(data, x, y, fill_col)

  x_lab <- if (!is.null(x_label)) x_label else x
  y_lab <- if (!is.null(y_label)) y_label else y

  # Ensure x is treated as a factor so bars are discrete
  if (!is.factor(data[[x]])) {
    data[[x]] <- factor(data[[x]])
  }

  base_mapping <- if (!is.null(fill_col)) {
    ggplot2::aes(x    = !!rlang::sym(x),
                 y    = !!rlang::sym(y),
                 fill = !!rlang::sym(fill_col))
  } else {
    ggplot2::aes(x = !!rlang::sym(x),
                 y = !!rlang::sym(y))
  }

  # Bar layer
  if (bar_stat == "summary") {
    bar_layer <- ggplot2::geom_bar(
      stat  = "summary",
      fun   = bar_fun,
      width = width,
      fill  = if (is.null(fill_col)) bar_colour else NULL,
      alpha = 0.85
    )
  } else {
    bar_layer <- ggplot2::geom_bar(
      stat  = "identity",
      width = width,
      fill  = if (is.null(fill_col)) bar_colour else NULL,
      alpha = 0.85
    )
  }

  p <- ggplot2::ggplot(data, base_mapping) +
    bar_layer +
    ggplot2::geom_jitter(
      width  = jitter_width,
      size   = point_size,
      colour = point_colour,
      alpha  = point_alpha
    ) +
    ggplot2::labs(title    = title,
                  subtitle = subtitle,
                  caption  = caption,
                  x        = x_lab,
                  y        = y_lab) +
    ecec_theme(text_scale = text_scale, x_angle = x_angle, y_angle = y_angle) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend_nrow, ncol = legend_ncol))

  if (orientation == "horizontal") p <- p + ggplot2::coord_flip()
  p
}
