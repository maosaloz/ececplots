#' @title Shared utilities for ececplots
#'
#' @description
#' Shared width/height dictionaries, font loader, and the base ggplot2 theme
#' used by every plot in the package.  All plot functions call
#' \code{ecec_get_width()}, \code{ecec_get_height()}, and
#' \code{ecec_theme()} so that figures are visually consistent.

# ── Width / Height dictionaries ──────────────────────────────────────────────

#' Figure width dictionary (inches)
#'
#' A named numeric vector that maps descriptive size keys to figure widths in
#' inches.  Pass any of these keys to the \code{width} argument of
#' \code{\link{ecec_base}}.
#'
#' \describe{
#'   \item{"1/3"}{One-third-page width (6 in)}
#'   \item{"1/2"}{Half-page width (9.5 in)}
#'   \item{"2/3"}{Two-thirds-page width (13.5 in)}
#'   \item{"full"}{Full-page width (18.6 in)}
#' }
#'
#' @export
ECEC_WIDTHS <- c(
  "1/3"  =  6,
  "1/2"  =  9.5,
  "2/3"  = 13.5,
  "1" = 18.6
)

#' Figure height dictionary (inches)
#'
#' A named numeric vector that maps descriptive size keys to figure heights in
#' inches.  Pass any of these keys to the \code{height} argument of
#' \code{\link{ecec_base}}.
#'
#' \describe{
#'   \item{"1/3"}{One-third-page height (6 in)}
#'   \item{"1/2"}{Half-page height (9.5 in)}
#'   \item{"2/3"}{Two-thirds-page height (12 in)}
#'   \item{"full"}{Full-page height (21 in)}
#' }
#'
#' @export
ECEC_HEIGHTS <- c(
  "1/3"  =  6,
  "1/2"  =  9.5,
  "2/3"  = 12,
  "1" = 21
)

# ── Text size dictionary ─────────────────────────────────────────────────────

#' Default text sizes used throughout ececplots
#'
#' A named numeric vector that defines the default font sizes used by
#' \code{ecec_theme()}. All sizes are specified in points and can be scaled
#' globally using the \code{text_scale} argument.
#'
#' \describe{
#'   \item{"axis_title"}{Axis title font size (12 pt)}
#'   \item{"axis_text"}{Axis tick label font size (11 pt)}
#'   \item{"legend_title"}{Legend title font size (12 pt)}
#'   \item{"legend_text"}{Legend label font size (11 pt)}
#'   \item{"strip_text"}{Facet strip label font size (11 pt)}
#'   \item{"plot_title"}{Plot title font size (14 pt)}
#'   \item{"plot_subtitle"}{Plot subtitle font size (12 pt)}
#'   \item{"plot_caption"}{Plot caption font size (11 pt)}
#' }
#'
#' @keywords internal
ECEC_TEXT_SIZES <- c(
  axis_title   = 60,
  axis_text    = 55,
  legend_title = 60,
  legend_text  = 55,
  strip_text   = 55,
  plot_title   = 70,
  plot_subtitle = 60,
  plot_caption = 55
)

# ── Dimension helpers ─────────────────────────────────────────────────────────

#' Resolve a figure width
#'
#' Converts a size key (e.g. \code{"1/2"}) or a raw numeric value to an
#' absolute width in inches using \code{\link{ECEC_WIDTHS}}.
#'
#' @param width Either a key from \code{ECEC_WIDTHS} (character) or a positive
#'   numeric value in inches.
#'
#' @return A single numeric value (width in inches).
#' @export
#'
#' @examples
#' ecec_get_width("1/2")   # 9.5
#' ecec_get_width(7)       # 7
ecec_get_width <- function(width = "1/2") {
  if (is.character(width)) {
    if (!width %in% names(ECEC_WIDTHS)) {
      stop(
        "Unknown width key '", width, "'. ",
        "Valid keys: ", paste(names(ECEC_WIDTHS), collapse = ", ")
      )
    }
    return(unname(ECEC_WIDTHS[width]))
  }
  if (!is.numeric(width) || width <= 0) {
    stop("'width' must be a positive number or a key from ECEC_WIDTHS.")
  }
  width
}

#' Resolve a figure height
#'
#' Converts a size key (e.g. \code{"medium"}) or a raw numeric value to an
#' absolute height in inches using \code{\link{ECEC_HEIGHTS}}.
#'
#' @param height Either a key from \code{ECEC_HEIGHTS} (character) or a
#'   positive numeric value in inches.
#'
#' @return A single numeric value (height in inches).
#' @export
#'
#' @examples
#' ecec_get_height("1/2")  # 9.5
#' ecec_get_height(5)         # 5
ecec_get_height <- function(height = "1/2") {
  if (is.character(height)) {
    if (!height %in% names(ECEC_HEIGHTS)) {
      stop(
        "Unknown height key '", height, "'. ",
        "Valid keys: ", paste(names(ECEC_HEIGHTS), collapse = ", ")
      )
    }
    return(unname(ECEC_HEIGHTS[height]))
  }
  if (!is.numeric(height) || height <= 0) {
    stop("'height' must be a positive number or a key from ECEC_HEIGHTS.")
  }
  height
}

# ── Font loading ──────────────────────────────────────────────────────────────

# Package-level font name used in ecec_theme()
.ecec_font <- new.env(parent = emptyenv())
.ecec_font$name <- "sans"   # fallback until fonts are loaded

#' Load ECEC package fonts
#'
#' Scans the package \code{inst/fonts} directory (installed) or a
#' \code{fonts/} directory relative to the package source root for
#' \code{.ttf} files and registers them with \pkg{sysfonts} /
#' \pkg{showtext}.  The first font found becomes the default font used by
#' \code{\link{ecec_theme}}.
#'
#' Place your \code{.ttf} font files in the \code{fonts/} folder at the
#' repository root before building the package, or in
#' \code{inst/fonts/} inside the package source.
#'
#' @return Invisible \code{NULL}.  Called for its side-effect of registering
#'   fonts.
#' @importFrom sysfonts font_add
#' @importFrom showtext showtext_auto
#' @importFrom tools file_path_sans_ext
#' @export
ecec_load_fonts <- function() {
  # Prefer installed package fonts; fall back to source-tree fonts/
  font_dir <- system.file("fonts", package = "ececplots")
  if (!nzchar(font_dir)) {
    font_dir <- file.path(find.package("ececplots", quiet = TRUE), "fonts")
  }
  # Development fallback: fonts/ at the repo root
  if (!nzchar(font_dir) || !dir.exists(font_dir)) {
    font_dir <- file.path(getwd(), "fonts")
  }

  if (!dir.exists(font_dir)) {
    message(
      "ececplots: fonts directory not found. ",
      "Using system default font. Add .ttf files to inst/fonts/ to customise."
    )
    return(invisible(NULL))
  }

  ttf_files <- list.files(font_dir, pattern = "\\.ttf$",
                          full.names = TRUE, ignore.case = TRUE)

  if (length(ttf_files) == 0L) {
    message(
      "ececplots: No .ttf files found in '", font_dir, "'. ",
      "Using system default font."
    )
    return(invisible(NULL))
  }

  loaded <- character(0)
  for (f in ttf_files) {
    font_name <- tools::file_path_sans_ext(basename(f))
    tryCatch({
      sysfonts::font_add(font_name, f)
      loaded <- c(loaded, font_name)
    }, error = function(e) {
      message("ececplots: Could not load font '", basename(f), "': ", e$message)
    })
  }

  if (length(loaded) > 0L) {
    .ecec_font$name <- loaded[1L]
    showtext::showtext_auto()
    message("ececplots: Loaded fonts: ", paste(loaded, collapse = ", "),
            ". Active font: '", .ecec_font$name, "'.")
  }

  invisible(NULL)
}

# ── Base ggplot2 theme ────────────────────────────────────────────────────────

#' ECEC base ggplot2 theme
#'
#' A clean, minimal theme used by all \pkg{ececplots} plot functions.
#' Inherits from \code{\link[ggplot2]{theme_bw}} and applies the font loaded
#' by \code{\link{ecec_load_fonts}}.
#'
#' @param text_scale Scaling factor for text sizes (default \code{1}).
#' @param legend_position Position of the legend; passed directly to
#'   \code{ggplot2::theme(legend.position = ...)}.
#' @param legend_nrow Number of rows in the legend (default \code{NULL}: automatic).
#' @param legend_ncol Number of columns in the legend (default \code{NULL}: automatic).
#' @param x_angle Rotation angle for x-axis labels in degrees (default \code{0}).
#' @param y_angle Rotation angle for y-axis labels in degrees (default \code{0}).
#'
#' @return A \code{ggplot2::theme} object.
#' @importFrom ggplot2 theme_bw theme element_text element_blank element_line element_rect margin guide_legend
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ecec_theme()
ecec_theme <- function(text_scale = 1, legend_position = "top", 
                       legend_nrow = NULL, legend_ncol = NULL,
                       x_angle = 0, y_angle = 0) {
  font <- .ecec_font$name

  theme_obj <- ggplot2::theme_bw(base_family = font) +
    ggplot2::theme(
      # Panel
      panel.grid.major   = ggplot2::element_line(colour = "#FFFFFF",
                                                  linewidth = 0.4),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_rect(colour = "#000000",
                                                  fill = NA, linewidth = 0.6),
      panel.background   = ggplot2::element_rect(fill = "#E5E5E5"),
      # Axes
      axis.title         = ggplot2::element_text(size = ECEC_TEXT_SIZES["axis_title"] * text_scale,
                                                  colour = "#000000",
                                                  margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)),
      axis.text.x        = ggplot2::element_text(size = ECEC_TEXT_SIZES["axis_text"] * text_scale,
                                                  colour = "#000000",
                                                  angle = x_angle,
                                                  hjust = if (x_angle != 0) 1 else 0.5,
                                                  vjust = if (x_angle != 0) 0.5 else 1),
      axis.text.y        = ggplot2::element_text(size = ECEC_TEXT_SIZES["axis_text"] * text_scale,
                                                  colour = "#000000",
                                                  angle = y_angle,
                                                  hjust = if (y_angle != 0) 0.5 else 1,
                                                  vjust = if (y_angle != 0) 0.5 else 0.5),
      axis.ticks         = ggplot2::element_line(colour = "#000000",
                                                  linewidth = 0.4),
      # Legend
      legend.position    = legend_position,
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = ECEC_TEXT_SIZES["legend_text"] * text_scale,
                                                  colour = "#000000"),
      legend.background  = ggplot2::element_rect(fill = "#E5E5E5",
                                                  colour = "#000000",
                                                  linewidth = 0.6),
      legend.key         = ggplot2::element_rect(fill = "#E5E5E5",
                                                  colour = NA),
      # Strip (facet labels)
      strip.background   = ggplot2::element_rect(fill = "#f5f5f5",
                                                  colour = "#cccccc"),
      strip.text         = ggplot2::element_text(size = ECEC_TEXT_SIZES["strip_text"] * text_scale,
                                                  colour = "#000000"),
      # Plot title / subtitle / caption
      plot.title         = ggplot2::element_text(size = ECEC_TEXT_SIZES["plot_title"] * text_scale,
                                                  colour = "#000000",
                                                  face = "bold",
                                                  margin = margin(b = 6 * text_scale)),
      plot.subtitle      = ggplot2::element_text(size = ECEC_TEXT_SIZES["plot_subtitle"] * text_scale,
                                                  colour = "#000000",
                                                  margin = margin(b = 8 * text_scale)),
      plot.caption       = ggplot2::element_text(size = ECEC_TEXT_SIZES["plot_caption"] * text_scale,
                                                  colour = "#000000",
                                                  hjust = 1),
      plot.background    = ggplot2::element_rect(fill = "#FFFFFF",
                                                  colour = NA),
      plot.margin        = ggplot2::margin(
        10 * text_scale, 
        10 * text_scale, 
        10 * text_scale, 
        10 * text_scale)
    )
  
  theme_obj
}

# ── Package initialisation ────────────────────────────────────────────────────

.onLoad <- function(libname, pkgname) {
  tryCatch(ecec_load_fonts(), error = function(e) NULL)
}

# ── Internal helpers shared across plot files ─────────────────────────────────

#' Validate orientation argument
#'
#' @param orientation Character string; must be \code{"vertical"} or
#'   \code{"horizontal"} (case-insensitive).
#' @return Normalised orientation string (lower-case).
#' @keywords internal
.check_orientation <- function(orientation) {
  orientation <- tolower(orientation)
  if (!orientation %in% c("vertical", "horizontal")) {
    stop("'orientation' must be \"vertical\" or \"horizontal\".")
  }
  orientation
}

#' Validate that column names exist in a data frame
#'
#' Stops with an informative message if any of the supplied column names are
#' not present in \code{data}.  \code{NULL} entries are silently ignored so
#' that optional column arguments can be passed directly.
#'
#' @param data A data frame.
#' @param ... Character column names (or \code{NULL}) to check.
#' @keywords internal
.check_cols <- function(data, ...) {
  cols    <- c(...)
  missing <- setdiff(cols[!is.na(cols) & !is.null(cols)], names(data))
  if (length(missing)) {
    stop("Column(s) not found in data: ", paste(missing, collapse = ", "))
  }
}
