# Fonts Directory

Place your `.ttf` font files in this directory.

They will be automatically discovered and loaded by `ecec_load_fonts()` when
the package is attached (via the `.onLoad` hook in `R/utils.R`).

## How it works

1. Add one or more `.ttf` files here (e.g. `Calibri.ttf`, `Arial.ttf`).
2. When the package loads it registers every font with **sysfonts** and enables
   **showtext** so that ggplot2 renders text with your fonts correctly.
3. The *first* font found (alphabetically) becomes the default font used by
   `ecec_theme()`.  You can override the order by prefixing file names
   (e.g. `01_primary_font.ttf`, `02_secondary_font.ttf`).

## Package build

For fonts to be included in the installed package they must live in
`inst/fonts/` (this directory is automatically copied during `R CMD INSTALL`
or `devtools::install()`).  The `fonts/` directory at the repository root is
a convenience location for development; `ecec_load_fonts()` falls back to it
when running from source.

## Font licensing

Make sure you have the appropriate rights to redistribute any font files
you add here.
