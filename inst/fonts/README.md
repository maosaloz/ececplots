# inst/fonts

This directory is the **installed** location for ECEC custom fonts.

When the package is built with `R CMD build` / `devtools::build()`, all files
in `inst/fonts/` are copied into the installed package tree and become
accessible via `system.file("fonts", package = "ececplots")`.

## Adding fonts

1. Copy your `.ttf` files into this directory.
2. The same file should also exist (or be symlinked) in the repository-root
   `fonts/` directory so that `ecec_load_fonts()` can find them during
   interactive development (`devtools::load_all()`).
3. Re-install the package (`devtools::install()`) to make the fonts available
   in the installed copy.
