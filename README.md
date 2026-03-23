# ececplots

> Repository holding the code for the ECEC/ECS teams visualization package
> used in reports and any kind of media.

---

## Overview

**ececplots** is an R package that provides a consistent set of visualization
functions for the ECEC/ECS teams.  Every chart uses the same:

| Feature | Detail |
|---|---|
| Width dictionary | `"1/3"` = 6 in, `"1/2"` = 9.5 in, `"2/3"` = 13 in, `"full"` = 19 in |
| Height dictionary | `"small"` = 4 in, `"medium"` = 6 in, `"large"` = 9 in |
| Font | `.ttf` files placed in `fonts/` / `inst/fonts/` |
| Theme | `ecec_theme()` – clean ggplot2 style |

A companion **Excel VBA macro** (`excel/ececplots_macro.bas`) lets you import
the exported figures into Excel and attach the underlying data as a separate
worksheet.

---

## Package structure

```
ececplots/
├── R/
│   ├── utils.R        # ECEC_WIDTHS, ECEC_HEIGHTS, ecec_load_fonts(),
│   │                  # ecec_theme(), ecec_get_width(), ecec_get_height()
│   ├── base.R         # ecec_base(), ecec_combine(), ecec_save()
│   ├── scatterplot.R  # ecec_scatter(), ecec_scatter_quadrant(),
│   │                  # ecec_scatter_connected()
│   └── barplots.R     # ecec_bar_stacked(), ecec_bar_grouped(),
│                      # ecec_bar_scatter()
├── fonts/             # Add your .ttf font files here (development)
├── inst/fonts/        # .ttf fonts installed with the package
├── excel/
│   └── ececplots_macro.bas   # Excel VBA macro
├── tests/testthat/    # Package tests
├── DESCRIPTION
└── NAMESPACE
```

---

## Installation

```r
# From source (inside the repo root)
devtools::install()

# Or using pak
pak::pkg_install("maosaloz/ececplots")
```

---

## Adding custom fonts

1. Copy your `.ttf` font files into the `fonts/` folder (repo root).
2. Also copy them into `inst/fonts/` so they are included in the installed
   package.
3. `ecec_load_fonts()` is called automatically when the package loads.  The
   first font found becomes the default used by `ecec_theme()`.

---

## Quick start

```r
library(ececplots)

# --- Layout frame -----------------------------------------------------------
layout <- ecec_base(width = "1/2", height = "medium")

# --- Scatterplots -----------------------------------------------------------
# Simple scatter
p1 <- ecec_scatter(mtcars, x = "wt", y = "mpg", colour = "cyl",
                   title = "Weight vs Fuel Economy")

# Quadrant scatter with dashed reference lines
p2 <- ecec_scatter_quadrant(mtcars, x = "wt", y = "mpg",
                             x_line = 3.2, y_line = 20,
                             colour = "cyl",
                             title  = "Quadrant: Weight vs MPG")

# Scatter with connecting lines between points
p3 <- ecec_scatter_connected(mtcars, x = "wt", y = "mpg", group = "cyl",
                              title = "Connected: Weight vs MPG by Cylinder")

# --- Barplots ---------------------------------------------------------------
df <- data.frame(
  category = rep(c("A", "B", "C"), each = 2),
  segment  = rep(c("X", "Y"), times = 3),
  value    = c(10, 15, 8, 12, 9, 14)
)

# Stacked (vertical / horizontal)
p4 <- ecec_bar_stacked(df, x = "category", y = "value", fill = "segment")
p5 <- ecec_bar_stacked(df, x = "category", y = "value", fill = "segment",
                       orientation = "horizontal")

# Grouped / non-stacked (vertical / horizontal)
p6 <- ecec_bar_grouped(df, x = "category", y = "value", fill = "segment")
p7 <- ecec_bar_grouped(df, x = "category", y = "value", fill = "segment",
                       orientation = "horizontal")

# Bar + scatter overlay (vertical / horizontal)
p8  <- ecec_bar_scatter(mtcars, x = "cyl", y = "mpg",
                        title = "MPG by Cylinders")
p9  <- ecec_bar_scatter(mtcars, x = "cyl", y = "mpg",
                        orientation = "horizontal",
                        title = "Horizontal: MPG by Cylinders")

# --- Save at correct dimensions ---------------------------------------------
ecec_save(p1, "scatter_wt_mpg.png", layout)

# --- Multi-panel figure -----------------------------------------------------
layout2 <- ecec_base(width = "2/3", height = "medium",
                     n_panels = 2, n_rows = 1, n_cols = 2)
combined <- ecec_combine(list(p1, p2), layout2)
ecec_save(combined, "scatter_2panel.png", layout2)
```

---

## Width & Height reference

```r
# Print available keys
ECEC_WIDTHS   # "1/3"=6, "1/2"=9.5, "2/3"=13, "full"=19
ECEC_HEIGHTS  # "small"=4, "medium"=6, "large"=9

# Or resolve programmatically
ecec_get_width("1/3")    # 6
ecec_get_height("large") # 9
```

---

## Excel macro

The companion macro in `excel/ececplots_macro.bas` lets you import the
exported PNG files into Excel with the data attached.

**Setup:**
1. Open Excel and press **Alt + F11** to open the VBA editor.
2. Choose **Insert → Module** and paste the contents of
   `excel/ececplots_macro.bas`.
3. Close the editor.

**Running:**
- Press **Alt + F8**, select `ImportEcecPlots`, and click **Run**.
- You will be prompted to select an image file exported by `ecec_save()` and,
  optionally, a CSV of the underlying data.
- The macro creates:
  - A **`Plot_<name>`** worksheet containing the image.
  - A **`Data_<name>`** worksheet with the CSV data formatted as an Excel
    table (if a CSV was provided).
  - A **`Summary`** sheet listing all imported plots with hyperlinks.

For bulk import, run `ExportAllPlotsFromFolder` instead.
