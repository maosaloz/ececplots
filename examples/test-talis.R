# ── Setup ─────────────────────────────────────────────────────────────────────
pkgload::load_all(here::here())
library(tidyverse)
library(haven)
library(readxl)

out_dir <- here::here("examples", "output", "barplots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

file_path <- "V:\\ECEC_TALIS_STARTING_STRONG_2024\\results\\Initial Report\\Chapter_Future\\R tables\\talis3s2024_ir_future_r-formatted-tables_v2025-11-06.RData"

e <- new.env()      # creating environment to store the datafiles
data <- load(
  file_path,
  envir = e
)

figure <- e$`Table FUTURE.1_sq14h_sq16i`
rm(e)

# ── Figure 9.4 ─────────────────────────────────────────────────────────────────────

figure <- figure %>%
  select(Countries, level, b.meanpct.d_cat_ss2g14h_ss2g16i_1, b.meanpct.d_cat_ss2g14h_ss2g16i_2,
         b.meanpct.d_cat_ss2g14h_ss2g16i_3, b.meanpct.d_cat_ss2g14h_ss2g16i_4) %>%
  rename(
    "Neither in initial education and training nor in continuous professional development activities" = b.meanpct.d_cat_ss2g14h_ss2g16i_1,
    "Initial education/training programmes only" = b.meanpct.d_cat_ss2g14h_ss2g16i_2,
    "Continuous professional development only" = b.meanpct.d_cat_ss2g14h_ss2g16i_3,
    "Both initial training and continuous professional development" = b.meanpct.d_cat_ss2g14h_ss2g16i_4
  )%>%
  drop_na()%>%
  pivot_longer(
    cols = c(
      "Neither in initial education and training nor in continuous professional development activities",
      "Initial education/training programmes only",
      "Continuous professional development only",
      "Both initial training and continuous professional development"
    ),
    names_to = "Indicator",
    values_to = "Values"
  )%>%
  filter(Countries != "TALIS Starting Strong average (ISCED 02)")

# Example 1 - having a bar chart divided by ECEC level
example1 <- ecec_bar_stacked(
  data        = figure,
  x           = "Countries",
  y           = "Values",
  fill        = "Indicator",
  facet       = "level",
  facet_var   = c("ISCED 02", "Under age 3"),
  orientation = "vertical",
  position    = "stack",
  title       = "",
  subtitle    = "",
  x_label     = "",
  y_label     = "", 
  x_angle     = 90,
  legend_nrow = 4,
  legend_ncol = 1
)
ecec_save(example1,
          file   = file.path(out_dir, "TALIS Figure 9.4.png"),
          layout = ecec_base(width = "2/3", height = "2/3"))
