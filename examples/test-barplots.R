# ── Setup ─────────────────────────────────────────────────────────────────────
pkgload::load_all(here::here())
library(readxl)

out_dir <- here::here("examples", "output", "barplots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path <- here::here("data", "data.xlsx")

# Load sheets
gov_exp  <- read_excel(data_path, sheet = "Government expenditure on educa")
priv_exp <- read_excel(data_path, sheet = "Private expenditure on educatio")
enrol_02 <- read_excel(data_path, sheet = "Enrolment rate, ages 0-2")
enrol_35 <- read_excel(data_path, sheet = "Enrolment rate, ages 3-5")

names(gov_exp)  <- c("country", "year", "gov_exp_pct")
names(priv_exp) <- c("country", "year", "priv_exp_pct")
names(enrol_02) <- c("country", "year", "enrol_02")
names(enrol_35) <- c("country", "year", "enrol_35")

# Stacked data: combine govt + private expenditure in long format
exp_long <- rbind(
  data.frame(country    = gov_exp$country,
             exp_type   = "Government",
             exp_pct    = gov_exp$gov_exp_pct,
             stringsAsFactors = FALSE),
  data.frame(country    = priv_exp$country,
             exp_type   = "Private",
             exp_pct    = priv_exp$priv_exp_pct,
             stringsAsFactors = FALSE)
)
exp_long <- exp_long[complete.cases(exp_long), ]

# Use a subset of 12 countries for visual clarity
sel_countries <- c("Australia", "Austria", "Belgium", "Canada", "Denmark",
                   "Finland", "France", "Germany", "Netherlands", "Norway",
                   "Sweden", "United Kingdom")
exp_sub <- exp_long[exp_long$country %in% sel_countries, ]

# Enrolment data (drop NAs, subset countries)
enrol_02_sub <- enrol_02[enrol_02$country %in% sel_countries &
                            !is.na(enrol_02$enrol_02), ]
enrol_35_sub <- enrol_35[enrol_35$country %in% sel_countries &
                            !is.na(enrol_35$enrol_35), ]

layout <- ecec_base(width = "2/3", height = "2/3")

# ── ecec_bar_stacked ──────────────────────────────────────────────────────────

# 1. Vertical stacked bar: total expenditure split by type
p_stacked_v <- ecec_bar_stacked(
  data        = exp_sub,
  x           = "country",
  y           = "exp_pct",
  fill        = "exp_type",
  orientation = "vertical",
  position    = "stack",
  title       = "Government and private expenditure on ECEC",
  subtitle    = "Percent of GDP per capita, 2022",
  x_label     = "",
  y_label     = "", 
  x_angle     = 90
)
ecec_save(p_stacked_v,
          file   = file.path(out_dir, "ecec_bar_stacked.png"),
          layout = layout)

# 2. Horizontal stacked bar (proportion fill)
p_stacked_h <- ecec_bar_stacked(
  data        = exp_sub,
  x           = "country",
  y           = "exp_pct",
  fill        = "exp_type",
  orientation = "horizontal",
  position    = "fill",
  title       = "ECEC Expenditure Mix by Country (Horizontal, 100% Fill)",
  subtitle    = "Share of government vs private expenditure, 2022",
  x_label     = "Country",
  y_label     = "Proportion"
)
ecec_save(p_stacked_h,
          file   = file.path(out_dir, "ecec_bar_stacked_horizontal.png"),
          layout = layout)

# ── ecec_bar_grouped ──────────────────────────────────────────────────────────

# 3. Vertical grouped bar: govt vs private side by side
p_grouped_v <- ecec_bar_grouped(
  data        = exp_sub,
  x           = "country",
  y           = "exp_pct",
  fill        = "exp_type",
  orientation = "vertical",
  title       = "ECEC Expenditure by Country (Grouped, Vertical)",
  subtitle    = "Government and private expenditure, % GDP per capita, 2022",
  x_label     = "Country",
  y_label     = "Expenditure (% GDP per capita)"
)
ecec_save(p_grouped_v,
          file   = file.path(out_dir, "ecec_bar_grouped.png"),
          layout = layout)

# 4. Horizontal grouped bar
p_grouped_h <- ecec_bar_grouped(
  data        = exp_sub,
  x           = "country",
  y           = "exp_pct",
  fill        = "exp_type",
  orientation = "horizontal",
  title       = "ECEC Expenditure by Country (Grouped, Horizontal)",
  subtitle    = "Government and private expenditure, % GDP per capita, 2022",
  x_label     = "Country",
  y_label     = "Expenditure (% GDP per capita)"
)
ecec_save(p_grouped_h,
          file   = file.path(out_dir, "ecec_bar_grouped_horizontal.png"),
          layout = layout)

# ── ecec_bar_scatter ──────────────────────────────────────────────────────────

# 5. Bar + scatter (mean bar + individual country dots): enrolment 0-2
p_bar_scatter_v <- ecec_bar_scatter(
  data          = enrol_02_sub,
  x             = "year",
  y             = "enrol_02",
  bar_stat      = "summary",
  bar_fun       = "mean",
  orientation   = "vertical",
  title         = "ECEC Enrolment Rate Ages 0–2 (Bar + Scatter, Vertical)",
  subtitle      = "Mean bar with individual country values overlaid, 2023",
  x_label       = "Year",
  y_label       = "Enrolment rate (%)",
  text_scale    = 0.5
)
ecec_save(p_bar_scatter_v,
          file   = file.path(out_dir, "ecec_bar_scatter.png"),
          layout = ecec_base(width = "1/3", height = "1/3"))

# 6. Horizontal bar + scatter: enrolment 3-5 by country
p_bar_scatter_h <- ecec_bar_scatter(
  data          = enrol_35_sub,
  x             = "country",
  y             = "enrol_35",
  bar_stat      = "summary",
  bar_fun       = "mean",
  orientation   = "horizontal",
  title         = "ECEC Enrolment Rate Ages 3–5 (Bar + Scatter, Horizontal)",
  subtitle      = "Mean bar with individual country values, 2023",
  x_label       = "Country",
  y_label       = "Enrolment rate (%)"
)
ecec_save(p_bar_scatter_h,
          file   = file.path(out_dir, "ecec_bar_scatter_horizontal.png"),
          layout = layout)

message("All barplot PNGs saved to: ", out_dir)
