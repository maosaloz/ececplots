# ── Setup ─────────────────────────────────────────────────────────────────────
pkgload::load_all(here::here())
library(readxl)

out_dir <- here::here("examples", "output", "base")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path <- here::here("data", "data.xlsx")

# Load and clean data
gov_exp  <- read_excel(data_path, sheet = "Government expenditure on educa")
priv_exp <- read_excel(data_path, sheet = "Private expenditure on educatio")
enrol_02 <- read_excel(data_path, sheet = "Enrolment rate, ages 0-2")
enrol_35 <- read_excel(data_path, sheet = "Enrolment rate, ages 3-5")

names(gov_exp)  <- c("country", "year", "gov_exp_pct")
names(priv_exp) <- c("country", "year", "priv_exp_pct")
names(enrol_02) <- c("country", "year", "enrol_02")
names(enrol_35) <- c("country", "year", "enrol_35")

gov_exp  <- gov_exp[complete.cases(gov_exp), ]
priv_exp <- priv_exp[complete.cases(priv_exp), ]
enrol_02 <- enrol_02[complete.cases(enrol_02), ]
enrol_35 <- enrol_35[complete.cases(enrol_35), ]

# Merge enrolment sheets for multi-panel use
enrol_both <- merge(enrol_02[, c("country", "enrol_02")],
                    enrol_35[, c("country", "enrol_35")],
                    by = "country")

# ── ecec_base: layout variants ────────────────────────────────────────────────

# 1. Default layout (1/2 width, medium height)
layout_default <- ecec_base()
print(layout_default)

# 2. Full-width, large height
layout_full <- ecec_base(width = "full", height = "large")

# 3. 1/3-page, small height
layout_small <- ecec_base(width = "1/3", height = "small")

# 4. Two-panel layout (1 row × 2 cols)
layout_2panel <- ecec_base(width = "full", height = "medium",
                           n_panels = 2, n_rows = 1, n_cols = 2)

# 5. Four-panel layout (2 rows × 2 cols)
layout_4panel <- ecec_base(width = "full", height = "large",
                           n_panels = 4, n_rows = 2, n_cols = 2)

# ── ecec_save: single plots with different layouts ────────────────────────────

# Single scatter saved with default layout
p1 <- ecec_scatter(
  gov_exp,
  x        = "gov_exp_pct",
  y        = "year",
  title    = "Government ECEC Expenditure (ecec_base default layout)",
  subtitle = "% GDP per capita, 2022",
  x_label  = "Expenditure (% GDP per capita)",
  y_label  = "Year"
)
ecec_save(p1,
          file   = file.path(out_dir, "ecec_base_default.png"),
          layout = layout_default)

# Single bar saved with full-width large layout
exp_long <- rbind(
  data.frame(country  = gov_exp$country,
             exp_type = "Government",
             exp_pct  = gov_exp$gov_exp_pct),
  data.frame(country  = priv_exp$country,
             exp_type = "Private",
             exp_pct  = priv_exp$priv_exp_pct)
)
sel <- c("Australia", "Austria", "Belgium", "Canada", "Denmark",
         "Finland", "France", "Germany", "Netherlands", "Norway",
         "Sweden", "United Kingdom")
exp_sub <- exp_long[exp_long$country %in% sel, ]

p2 <- ecec_bar_stacked(
  exp_sub,
  x        = "country",
  y        = "exp_pct",
  fill     = "exp_type",
  title    = "ECEC Expenditure (ecec_base full-width large)",
  subtitle = "% GDP per capita, 2022",
  x_label  = "Country",
  y_label  = "Expenditure (%)"
)
ecec_save(p2,
          file   = file.path(out_dir, "ecec_base_full_large.png"),
          layout = layout_full)

# Small layout
p3 <- ecec_scatter(
  enrol_02,
  x        = "enrol_02",
  y        = "year",
  title    = "Enrolment 0–2 (ecec_base 1/3 small)",
  x_label  = "Enrolment rate (%)",
  y_label  = "Year"
)
ecec_save(p3,
          file   = file.path(out_dir, "ecec_base_third_small.png"),
          layout = layout_small)

# ── ecec_combine: multi-panel figures ─────────────────────────────────────────

# 2-panel: enrolment 0-2 and enrolment 3-5 side by side
p_e02 <- ecec_scatter(
  enrol_02,
  x        = "enrol_02",
  y        = "year",
  title    = "Enrolment Rate Ages 0–2",
  x_label  = "Enrolment rate (%)",
  y_label  = "Year"
)
p_e35 <- ecec_scatter(
  enrol_35,
  x        = "enrol_35",
  y        = "year",
  title    = "Enrolment Rate Ages 3–5",
  x_label  = "Enrolment rate (%)",
  y_label  = "Year"
)
combo_2 <- ecec_combine(list(p_e02, p_e35), layout = layout_2panel)
ecec_save(combo_2,
          file   = file.path(out_dir, "ecec_combine_2panel.png"),
          layout = layout_2panel)

# 4-panel: scatter + grouped bar + stacked bar + quadrant
quad_df <- merge(gov_exp[, c("country", "gov_exp_pct")],
                 priv_exp[, c("country", "priv_exp_pct")],
                 by = "country")
quad_df <- quad_df[complete.cases(quad_df), ]
quad_df$region <- ifelse(quad_df$country %in%
  c("Australia", "Canada", "Chile", "Colombia", "Costa Rica",
    "Mexico", "New Zealand", "United States"),
  "Americas/Pacific", "Europe/Other")

p_a <- ecec_scatter(
  quad_df,
  x       = "gov_exp_pct",
  y       = "priv_exp_pct",
  colour  = "region",
  title   = "A: Gov vs Private Exp",
  x_label = "Government (%)",
  y_label = "Private (%)"
)
p_b <- ecec_scatter_quadrant(
  quad_df,
  x       = "gov_exp_pct",
  y       = "priv_exp_pct",
  x_line  = median(quad_df$gov_exp_pct),
  y_line  = median(quad_df$priv_exp_pct),
  colour  = "region",
  title   = "B: Expenditure Quadrant",
  x_label = "Government (%)",
  y_label = "Private (%)"
)
p_c <- ecec_bar_grouped(
  exp_sub,
  x       = "country",
  y       = "exp_pct",
  fill    = "exp_type",
  title   = "C: Expenditure Grouped",
  x_label = "Country",
  y_label = "Exp (%)"
)
p_d <- ecec_bar_stacked(
  exp_sub,
  x           = "country",
  y           = "exp_pct",
  fill        = "exp_type",
  orientation = "horizontal",
  position    = "fill",
  title       = "D: Expenditure Mix",
  x_label     = "Country",
  y_label     = "Proportion"
)
combo_4 <- ecec_combine(list(p_a, p_b, p_c, p_d), layout = layout_4panel)
ecec_save(combo_4,
          file   = file.path(out_dir, "ecec_combine_4panel.png"),
          layout = layout_4panel)

message("All base/layout PNGs saved to: ", out_dir)
