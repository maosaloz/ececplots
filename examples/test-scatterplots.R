# ── Setup ─────────────────────────────────────────────────────────────────────
pkgload::load_all(here::here())
library(readxl)

out_dir <- here::here("examples", "output", "scatterplots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path <- here::here("data", "data.xlsx")

# Load sheets
gov_exp  <- read_excel(data_path, sheet = "Government expenditure on educa")
priv_exp <- read_excel(data_path, sheet = "Private expenditure on educatio")
enrol_02 <- read_excel(data_path, sheet = "Enrolment rate, ages 0-2")
enrol_35 <- read_excel(data_path, sheet = "Enrolment rate, ages 3-5")

# Rename columns for convenience
names(gov_exp)  <- c("country", "year", "gov_exp_pct")
names(priv_exp) <- c("country", "year", "priv_exp_pct")
names(enrol_02) <- c("country", "year", "enrol_02")
names(enrol_35) <- c("country", "year", "enrol_35")

# Quadrant data: merge govt vs private expenditure (drop NAs)
quad_df <- merge(gov_exp[, c("country", "gov_exp_pct")],
                 priv_exp[, c("country", "priv_exp_pct")],
                 by = "country")
quad_df <- quad_df[complete.cases(quad_df), ]

# Connected data: enrolment 0-2 vs 3-5 by country (merge on country)
connected_df <- merge(enrol_02[, c("country", "enrol_02")],
                      enrol_35[, c("country", "enrol_35")],
                      by = "country")
connected_df <- connected_df[complete.cases(connected_df), ]
# Reshape to long for connected plot
connected_long <- rbind(
  data.frame(country = connected_df$country,
             age_group = "Ages 0-2",
             enrolment = connected_df$enrol_02,
             order = 1),
  data.frame(country = connected_df$country,
             age_group = "Ages 3-5",
             enrolment = connected_df$enrol_35,
             order = 2)
)

layout <- ecec_base(width = "1/2", height = "medium")

# ── ecec_scatter ──────────────────────────────────────────────────────────────

# 1. Basic scatter: government expenditure vs private expenditure
p_scatter <- ecec_scatter(
  data     = quad_df,
  x        = "gov_exp_pct",
  y        = "priv_exp_pct",
  title    = "Government vs Private ECEC Expenditure",
  subtitle = "Percentage of GDP per capita, 2022",
  x_label  = "Government expenditure (% GDP per capita)",
  y_label  = "Private expenditure (% GDP per capita)"
)
ecec_save(p_scatter,
          file   = file.path(out_dir, "ecec_scatter.png"),
          layout = layout)

# 2. Scatter with colour by broad region (using first letter of country as proxy)
quad_df$region <- ifelse(quad_df$country %in%
  c("Australia", "Canada", "Chile", "Colombia", "Costa Rica",
    "Mexico", "New Zealand", "United States"),
  "Americas/Pacific", "Europe/Other")

p_scatter_colour <- ecec_scatter(
  data     = quad_df,
  x        = "gov_exp_pct",
  y        = "priv_exp_pct",
  colour   = "region",
  title    = "Government vs Private ECEC Expenditure",
  subtitle = "Coloured by broad region",
  x_label  = "Government expenditure (% GDP per capita)",
  y_label  = "Private expenditure (% GDP per capita)"
)
ecec_save(p_scatter_colour,
          file   = file.path(out_dir, "ecec_scatter_colour.png"),
          layout = layout)

# ── ecec_scatter_quadrant ─────────────────────────────────────────────────────

# Quadrant split at median values
x_mid <- median(quad_df$gov_exp_pct, na.rm = TRUE)
y_mid <- median(quad_df$priv_exp_pct, na.rm = TRUE)

p_quadrant <- ecec_scatter_quadrant(
  data        = quad_df,
  x           = "gov_exp_pct",
  y           = "priv_exp_pct",
  x_line      = x_mid,
  y_line      = y_mid,
  colour      = "region",
  title       = "ECEC Expenditure Quadrant Chart",
  subtitle    = paste0("Reference lines at medians (Gov: ",
                       round(x_mid, 1), "%, Priv: ",
                       round(y_mid, 1), "%)"),
  x_label     = "Government expenditure (% GDP per capita)",
  y_label     = "Private expenditure (% GDP per capita)"
)
ecec_save(p_quadrant,
          file   = file.path(out_dir, "ecec_scatter_quadrant.png"),
          layout = layout)

# ── ecec_scatter_connected ────────────────────────────────────────────────────

# Connected: enrolment rates for ages 0-2 vs 3-5, points connected per country
# Use a subset of countries for readability
top_countries <- head(sort(unique(connected_long$country)), 15)
connected_sub <- connected_long[connected_long$country %in% top_countries, ]

p_connected <- ecec_scatter_connected(
  data      = connected_sub,
  x         = "order",
  y         = "enrolment",
  group     = "country",
  colour    = "country",
  title     = "ECEC Enrolment Rates: Ages 0–2 vs Ages 3–5",
  subtitle  = "Each line = one country (selected OECD members, 2023)",
  x_label   = "Age group (1 = 0-2, 2 = 3-5)",
  y_label   = "Enrolment rate (%)"
)
ecec_save(p_connected,
          file   = file.path(out_dir, "ecec_scatter_connected.png"),
          layout = layout)

message("All scatterplot PNGs saved to: ", out_dir)
