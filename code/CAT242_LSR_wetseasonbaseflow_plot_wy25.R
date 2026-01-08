# --- Code Description ---
# Developed to plot observed wetseason base flow based on Yarnell et al 2022 Functional Flow metrics
# Highlights period of observed fall pulse flow and period used for wet season base flow for W3T model
# Uses data pulled from Eyasco's LSR monitoring station.
# Created for CAT242, October 2025 by A. Chenette

# --- Load packages ---
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# --- Read data ---
excel_path <- "C:/Users/AdrienneChenette/Desktop/LSR_wetseasonbase_data_wy25.xlsx"
df <- read_excel(excel_path)

# --- Rename columns ---
df <- df |>
  rename(datetime = Date,
         discharge_cfs = '1 - TNC LS Host / RiverFlow / CalcFlow - cfs')

# --- Ensure datetime is in proper format ---
df$datetime <- as.POSIXct(df$datetime, tz = "UTC")

# --- Filter data to 1/1/25 - 2/12/25 ---
df <- df |>
  filter(datetime >= as.POSIXct("2025-01-01", tz = "UTC") &
           datetime <= as.POSIXct("2025-02-12", tz = "UTC"))

# --- Highlight window for Wet Season Baseflow ---
highlight_start <- as.POSIXct("2025-01-26", tz = "UTC")
highlight_end   <- as.POSIXct("2025-02-01", tz = "UTC")

# --- Plot ---
ggplot(df, aes(x = datetime, y = discharge_cfs)) +
  # --- Base shaded area under the main hydrograph line ---
  geom_ribbon(aes(ymin = 0, ymax = discharge_cfs),
              fill = "lightblue", alpha = 0.35) +
  
  # --- Bright pink highlight (Wet Season Baseflow used in W3T model) ---
  geom_ribbon(data = filter(df, datetime >= highlight_start & datetime <= highlight_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "deeppink", alpha = 0.45) +
  
  # --- Line for the main hydrograph ---
  geom_line(color = "#008DA5", size = 0.5) +
  
  # --- Pink overlay line for Wet Season Baseflow ---
  geom_line(data = filter(df, datetime >= highlight_start & datetime <= highlight_end),
            color = "deeppink3", size = 0.8) +
  
  # --- Theme ---
  theme_gray(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  # --- Labels and scales ---
  labs(
    title = "Observed wet season baseflow",
    x = "Date",
    y = "Discharge (CFS)"
  ) +
  scale_y_continuous(
    limits = c(0, 160),
    breaks = seq(0, 160, by = 20),
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_datetime(
    date_labels = "%b %d",
    breaks = seq(as.POSIXct("2025-01-01", tz = "UTC"),
                 as.POSIXct("2025-02-12", tz = "UTC"),
                 by = "7 days"),
    expand = c(0, 0)
  )

ggsave(filename = "output/CAT242_LSR_obsv_wet_season_base_plot.png", dpi = 300, width = 8, height = 5, units = "in")
