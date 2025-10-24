# --- Code Description ---
# Developed to plot predicted and observed fall pulse and wetseason base flow based on Yarnell et al 2022 Functional Flow metrics
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

# --- Inspect first few columns to confirm names ---
print(head(df))

# --- Rename columns as needed (adjust to your data) ---
# Example: assume columns are named "Date" and "Discharge_cfs"
df <- df |>
  rename(datetime = Date,
         discharge_cfs = '1 - TNC LS Host / RiverFlow / CalcFlow - cfs')

# --- Ensure datetime is in proper format ---
df$datetime <- as.POSIXct(df$datetime, tz = "UTC")

# --- Highlight windows ---
highlight1_start <- as.POSIXct("2024-11-21", tz = "UTC")
highlight1_end   <- as.POSIXct("2024-11-27", tz = "UTC")

highlight2_start <- as.POSIXct("2025-01-26", tz = "UTC")
highlight2_end   <- as.POSIXct("2025-02-01", tz = "UTC")

# --- Plot ---
ggplot(df, aes(x = datetime, y = discharge_cfs)) +
  # --- Base shaded area under the main hydrograph line ---
  geom_ribbon(aes(ymin = 0, ymax = discharge_cfs),
              fill = "lightblue", alpha = 0.35) +
  
  # --- Yellow highlight (Fall Pulse) ---
  geom_ribbon(data = filter(df, datetime >= highlight1_start & datetime <= highlight1_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "#F9A51A", alpha = 0.4) +
  
  # --- Bright pink highlight (Wet Season Baseflow used in W3T model) ---
  geom_ribbon(data = filter(df, datetime >= highlight2_start & datetime <= highlight2_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "deeppink", alpha = 0.45) +
  
  # ---line for the main hydrograph ---
  geom_line(color = "#008DA5", size = 0.5) +
  
  # --- Yellow overlay line for Fall Pulse ---
  geom_line(data = filter(df, datetime >= highlight1_start & datetime <= highlight1_end),
            color = "#F9A51A", size = 0.8) +
  
  # --- Pink overlay line for Wet Season Baseflow ---
  geom_line(data = filter(df, datetime >= highlight2_start & datetime <= highlight2_end),
            color = "deeppink3", size = 0.8) +

  # --- Classic ggplot2 theme (gray background, white grid) ---
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
    title = "Predicted wet season base with fall pulse highlighted",
    x = "Date",
    y = "Discharge (CFS)"
  ) +
  scale_y_continuous(
    limits = c(0, 160),               # set range
    breaks = seq(0, 160, by = 20),    # tick every 10 CFS
    labels = scales::comma,          # format with commas
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_datetime(
    date_labels = "%b %d",
    breaks = c(
      min(df$datetime, na.rm = TRUE),
      seq(min(df$datetime, na.rm = TRUE),
          max(df$datetime, na.rm = TRUE),
          by = "14 days"),
      max(df$datetime, na.rm = TRUE)
    ),
    expand = c(0, 0)
  )
