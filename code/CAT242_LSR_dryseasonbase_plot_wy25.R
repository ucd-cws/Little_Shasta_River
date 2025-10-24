# --- Code Description ---
# Developed to plot predicted and observed dry season base flow based on Yarnell et al 2022 Functional Flow metrics
# This version highlights two base flow ranges that will be used in W3T analysis
# Uses data pulled from Eyasco's LSR monitoring station
# Created for CAT242, October 2025 by A. Chenette

# --- Load required packages ---
library(readxl)
library(ggplot2)
library(scales)

# --- User input: path to Excel file ---
excel_path <- "C:/Users/AdrienneChenette/Desktop/LSR_predicted_drybaseflow_wy25.xlsx"  # Adjust path if needed

# --- Read data ---
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

# --- Define highlight start and end for range 1 ---
highlight1_start <- as.POSIXct("2025-07-31", tz = "UTC")
highlight1_end   <- as.POSIXct("2025-08-08", tz = "UTC")

# --- Define highlight start and end for range 2 ---
highlight2_start <- as.POSIXct("2025-08-11", tz = "UTC")
highlight2_end   <- as.POSIXct("2025-08-23", tz = "UTC")

# --- Plot ---
ggplot(df, aes(x = datetime, y = discharge_cfs)) +
  # --- To shade area under the main hydrograph line ---
  geom_ribbon(aes(ymin = 0, ymax = discharge_cfs),
              fill = "lightblue", alpha = 0.35) +
  
  # --- Stylize range 1 highlight ---
  geom_ribbon(data = filter(df, datetime >= highlight1_start & datetime <= highlight1_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "#EE4023", alpha = 0.4) +
  
  # --- Stylize range 2 highlight ---
  geom_ribbon(data = filter(df, datetime >= highlight2_start & datetime <= highlight2_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "#EE4023", alpha = 0.45) +
  
  # ---Stylize line for the main hydrograph ---
  geom_line(color = "#008DA5", size = 0.5) +
  
  # --- Stylize line for range 1 ---
  geom_line(data = filter(df, datetime >= highlight1_start & datetime <= highlight1_end),
            color = "#EE4023", size = 0.8) +
  
  # --- Stylize line for range 2 ---
  geom_line(data = filter(df, datetime >= highlight2_start & datetime <= highlight2_end),
            color = "#EE4023", size = 0.8) +
  
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
    title = "Predicted dry base flow period with two ranges identified",
    x = "Date",
    y = "Discharge (CFS)"
  ) +
  scale_y_continuous(
    limits = c(0, 14),               # set range
    breaks = seq(0, 14, by = 1),    # tick every 1 CFS
    labels = scales::comma,          # format with commas
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_datetime(
    date_labels = "%b %d",
    breaks = c(
      min(df$datetime, na.rm = TRUE),
      seq(min(df$datetime, na.rm = TRUE),
          max(df$datetime, na.rm = TRUE),
          by = "5 days"),
      max(df$datetime, na.rm = TRUE)
    ),
    expand = c(0, 0)
  )
