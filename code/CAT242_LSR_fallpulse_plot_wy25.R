# --- Code Description ---
# Developed October 2025 to plot predicted fall pulse window (based on Yarnell et al 2022 Functional Flow metrics) and to highlight observed fall pulse for WY2025 at LSR.
# Created for CAT242 

# --- Load required packages ---
library(readxl)
library(ggplot2)
library(scales)

# --- User input: path to Excel file ---
excel_path <- "C:/Users/AdrienneChenette/Desktop/LSR_predicted_fallpulsewindow_wy25.xlsx"  # Adjust path if needed

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

# --- Define highlight period ---
# This one is based on observed fall pulse window for wy25
highlight_start <- as.POSIXct("2024-11-21", tz = "UTC")
highlight_end   <- as.POSIXct("2024-11-27", tz = "UTC")

# --- Create highlight flag ---
df <- df |>
  mutate(highlight = if_else(datetime >= highlight_start & datetime <= highlight_end,
                             "highlight", "normal"))

# --- Plot ---
ggplot(df, aes(x = datetime, y = discharge_cfs)) +
  # --- Base shaded area under main hydrograph line ---
  geom_ribbon(aes(ymin = 0, ymax = discharge_cfs), fill = "#008DA5", alpha = 0.3) +
  
  # --- shaded highlight overlay ---
  geom_ribbon(data = filter(df, datetime >= highlight_start & datetime <= highlight_end),
              aes(ymin = 0, ymax = discharge_cfs),
              fill = "#F9A51A", alpha = 0.4) +
  
  # --- Colored line for main hydrograph ---
  geom_line(color = "#008DA5", size = 0.8) +
  
  # --- Colored overlay line for highlighted period (observed fall pulse)---
  geom_line(data = filter(df, datetime >= highlight_start & datetime <= highlight_end),
            color = "#F9A51A", size = 1.2) +
  
  # --- ggplot2 classic theme look ---
  theme_gray(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  
  labs(
    title = "Predicted Fall Pulse window based on Natural Functional Flow Metrics with Observed Fall Pulse (Nov 21–27, 2024)",
    x = "Date",
    y = "Discharge (CFS)"
  ) +
  scale_y_continuous(
    limits = c(0, 90),               # set range
    breaks = seq(0, 90, by = 10),    # tick every 10 CFS
    labels = scales::comma,          # format with commas
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "7 days")

ggsave(filename = "output/CAT242_LSR_fallpulse_plot.png", dpi = 300, width = 8, height = 5, units = "in")
