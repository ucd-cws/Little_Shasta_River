# --- Code Description ---
# Written to calculate Solar Radiation daily max. Based on data pulled from CDEC Weed Airport hourly solar radiation data. Used for Evan's Spring W3T modeling, October 2025. 

# --- Load required libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(lubridate)   # For date handling
library(ggplot2)     # For plotting
library(tibble)      # For tibbles

# --- Read in data file ---
# (replace with your actual file name & path)

WED_SR_WY25 <- read_excel("WED_WY25_SolarRadiationRawHourly.xlsx")

# --- Compute daily maximum values ---
daily_max <- WED_SR_WY25 %>%
  mutate(
    date_time = ymd_hm(date_time),   # Parse datetime (YYYY-MM-DD HH:MM)
    date = as_date(date_time),       # Extract just the date
    value = as.numeric(value)        # Ensure numeric values
  ) %>%
  group_by(date) %>%
  summarise(daily_max_value = max(value, na.rm = TRUE)) %>%
  arrange(date)


# --- Display the tibble to check results ---
daily_max_tbl <- as_tibble(daily_max)
print(daily_max_tbl)

# --- Plot daily max solar radiation ---
ggplot(daily_max, aes(x = date, y = daily_max_value)) +
  geom_point(color = "darkorchid3", size = 2) +
  labs(
    title = "Daily Maximum Solar Radiation",
    x = "Date",
    y = "Daily Maximum Value"
  ) +
  theme_minimal()

# --- Save data as new csv ---
write.csv(daily_max,
        "C:/Users/YourFilePathHere",
          row.names = FALSE)
