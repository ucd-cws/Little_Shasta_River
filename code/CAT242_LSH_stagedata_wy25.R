# --- Code description ---
# Developed by Adrienne C. October, 2025. Used for CAT242 monitoring.
# Written to plot spot stage or discharge measurements along with real-time station continuous measurements. 
# This example was created for data pulled from Eyasco for LSH monitoring site. 
# You must first download the set of data for both manual stage and uPressure stage, "Chart All" then "Export Data" then rename columns in .xsl file. 

# Load necessary libraries
library(readxl) # for reading Excel files
library(lubridate) #for handling dates
library(ggplot2) # for plotting
library(dplyr) # optional, for data manipulation
library(tidyr) # reshapes data to plot on same axis

# Read Excel file
# Change the file name or path if needed
df <- read_excel("C:/Users/AdrienneChenette/Desktop/WorkspaceRawDataStageLSH.xlsx",
                 col_types = c("date", "numeric", "numeric"))

# Rename Columns for Consistency
# Remember to rename based on sensor type and data type (stage or discharge)
df <- rename(df,
             datetime = Date,
             `real-time-station-stage` = `Real-Time Station Stage`,
             `manual stage` = `Manual Stage`
)

# Ensure datetime is in POSIXct format
df$datetime <- as.POSIXct(df$datetime)

# Reshape for plotting 
df_long <- df %>%
  pivot_longer(
    cols = c(`real-time-station-stage`, `manual stage`),
    names_to = "flow_type",
    values_to = "flow_value"
  ) %>%
  mutate(flow_value = as.numeric(flow_value))

# Split Data for Plotting 
line_data <- df_long %>% filter(flow_type == "real-time-station-stage")
point_data <- df_long %>% filter(flow_type == "manual stage" & !is.na(flow_value))

# Calculate hourly averages
hourly_avg <- df %>%
  mutate(hour = floor_date(datetime, unit = "hour")) %>%
  group_by(hour) %>%
  summarise(avg_real_time = mean(`real-time-station-stage`, na.rm = TRUE)) %>%
  ungroup()

# Filter manual points to leave unchanged 
manual_points <- df %>%
  filter(!is.na(`manual stage`)) %>%
  select(datetime, `manual stage`)

# Calculate limits for the x-axis 
x_min <- min(hourly_avg$hour, na.rm = TRUE)
x_max <- max(hourly_avg$hour, na.rm = TRUE) + lubridate::days(1)

# Plot! 
ggplot() +
  geom_line(data = hourly_avg,
            aes(x = hour, y = avg_real_time),
            color = "#008DA5", # AR Ocean Blue
            size = 0.8) +
  geom_point(data = manual_points,
             aes(x = datetime, y = `manual stage`),
             color = "#EE4023", # AR Wild Salmon
             size = 4) +
  scale_y_continuous(
    breaks = seq(0, max(c(hourly_avg$avg_real_time, manual_points$`manual stage`), na.rm = TRUE) + 0.1, by = 0.1)
  ) +
  scale_x_datetime(
    limits = c(x_min, x_max),
    date_breaks = "15 days", # Change date tick spacing here
    date_labels = "%b %d",
    expand = c(0, 0) # Removes padding on both ends of the data series
  ) +  
  labs(
    title = "Hourly Average Stage (Blue Line) and Manual Stage (Red Dot) at CWL", #CHANGE TITLE BASED ON LOCATION
    x = "Date",
    y = "Stage (feet)"
  ) +
  theme(
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


  
