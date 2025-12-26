# Code description --------------------------------------------------------
# Updated version of LSR_hydro_WYT_1951_2021.R, used to analyze hydrological year type of runoff at the LSR gage on Little Shasta River, water years 1951-2025.
# Estimated unimpaired runoff was downloaded from rivers.codefornature.org on 12/26/2025. 
# Estimated unimpaired runoff is calculated as the monthly average flow. 
# Hydrologic year types are calculated using tercile groupings to be consistent with the methods used in functional flow analyses.


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(lfstat)
library(plotly)


# Load data ---------------------------------------------------------------

LSR_monthly_flow_1950_2025 <- read_csv("data/LSR_processed/flow_3917176_mean_estimated_1950_2025.csv")

# Wrangle data ------------------------------------------------------------

LSR_monthly_flow_1950_2025 <- LSR_monthly_flow_1950_2025 %>% 
  mutate(
    # make a date-formatted column
    year_month = make_date(year, month),
    
    # add water year (FORCE numeric to avoid factor comparison issues)
    water_year = as.numeric(as.character(
      water_year(year_month, origin = "usgs")
    ))
  )


# Filter to include only whole water years (WY1951–2025) ------------------

LSR_monthly_flow_1951_2025 <- LSR_monthly_flow_1950_2025 %>% 
  filter(water_year > 1950)


# Calculate total annual flow from monthly estimated average flows --------

LSR_annual_flow_1951_2025 <- LSR_monthly_flow_1951_2025 %>% 
  group_by(water_year) %>% 
  summarize(
    TAF = sum(value),
    .groups = "drop"
  )


# Determine water year types ----------------------------------------------

LSR_percentile_33 <- quantile(LSR_annual_flow_1951_2025$TAF, 0.33)
LSR_percentile_66 <- quantile(LSR_annual_flow_1951_2025$TAF, 0.667)

LSR_3_WYT <- LSR_annual_flow_1951_2025 %>% 
  mutate(
    WYT = case_when(
      TAF >= LSR_percentile_66 ~ "wet",
      TAF >= LSR_percentile_33 ~ "moderate",
      TRUE ~ "dry"
    )
  )

# Export results ----------------------------------------------------------

write_csv(LSR_3_WYT, "output/LSR_3_WYT_1950_2025.csv")
