#CAT242 
#Title: Wet Bulb Calculator
#Author: Adrienne C. & Ann W.
#Develop date: 10/03/2025

#Code Description
#Code developed to calculate Wetbulb temperature (Twb) from Air Temp (C) and Relative Humidity (RH)(%). 
#Data will be applied to W3T model for CAT242 project. 

# --- Load libraries ---
library(tidyverse) #this library is for general data-wrangling
library(lubridate) #this library is for formatting dates and times

# --- Name object and import data files ---
TwbCalc_2025 <- read_csv("data/W3T_input/Ta_RH_DrySeason1.csv")

# --- Calculate Twb ---
TwbCalc_2025$Tw <- TwbCalc_2025$Ta_C * atan(0.151977 * sqrt(TwbCalc_2025$RH + 8.313659)) + #creates new column "Tw"
  atan(TwbCalc_2025$Ta_C + TwbCalc_2025$RH) -
  atan(TwbCalc_2025$RH - 1.676331) +
  0.00391838 * (TwbCalc_2025$RH^(3/2)) * atan(0.023101 * TwbCalc_2025$RH) -
  4.686035

# --- Save results ---
write_csv(TwbCalc_2025,"data/W3T_input/Twb_DrySeason1.csv")
