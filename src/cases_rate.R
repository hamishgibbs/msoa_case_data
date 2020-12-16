# -- Template by bubble with <3. --

# Script to collect cases and rates at MSOA level
# cases source: https://coronavirus.data.gov.uk/details/about-data#cases-by-middle-super-output-area-msoa

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/raw/cases/MSOAs_latest.csv",
              "/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_weekly_cases.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

cases <- read_csv(.args[1]) %>% 
  rename(msoa_code = areaCode, msoa_name = areaName, cases_rolling_sum = newCasesBySpecimenDateRollingSum, cases_rolling_rate = newCasesBySpecimenDateRollingRate)

high_codes <- cases %>% 
  drop_na(cases_rolling_rate) %>% 
  group_by(msoa_code) %>% 
  summarise(num = n()) %>% 
  arrange(-num)

p <- high_codes %>% 
  ggplot() + 
  geom_density(aes(x = num))

high_codes <- high_codes %>% pull(msoa_code)

p <- cases %>% 
  filter(msoa_code == high_codes[2]) %>% 
  ggplot() + 
  geom_path(aes(x = date, y = cases_rolling_rate))

# Save csv result
write_csv(cases, tail(.args, 1))
