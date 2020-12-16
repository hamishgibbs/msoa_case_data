# -- Template by bubble with <3. --

# Script to interpolate weekly cases to daily
# cases source: https://coronavirus.data.gov.uk/details/about-data#cases-by-middle-super-output-area-msoa
# permalink: https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
  require(zoo)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_weekly_cases.csv",
              "/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_daily_cases.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

cases <- read_csv(.args[1])

cases_interp <- cases %>%
  group_by(msoa_code) %>%
  arrange(date) %>%
  # Linear interpolation of missing values
  # This may not be appropriate - null values for cases
  # sum should be set to 0
  # see how missing the rate is
  replace_na(list(cases_rolling_sum = 0, cases_rolling_rate = 0)) %>% 
  mutate(cases_rolling_sum_interp = na.approx(cases_rolling_sum, na.rm = F),
         cases_rolling_rate_interp = na.approx(cases_rolling_rate, na.rm = F)) %>%
  # Move Saturday values to Monday
  mutate(date = date - 5)

min_date <- min(cases_interp$date)
max_date <- max(cases_interp$date) + 6

msoas <- unique(cases_interp$msoa_code)

msoa_daily <- function(msoa, min_date, max_date){

  return(
    tibble(
      msoa_code = msoa,
      date = seq.Date(min_date, max_date, by = 1)
    )
  )

}

daily <- lapply(msoas, msoa_daily, min_date = min_date, max_date = max_date)

daily <- do.call(rbind, daily)

daily <- daily %>%
  left_join(cases_interp, by = c('msoa_code', 'date')) %>%
  #Another linear interpolation on daily data
  mutate(cases_rolling_sum_interp = na.approx(cases_rolling_sum_interp, na.rm = F),
         cases_rolling_rate_interp = na.approx(cases_rolling_rate_interp, na.rm = F)) %>% 
  select(msoa_code, date, cases_rolling_sum_interp, cases_rolling_rate_interp)

# Save csv result
write_csv(daily, tail(.args, 1))
