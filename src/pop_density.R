# -- Template by bubble with <3. --

# Script to compute population density in MSOAs

# Load libraries
suppressPackageStartupMessages({
  require(tidyverse)
})

# Define args interactively or accept commandArgs
if(interactive()){
  .args <-  c("/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/raw/population/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx",
              "/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/raw/geodata/area.csv",
              "/Users/hamishgibbs/Documents/Covid-19/msoa_data/data/processed/msoa_pop_density.csv")
} else {
  .args <- commandArgs(trailingOnly = T)
}

pop <- readxl::read_excel(.args[1], sheet = 4, skip = 3) %>% 
  rename(msoa_code = `MSOA Code`, 
         pop = `All Ages`) %>% 
  select(msoa_code, pop)

area <- read_csv(.args[2], col_types = cols(LAD11NMW = col_character())) %>% 
  rename(msoa_code = MSOA11CD, 
         area_hect = AREAEHECT) %>% 
  select(msoa_code, area_hect) %>% 
  mutate(area_km = area_hect / 100)

res <- area %>% 
  left_join(pop, by = c('msoa_code')) %>% 
  mutate(pop_density = pop / area_km) %>% 
  select(msoa_code, area_km, pop, pop_density)

testthat::expect_equal(res %>% filter(is.na(pop)) %>% pull(1) %>% length(), 0)

# Save csv result
write_csv(res, tail(.args, 1))
