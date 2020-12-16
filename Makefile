# -- Template by bubble with <3. --

# Python interpreter
PYTHON_INTERPRETER = python3

# R interpreter
R_INTERPRETER = /usr/local/bin/Rscript

# Search for .env file variables
ifneq (,$(wildcard ./.env))
    include .env
    export
endif


cases_rate: ${PWD}/data/processed/msoa_weekly_cases.csv

${PWD}/data/processed/msoa_weekly_cases.csv: ${PWD}/src/cases_rate.R \
	${PWD}/data/raw/cases/MSOAs_latest.csv
	$(R_INTERPRETER) $^ $@


interpolate_daily: ${PWD}/data/processed/msoa_daily_cases.csv

${PWD}/data/processed/msoa_daily_cases.csv: ${PWD}/src/interpolate_daily.R \
	${PWD}/data/processed/msoa_weekly_cases.csv
	$(R_INTERPRETER) $^ $@


hex: ${PWD}/data/processed/msoa_hex.shp

${PWD}/data/processed/msoa_hex.shp: ${PWD}/src/hex.R \
	/Users/hamishgibbs/Documents/Covid-19/uk_demographic_mobility/data/raw/msoa_centroids/Middle_Layer_Super_Output_Areas__December_2011__Population_Weighted_Centroids.shp
	$(R_INTERPRETER) $^ $@
