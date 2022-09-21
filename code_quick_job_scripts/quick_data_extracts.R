#!/usr/bin/env Rscript
# draft paper : https://docs.google.com/document/d/1JDVJVxQ-4XFap5CTxPGUEdG5r0SKQm2QEsxCYYcBRcU/edit
library(RSQLite)
library(dplyr)
library(purrr)
library(testthat)
library(tidyverse)
Sys.setenv(T1D_DATA_FILE = "/Users/feiwang/data.db")
Sys.setenv(T1D_DATA_FILE = "/Users/feiwang/Documents/GitHub/t1d_global_model/t1dGlobalModel/data_outputs/data.db")
con <- get_database_connection(read_only=TRUE)
dbListTables(con)

country <- dbGetQuery(con, 'SELECT * FROM country;')

hba1c <- dbGetQuery(con, 'SELECT * FROM hba1c;')
classical_model_smr <- dbGetQuery(con, 'SELECT * FROM classical_model_smr;')

incidence_curve <- dbGetQuery(con, 'SELECT * FROM incidence_curve;')
incidence_growth_rate <- dbGetQuery(con, 'SELECT * FROM incidence_growth_rate;')

onset_death_rate <- dbGetQuery(con, 'SELECT * FROM onset_death_rate;')

life_table <- dbGetQuery(con, 'SELECT * FROM life_table;')
# population <- dbGetQuery(con, 'SELECT * FROM population;')
# population_single_year <- dbGetQuery(con, 'SELECT * FROM population_single_year;')


country$has_incidence_rate_data   <- country$loc_id %in% incidence_curve$loc_id
country$has_onset_death_rate_data <- country$loc_id %in% onset_death_rate$loc_id
country$has_morality_rate_data    <- country$loc_id %in% classical_model_smr$loc_id

country <- country[order(country$has_onset_death_rate_data),]

write.csv(country,'country_missing_rates.csv',row.names = F)



