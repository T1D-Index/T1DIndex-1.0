#!/usr/bin/env Rscript
# setwd( "E:/GithubCode/t1d_global_model/t1dGlobalModel")
library(RSQLite)
library(dplyr)
library(purrr)
library(testthat)
library(tidyverse)
library(doParallel)
library(futile.logger)
library(data.table)
library("writexl")
library(zip)
# library(arrow)

source('code_R/runner.R')
source('code_R/prevalence.R')
source('code_R/data.R')
source('code_R/population.R')
source('code_R/utils.R')
source('code_R/incidence.R')
source('code_R/onset_death.R')
source('code_R/mortality.R')
source('code_R/hba1c.R')
source('code_R/complications.R')
source('code_R/burden.R')

time_start <- Sys.time()
if (!dir.exists('cache')) {
  dir.create('cache')
}

source('code_R/000_parameters.R')



config$run_days_lost       <- FALSE
config$run_days_lost_lever <- FALSE
config$run_projection      <- FALSE


start_year=1960;
end_year=2040

version_no <- "0.4.11"

Sys.setenv(T1D_DATA_FILE = paste0("data_outputs/data_",version_no,"/","data.db") )
data_dir  <- paste0( "data_outputs/data_",version_no)


# apply growth rate after 2020 ------------------
apply_Growth_Rate <- function(matrix_rate,year_start=2021, rate=NA)
{
  # matrix_rate <- smr_matrix_m
  # data.frame(x=rownames(matrix_rate),age_1= matrix_rate[,1],age_25=matrix_rate[,25],age_50=matrix_rate[,50],age_75=matrix_rate[,75],age_99=matrix_rate[,99]) %>%
  #   e_charts(x) %>%e_line(age_1)%>%e_line(age_25)%>%e_line(age_50)%>%e_line(age_75)%>%e_line(age_99)%>%e_tooltip(trigger = "item")%>% e_x_axis(min='1980')

  rate_avg_5 <- (    matrix_rate[as.character(year_start),]/matrix_rate[as.character(year_start-1),] +
                       matrix_rate[as.character(year_start-1),]/matrix_rate[as.character(year_start-2),] +
                       matrix_rate[as.character(year_start-2),]/matrix_rate[as.character(year_start-3),] +
                       matrix_rate[as.character(year_start-3),]/matrix_rate[as.character(year_start-4),] +
                       matrix_rate[as.character(year_start-4),]/matrix_rate[as.character(year_start-5),]
                     +matrix_rate[as.character(year_start-5),]/matrix_rate[as.character(year_start-6),]
                     +matrix_rate[as.character(year_start-6),]/matrix_rate[as.character(year_start-7),]
                     +matrix_rate[as.character(year_start-7),]/matrix_rate[as.character(year_start-8),]
                     +matrix_rate[as.character(year_start-8),]/matrix_rate[as.character(year_start-9),]
                     +matrix_rate[as.character(year_start-9),]/matrix_rate[as.character(year_start-10),]
  )/10
  if(!is.na(rate))
  {rate_avg_5[!is.nan(rate_avg_5)] <- 1+rate/100 }

  rate_avg_5[is.nan(rate_avg_5)] <- 0

  for(year in (year_start+1): max(rownames(matrix_rate)))
  {#  year <- 2022
    matrix_rate[as.character(year),] <- matrix_rate[as.character(year-1),] *  rate_avg_5
  };return(matrix_rate)
}


# Scenario 1: growth rates. IF we increase incidence by 15% so that U20 prevalence is 187,000 in 2018;
# what incidence growth rate is required for prevalence to be 244,000 in 2020?


country_wb_name <- "United States"
loc_id      <- get_loc_id(country_wb_name)
# data_long   <- run_query_df (paste0("SELECT * FROM input_rates_combined  WHERE loc_id = '",loc_id,"'" ) )
data_long   <- readRDS(paste0(data_dir,"/",loc_id,".Rds"))


data_wide <- spread(dplyr::select(data_long,year,age,value=incidence_rate ), age, value)
rownames(data_wide) <- data_wide$year
data_wide           <- dplyr::select(data_wide,-year)%>% as.matrix
i                   <- 1 * data_wide / 1e5  * 1.148249


# if(TRUE)

i   <- apply_Growth_Rate(i,year_start=2018,rate=102)


prev <- prevalence_and_ghost_pop(
  country_wb_name=country_wb_name,
  # hba1c=hba1c_function(country_name),
  start_year=start_year,
  end_year=end_year,
  incidence_scale_factor=1.148249
  ,data_long=data_long
  ,over_write_i = i
)



prev_merge_wide <- spread(prev$prev_merge_long, Value_type, Value )
sum(prev_merge_wide$Prevalence[prev_merge_wide$Year==2018 & prev_merge_wide$Age<20])


sum(prev_merge_wide$Prevalence[prev_merge_wide$Year==2020 & prev_merge_wide$Age<20])


# ----------------------------------------------------------------------------------------------------------------------------------------
#   Scenario 2: adult mortality. IF we increase incidence by 46% so that U20 prevalence is 244,000 in 2020;
# how much does mortality have to increase for 20+ prevalence to be 1.6M? What is the SMR and life expectancy?

rm(list=ls())

country_wb_name <- "United States"
loc_id      <- get_loc_id(country_wb_name)
# data_long   <- run_query_df (paste0("SELECT * FROM input_rates_combined  WHERE loc_id = '",loc_id,"'" ) )
data_long   <- readRDS(paste0(data_dir,"/",loc_id,".Rds"))


data_wide <- spread(dplyr::select(data_long,year,age,value=incidence_rate ), age, value)
rownames(data_wide) <- data_wide$year
data_wide           <- dplyr::select(data_wide,-year)%>% as.matrix
i                   <- 1 * data_wide / 1e5  * 1.46

data_wide <- spread(dplyr::select(data_long,year,age,value=value_smr_non_minimal_care ), age, value)
rownames(data_wide) <- data_wide$year
data_wide           <- dplyr::select(data_wide,-year)%>% as.matrix
smr_matrix_n                  <- 1 * data_wide

data_wide <- spread(dplyr::select(data_long,year,age,value=value_smr_minimal_care ), age, value)
rownames(data_wide) <- data_wide$year
data_wide           <- dplyr::select(data_wide,-year)%>% as.matrix
smr_matrix_m                  <- 1 * data_wide

# smr_matrix_n [,20:100]  <- apply_Growth_Rate(smr_matrix_n[,20:100],year_start=2018,rate=290)
# smr_matrix_m [,20:100]  <- apply_Growth_Rate(smr_matrix_m[,20:100],year_start=2018,rate=290)

smr_matrix_n [,20:100]  <- smr_matrix_n [,20:100] * 1.65
smr_matrix_m [,20:100]  <- smr_matrix_m [,20:100] * 1.65


prev <- prevalence_and_ghost_pop(
  country_wb_name=country_wb_name,
  # hba1c=hba1c_function(country_name),
  start_year=start_year,
  end_year=end_year
  # incidence_scale_factor=1.148249
  ,data_long=data_long
  ,over_write_i = i
 , over_write_smr_matrix_n = smr_matrix_n
 , over_write_smr_matrix_m = smr_matrix_m
)




prev_merge_wide <- spread(prev$prev_merge_long, Value_type, Value )

sum(prev_merge_wide$Prevalence[prev_merge_wide$Year==2020 & prev_merge_wide$Age<20])


sum(prev_merge_wide$Prevalence[prev_merge_wide$Year==2020 & prev_merge_wide$Age>=20])

(prev_merge_wide$`Life expectency (2 t1d base)`[prev_merge_wide$Year==2020 & prev_merge_wide$Age==10])




smr_matrix_n["2020","20"]
