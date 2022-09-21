build_database <- function()
{
    # rm(list=ls())

    library(dplyr)
    library(readxl)
    library(RSQLite)
    library(DBI)
    library(data.table)
    library(stringi)
    source('code_R_data_prep/read_and_clean_data_csv.R')
    source('code_R_data_prep/DATA_CONSTANTS.R')


    country                <- read_country()
    country_region_mapping <-  read_country_region_mapping()


    country_region <- country
    country_region$world_bank_name_parent <- country_region$world_bank_name
    country_region$sub_nation_id               <- 0
    country_region$population_ratio_percentage            <- 1
    country_region$background_mortality_ratio             <- 1
    country_region$gdp_ratio                              <- 1
    country_region_add                         <- data.frame()


    for(i in 1:nrow(country_region_mapping))
    { # i <- 1
      country_region_temp <- country_region[country_region$world_bank_name %in% country_region_mapping$Country[i] ,]

      country_region_temp$sub_nation_id <- country_region_mapping$id[i]
      country_region_temp$world_bank_name <- paste0(country_region_temp$world_bank_name,"(", country_region_mapping$name[i],")")
      country_region_temp$world_bank_code <- paste0(country_region_temp$world_bank_code,"_", country_region_mapping$Code[i])
      country_region_temp$loc_id          <- paste0(country_region_temp$loc_id,"_", country_region_mapping$id[i])


      country_region_temp$population_ratio_percentage                     <- country_region_mapping$population_ratio_percentage[i]
      country_region_temp$background_mortality_ratio                      <- country_region_mapping$background_mortality_ratio[i]
      country_region_temp$gdp_ratio                               <- country_region_mapping$gdp_ratio[i]
      country_region_temp$wd_income_category                      <- country_region_mapping$`income class`[i]
      country_region_temp$world_bank_classification                     <- country_region_mapping$`income class`[i]

      country_region_add <- rbind(country_region_add, country_region_temp)
    }

    country_region <- rbind(country_region,country_region_add)

    # write to Data base,  local or AWS
    schema_name <- "index_parameters"
    Data_dump_data_frame (df=country       ,schema_name=schema_name, table_name="country")
    Data_dump_data_frame (df=country_region,schema_name=schema_name, table_name="country_region")
    Data_dump_data_frame (df=read.csv('data_internal/model_constants.csv', stringsAsFactors = F)       ,schema_name=schema_name, table_name="model_constants")
    Data_dump_data_frame (df=read.csv('data_internal/prevalence_reference.csv', stringsAsFactors = F)  ,schema_name=schema_name, table_name="prevalence_reference")
    Data_dump_data_frame (df=read.csv('data_internal/disease_weights.csv', stringsAsFactors = F)       ,schema_name=schema_name, table_name="disease_weights")
    Data_dump_data_frame (df=read.csv('data_internal/weibull_complications.csv', stringsAsFactors = F) ,schema_name=schema_name, table_name="weibull_complications")
    Data_dump_data_frame (df=read.csv('data_internal/constant_complications.csv', stringsAsFactors = F),schema_name=schema_name, table_name="constant_complications")

    country_indicator <- readRDS("data_wb/country_indicator_imputed_avg_0.4.5.Rds")
    Data_dump_data_frame (df=country_indicator       ,schema_name=schema_name, table_name="country_indicator")



}


# build_database ()
  
