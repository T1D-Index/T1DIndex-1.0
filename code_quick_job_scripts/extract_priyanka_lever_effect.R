library(data.table)
library(dplyr)
library(RPostgreSQL)

source("code_R_data_prep/DATA_CONSTANTS.R")
library(readxl)

year <- 2021
table_name <- "main_0_4_15_lever2023"
index_data <- Data_Run_query_return_df  ( paste0('SELECT *
                     FROM ',table_name,'
                     WHERE 1=1
                     AND "Country"= \'GLOBAL\'

                    -- AND "Age" >= ',0,' AND "Age" <= ',19
))


index_data_ <- setDT(index_data)[,list(Prevalence= sum(Prevalence)
                                       ,Missing_Prevalence=sum(Ghosts)
                                       ,Prevalence_increment_lever_1= sum(`Ghosts (onset death)`)
                                       ,Prevalence_increment_lever_2= sum(`Ghosts (delta basic care)`)
                                       ,Prevalence_increment_lever_3= sum(`Ghosts (delta best care)`)
                                       ,Prevalence_increment_lever_4= sum(`Ghosts (delta cure)`)
                                       ),by=c("Year") ]
index_data_ <- index_data_[index_data_$Year >=2022,]


write.csv(index_data_,"temp/export_lever_effect.csv",row.names = FALSE)
