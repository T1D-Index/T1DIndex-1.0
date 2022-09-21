library(data.table)
library(dplyr)
library(RPostgreSQL)

source("code_R_data_prep/DATA_CONSTANTS.R")
library(readxl)
chai_extract1 <- readxl::read_xlsx("code_quick_job_scripts/CHAI Forecast & T1DM Index_Template Proposal V1.xlsx",sheet = 1)
chai_extract2 <- readxl::read_xlsx("code_quick_job_scripts/CHAI Forecast & T1DM Index_Template Proposal V1.xlsx",sheet = 2)
chai_extract3 <- readxl::read_xlsx("code_quick_job_scripts/CHAI Forecast & T1DM Index_Template Proposal V1.xlsx",sheet = 3)

year <- 2021
index_data <- Data_Run_query_return_df  ( paste0('SELECT *
                     FROM main_0_4_15
                     WHERE 1=1
                     AND "Year"=  ',year,'
                    -- AND "Age" >= ',0,' AND "Age" <= ',19
))


index_input <- Data_Run_query_return_df  ( paste0('SELECT *
                     FROM index_input_0_4_15
                     WHERE 1=1
                     AND year=  ',year,'
                     and age = 15
                    -- AND "Age" >= ',0,' AND "Age" <= ',19
))

index_input <- index_input[!grepl("_",index_input$loc_id) ,]

country_name_list <- Data_Run_query_return_df  ( paste0("SELECT * FROM country"))


 chai_extract2$`Country Name` [!chai_extract2$`Country Name` %in% country_name_list$world_bank_name]


 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Congo, Rep."]       <- "Republic of Congo"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Eswatini"]     <- "Swaziland"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Lao PDR"] <- "Lao People's Democratic Republic"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Marshall Islands"] <- ""
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Micronesia, Fed. Sts."] <- "Federated States of Micronesia"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`== "Sao Tomé"] <- "Sao Tome and Principe"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Tanzania"] <- "United Republic of Tanzania"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Timor-Leste"] <- "Timor L'Este"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Vietnam"] <- "Viet Nam"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="Yemen, Rep."] <- "Yemen"
 chai_extract2$`Country Name`[chai_extract2$`Country Name`=="St. Lucia"] <- "St Lucia"

 chai_extract2$world_bank_name <- chai_extract2$`Country Name`

 chai_extract2$`Country Name` <- chai_extract1 $`Country Name`


 export <- dplyr::select(chai_extract2,`Country Name`,`Income Classification`,world_bank_name)

 export <- select(country_name_list,world_bank_name)


 # export$`Children & Adolescent T1DM Estimates (0-19y)  in 2021` <- NA
 # export$`Annual Incidence of  Adult T1DM  (<20years), 2021 Estimates` <- NA
 #
 # export$`Estimated number of Diagnosed Children & Adolescent T1DM Estimates (0-19y)  in 2021` <- NA
 # export$`Estimated number of Diagnosed Adult T1DM  (<20years), 2021 Estimates`<- NA
 #
 # export$`Estimated number of Diagnosed and  on Treatment Children & Adolescent T1DM Estimates (0-19y)  in 2021`<- NA
 # export$`Estimated number of Diagnosed and  on Treatment Adult T1DM  (<20years), 2021 Estimates`<- NA



 export$`Year` <- year
 for(i in 1:nrow(export))
 {
   export$`Prevalence (0-19y)  in 2021`[i] <- sum(index_data$Prevalence[index_data$Country==export$world_bank_name[i] & index_data$Age <=19 ])
   export$`Prevalence (20+ys)  in 2021`[i] <- sum(index_data$Prevalence[index_data$Country==export$world_bank_name[i] & index_data$Age >19 ])


   export$`Incidence (0-19y)  in 2021`[i] <- sum(index_data$`Incidence (1 base)`[index_data$Country==export$world_bank_name[i] & index_data$Age <=19 ])
   export$`Incidence (20+ys)  in 2021`[i] <- sum(index_data$`Incidence (1 base)`[index_data$Country==export$world_bank_name[i] & index_data$Age >19 ])

   export$`Annual_death_undiagnosed (0-19y)  in 2021`[i] <- sum(index_data$`Ann. onset deaths`[index_data$Country==export$world_bank_name[i] & index_data$Age <=19 ])
   export$`Annual_death_undiagnosed (20+ys)  in 2021`[i] <- sum(index_data$`Ann. onset deaths`[index_data$Country==export$world_bank_name[i] & index_data$Age > 19 ])

   export$`Annual_death_diagnosed (0-19y)  in 2021`[i] <- sum(index_data$`Ann. early deaths`[index_data$Country==export$world_bank_name[i] & index_data$Age <=19 ])
   export$`Annual_death_diagnosed (20+ys)  in 2021`[i] <- sum(index_data$`Ann. early deaths`[index_data$Country==export$world_bank_name[i] & index_data$Age > 19 ])

   export$`Diagnosis rate %`[i] <- round(index_input$mortality_undiagnosed_rate[index_input$world_bank_name==export$world_bank_name[i] ] * 100,2)
   export$`Percent of minimal care %`[i] <- 100-index_input$value_percent_non_minimal_care[index_input$world_bank_name==export$world_bank_name[i] ]* 100


 }



 Standards_of_care_mortality <- readxl::read_xlsx('data_mortality/Standards of care mortality 5.xlsx',sheet=2)[1:4,]

 export[export$`Country Name`=="Marshall Islands",4:11]  <- NA


write.csv(export,"temp/export_CHAI_all_countries.csv",row.names = FALSE)

