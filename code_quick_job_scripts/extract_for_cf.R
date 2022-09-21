library(data.table)
library(dplyr)
library(RPostgreSQL)

source("code_R/data.R")
library(readxl)

query<- paste0( 'SELECT *
                     FROM main
                     WHERE 1=1
                     AND "Year" >=  2000
                     AND "Type" = \'','Country','\'
                    -- AND "Age" >= ',0,' AND "Age" <= ',19
                     )

index_data <-  run_query_df_path(query,"")

index_data_export <- dplyr::select(index_data, "Country","Year","Age"
                                   ,"Ann. background mortality","Ann. background population"
                                   ,"Ann. background mortality","Ann. background population"
                                   ,Death_undiagnosed="Ann. onset deaths"
                                   ,Death_diagnosed  ="Ann. early deaths"
)


country_name_list <-  run_query_df_path("SELECT * FROM country","")




write.csv(export,"temp/export_CHAI.csv",row.names = FALSE)

