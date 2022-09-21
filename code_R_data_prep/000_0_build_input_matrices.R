build_input_matrices <- function(version_no, random_data_ci = FALSE,re_run_data_points=FALSE)
{  # rm(list=ls()); version_no <- "0.4.12"; random_data_ci = TRUE
  # rm(list=ls()); version_no <- "0.4.15"; random_data_ci = FALSE ; re_run_data_points <- TRUE
  # version_no <- "0.4.12"
  # version_no <- "0.4.12.smr_age_cruve_flat_average"
  # version_no <- "0.4.12.adult_incidence_all_studies"
  # version_no <- "0.4.12.iot_no_growth"
  # version_no <- "0.4.12.iot_global_curve_for_all"
  # version_no <- "0.4.12.adult_onset_zero"
  # tryCatch({


    library(RCurl)
    library(dplyr)
    library(readxl)
    library(tidyverse)
    library(RSQLite)
    library(DBI)
    library(data.table)
    library(arrow)
    # library(saves)
    library(stringi)

    source('code_R/utils.R')
    source('code_R_data_prep/read_and_clean_data_csv.R')
    source('code_R_data_prep/DATA_CONSTANTS.R')
    download_csvs          <- FALSE

    # Create SQLite database  data.db ------------------------------------------------------------------------------
    dir.create(paste0("data_outputs/data_",version_no))

    country <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country;")


    if(FALSE)
    {
      # Data from UN  --------------------------------------------------------------------------------------------------------------------------------------------
      if(download_csvs)
      {source('code_R_data_prep/000_0_1_download_csvs.R')}

      WPP2019_F01_LOCATIONS <- readxl::read_xlsx('../data_un/WPP2019_F01_LOCATIONS.XLSX',2)
      WPP2019_F01_LOCATIONS <- WPP2019_F01_LOCATIONS[!is.na(WPP2019_F01_LOCATIONS$ISO3_Code) , ]

      # Background population, mortality rate ---------------------------------------------------------------------------------------------------------------

      source('code_R_data_prep/000_0_2_background_pop_mr.R')
      background_pop_mr_results <- background_pop_mr(country,data_un_path="../data_un")
      life_table_single_year <- background_pop_mr_results$life_table_single_year
      population_single_year <- background_pop_mr_results$population_single_year

      input_rates_combined <- dplyr::select(life_table_single_year, loc_id,year = start_year, age = age_grp_start , background_mortality_rate= qB )
      input_rates_combined <- dplyr::select(country,loc_id,world_bank_name) %>% dplyr::inner_join(input_rates_combined, by=c("loc_id"))

      input_rates_combined <- input_rates_combined %>%
        dplyr::left_join(dplyr::select(population_single_year,loc_id,year, age=age_grp_start,background_population=population), by=c("loc_id","age","year"))

      save(input_rates_combined, file = "data_default_run/input_rates_combined.Rdata")

    }else
    {
      load("data_default_run/input_rates_combined.Rdata")
    }

    if(re_run_data_points)
    {
      # Incidence --------------------------------------------------------------------------------------------------------------------------------------------
      source('code_R_data_prep/000_0_3_incidence_rate.R')
      get_Incidence_Full_result <- get_Incidence_Full(random_data_ci="FALSE")
      incidence_curve <- get_Incidence_Full_result$incidence_curve
      save(get_Incidence_Full_result, file = "data_default_run/get_Incidence_Full_result.Rdata")

    }else
    {
      load("data_default_run/get_Incidence_Full_result.Rdata")

      incidence_curve <- get_Incidence_Full_result$incidence_curve
      if(random_data_ci)
      {
        # unified draw by country vs by year
        draw_level <- c("loc_id","year")
        # draw_level <- c("loc_id")
        incidence_curve_ci <- read_parquet("data_incidence/incidence_curve_ci.binary")

        df_ran_draw_level <- setDT(incidence_curve_ci)[,list(value_median=median(value_median)
                                                       ,lower=median(lower)
                                                       ,upper=median(upper)
        ),by=draw_level]

        df_ran_draw_level$smr_predicted <- random_gen(mean=df_ran_draw_level$value_median, smr_ci_lower= df_ran_draw_level$lower,smr_ci_upper=df_ran_draw_level$upper )
        df_ran_draw_level$deviation <- (df_ran_draw_level$smr_predicted-(df_ran_draw_level$upper+df_ran_draw_level$lower)/2 )/ (df_ran_draw_level$upper-df_ran_draw_level$lower)
        df_ran_draw_level <- dplyr::select(df_ran_draw_level,c(draw_level,"deviation"))


        incidence_curve_ci <- incidence_curve_ci %>% dplyr::left_join(df_ran_draw_level,by = draw_level)
        incidence_curve_ci$incidence_rate <-incidence_curve_ci$deviation *  (incidence_curve_ci$upper-incidence_curve_ci$lower) + (incidence_curve_ci$upper+incidence_curve_ci$lower)/2

        incidence_curve_ci<- setDF(incidence_curve_ci)[,colnames(incidence_curve)]
        incidence_curve   <- incidence_curve_ci
      }

    }


    input_rates_combined <- input_rates_combined %>% dplyr::left_join(incidence_curve, by=c("loc_id","age","year"))


    # write_rds(country,"data_internal/country.Rds")

    # machine_learning_model_smr -------------------------------------------------------------------------------------------------------------
    source('code_R_data_prep/000_0_4_mortality_rate.R')
    mortality_rate_result <- mortality_rate(country,random_data_ci=random_data_ci)

    input_rates_combined  <- input_rates_combined %>% dplyr::inner_join(mortality_rate_result$machine_learning_model_smr, by=c("loc_id","age","year"))

    #death_of_undiagnosed.R  -------------------------------------------------------------------------------------------------------------
    # source('code_R_data_prep/000_0_5_death_of_undiagnosed_alex.R')
    source('code_R_data_prep/000_0_5_death_of_undiagnosed.R')
    onset_death_results  <- onset_death_rates()

    onset_death_rate_single_year <- onset_death_results$od_rates
    onset_death_rate_single_year$mortality_undiagnosed_rate       <- onset_death_rate_single_year$value
    onset_death_rate_single_year$mortality_undiagnosed_rate_left  <- onset_death_rate_single_year$value_left
    onset_death_rate_single_year$mortality_undiagnosed_rate_right <- onset_death_rate_single_year$value_right
    onset_death_rate_single_year$value <- NULL
    onset_death_rate_single_year$value_left <- NULL
    onset_death_rate_single_year$value_right <- NULL

    input_rates_combined <- input_rates_combined %>% dplyr::inner_join(onset_death_rate_single_year, by=c("loc_id","age","year"))

    if(FALSE)
    { # exctract for rachel
    asdf  <-   input_rates_combined[input_rates_combined$year==2020 & input_rates_combined$age==0,]
    asdf <- dplyr::select(asdf, world_bank_name, year,mon_minimal_care =  "value_percent_non_minimal_care")

    asdf$minimal_care <- 1- asdf$mon_minimal_care
    write.csv(asdf,"country_level_of_care_2020.csv",row.names = F)

    }
    # input_rates_combined_thinkcell <- dplyr::select(country,loc_id,wd_region) %>% dplyr::left_join( input_rates_combined[input_rates_combined$year==2021,])
    # input_rates_combined_thinkcell$incidence_rate_weighted <- input_rates_combined_thinkcell$background_population * input_rates_combined_thinkcell$incidence_rate
    # input_rates_combined_thinkcell <- setDT(input_rates_combined_thinkcell)[,list(incidence_rate_weighted= sum(incidence_rate_weighted)/sum(background_population) ),by=c("wd_region","age") ]
    # input_rates_combined_thinkcell <- spread(input_rates_combined_thinkcell, wd_region, incidence_rate_weighted)
    # write.csv(input_rates_combined_thinkcell,"temp/incidence_curve_thinkcell_region.csv",row.names = FALSE)

    loc_id_list <- unique(input_rates_combined$loc_id)
    for( i in 1: length(loc_id_list) )
    { # i <- 1
      input_rates_combined_t <-  input_rates_combined[input_rates_combined$loc_id==loc_id_list[i],]
      # write.fst(input_rates_combined_t, paste0(paste0("data_outputs/data_",version_no,"/",loc_id_list[i],".binary") ),compress = 100)
      saveRDS(input_rates_combined_t,paste0(paste0("data_outputs/data_",version_no,"/",loc_id_list[i],".Rds") ) )

    }
    # write_csv(input_rates_combined, 'input_rates_combined.csv')
    # Sub national breakdown -------------------------------------------------------------------------------------
    country_region <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country_region where sub_nation_id !=0 ;")


    machine_learning_model_smr_states <- readRDS('data_mortality/machine_learning_model_smr_states.Rds')
    machine_learning_model_smr_states <- machine_learning_model_smr_states[machine_learning_model_smr_states$year==2017,]
    machine_learning_model_smr_states <- machine_learning_model_smr_states[machine_learning_model_smr_states$standard_of_care=="Non Minimal Care",]
    machine_learning_model_smr_states$smr_ratio <- machine_learning_model_smr_states$smr_predicted /  median(machine_learning_model_smr_states$smr_predicted)
    setDT(machine_learning_model_smr_states)[,smr_ratio:=list(   smr_predicted/median(smr_predicted)    ),by=c("world_bank_name")]


    input_rates_combined_sub_all <- data.frame()
    for(i in 1:nrow(country_region))
    { # i <- 61
      input_rates_combined_sub <-  input_rates_combined[input_rates_combined$world_bank_name == country_region$world_bank_name_parent[i],]
      input_rates_combined_sub$income_class  <- country_region$wd_income_category[i]
      input_rates_combined_sub$background_population     <- input_rates_combined_sub$background_population     * country_region$population_ratio_percentage[i]
      input_rates_combined_sub$background_mortality_rate <- input_rates_combined_sub$background_mortality_rate * country_region$background_mortality_ratio[i]

      # diagnosis rate adjust by income level
      results <-  onset_death_results$get_od_rates(country_region[i,],onset_death_results$model_data_method)
      results <- results[ with(results, order(year,age)), ]   # order properly for transforming to matrix
      input_rates_combined_sub$mortality_undiagnosed_rate <- results$value

      # levels of care  adjust by income level
      input_rates_combined_sub$value_percent_non_minimal_care <- NULL
      input_rates_combined_sub  <- input_rates_combined_sub %>% dplyr::inner_join(mortality_rate_result$Standards_of_care_mortality, by=c('year','age','income_class'))

      # SMR   adjust by features

      smr_ratio <- machine_learning_model_smr_states$smr_ratio[machine_learning_model_smr_states$country== country_region$world_bank_name[i]]
      input_rates_combined_sub$value_smr_minimal_care     <- input_rates_combined_sub$value_smr_minimal_care     * smr_ratio
      input_rates_combined_sub$value_smr_non_minimal_care <- input_rates_combined_sub$value_smr_non_minimal_care * smr_ratio
      input_rates_combined_sub <- input_rates_combined_sub[,colnames(input_rates_combined)]
      input_rates_combined_sub$loc_id <-country_region$loc_id[i]
      saveRDS(input_rates_combined_sub,paste0(paste0("data_outputs/data_",version_no,"/",country_region$loc_id[i],".Rds") ) )
      input_rates_combined_sub_all <- rbind(input_rates_combined_sub_all,input_rates_combined_sub )
    }

    input_rates_combined_final <-   rbind(input_rates_combined, input_rates_combined_sub_all)

    # Data_dump_data_frame(df=input_rates_combined_final,"public",paste0("index_input_",gsub("\\.","_",version_no)) )


    print(paste0("NAs : ", sum(is.na(input_rates_combined))))
  # },
  # error = function(cond) {
  #   sink("log.txt",append=TRUE);cat(paste0("  ",  cond, " \n") );sink()
  # },
  # finally = {
  # })
}

if(FALSE)
{ # default run --------------------------------------------------------------------------------
  # rm(list=ls());
  build_input_matrices(version_no= "0.4.15", random_data_ci = FALSE,re_run_data_points=TRUE)

}
