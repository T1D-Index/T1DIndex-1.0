
  
  # sensitivity analysis
  library(arrow)
  library(dplyr)
  library(data.table)
  
  scenarios <- list(    pediatric_incidence_plus_10_perc = TRUE
                        , pediatric_incidence_minus_10_perc = TRUE
                        , pediatric_incidence_plus_25_perc = TRUE
                        , pediatric_incidence_minus_25_perc = TRUE
                        
                        , adult_incidence_all_studies = TRUE
                        # , adult_incidence_17_studies = TRUE
                        
                        , iot_no_growth = TRUE
                        , iot_global_curve_for_all = TRUE
                        
                        , smr_age_cruve_flat_average = TRUE
                        
                        , smr_plus_10_perc  = TRUE
                        , smr_minus_10_perc = TRUE
                        
                        , diagnosis_rate_plus_25_pp = TRUE
                        , diagnosis_rate_minus_25_pp = TRUE
                        , adult_onset_zero = TRUE
                        , diagnosis_rate_left = TRUE
                        , diagnosis_rate_right = TRUE
                        
  )
  
  compare_to_published_estimates <- function(country_data_merge)
  {
    library(sqldf)
    library(readxl)
    published_estimates <- data.frame(read.csv('data_sensitivity_analysis/published estimates - Sheet1.csv',stringsAsFactors = FALSE))
    
    published_estimates <- published_estimates[1:27,]
    published_estimates <- published_estimates[!is.na(published_estimates$Year),]
    
    published_estimates$Prevalence <- gsub("\\,","", published_estimates$Prevalence )
    published_estimates$Prevalence <- trimws(published_estimates$Prevalence )
    published_estimates$Prevalence <- as.integer( published_estimates$Prevalence )
    published_estimates$row_no <- 1:nrow(published_estimates)
    
    published_estimates$Country[published_estimates$Country=="Taiwan"] <- "China, Taiwan Province of China"
    
    
    
    
    published_estimates <-  sqldf("select t1.row_no,t1.Country,t1.Year,t1.Source,t1.Low, t1.High,t1.Prevalence, round(sum(t2.Prevalence),0) as Prevalence_T1D_index_estimates
                                      FROM published_estimates as  t1 left join  country_data_merge t2
                                      WHERE t1.Year=t2.Year
                                      AND t1.Country= t2.Country
                                      AND (t2.Age >= t1.Low AND t2.Age <= t1.High)
                                      GROUP BY t1.row_no, t1.Country,t1.Year,t1.Source,t1.Low, t1.High,t1.Prevalence
                                      ORDER BY t1.row_no asc
                                      --limit 100")
    published_estimates$Country[published_estimates$Country=="China, Taiwan Province of China"] <- "Taiwan"
    
    published_estimates$Variance <- round((published_estimates$Prevalence_T1D_index_estimates - published_estimates$Prevalence)/  published_estimates$Prevalence_T1D_index_estimates * 100,0)
    
    published_estimates$"Var(abs)" <- abs(published_estimates$Variance)
    
    return(published_estimates)
  }
  
  get_data_merge <- function( file_path)
  {
    filenames <- list.files(paste0(file_path), pattern="*.binary", full.names=TRUE)
    ldf <- lapply(filenames, read_parquet)
    country_data_merge   <- rbindlist(ldf)
    # country_data_merge      <- rbindlist( lapply(filenames,function(x){df <- read_parquet(x);df$run<- x; df} ))
    
  }
  
  # sensitivity summary
  # country_data_merge <-  arrow::read_parquet( paste0("reruns/rerun_0.4.10_0/aggregates/country_data_merge.parquet") )
  country_data_merge <-  get_data_merge( file_path= paste0("../../reruns_scenario_2/rerun_0.4.12_0/"))
  
  # sum(country_data_merge[ country_data_merge$Year==2021  & country_data_merge$Country=="Mali"& country_data_merge$Age <= 25 ])
  
  # sum(country_data_merge$Prevalence[ country_data_merge$Year==2021  & country_data_merge$Country=="Mali"& country_data_merge$Age <= 25 ])
  
  prevalance_base                  <- sum(country_data_merge$Prevalence[ country_data_merge$Year==2021  & country_data_merge$Country=="GLOBAL" ])
  Ghosts_base                           <- sum(country_data_merge$Ghosts    [ country_data_merge$Year==2021  & country_data_merge$Country=="GLOBAL" ])
  published_estimates <- compare_to_published_estimates(country_data_merge)
  Var_base            <- round( median( published_estimates$`Var(abs)`) ,0)
  
  scenarios_df <- as.data.frame( t(as.data.frame(scenarios)) )
  scenarios_df$Scenario  <- rownames(scenarios_df)
  scenarios_df$Prevalance <- NA
  scenarios_df$Ghosts     <- NA
  scenarios_df$"Var(abs)"     <- NA
  
  for(i in 1:15)
  { # i <- 13
    print(i)
    country_data_merge <-  get_data_merge( file_path= paste0("../../reruns_scenario_2/rerun_0.4.12_",i,"/"))
    
    scenarios_df$Prevalance[i] <- sum(country_data_merge$Prevalence[ country_data_merge$Year==2021  & country_data_merge$Country=="GLOBAL" ])
    scenarios_df$Ghosts[i]     <- sum(country_data_merge$Ghosts     [ country_data_merge$Year==2021  & country_data_merge$Country=="GLOBAL" ])
    published_estimates <- compare_to_published_estimates(country_data_merge)
    scenarios_df$`Var(abs)` [i]    <-         round( median( published_estimates$`Var(abs)`) ,0)
    
  }
  
  scenarios_df_base <- scenarios_df[1,]
  scenarios_df_base$Scenario <- "base_scenario"
  scenarios_df_base$Prevalance <- prevalance_base
  scenarios_df_base$Ghosts <- Ghosts_base
  scenarios_df_base$`Var(abs)`  <- Var_base
  scenarios_df <- rbind(scenarios_df_base,scenarios_df)
  
  scenarios_df$`Change in global prevalence (2021)`         <- round( (scenarios_df$Prevalance - scenarios_df$Prevalance[1]) / scenarios_df$Prevalance[1] *100,0)
  scenarios_df$`Change in global missing prevalence (2021)` <- round( (scenarios_df$Ghosts - scenarios_df$Ghosts[1]) / scenarios_df$Ghosts[1] *100,0)
  scenarios_df$`Change in 'fit' with observed prevalence(registries & studies)` <- round( (scenarios_df$`Var(abs)` - scenarios_df$`Var(abs)`[1]) / scenarios_df$`Var(abs)`[1] *100,0)
  
  scenarios_df$V1 <- NULL
  
  # write.csv(scenarios_df, "data_sensitivity_analysis/sensivitity_analysis_output_v2.csv",row.names = FALSE)
  # write.csv(scenarios_df, "data_sensitivity_analysis/sensivitity_analysis_output_v0.4.12.csv",row.names = FALSE)
  write.csv(scenarios_df, "data_sensitivity_analysis/sensivitity_analysis_output_v0.4.12_ci.csv",row.names = FALSE)
  
