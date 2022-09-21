#!/usr/bin/env Rscript
# setwd( "E:/GithubCode/t1d_global_model/t1dGlobalModel")
main_ <- function(version_no,run_per_country=TRUE,run_merge_country=TRUE,run_few_countries= c() ,scenario=2)
{
  # version_no <- "0.4.12_mcmc_1"
  # version_no_input <- "0.4.15"; version_no <- "0.4.15"; scenario <- 2 ; run_per_country=TRUE; run_merge_country=TRUE; run_few_countries= c()

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
  library(fst)
  library(arrow)

  source('code_R_data_prep/DATA_CONSTANTS.R')
  source('code_R/runner.R')
  source('code_R/prevalence.R')
  source('code_R/prevalence_utils.R')
  source('code_R/data.R')
  source('code_R/utils.R')
  source('code_R/incidence.R')
  source('code_R/mortality.R')
  source('code_R/complications.R')
  source('code_R/burden.R')
  source('code_quick_job_scripts/extract_for_purpose.R')

  time_start <- Sys.time()
  if (!dir.exists('cache')) {
    dir.create('cache')
  }

  source('code_R/000_parameters.R')

  # version_no <- "0.4.12"; scenario <- 2; config$run_days_lost <- TRUE;  config$run_days_lost_lever <- TRUE


  if(scenario==1)
  {
    config$run_projection      <- FALSE
  }else
  {
    config$run_projection      <- TRUE
  }
  print(config)
  # config$run_projection      <- FALSE
  # version_no <- "0.4.2"
  # version_no <- "0.4.9"
  # version_no <- "0.4.11"
  # version_no <- "0.4.13"

  # for(i in 1:15)  {

  for(i in 1) {
    i <- 0  # default run

    # i <- 14  # diagnosis_rate_left run
    # i <- 16  # diagnosis_rate_right run
    print(i)
    scenarios[] <- FALSE
    ifelse(i==0,scenarios[i] <- FALSE,scenarios[i] <- TRUE )

    # scenarios[] <- FALSE , i <- cha


    Sys.setenv(T1D_DATA_FILE = paste0("data_outputs/data.db") )
    data_dir  <- paste0( "data_outputs/data_",version_no)
    # cache_dir <- paste0('reruns/rerun_',version_no)
    # cache_dir <- paste0('../../reruns_scenario_',scenario,'/rerun_',version_no,"_",i)
    cache_dir <- paste0('reruns_scenario_',scenario,'_launch/rerun_',version_no,"_",i)
    dir.create(cache_dir)

    if(scenarios$adult_incidence_all_studies)
    { data_dir  <- paste0( "data_outputs/data_",version_no,".adult_incidence_all_studies")   }
    if(scenarios$iot_no_growth)
    {  data_dir  <- paste0( "data_outputs/data_",version_no,".iot_no_growth")}
    if(scenarios$iot_global_curve_for_all)
    { data_dir  <- paste0( "data_outputs/data_",version_no,".iot_global_curve_for_all")}
    if(scenarios$smr_age_cruve_flat_average)
    { data_dir  <- paste0( "data_outputs/data_",version_no,".smr_age_cruve_flat_average")}
    if(scenarios$adult_onset_zero)
    { data_dir  <- paste0( "data_outputs/data_",version_no,".adult_onset_zero")}



    cat('Global summary script. Output is to calc.log.\n')
    flog.appender(appender.file('calc.log'), name='ROOT')
    flog.info('Starting cache refresh')

    # Parallell
    Log=function(fmt, ...) { cat(sprintf(paste0(fmt, '\n'), ...)) }

    countries <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country_region;")

    countries$wd_income_category[countries$wd_income_category==''] <- countries$world_bank_classification[countries$wd_income_category=='' ]
    countries <- countries[countries$idf_country_name!='',]
    # write.csv(dplyr::select(countries,-world_bank_classification ), "temp/countries.csv",row.names = F)
    country_name_list <- countries$world_bank_name
    # country_name_list <- c("France","Germany")

    Sys.time()
    if(FALSE)
    {
      num_thread <- 16
      clust <- parallel::makeCluster(num_thread, setup_strategy = "sequential")
      clusterExport(cl=clust, varlist=c('Log','config' , 'scenarios','write.fst','extract_for_purpose','setDT'
                                        ,'cache_dir','data_dir','run_query_df'
                                        ,'countries'
                                        ,'get_loc_id','get_database_connection'
                                        ,'is_testing','get_prevalence_reference'
                                        ,'MAX_AGE','incidence_function'
                                        ,'get_incidence_curve' ,'pivot_wider'
                                        ,'get_incidence_growth_rates','AGES'
                                        ,'spread'
                                        ,'matrix_from_function','make_age_function'
                                        ,'matrix_from_function'  ,'background_mortality_function','t1d_mortality_function'
                                        ,'calculate_prevalence'
                                        ,'complication_prevalence'
                                        ,'get_complication_parameters','get_hba1c_assumption'
                                        ,'weib_survival','calculate_dalys'
                                        ,'get_disease_weights','prevalence_and_ghost_pop','calculate_ex','calculate_ex_lifetime_years_lost','data_long_2_matrices','Data_Run_query_return_df','host_name','assert'))

      # num_loops <- ceiling( length(country_name_list)/num_thread)
      # for(p in 1:num_loops)
      # {   # p <- 51
      #     # incProgress(1/num_loops, detail = paste("Doing part ", p, "/",num_loops)) # shiny run
      #     start <- 1+(p-1)*num_thread
      #     end   <- min(p*num_thread, length(country_name_list))
      #     country_name_list_t <-  country_name_list[start:end]
      #     print(country_name_list_t) ; print(Sys.time())
      #     system.time({a <- parLapply(clust, country_name_list_t, refresh_one_country_file)})
      # }
      system.time({a <- parLapply(clust, country_name_list, refresh_one_country_file,data_dir=data_dir,cache_dir=cache_dir,countries)})
      stopCluster(clust)
      Sys.time()
    }
    if(run_per_country)
    {
      # system.time({a <- lapply( country_name_list[1:4], refresh_one_country_file)})

      if(length(run_few_countries))
      {
        country_name_list <- country_name_list[country_name_list %in% run_few_countries]
      }
      for(c in 1:length(country_name_list))
      { # c <- 84 ; country_wb_name=country_name_list[c]
        refresh_one_country_file(country_wb_name=country_name_list[c],data_dir=data_dir,cache_dir=cache_dir,countries)
      }

      # run sub national models
      # lapply( country_name_list[1:4], refresh_one_country_file,data_dir=data_dir,cache_dir=cache_dir,countries )
      # refresh_one_country_file(country_wb_name="United States")

    }
   # for ( i in 0:15) { print(i); cache_dir <- paste0('../../reruns_scenario_',scenario,'/rerun_',version_no,"_",i)
    if(run_merge_country)
    {
      countries <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country;")

      # Merge and write to xlsx ----------------------------------------------------------------------------------------------
      flog.info('Generate summary_break_down_age.xlsx..')
      # combine data
      files_db <- list.files(cache_dir,full.names=TRUE)
      files_db <- files_db[files_db %in% paste0(cache_dir,"/",countries$world_bank_code,".binary")]
      # files_db <- files_db[!grepl(".parquet", files_db)]
      country_data_merge      <- rbindlist( lapply(files_db, read_parquet))

      colnames_saved          <- colnames(country_data_merge)
      country_data_merge            <- dplyr::select(countries,Country=world_bank_name,wd_region,wd_income_category  ) %>% dplyr::inner_join(country_data_merge,by="Country")

      country_data_merge_agg <- Agg_country(country_data_merge ,key_list = c("Year","Age"))
      country_data_merge_agg$Country <-  'GLOBAL'
      country_data_merge_agg$Type    <-  'GLOBAL'
      country_data_merge_global <- setDF(country_data_merge_agg)[,colnames_saved]

      country_data_merge_agg <- Agg_country(country_data_merge ,key_list = c("Year","Age","wd_region") )
      country_data_merge_agg$Country <-  country_data_merge_agg$wd_region
      country_data_merge_agg$Type    <-  'wd_region'
      country_data_merge_wd_region <- setDF(country_data_merge_agg)[,colnames_saved]


      country_data_merge_agg <- Agg_country(country_data_merge ,key_list = c("Year","Age","wd_income_category") )
      country_data_merge_agg$Country <-  country_data_merge_agg$wd_income_category
      country_data_merge_agg$Type    <-  'wd_income_category'
      country_data_merge_wd_income_category <- setDF(country_data_merge_agg)[,colnames_saved]


      country_data_merge_all <- rbind(setDF(country_data_merge)[,colnames_saved]
                                      ,country_data_merge_global
                                      ,country_data_merge_wd_region
                                      ,country_data_merge_wd_income_category)

      country_data_merge_all <- country_data_merge_all %>% mutate_if(is.numeric, round, digits=2)

      country_parquet_list <- data.frame()
      country_parquet_list <- rbind(country_parquet_list, data.frame(country_name="GLOBAL", file_name=paste0("GLOBAL") ))
      country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(countries$world_bank_classification), file_name=paste0(unique(countries$world_bank_classification), "")) )
      country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(countries$wd_region), file_name=paste0(unique(countries$wd_region), "")))
      # country_parquet_list <- rbind(country_parquet_list, dplyr::select(country, country_name=world_bank_name,file_name=world_bank_code))

      # save parquet for each aggregates , including regions,
      for(p in 1:nrow(country_parquet_list) )
      { print(paste0("save prev_wide parquet for ",p) )  # p  <- 1
        prev_merge_wide_t <-  country_data_merge_all[country_data_merge_all$Country==country_parquet_list$country_name[p],colnames_saved]
        # write_parquet(prev_merge_wide_t, paste0(cache_dir,"/",country_parquet_list$file_name[p],".binary"))
        # saveRDS(prev_merge_wide_t, paste0(cache_dir,"/prev_merge_wide/",countrys_and_regions$file_name[p],".test"))
        write_parquet(prev_merge_wide_t, paste0(cache_dir,"/",country_parquet_list$file_name[p],".binary"))
      }

    }

    # arrow::write_parquet(country_stats_over_year_wide,paste0(cache_dir,"/aggregates/country_data_merge.parquet"),compression ="uncompressed")
    if(FALSE)
    {
      files_db <- list.files(cache_dir,full.names=TRUE)
      # files_db <- files_db[files_db %in% paste0(cache_dir,"/",countries$world_bank_code,".binary")]
      files_db <- files_db[grepl(".binary", files_db)]
      country_data_merge      <- rbindlist( lapply(files_db, read_parquet))
      # table_name <- "main_0_4_15_lifetime_low_high"
      table_name <- paste0("main_",gsub("\\.","_",version_no))

      Data_dump_data_frame(country_data_merge ,schema_name="public",table_name=table_name )

      Data_Run_query_return_df(paste0('CREATE INDEX idx_country_year_',table_name,' ON ',table_name,' ("Country", "Year");'))
      Data_Run_query_return_df(paste0('CREATE INDEX idx_country_'     ,table_name,' ON ',table_name,' ("Country");'))
      Data_Run_query_return_df(paste0('CREATE INDEX idx_year_'        ,table_name,' ON ',table_name,' ("Year");'))



    }

    # exports to excel , purpose  -----------------------------  -----------------------------  -----------------------------  -----------------------------
    if(FALSE)
    {
      # paper info, get proportion of prevalence from extrapolated countries idf ------
      # what's the proportion of stats when a country is using extrapolated incidence number from IDF ? 2021
      data <- country_stats_over_year[country_stats_over_year$Year==2021,]
      data <- data[data$world_bank_name %in% countries$world_bank_name,]

      data$is_extrapolated <- data$idf_reference_country!= 'N/A'
      data <- setDT(data)[,list(
        number_of_countries= .N,
        HIC= sum(world_bank_classification=="HIC"),
        LIC= sum(world_bank_classification=="LIC"),
        LMIC= sum(world_bank_classification=="LMIC"),
        UMIC = sum(world_bank_classification=="UMIC"),
        `Ann. background population`=sum(`Ann. background population`),
        prevalence=sum(Prevalence),
        Ghosts=sum(Ghosts)
      ),by=is_extrapolated]

    }

    if(FALSE)
    {
      setDF(country_data_merge)
      country_data_merge_wide  <- round(country_stats_over_year_wide[,c(-1,-2,-3,-4)],2)
      country_data_merge_wide  <- cbind(country_stats_over_year_wide[,c(1,2,3,4)], country_data_merge_wide)

      # fwrite(country_data_merge_wide[,], "summary_break_down_age.csv")
      # saveRDS(country_data_merge_wide, paste0(cache_dir,"/aggregates/t1dGlobalModel.Rds") )
      fwrite(country_data_merge_wide[1:1000000,],  paste0(cache_dir,"/aggregates/summary_break_down_age_1-1000000.csv") )
      fwrite(country_data_merge_wide[1000001:nrow(country_data_merge_wide),],  paste0(cache_dir,"/aggregates/summary_break_down_age_1000000-end.csv"))


      # age bin 5 years , eg: Age=0 denotes  0 <= age <=4 ------------------------------------------------
      prev_merge_long      <- gather(country_data_merge_wide ,key = "Value_type", value = "Value",-Country,-Type,-Year,-Age)
      prev_merge_long$Year <- as.numeric(prev_merge_long$Year)
      prev_merge_long$Age  <- as.numeric(prev_merge_long$Age)
      prev_merge_long <- prev_merge_long[,colnames(country_data_merge)]


      country_data_merge_bin       <- prev_merge_long
      country_data_merge_bin$Age   <- (country_data_merge_bin$Age) -  (country_data_merge_bin$Age) %% 5

      country_data_merge_bin_le       <- setDT(country_data_merge_bin)[ grepl("Life expectency",Value_type),list(Value=Value[1] ),by=c( 'Country', 'Type','Value_type','Year','Age')]
      country_data_merge_bin_non_le   <- setDT(country_data_merge_bin)[!grepl("Life expectency",Value_type),list(Value=sum(Value) ),by=c( 'Country', 'Type','Value_type','Year','Age')]

      country_data_merge_bin       <- rbind(country_data_merge_bin_le,country_data_merge_bin_non_le )
      country_data_merge_bin       <- spread(country_data_merge_bin, Value_type, Value )
      fwrite(country_data_merge_bin,  paste0(cache_dir,"/aggregates/summary_break_down_age_bin.csv" ) )

      print(Sys.time() - time_start)
      # Read the 2 CSV file names from working directory
      Zip_Files <- list.files( paste0(cache_dir,"/aggregates/"),pattern = "*.csv")
      # Zip the files and place the zipped file in working directory
      zip::zip(zipfile  =  paste0("t1dGlobalModel_",version_no,".zip"),
               files    =  Zip_Files ,
               root     =  paste0(cache_dir,"/aggregates/"))

      unlink(paste0(cache_dir,"/aggregates/",Zip_Files))
    }

  }

  gc()

}

if(FALSE)
{
  # single run 
  main_(version_no="0.4.15",run_per_country=TRUE,run_merge_country=TRUE ,scenario=2)
    
    
}




if(FALSE)
{
  library(data.table)
  library(dplyr)
  library(echarts4r)
  library(parallel)
  library(arrow)
  library(RPostgreSQL)

  rm(list=ls())
  source('code_R_data_prep/000_0_build_database.R')
  source('code_R/001_main.R')

  sink("log.txt",append=TRUE);cat(paste0(" Begin \n") );sink()
  # main_(version_no="0.4.12",run_per_country=TRUE,run_merge_country=TRUE)  # default run
  main_mcmc <- function(i)
  {  # i <- 1
    run_name <- paste0("0.4.12_mcmc_",i)
    # build_database(version_no=run_name,random_data_ci=TRUE)
    # main_(run_name,run_per_country=TRUE,run_merge_country=FALSE,run_few_countries=c("India","United States","Netherlands","Democratic Republic of the Congo"))
    # main_(run_name,run_per_country=TRUE,run_merge_country=FALSE,run_few_countries=c("Afghanistan"))
    # main_(version_no=run_name,run_per_country=TRUE,run_merge_country=FALSE,scenario = 1)
    main_(version_no=run_name,run_per_country=TRUE,run_merge_country=FALSE,scenario = 2)

    # sink("log.txt",append=TRUE);cat(paste0(Sys.time()," main_mcmc, process id: ", i ," finished \n") );sink()
    # closeAllConnections()
    gc()
  }
  if(FALSE)
  {# check number
    data_new <- read_parquet("../../reruns_scenario_2_launch/rerun_0.4.12_mcmc_490_0/AFG.binary")
    data_old <- read_parquet("../../reruns_scenario_2/rerun_0.4.12_mcmc_490_0/AFG.binary")
    sum(data_old$Prevalence==data_new$Prevalence)
    sum(data_old$Ghosts==data_new$Ghosts)
    sum(data_old$`Life expectency (2 t1d base)`==data_new$`Life expectency (2 t1d base)`)
  }

  sink("log.txt",append=TRUE);cat(paste0(Sys.time()," main_mcmc, Start \n") );sink()

  num_thread <- 2
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  # clusterExport(cl=clust, varlist=c("build_database","main_"))
  clusterExport(cl=clust, varlist=c("main_"))
  system.time({a <- clusterApply(clust, 76:78, main_mcmc)})
  stopCluster(clust)
  Sys.time()

  # if(FALSE)
  # {
  #   #  SparkR install guild: https://phoenixnap.com/kb/install-spark-on-windows-10
  #   library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
  #   sparkR.session(master = "local[10]", sparkConfig = list(
  #     spark.executor.memory='1G'
  #     ,spark.driver.memory='2G'
  #     ,spark.r.backendConnectionTimeout	=60000
  #     # ,spark.executor.rpc.netty.dispatcher.numThreads = 10
  #     ,spark.serializer="org.apache.spark.serializer.KryoSerializer"
  #     # ,spark.sql.execution.arrow.sparkr.enabled    =TRUE
  #   ))
  #   # sparkR.session.stop()
  #   system.time({SparkR::spark.lapply(571:1000,main_mcmc)})
  #   # sparkR.session.stop()
  #   detach("package:SparkR", unload = TRUE)
  # }



  # merge to GLOBAL, REGION, ICOME LEVEL
  num_thread <- 40
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c("build_database","main_"))
  system.time({a <- clusterApply(clust, 1:1000, function(i) {
    sink("log.txt",append=TRUE);cat(paste0(Sys.time()," Merge coutry to GLOBAL , process id: ", i ," \n") );sink()
    main_( version_no=paste0("0.4.12_mcmc_",i),run_per_country=FALSE,run_merge_country=TRUE,scenario = 1)
    # main_( version_no=paste0("0.4.12_mcmc_",i),run_per_country=FALSE,run_merge_country=TRUE,scenario = 2)
  })})
  stopCluster(clust)
  Sys.time()



  # Save to AWS RDS DB Cis for all ----------------------------------------------------------------------------------------------------------------------------
  con <-  dbConnect(RSQLite::SQLite(), paste0("../../data_outputs/data_0.4.12/","data.db") , read_only=read_only)
  country <- dbGetQuery( con, "SELECT *   FROM country ")
  dbDisconnect(con)
  country_parquet_list <- data.frame()
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name="GLOBAL", file_name=paste0("GLOBAL") ))
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(country$world_bank_classification), file_name=paste0(unique(country$world_bank_classification), "")) )
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(country$wd_region), file_name=paste0(unique(country$wd_region), "")))
  country_parquet_list <- rbind(country_parquet_list, dplyr::select(country, country_name=world_bank_name,file_name=world_bank_code))


  scenario  <- 2
  schema    <- paste0("main_ci_partition_scenario_",scenario)


  file_list_country_list <- list.files(paste0("../../reruns_scenario_",scenario,"/rerun_0.4.12_mcmc_1_0"))
  file_list_country_list <- file_list_country_list[grepl(".binary",file_list_country_list)]
  # file_list_country_list <- "GLOBAL.binary"
  run_query_return_df_aws <- function(query)
  {
    host <- "localhost"
    connec <- dbConnect(RPostgres::Postgres(),  dbname = "t1d", host = host,
                        port = "5432",  user = "postgres",   password = "postgrest1d")
    data_put <- RPostgres::dbSendQuery(connec, query )
    dbDisconnect(connec)
    return(data_put)
  }

  get_ci_per_country <-  function(i,schema,scenario)
  {
    # i <- 2
    library(data.table)
    library(arrow)
    library(RPostgreSQL)
    library(dplyr)
    host <- "localhost"
    connec <- dbConnect(RPostgres::Postgres(),  dbname = "t1d", host = host,
                        port = "5432",  user = "postgres",   password = "postgrest1d")
    file_list_runs <- list.files(paste0("../../reruns_scenario_",scenario),full.names = TRUE)
    file_list_runs <- file_list_runs[grepl("0.4.12_mcmc_",file_list_runs)]
    file_list_runs <- paste0(file_list_runs, "/",file_list_country_list[i])
    country_data_merge      <- rbindlist( lapply(1:1000,function(x,file_list_runs){df <- read_parquet(file_list_runs[x]);df$run<- x; df},file_list_runs ),use.names=TRUE)
    country_data_merge      <- dplyr::select(country_data_merge,run, Country,Year,Age,`Ann. background population`
                                             ,Prevalence
                                             ,Ghosts
                                             ,`Ghosts (early death)`
                                             ,`Ghosts (onset death)`
                                             , `Incidence (1 base)`
                                             , `Life expectency (1 background)`
                                             ,`Life expectency (2 t1d base)`
                                             ,`Ann. early deaths`
                                             ,`Ann. onset deaths`
    )
    # for(y in c(2000:2022,2040) )
    for(y in c(2025,2030,2035))
    {  # y <- 2000
       print(y)
      country_data_merge_ <- country_data_merge[country_data_merge$Year %in% c(y),]
      RPostgreSQL::dbWriteTable(connec, DBI::SQL(paste0(schema,".main_ci_",y))    , setDF(country_data_merge_), overwrite=FALSE,row.names=FALSE,append=TRUE)

    }

    dbDisconnect(connec)

    sink("log.txt",append=TRUE);cat(paste0(Sys.time()," get_ci_per_country, process id: ", i ," \n") );sink()

  }

  # create tables
  get_query_create <- function(year,schema){ paste0('

          CREATE TABLE  ',schema,'.main_ci_',year,'
          (
              run integer,
              "Country" text COLLATE pg_catalog."default",
              "Year" double precision,
              "Age" double precision,
              "Ann. background population" double precision,
              "Prevalence" double precision,
              "Ghosts" double precision,
              "Ghosts (early death)" double precision,
              "Ghosts (onset death)" double precision,
              "Incidence (1 base)" double precision,
              "Life expectency (1 background)" double precision,
              "Life expectency (2 t1d base)" double precision,
              "Ann. early deaths" double precision,
              "Ann. onset deaths" double precision
          )

          PARTITION BY LIST ("Country");
  ')}



  # for(year in c(2000:2022,2040))
    for(year in c(2025,2030,2035))
    {   # year <- 2000
    print(year)
    run_query_return_df_aws(paste0('DROP TABLE IF EXISTS ',schema,'.main_ci_',year,''))
    run_query_return_df_aws(query=get_query_create(year,schema))
    for(i in 1: nrow(country_parquet_list))
    {   # i <- 191
      # print(i)
      query_partition <-  paste0("CREATE TABLE \" ",scenario,".main_ci_",year,"main_ci_",year,"_",tolower(country_parquet_list$file_name[i]),"\"
             PARTITION OF ",schema,".main_ci_",year," FOR VALUES IN  ('",gsub("'","''",(country_parquet_list$country_name[i])),"');")
      run_query_return_df_aws(query_partition)

    }

    }
  # system.time({get_ci_per_country(1,file_list_country_list)})
  num_thread <- 6
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c("file_list_country_list"))
  system.time({a <- clusterApply(clust, 1:213, get_ci_per_country,schema,scenario)})
  stopCluster(clust)
  Sys.time()



  # check global prevalence , missing, le diff -------------------------------
  country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')
  # USA, GLOBAL, India, demo of congo.  HTML files, csv . upper and lower. 2000-2040. ------- ------- ------- ------- ------- -------
  country_name <- "GLOBAL"
  # country_name <- "USA"
  # country_name <- "IND"
  # country_name <- "COD"
  # country_name <- "NLD"

  file_list <- list.files(paste0("../../reruns_scenario_2"),full.names = TRUE)
  # file_list <- paste0( "../t1dGlobalModel_data/reruns/rerun_0.4.12_mcmc_",1:500,"_0")
  file_list <- file_list[grepl("0.4.12_mcmc_",file_list)]
  file_list <- paste0(file_list, "/",country_name,".binary")
  country_name <- country$world_bank_name[country$world_bank_code==country_name]
  output_file <- "t1dGlobalModel_data/mcmc_plot_paper"
  country_data_merge      <- rbindlist( lapply(file_list,function(x){df <- read_parquet(x);df$run<- x; df} ))

  data_plot <- setDT(country_data_merge)[,list(Value=sum(Prevalence[Age<100])),by=c("Country","Year","run")]
  data_plot <- data_plot[data_plot$Year>=2000,]
  # data_plot <- setDT(country_data_merge)[,list(Value=`Life expectency (2 t1d base)`[Age==10]),by=c("Country","Year","run")]
  data_plot <- data_plot[,list(value_median= median(Value)
                               ,lower= quantile(Value,probs=c(.025))
                               ,upper= quantile(Value,probs=c(.975))
                               ,n_run=.N),by=c("Year")]






  write.csv(data_plot,paste0("../",output_file,"/",country_name,"_prevalence.csv") )

  e1 <- data_plot %>%
    e_charts(Year) %>%
    e_line(value_median,name="Value") %>%
    e_band(lower,upper,
           stack = "confidence-band",
           symbol = c("none", "none"),)%>%e_x_axis(name="Year")%>%e_y_axis(name="Prevalence") %>%
    e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)

  data_plot <- setDT(country_data_merge)[,list(Value=sum(Ghosts)),by=c("Country","Year","run")]
  data_plot <- data_plot[data_plot$Year>=2000,]
  # data_plot <- setDT(country_data_merge)[,list(Value=`Life expectency (2 t1d base)`[Age==10]),by=c("Country","Year","run")]
  data_plot <- data_plot[,list(value_median= median(Value)
                               ,lower= quantile(Value,probs=c(.025))
                               ,upper= quantile(Value,probs=c(.975))),by=c("Year")]
  write.csv(data_plot,paste0("../",output_file,"/",country_name,"_missing_prevalence.csv") )

  e2 <- data_plot %>%
    e_charts(Year) %>%
    e_line(value_median,name="Value") %>%
    e_band(lower,upper,
           stack = "confidence-band",
           symbol = c("none", "none"),)%>%e_x_axis(name="Year")%>%e_y_axis(name="Missing Prevalence") %>%
    e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)


  data_plot <- setDT(country_data_merge)[,list(Value=sum(`Incidence (1 base)`)),by=c("Country","Year","run")]
  data_plot <- data_plot[data_plot$Year>=2000,]
  # data_plot <- data_plot[data_plot$Year>=2000 & data_plot$Year<=2020,]
  # data_plot <- setDT(country_data_merge)[,list(Value=`Life expectency (2 t1d base)`[Age==10]),by=c("Country","Year","run")]
  data_plot <- data_plot[,list(value_median= median(Value)
                               ,lower= quantile(Value,probs=c(.025))
                               ,upper= quantile(Value,probs=c(.975))),by=c("Year")]

  write.csv(data_plot,paste0("../",output_file,"/",country_name,"_incidence.csv") )

  e3 <- data_plot %>%
    e_charts(Year) %>%
    e_line(value_median,name="Value") %>%
    e_band(lower,upper,
           stack = "confidence-band",
           symbol = c("none", "none"),)%>%e_x_axis(name="Year")%>%e_y_axis(name="Incidence") %>%
    e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)


  # data_plot <- setDT(country_data_merge)[,list(Value=sum(`Incidence (1 base)`)),by=c("Country","Year","run")]
  data_plot <- setDT(country_data_merge)[,list(Value=`Life expectency (2 t1d base)`[Age==10]),by=c("Country","Year","run")]
  data_plot <- data_plot[data_plot$Year>=2000,]
  data_plot <- data_plot[,list(value_median= median(Value)
                               ,lower= quantile(Value,probs=c(.025))
                               ,upper= quantile(Value,probs=c(.975))),by=c("Year")]
  write.csv(data_plot,paste0("../",output_file,"/",country_name,"_life_expectency_at10.csv") )

  e4 <- data_plot %>%
    e_charts(Year) %>%
    e_line(value_median,name="Value") %>%
    e_band(lower,upper,
           stack = "confidence-band",
           symbol = c("none", "none"),)%>%e_x_axis(name="Year")%>%e_y_axis(name="Life Expectency") %>%
    e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)


  e_all <-  e_arrange( e1,e2,e3,e4,   cols = 2)





  # get ci for all -----------------------------



}



