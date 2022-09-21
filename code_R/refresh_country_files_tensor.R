

refresh_country_files_tensor <- function(input)
{

  loc_ids  <- c()
  loc_ids  <- c("840","4")
  MAX_AGE <- 100
  AGES    <- 0: 99

  years   <- (1960 - 100):2040

  library(data.table)
  library(dplyr)
  library(tidyr)
  library(abind)
  library(arrow)
  library(RSQLite)
  countries         <- readRDS("data_default_run/country.Rds")
  data_dir          <- 'C:/DropboxT1D/data_outputs/data_0.4.12'

  file_list <- list.files(data_dir,full.names = TRUE)
  # file_list <- paste0( data_dir,"/",loc_ids,".Rds" )
  # file_list <- file_list[1:2]
  file_list <- file_list[grepl(".Rds",file_list)]
  data_long      <- rbindlist( lapply(file_list,function(x){df <- readRDS(x); df} ))
  loc_ids  <- unique(data_long$loc_id)

  #  pre fill from 1860 to 1899-------------------------------------
  data_long_1860_1900 <- data_long[data_long$year==1900,] %>% slice(rep(1:n(), 1900-1860))
  data_long_1860_1900$year <- sort(rep(1860:1899,length(file_list)*100))
  data_long <- rbind(data_long,data_long_1860_1900 )
  data_long <- data_long[ with(data_long, order( loc_id,year,age)), ]   # order properly for transforming to matrix

  #--------------
  life_table_background_    <- data.matrix(dplyr::select(data_long, loc_id, year,age,qx =background_mortality_rate),rownames.force = NULL) # order is important
  life_table_background     <- calculate_ex_gpu(life_table_background_)
  #-----------
  data_long$smr <- data_long$value_smr_minimal_care      * (1-data_long$value_percent_non_minimal_care)  +
                   data_long$value_smr_non_minimal_care  *    data_long$value_percent_non_minimal_care
  data_long$qx      <- data_long$background_mortality_rate / (1 - data_long$background_mortality_rate) * data_long$smr
  data_long$qx      <- data_long$qx / (1+ data_long$qx)
  data_long$qx[is.nan(data_long$qx)] <- 1

  life_table_t1d_    <- data.matrix(dplyr::select(data_long, loc_id, year,age,qx),rownames.force = NULL) # order is important
  life_table_t1d     <- calculate_ex_gpu(life_table_t1d_)

  #--------------
  data_long$qx <- data_long$value_smr_non_minimal_care *  data_long$background_mortality_rate
  life_table_t1d_basic_care_    <- data.matrix(dplyr::select(data_long, loc_id, year,age,qx),rownames.force = NULL) # order is important
  life_table_t1d_basic_care     <- calculate_ex_gpu(life_table_t1d_basic_care_)

  #--------------
  data_long$smr <- (1 + (data_long$value_smr_minimal_care-1) * 0.37)       * (1-data_long$value_percent_non_minimal_care)  +
                   (1 + (data_long$value_smr_non_minimal_care-1) * 0.37)   * data_long$value_percent_non_minimal_care
  data_long$qx      <- data_long$background_mortality_rate / (1 - data_long$background_mortality_rate) * data_long$smr
  data_long$qx      <- data_long$qx / (1+ data_long$qx)
  data_long$qx[is.nan(data_long$qx)] <- 1

  life_table_t1d_best_care_    <- data.matrix(dplyr::select(data_long, loc_id, year,age,qx),rownames.force = NULL) # order is important
  life_table_t1d_best_care    <- calculate_ex_gpu(life_table_t1d_best_care_)

  #--------------

  data_long <- data_long[ with(data_long, order(age,year, loc_id)), ]   # order properly for transforming to matrix

  #--------------

  data_long_to_array <- function(data_long, value_name)
  {
    df <- dplyr::select(data_long,loc_id,year,age,value=value_name )
    qB <- array(data = df$value,
                dim= c(length(unique(df$loc_id)), length(unique(df$year)), length(unique(df$age))),
                dimnames=list(unique(df$loc_id)        , unique(df$year)        , unique(df$age)))
    qB
  }

  qB_full  <- data_long_to_array(data_long, value_name="background_mortality_rate" )
  pop_full <- data_long_to_array(data_long, value_name="background_population" )
  dDx_full <- data_long_to_array(data_long, value_name="mortality_undiagnosed_rate" )
  i_full   <- data_long_to_array(data_long, value_name="incidence_rate" )/ 1e5
  smr_matrix_n   <- data_long_to_array(data_long, value_name="value_smr_non_minimal_care" )
  smr_matrix_m   <- data_long_to_array(data_long, value_name="value_smr_minimal_care" )
  qT1D_percent_n_full   <- data_long_to_array(data_long, value_name="value_percent_non_minimal_care" )

  diab_odds    <- qB_full / (1 - qB_full) * smr_matrix_n
  qT1D_n_full       <- diab_odds / (1 + diab_odds)
  diab_odds    <- qB_full / (1 - qB_full) * smr_matrix_m
  qT1D_m_full       <- diab_odds / (1 + diab_odds)


  system.time({
    # base scenario --------------------------------------------------------------------
    i <- i_full; qB <- qB_full; qT1D_n <- qT1D_n_full; qT1D_m <- qT1D_m_full; qT1D_percent_n <- qT1D_percent_n_full; dDx <- dDx_full
    prev                    <- calculate_prevalence_tensor(i        , qB, qT1D_n, qT1D_m, qT1D_percent_n, dDx )
    # full diag --------------------------------------------------------------------
    i <- i_full; qB <- qB_full; qT1D_n <- qT1D_n_full; qT1D_m <- qT1D_m_full; qT1D_percent_n <- qT1D_percent_n_full; dDx <- dDx_full
    i_full_diag   <- i;       i_full_diag   <- (i/(1-dDx))
    dDx_full_diag <- dDx;     dDx_full_diag[] <- 0
    prev_100d               <- calculate_prevalence_tensor(i_full_diag, qB, qT1D_n, qT1D_m, qT1D_percent_n, dDx_full_diag) # deaths on diagnosis are converted to higer incidence
    # basic care  --------------------------------------------------------------------
    i <- i_full; qB <- qB_full; qT1D_n <- qT1D_n_full; qT1D_m <- qT1D_m_full; qT1D_percent_n <- qT1D_percent_n_full; dDx <- dDx_full
    qT1D_n_bacare <- qT1D_n
    qT1D_m_bacare <- qT1D_m;   qT1D_m_bacare <- qT1D_n
    prev_100d_basic_care    <- calculate_prevalence_tensor(i_full_diag, qB, qT1D_n_bacare, qT1D_m_bacare, qT1D_percent_n, dDx_full_diag) # smr all non-minimal
    # best care  --------------------------------------------------------------------
    i <- i_full; qB <- qB_full; qT1D_n <- qT1D_n_full; qT1D_m <- qT1D_m_full; qT1D_percent_n <- qT1D_percent_n_full; dDx <- dDx_full
    smr_matrix_n_best_care  <- (1 + (smr_matrix_n-1) * 0.37) ;
    diab_odds  <- qB / (1 - qB) * smr_matrix_n_best_care;  qT1D_n_becare <- diab_odds / (1 + diab_odds)
    smr_matrix_m_best_care  <- (1 + (smr_matrix_m-1) * 0.37) ;
    diab_odds  <- qB / (1 - qB) * smr_matrix_m_best_care;  qT1D_m_becare <- diab_odds / (1 + diab_odds)
    prev_100d_best_care     <- calculate_prevalence_tensor(i_full_diag, qB, qT1D_n_becare, qT1D_m_becare, qT1D_percent_n, dDx_full_diag) # deaths on diagnosis are converted to higer incidence
    # cure --------------------------------------------------------------------
    i <- i_full; qB <- qB_full; qT1D_n <- qT1D_n_full; qT1D_m <- qT1D_m_full; qT1D_percent_n <- qT1D_percent_n_full; dDx <- dDx_full
    qT1D_n_cure <- qT1D_n  ;qT1D_n_cure <- qB
    qT1D_m_cure <- qT1D_m  ;qT1D_m_cure <- qB
    prev_100d_cure          <- calculate_prevalence_tensor(i_full_diag, qB, qT1D_n_cure,     qT1D_m_cure,     qT1D_percent_n, dDx_full_diag)
  })



  pop_scale_factor       <- pop_full / (prev$P + prev$S)   # exclude the dead from population

  P_level         <- pop_scale_factor * prev$P  #  sum(P_level["2021",]) ; sum( (pop_scale_factor_100d * prev_100d$P) ["2021",])
  S_level         <- pop_scale_factor * prev$S
  I_flow          <- pop_scale_factor * prev$I

  I_flow_diagnosis           <- pop_scale_factor * prev_100d$I
  I_flow_basic_care          <- pop_scale_factor * prev_100d_basic_care$I
  I_flow_best_care           <- pop_scale_factor * prev_100d_best_care$I
  I_flow_cure                <- pop_scale_factor * prev_100d_cure$I

  DDx_flow                   <- pop_scale_factor * prev$DDx   # sum(DDx_flow["2021",])
  DT1D_flow                  <- pop_scale_factor * prev$DT1D
  DT1D_flow_100d             <- pop_scale_factor * prev_100d$DT1D
  DT1D_flow_basic_care       <- pop_scale_factor * prev_100d_basic_care$DT1D
  DT1D_flow_best_care        <- pop_scale_factor * prev_100d_best_care$DT1D
  DT1D_flow_cure             <- pop_scale_factor * prev_100d_cure$DT1D

  BD_flow         <- pop_full * qB_full

  # Calculate missing prevalence  and delta of levers. --------------------------------------------------------------------------------
  ghost_ddx_level   <- (prev_100d$P              - prev$P) * pop_scale_factor #
  ghost_hba1c_level <- (prev_100d_cure$P         - prev_100d$P) * pop_scale_factor #

  ghost_basic_care  <- (prev_100d_basic_care$P   - prev_100d$P) * pop_scale_factor #  delta
  ghost_best_care   <- (prev_100d_best_care$P    - prev_100d_basic_care$P) * pop_scale_factor #
  ghost_cure        <- (prev_100d_cure$P         - prev_100d_best_care$P) * pop_scale_factor #  sum(ghost_hba1c_level["2021",])
  ghost_level <- ghost_hba1c_level + ghost_ddx_level #   sum(ghost_level["2021",])

  # Days lost
  con   <- dbConnect(RSQLite::SQLite(), paste0(data_dir,"/data.db"), read_only=read_only)
  const <- dbGetQuery(con, 'SELECT complication, hba1c_lt_8, hba1c_8_to_9, hba1c_gteq_9 FROM constant_complications;')
  weib  <- dbGetQuery(con, 'SELECT abbrev, intercept, slope, scale, name, graph_title FROM weibull_complications;')
  dw    <- dbGetQuery(con, 'SELECT complication, DALYs FROM disease_weights;') # desease weight
  disease_weights <- dw$DALYs
  names(disease_weights) <- dw$complication
  dbDisconnect(con)
  complication_parameters <- list(constant=const, weibull=weib)

  dalys <- array(NA, dim=list(length(loc_ids), length(years), MAX_AGE), dimnames=list(loc_ids,years, AGES))# S - susceptible (non-T1D)
  dalys_100d              <- dalys
  dalys_100d_basic_care   <- dalys
  dalys_100d_best_care    <- dalys
  dalys_100d_cure         <- dalys

  if(FALSE) # do not run burden,  days lost same as prevalance----
  {
    P_cohorts_level <- prev      $Pcohorts * array(pop_scale_factor, c(length(loc_ids),length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[,-(1:MAX_AGE),,,drop=FALSE]
    dalys_temp  <- complication_prevalence_tensor(years=1960:2040,P_cohorts_level[1,,,],smr_matrix_n[1,,], smr_matrix_m[1,,],qT1D_percent_n[1,,],complication_parameters=complication_parameters,disease_weights=disease_weights)
    dalys[1,101:181,]  <- dalys_temp$year_age

    P_cohorts_level <- prev_100d$Pcohorts * array(pop_scale_factor, c(length(loc_ids),length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[,-(1:MAX_AGE),,,drop=FALSE]
    dalys_temp  <- complication_prevalence_tensor(years=1960:2040,P_cohorts_level[1,,,],smr_matrix_n[1,,], smr_matrix_m[1,,],qT1D_percent_n[1,,],complication_parameters=complication_parameters,disease_weights=disease_weights)
    dalys_100d[1,101:181,]  <- dalys_temp$year_age

    P_cohorts_level <- prev_100d_basic_care$Pcohorts * array(pop_scale_factor, c(length(loc_ids),length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[,-(1:MAX_AGE),,,drop=FALSE]
    dalys_temp  <- complication_prevalence_tensor(years=1960:2040,P_cohorts_level[1,,,],smr_matrix_n[1,,], smr_matrix_m[1,,],qT1D_percent_n[1,,],complication_parameters=complication_parameters,disease_weights=disease_weights)
    dalys_100d_basic_care[1,101:181,]  <- dalys_temp$year_age


    P_cohorts_level <- prev_100d_best_care$Pcohorts * array(pop_scale_factor, c(length(loc_ids),length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[,-(1:MAX_AGE),,,drop=FALSE]
    dalys_temp  <- complication_prevalence_tensor(years=1960:2040,P_cohorts_level[1,,,],smr_matrix_n[1,,], smr_matrix_m[1,,],qT1D_percent_n[1,,],complication_parameters=complication_parameters,disease_weights=disease_weights)
    dalys_100d_best_care[1,101:181,]  <- dalys_temp$year_age

    P_cohorts_level <- prev_100d_cure$Pcohorts * array(pop_scale_factor, c(length(loc_ids),length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[,-(1:MAX_AGE),,,drop=FALSE]
    smr_matrix_n_cure <- smr_matrix_n
    smr_matrix_m_cure <- smr_matrix_m
    smr_matrix_n_cure[,year_range,] <- 1
    smr_matrix_m_cure[,year_range,] <- 1
    dalys_temp  <- complication_prevalence_tensor(years=1960:2040,P_cohorts_level[1,,,],smr_matrix_n_cure[1,,], smr_matrix_m_cure[1,,],qT1D_percent_n[1,,],complication_parameters=complication_parameters,disease_weights=disease_weights)
    dalys_100d_cure[1,101:181,]  <- dalys_temp$year_age

  }

  prev_merge <-       as.data.frame.table(pop_full,stringsAsFactors = F)[,1:3]
  colnames(prev_merge) <- c("loc_id","Year","Age")
  prev_merge$"Ann. background population"           <- as.data.frame.table(pop_full)[,4]
  prev_merge$"Ann. background mortality"            <- as.data.frame.table(BD_flow)[,4]
  prev_merge$"Prevalence"                           <- as.data.frame.table(P_level)[,4]
  prev_merge$"Incidence (1 base)"                   <- as.data.frame.table(I_flow)[,4]
  prev_merge$"Incidence (2 diagnosis)"              <- as.data.frame.table(I_flow_diagnosis)[,4]

  prev_merge$"Ghosts"                                        <- as.data.frame.table(ghost_level)[,4]
  prev_merge$"Ghosts (onset death)"                          <- as.data.frame.table(ghost_ddx_level)[,4]
  prev_merge$"Ghosts (early death)"                          <- as.data.frame.table(ghost_hba1c_level)[,4]
  prev_merge$"Ghosts (delta basic care)"                     <- as.data.frame.table(ghost_basic_care)[,4]
  prev_merge$"Ghosts (delta best care)"                      <- as.data.frame.table(ghost_best_care)[,4]
  prev_merge$"Ghosts (delta cure)"                           <- as.data.frame.table(ghost_cure)[,4]
  prev_merge$"Ann. onset deaths"                             <- as.data.frame.table(DDx_flow)[,4]
  prev_merge$"Ann. early deaths"                             <- as.data.frame.table(DT1D_flow)[,4]

  prev_merge$"Ann. days lost (1 base)"                                 <- as.data.frame.table(dalys)[,4]
  prev_merge$"Ann. days lost (2 diagnosis)"                            <- as.data.frame.table(dalys_100d)[,4]
  prev_merge$"Ann. days lost (3 basic care)"                           <- as.data.frame.table(dalys_100d_basic_care)[,4]
  prev_merge$"Ann. days lost (4 best care)"                            <- as.data.frame.table(dalys_100d_best_care)[,4]
  prev_merge$"Ann. days lost (5 cure)"                                 <- as.data.frame.table(dalys_100d_cure)[,4]


  prev_merge$"Life expectency (1 background)"                                        <- life_table_background[,9]
  prev_merge$"Life expectency (2 t1d base)"                                          <- life_table_t1d[,9]
  prev_merge$"Life expectency (3 t1d diagnosis)"                                     <- life_table_t1d[,9]
  prev_merge$"Life expectency (4 t1d basic care)"                                    <- life_table_t1d_basic_care[,9]
  prev_merge$"Life expectency (5 t1d best care)"                                     <- life_table_t1d_best_care[,9]
  prev_merge$"Life expectency (6 t1d cure)"                                          <- life_table_background[,9]

  prev_merge <- prev_merge[prev_merge$Year>=1960,]
  prev_merge <- prev_merge %>% mutate_if(is.numeric, round, digits=2)
  prev_merge$Year    <- as.numeric(prev_merge$Year)
  prev_merge$Age     <- as.numeric(prev_merge$Age)
  prev_merge$Country <- prev_merge$loc_id
  JDRF_calculations <- extract_for_purpose(prev_merge,0,99,2022)

  prev_merge$Country <- NULL
  write_parquet(prev_merge, paste0("../../temp/prev_merge_",input) )

}

if(FALSE)
{
  library(parallel)
  source("code_R/refresh_country_files_tensor_utils.R")
  num_thread <- 10
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c("calculate_prevalence_tensor","calculate_ex","calculate_ex_gpu","extract_for_purpose"))
  system.time({a <- clusterApply(clust, 1:10, refresh_country_files_tensor)})
  stopCluster(clust)
  Sys.time()

  system.time({refresh_country_files_tensor(1)})

}
