
#' Construct prevalence and ghost population
#'
#' This function calculates prevalence for the given country, as well as the
#' 'ghost population'. For full details, see the working paper.
#'
#' Prevalence is calculated using \link{calculate_prevalence}(), where the
#' parameter matrices are derived from the callback functions given as
#' arguments. Where callbacks are not provided, defaults for
#' \code{country_wb_name} are used. The ghost population is calculated by taking
#' the difference between a counterfactual population with zero deaths at onset
#' and the T1D mortality rate the same as background mortality.
#'
#' The return value is a list with the following elements, where `N` is the
#' number of years of output:
#' \itemize{
#'   \item `P_level` - `N` x `MAX_AGE` matrix of prevalence (T1D) compartments,
#'         by year and age. Values are scaled by country population, so units
#'         are persons.
#'   \item `S_level` - `N` x `MAX_AGE` matrix of susceptible (healthy) compartments,
#'         by year and age. Values are scaled by country population, so units
#'         are persons.
#'   \item `P_cohorts_level` - `N` x `MAX_AGE` x `MAX_AGE` array of prevalence(T1D)
#'         compartments by year, age, and age at diagnosis. This is a
#'         more detailed breakdown of `P_level`, which we often refer to as a
#'         'cohort' breakdown. Values are scaled by country population, so units
#'         are persons.
#'   \item `I_flow` - `N` x `MAX_AGE` matrix of incidence flows, by year and age
#'         i.e. transitions from `S` (healthy/susceptible) to `P` (T1D/prevalent).
#'         Values are scaled by country population, so units are persons.
#'   \item `DDx_flow` - `N` x `MAX_AGE` matrix of death at onset flows by year and
#'         age. These are transitions directly from `S` (healthy/susceptible) to
#'         `D` (death). Terminology: this flow was historically called
#'         'death on diagnosis', hence the abbrevation DDx, although this was
#'         abandoned for reasons of accuracy. Values are scaled by country
#'         population, so units are persons.
#'   \item `DT1D_flow` - `N` x `MAX_AGE` matrix of excess deaths due to T1D by
#'         year and age. This is a subset of the flows from `P` (prevalent/T1D)
#'         to `D` over and above background deaths. Values are scaled by country
#'         population, so units are persons.
#'   \item `ghost_level` - `N` x `MAX_AGE` matrix of total ghost population,
#'         in units of persons.
#'   \item `ghost_ddx_level` - `N` x `MAX_AGE` matrix of ghost population due to
#'         deaths at onset, in units of persons.
#'   \item `ghost_hba1c_level` - `N` x `MAX_AGE` matrix of ghost population due to
#'         inadequate care (excess deaths due to T1D), in units of persons.
#'   \item `pop` - `N` x `MAX_AGE` matrix of ghost population
#'   \item `country` - World Bank name of country
#'   \item `time` - Elapsed time for the function to run
#'   \item `hba1c` - mean hba1c level
#'   \item `years` - vector of >=2 consecutive years to run the model for
#'   \item `pop_scale_factor` - population scaling factors used
#'}
#'
#' @param country_wb_name world bank name of country
#' @param hba1c average hba1c to model
#' @param incidence_fn Observed incidence function (excludes deaths on diagnosis)
#' @param bg_mort_fn Background mortality function
#' @param t1d_mort_fn Type-1 diabetes mortality function
#' @param population_fn Population function
#' @param death_on_diagnosis_fn Death on diagnosis rate function
#' @param start_year First year to model
#' @param end_year Last year to model
#' @param draw (integer) number of population trajectories to return
#' @param pop_scale_factor optional population scaling factor matrix
#' @param prev_reference optional prevalence reference level, a point estimate
#' @param prev_reference_year optional prevalence reference year
#' @return structure of class `prevalence` as described above
#' @export
prevalence_and_ghost_pop <- function(
    country_wb_name,
    # hba1c=make_age_function(7.5),
    start_year,
    end_year,
    data_dir,
    smr_scale_factor=1,
    incidence_scale_factor=1,
    data_long = NULL,
    matrices_list = NULL,
    log_timing=FALSE
) {

  # smr_scale_factor=1 ;   incidence_scale_factor=1;data_long = NULL;matrices_list = NULL;log_timing=FALSE; start_year=1960; end_year=2040


  if(log_timing){ sink("log.txt",append=TRUE);cat(paste0(Sys.time()," prevalence_and_ghost_pop() \n") );sink()}

  ptm <- proc.time()
  # function to drop warm up periods
  dwu <- function(X) X[-(1:MAX_AGE),]
  # warm up period to seed prevalence: start model MAX_AGE years earlier
  years <- (start_year - MAX_AGE):end_year

  # Take draws from prevalence posterior distribution by simulating
  # Load rates from input_rates_combined -----------------------------------------------------------------
  # country_wb_name <- "Morocco"
  country_region <- Data_Run_query_return_df('SELECT loc_id,world_bank_name  FROM index_parameters.country_region')
  loc_id <- country_region$loc_id[country_region$world_bank_name==country_wb_name]

  # loc_id      <- get_loc_id(country_wb_name)

  # data_long   <- run_query_df (paste0("SELECT * FROM input_rates_combined  WHERE loc_id = '",loc_id,"'" ) )
  if(is.null(data_long))             {data_long   <- readRDS(paste0(data_dir,"/",loc_id,".Rds"))}
  data_long_default_run   <- readRDS(paste0(strsplit(data_dir,"_mcmc")[[1]][1],"/",loc_id,".Rds"))

  if(is.null(matrices_list))
  {
    matrices_list <- data_long_2_matrices (data_long=data_long,data_long_default_run=data_long_default_run)
    i                 <- matrices_list$i   # mean(i[as.character(2010:2019),19]) *100000
    qB                <- matrices_list$qB
    pop               <- matrices_list$pop
    dDx               <- matrices_list$dDx
    smr_matrix_n      <- matrices_list$smr_matrix_n
    smr_matrix_m      <- matrices_list$smr_matrix_m
    qT1D_percent_n    <- matrices_list$qT1D_percent_n
  }else
  { # Shiny input
    i                 <- matrices_list$i   # mean(i[as.character(2010:2019),19]) *100000
    qB                <- matrices_list$qB
    pop               <- matrices_list$pop
    dDx               <- matrices_list$dDx
    smr_matrix_n      <- matrices_list$smr_matrix_n
    smr_matrix_m      <- matrices_list$smr_matrix_m
    qT1D_percent_n    <- matrices_list$qT1D_percent_n
  }
  # set up parameters
  zero <- matrix_from_function(make_age_function(0), years)
  lever_year_range        <- as.character(config$lever_change_start_at:max(years))  #  only apply lever to years after lever_change_at


  # life expectency , background ---------------------------------------------------------------------
  life_table_background    <- dplyr::select(data_long, year,age,qx =background_mortality_rate)
  life_table_background    <- calculate_ex(life_table_background)

  # make sure t1d mortality ( smr * backgroun_mortality  ) is always <=1 -------
  Get_qT1D_from_smr_matrix <- function(qB, smr_matrix)
  {
    diab_odds  <- qB / (1 - qB) * smr_matrix
    qT1D       <- diab_odds / (1 + diab_odds)
    qT1D
  }
  #----------- life expectancy t1ds -----------------------------------------------------------------------
  Get_life_expectancy_t1d <- function(qB, smr_matrix_m,smr_matrix_n, qT1D_percent_n)
  {
    smr_matrix <- smr_matrix_m * (1-qT1D_percent_n)  +  smr_matrix_n  *    qT1D_percent_n
    smr_matrix_long    <- as.data.frame.table(smr_matrix,stringsAsFactors = F)%>% mutate_all(as.numeric) # when shiny run simulation, apply percent back to data_long
    colnames(smr_matrix_long) <- c("year",  "age",   "smr")

    t1d_mortality_matrix <- Get_qT1D_from_smr_matrix (qB, smr_matrix)

    t1d_mortality_long <- as.data.frame.table(t1d_mortality_matrix,stringsAsFactors = F)%>% mutate_all(as.numeric) # when shiny run simulation, apply percent back to data_long
    colnames(t1d_mortality_long) <- c("year",  "age",   "Value")
    t1d_mortality_long$smr <- smr_matrix_long$smr
    t1d_mortality_long <- t1d_mortality_long[t1d_mortality_long$year >= 1900,]
    t1d_mortality_long <- t1d_mortality_long[with(t1d_mortality_long, order( year,age)), ]

    t1d_mortality_long    <- dplyr::select(t1d_mortality_long, year,age,smr,qx =Value)
    life_table_t1d        <- calculate_ex_lifetime_years_lost(t1d_mortality_long)
    life_table_t1d
  }

  life_table_base_scenario <- Get_life_expectancy_t1d (qB, smr_matrix_m,smr_matrix_n, qT1D_percent_n)

  qT1D_n       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_n)
  qT1D_m       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_m)

  # prev base scenario  --------------------------------------------------------------------
  prev                    <- calculate_prevalence(i        , qB, qT1D_n, qT1D_m,   qT1D_percent_n, dDx ,  years)
  # prev                    <- calculate_prevalence_gpu(i        , qB, qT1D_n, qT1D_m, qT1D_percent_n, dDx ,  years)


  matrices_list_lever1 <- Apply_levers_to_input_matrices (matrices_list, lever=1, lever_year_range=lever_year_range)

  i                 <- matrices_list_lever1$i   # mean(i[as.character(2010:2019),19]) *100000
  dDx               <- matrices_list_lever1$dDx
  smr_matrix_n      <- matrices_list_lever1$smr_matrix_n
  smr_matrix_m      <- matrices_list_lever1$smr_matrix_m
  qT1D_percent_n    <- matrices_list_lever1$qT1D_percent_n

  qT1D_n       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_n)
  qT1D_m       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_m)

  prev_100d               <- calculate_prevalence(i        , qB, qT1D_n, qT1D_m,   qT1D_percent_n, dDx ,  years) # deaths on diagnosis are converted to higer incidence
  # prev lever 2: basic care Insulin, strips and education SMR  median( c(3.7, 4.4)) , 4.05--------------------------------------------------------------------

  matrices_list_lever2 <- Apply_levers_to_input_matrices (matrices_list, lever=2, lever_year_range=lever_year_range)

  i                 <- matrices_list_lever2$i   # mean(i[as.character(2010:2019),19]) *100000
  dDx               <- matrices_list_lever2$dDx
  smr_matrix_n      <- matrices_list_lever2$smr_matrix_n
  smr_matrix_m      <- matrices_list_lever2$smr_matrix_m
  qT1D_percent_n    <- matrices_list_lever2$qT1D_percent_n

  qT1D_n       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_n)
  qT1D_m       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_m)
  prev_100d_basic_care    <- calculate_prevalence(i        , qB, qT1D_n, qT1D_m,   qT1D_percent_n, dDx ,  years) # smr all non-minimal

  life_table_lever_2     <- Get_life_expectancy_t1d (qB, smr_matrix_m,smr_matrix_n, qT1D_percent_n)


  # # prev best care Pumps and FGMs/CGMs SMR median( c(2.2, 2.6)) , median 2.4--------------------------------------------------------------------

  matrices_list_lever3 <- Apply_levers_to_input_matrices (matrices_list, lever=3, lever_year_range=lever_year_range)

  i                 <- matrices_list_lever3$i   # mean(i[as.character(2010:2019),19]) *100000
  dDx               <- matrices_list_lever3$dDx
  smr_matrix_n      <- matrices_list_lever3$smr_matrix_n
  smr_matrix_m      <- matrices_list_lever3$smr_matrix_m
  qT1D_percent_n    <- matrices_list_lever3$qT1D_percent_n

  qT1D_n       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_n)
  qT1D_m       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_m)

  prev_100d_best_care     <- calculate_prevalence(i        , qB, qT1D_n, qT1D_m,   qT1D_percent_n, dDx ,  years) # deaths on diagnosis are converted to higer incidence

  life_table_lever_3     <- Get_life_expectancy_t1d (qB, smr_matrix_m,smr_matrix_n, qT1D_percent_n)


  # counterfactual: ghost pop is diff between prevalence and counterfactual where deaths on diagnosis are converted to incidence, and hba1c is fully controlled (4.3)
  # prev cure care --------------------------------------------------------------------

  matrices_list_lever4 <- Apply_levers_to_input_matrices (matrices_list, lever=4, lever_year_range=lever_year_range)

  i                 <- matrices_list_lever4$i   # mean(i[as.character(2010:2019),19]) *100000
  dDx               <- matrices_list_lever4$dDx
  smr_matrix_n      <- matrices_list_lever4$smr_matrix_n
  smr_matrix_m      <- matrices_list_lever4$smr_matrix_m
  qT1D_percent_n    <- matrices_list_lever4$qT1D_percent_n

  qT1D_n       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_n)
  qT1D_m       <- Get_qT1D_from_smr_matrix (qB, smr_matrix_m)

  prev_100d_cure          <- calculate_prevalence(i        , qB, qT1D_n, qT1D_m,   qT1D_percent_n, dDx ,  years)


  life_table_lever_4     <- Get_life_expectancy_t1d (qB, smr_matrix_m,smr_matrix_n, qT1D_percent_n)

  # calculating years gained --------
  matrices_list_strips_low  <- Apply_smr_to_input_matrices(matrices_list, lever=1, 5.3,6.3,lever_year_range)
  matrices_list_strips_hig <- Apply_smr_to_input_matrices(matrices_list, lever=1, 3.1,3.7,lever_year_range)
  matrices_list_sensor_low  <- Apply_smr_to_input_matrices(matrices_list, lever=1, 4.0,4.7,lever_year_range)
  matrices_list_sensor_hig <- Apply_smr_to_input_matrices(matrices_list, lever=1, 2.2,2.8,lever_year_range)

  life_table_strips_low     <- Get_life_expectancy_t1d (qB, matrices_list_strips_low$smr_matrix_m,matrices_list_strips_low$smr_matrix_n, matrices_list_strips_low$qT1D_percent_n)
  life_table_strips_hig     <- Get_life_expectancy_t1d (qB, matrices_list_strips_hig$smr_matrix_m,matrices_list_strips_hig$smr_matrix_n, matrices_list_strips_hig$qT1D_percent_n)
  life_table_sensor_low     <- Get_life_expectancy_t1d (qB, matrices_list_sensor_low$smr_matrix_m,matrices_list_sensor_low$smr_matrix_n, matrices_list_sensor_low$qT1D_percent_n)
  life_table_sensor_hig     <- Get_life_expectancy_t1d (qB, matrices_list_sensor_hig$smr_matrix_m,matrices_list_sensor_hig$smr_matrix_n, matrices_list_sensor_hig$qT1D_percent_n)

  # write.csv(life_table_sensor_low[life_table_sensor_low$year==2022],paste0("temp/life_table_sensor_low_",country_wb_name,".csv") )
  # write.csv(life_table_sensor_hig[life_table_sensor_hig$year==2022],paste0("temp/life_table_sensor_hig_",country_wb_name,".csv") )

  # ----------------------------------------------------------------------------------------------------------------------------------------
  # translate proportions to population levels
  # P + S + D = pop_scale_factor
  # start simulation from 1800 ----------------------------------------------------

  pop_scale_factor       <- pop / (prev$P + prev$S)   # exclude the dead from population
  pop_scale_factor_100d  <- pop / (prev_100d$P + prev_100d$S)   # exclude the dead from population
  pop_scale_factor_100d_bacare  <- pop / (prev_100d_basic_care$P + prev_100d_basic_care$S)   # exclude the dead from population
  pop_scale_factor_100d_becare  <- pop / (prev_100d_best_care$P + prev_100d_best_care$S)   # exclude the dead from population
  pop_scale_factor_100d_cure    <- pop / (prev_100d_cure$P + prev_100d_cure$S)   # exclude the dead from population

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

  BD_flow         <- pop * qB

  # Calculate missing prevalence  and delta of levers. --------------------------------------------------------------------------------
  ghost_ddx_level   <- (prev_100d$P              - prev$P) * pop_scale_factor #
  ghost_hba1c_level <- (prev_100d_cure$P         - prev_100d$P) * pop_scale_factor #

  ghost_basic_care  <- (prev_100d_basic_care$P   - prev_100d$P) * pop_scale_factor #  delta
  ghost_best_care   <- (prev_100d_best_care$P    - prev_100d_basic_care$P) * pop_scale_factor #
  ghost_cure        <- (prev_100d_cure$P         - prev_100d_best_care$P) * pop_scale_factor #  sum(ghost_hba1c_level["2021",])

  # # Calculate missing prevalence  and delta of levers. --------------------------------------------------------------------------------
  # ghost_ddx_level   <- prev_100d$P *pop_scale_factor_100d             - prev$P* pop_scale_factor   #
  # ghost_hba1c_level <- prev_100d_cure$P   *pop_scale_factor_100d_cure       - prev_100d$P *pop_scale_factor_100d
  #
  # ghost_basic_care  <- prev_100d_basic_care$P *pop_scale_factor_100d_bacare   - prev_100d$P *pop_scale_factor_100d
  # ghost_best_care   <- prev_100d_best_care$P *pop_scale_factor_100d_becare    - prev_100d_basic_care$P *pop_scale_factor_100d_bacare
  # ghost_cure        <- prev_100d_cure$P *pop_scale_factor_100d_cure         - prev_100d_best_care$P *pop_scale_factor_100d_becare

  # ghost population is the sum of these two
  ghost_level <- ghost_hba1c_level + ghost_ddx_level #   sum(ghost_level["2021",])
  print(paste0("ghost onset death: ",round(sum(ghost_ddx_level["2021",]),2),"; ghost early death: ",round(sum(ghost_hba1c_level["2021",]),2) ) )

  # df <- data.frame(x=rownames(ghost_level)
  #                  ,diagnosed           = (rowSums(P_level))
  #                  ,ghost_undiagnosed   = (rowSums(ghost_ddx_level))
  #                  ,ghost_diagnosed     = (rowSums(ghost_hba1c_level))
  #                  ,ghost_basic_care    = (rowSums(ghost_basic_care))
  #                  ,ghost_best_care     = (rowSums(ghost_best_care))
  #                  ,ghost_cure          = (rowSums(ghost_cure))
  #                  )
  # df[df$x>=1960 & df$x<=2020,] %>%
  #   e_charts(x) %>%
  #   e_bar(diagnosed, stack = "grp") %>%
  #   e_bar(ghost_undiagnosed, stack = "grp") %>%
  #   # e_bar(ghost_diagnosed, stack = "grp") %>%
  #   e_bar(ghost_basic_care, stack = "grp") %>%
  #   e_bar(ghost_best_care, stack = "grp") %>%
  #   e_bar(ghost_cure, stack = "grp") %>%
  #   e_title("T1D population",country_wb_name)%>%
  #   e_legend(bottom=0)
  # complications , burden

  dalys <- list()
  dalys$year_age <-  dwu(P_level)

  if(config$run_days_lost) # do not run burden,  days lost same as prevalance----
  {
    P_cohorts_level <- prev      $Pcohorts * array(pop_scale_factor, c(length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[-(1:MAX_AGE),,]
    comp  <- complication_prevalence(years=seq(start_year, end_year),P_cohorts_level,matrices_list$smr_matrix_n, matrices_list$smr_matrix_m,matrices_list$qT1D_percent_n)
    dalys <- calculate_dalys(P_cohorts_level, comp)

  }

  # test  <- apply(P_cohorts_level, c(1, 2), sum) # check if cohort is adds up to prevalence.




  dalys_100d              <- dalys
  dalys_100d_basic_care   <- dalys
  dalys_100d_best_care    <- dalys
  dalys_100d_cure         <- dalys

  if(config$run_days_lost_lever)  # do not run levers for paper stats
  {
    P_cohorts_level <- prev_100d$Pcohorts * array(pop_scale_factor, c(length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[-(1:MAX_AGE),,]
    # comp  <- complication_prevalence(start_year, end_year,P_cohorts_level,smr_matrix_n, smr_matrix_m,qT1D_percent_n)
    dalys_100d <- calculate_dalys(P_cohorts_level, comp) # same complication , as smr did not change

    P_cohorts_level <- prev_100d_basic_care$Pcohorts * array(pop_scale_factor, c(length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[-(1:MAX_AGE),,]
    comp  <- complication_prevalence(years=seq(start_year, end_year),P_cohorts_level,matrices_list_lever2$smr_matrix_n, matrices_list_lever2$smr_matrix_m,matrices_list_lever2$qT1D_percent_n)
    dalys_100d_basic_care <- calculate_dalys(P_cohorts_level, comp)


    P_cohorts_level <- prev_100d_best_care$Pcohorts * array(pop_scale_factor, c(length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[-(1:MAX_AGE),,]
    comp  <- complication_prevalence(years=seq(start_year, end_year),P_cohorts_level,matrices_list_lever3$smr_matrix_n, matrices_list_lever3$smr_matrix_m,matrices_list_lever3$qT1D_percent_n)
    dalys_100d_best_care <- calculate_dalys(P_cohorts_level, comp)

    P_cohorts_level <- prev_100d_cure$Pcohorts * array(pop_scale_factor, c(length(years),MAX_AGE,MAX_AGE))
    P_cohorts_level <- P_cohorts_level[-(1:MAX_AGE),,]
    comp  <- complication_prevalence(years=seq(start_year, end_year),P_cohorts_level,matrices_list_lever4$smr_matrix_n, matrices_list_lever4$smr_matrix_m,matrices_list_lever4$qT1D_percent_n)
    dalys_100d_cure <- calculate_dalys(P_cohorts_level, comp)

  }

  # merge all outputs ------------------------------------------------------------------------------
  prev_merge <-                     cbind(Value_type="Ann. background mortality" , Year = rownames(dwu(pop))      , as.data.frame(dwu(BD_flow) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. background population", Year = rownames(dwu(pop))      , as.data.frame(dwu(pop) )))

  prev_merge <- rbind( prev_merge,  cbind(Value_type="Prevalence"                , Year = rownames(dwu(pop))      , as.data.frame(dwu(P_level))))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Incidence (1 base)"        , Year = rownames(dwu(pop))      , as.data.frame(dwu(I_flow))))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Incidence (2 diagnosis)"   , Year = rownames(dwu(pop))      , as.data.frame(dwu(I_flow_diagnosis))))

  # not needed, same value,
  # prev_merge <- rbind( prev_merge,  cbind(Value_type="Incidence (3 basic care)"   , Year = rownames(dwu(pop))      , as.data.frame(dwu(I_flow_basic_care))))
  # prev_merge <- rbind( prev_merge,  cbind(Value_type="Incidence (4 best care)"   , Year = rownames(dwu(pop))      , as.data.frame(dwu(I_flow_best_care))))
  # prev_merge <- rbind( prev_merge,  cbind(Value_type="Incidence (5 cure)"   , Year = rownames(dwu(pop))      , as.data.frame(dwu(I_flow_cure))))


  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts"                    , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_level)) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts (onset death)"      , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_ddx_level) ) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts (early death)"      , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_hba1c_level) ) ))

  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts (delta basic care)" , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_basic_care) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts (delta best care)"  , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_best_care) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ghosts (delta cure)"       , Year = rownames(dwu(pop))      , as.data.frame(dwu(ghost_cure) )))

  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. onset deaths"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DDx_flow) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. early deaths"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DT1D_flow) )))

  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. early deaths (2 diagnosis)"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DT1D_flow_100d) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. early deaths (3 basic care)"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DT1D_flow_basic_care) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. early deaths (4 best care)"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DT1D_flow_best_care) )))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. early deaths (5 cure)"         , Year = rownames(dwu(pop))      , as.data.frame(dwu(DT1D_flow_cure) )))

  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. days lost (1 base)"       , Year = rownames(dwu(pop))      , as.data.frame(dalys$year_age) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. days lost (2 diagnosis)"  , Year = rownames(dwu(pop))      , as.data.frame(dalys_100d$year_age) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. days lost (3 basic care)" , Year = rownames(dwu(pop))      , as.data.frame(dalys_100d_basic_care$year_age) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. days lost (4 best care)"  , Year = rownames(dwu(pop))      , as.data.frame(dalys_100d_best_care$year_age) ))
  prev_merge <- rbind( prev_merge,  cbind(Value_type="Ann. days lost (5 cure)"       , Year = rownames(dwu(pop))      , as.data.frame(dalys_100d_cure$year_age) ))

  prev_merge_long      <- gather(prev_merge ,key = "Age", value = "Value",-Value_type,-Year)
  prev_merge_long$Year <- as.numeric(prev_merge_long$Year)
  prev_merge_long$Age  <- as.numeric(prev_merge_long$Age)
  prev_merge_long      <- cbind(Country= country_wb_name,Type="Country",prev_merge_long)

  # saveRDS(prev_merge_long,"temp/test.Rds")
  prev_merge_wide   <- spread(prev_merge_long, Value_type, Value )

  prev_merge_wide$`Life expectency (1 background)`      <- life_table_background$ex[life_table_background$year>=1960]

  # for t1d base life expectancy, include diagnosis rate effect in the begining
  prev_merge_wide$`Life expectency (2 t1d base)`        <- life_table_base_scenario$ex[life_table_base_scenario$year>=1960]
  diagnosis_rate <- (prev_merge_wide$`Incidence (1 base)`+0.001) / (prev_merge_wide$`Ann. onset deaths`  + prev_merge_wide$`Incidence (1 base)`+0.001)
  prev_merge_wide$`Life expectency (2 t1d base)`        <- prev_merge_wide$`Life expectency (2 t1d base)` * diagnosis_rate +  (1-diagnosis_rate)*0.5

  prev_merge_wide$`Life expectency (3 t1d diagnosis)`   <- life_table_base_scenario$ex[life_table_base_scenario$year>=1960]
  prev_merge_wide$`Life expectency (4 t1d basic care)`  <- life_table_lever_2$ex[life_table_lever_2$year>=1960]
  prev_merge_wide$`Life expectency (5 t1d best care)`   <- life_table_lever_3$ex[life_table_lever_3$year>=1960]
  prev_merge_wide$`Life expectency (6 t1d cure)`        <- life_table_lever_4$ex[life_table_background$year>=1960]

  prev_merge_wide$`Lifetime years lost (2 t1d base) (complication)`        <- life_table_base_scenario$ex_complication[life_table_base_scenario$year>=1960] *  prev_merge_wide$`Life expectency (2 t1d base)`/ life_table_base_scenario$ex[life_table_base_scenario$year>=1960]
  prev_merge_wide$`Lifetime years lost (3 t1d diagnosis) (complication)`   <- life_table_base_scenario$ex_complication[life_table_base_scenario$year>=1960]
  prev_merge_wide$`Lifetime years lost (4 t1d basic care) (complication)`  <- life_table_lever_2$ex_complication[life_table_lever_2$year>=1960]
  prev_merge_wide$`Lifetime years lost (5 t1d best care) (complication)`   <- life_table_lever_3$ex_complication[life_table_lever_3$year>=1960]
  prev_merge_wide$`Lifetime years lost (6 t1d cure) (complication)`        <- life_table_lever_4$ex_complication[life_table_background$year>=1960]

  prev_merge_wide$`Lifetime years lost (2 t1d base) (treatment)`        <- life_table_base_scenario$ex_treatment[life_table_base_scenario$year>=1960] *  prev_merge_wide$`Life expectency (2 t1d base)`/ life_table_base_scenario$ex[life_table_base_scenario$year>=1960]
  prev_merge_wide$`Lifetime years lost (3 t1d diagnosis) (treatment)`   <- life_table_base_scenario$ex_treatment[life_table_base_scenario$year>=1960]
  prev_merge_wide$`Lifetime years lost (4 t1d basic care) (treatment)`  <- life_table_lever_2$ex_treatment[life_table_lever_2$year>=1960]
  prev_merge_wide$`Lifetime years lost (5 t1d best care) (treatment)`   <- life_table_lever_3$ex_treatment[life_table_lever_3$year>=1960]
  prev_merge_wide$`Lifetime years lost (6 t1d cure) (treatment)`        <- life_table_lever_4$ex_treatment[life_table_background$year>=1960]


  prev_merge_wide$`Life expectency (strip low) `        <- life_table_strips_low$ex[life_table_strips_low$year>=1960]
  prev_merge_wide$`Life expectency (strip hig) `        <- life_table_strips_hig$ex[life_table_strips_hig$year>=1960]
  prev_merge_wide$`Lifetime years lost (strip low) `        <- (life_table_strips_low$ex_complication + life_table_strips_low$ex_treatment)  [life_table_strips_low$year>=1960]
  prev_merge_wide$`Lifetime years lost (strip hig) `        <- (life_table_strips_hig$ex_complication + life_table_strips_hig$ex_treatment)  [life_table_strips_hig$year>=1960]


  prev_merge_wide$`Life expectency (sensor low) `        <- life_table_sensor_low$ex[life_table_sensor_low$year>=1960]
  prev_merge_wide$`Life expectency (sensor hig) `        <- life_table_sensor_hig$ex[life_table_sensor_hig$year>=1960]
  prev_merge_wide$`Lifetime years lost (sensor low) `        <- (life_table_sensor_low$ex_complication + life_table_sensor_low$ex_treatment)  [life_table_sensor_low$year>=1960]
  prev_merge_wide$`Lifetime years lost (sensor hig) `        <- (life_table_sensor_hig$ex_complication + life_table_sensor_hig$ex_treatment)  [life_table_sensor_hig$year>=1960]

  prev_merge_wide$`% Odds living to`  <- prev_merge_wide$Prevalence / (prev_merge_wide$Prevalence + prev_merge_wide$Ghosts) * 100
  prev_merge_wide$`% Odds living to`[is.na(prev_merge_wide$`% Odds living to`)] <- 0

  JDRF_calculations <- extract_for_purpose(prev_merge_wide,0,99,2022)
  days_ <- JDRF_calculations$days_1
  prev_merge_wide   <- prev_merge_wide %>% dplyr::inner_join(dplyr::select(days_,Country,Year,`1 in x families` = `1 in x families`
                                                                           # ,`Lifetime years lost (2 t1d base)`=lifetime_years_lost_complication_and_treatment
                                                                           # ,`Lifetime years lost (2 t1d base) (complication)`=lifetime_years_lost_complication
                                                                           # ,`Lifetime years lost (2 t1d base) (treatment)`   =lifetime_years_lost_treatment
                                                                           ), by=c("Country","Year"))


  # days_ <- JDRF_calculations$days_2
  # prev_merge_wide   <- prev_merge_wide %>% dplyr::inner_join(dplyr::select(days_,Country,Year
  #                                                                          ,`Lifetime years lost (3 t1d diagnosis)`=lifetime_years_lost_complication_and_treatment
  #                                                                          ,`Lifetime years lost (3 t1d diagnosis) (complication)`=lifetime_years_lost_complication
  #                                                                          ,`Lifetime years lost (3 t1d diagnosis) (treatment)`   =lifetime_years_lost_treatment
  #                                                                          ), by=c("Country","Year"))
  #
  #
  # days_ <- JDRF_calculations$days_3
  # prev_merge_wide   <- prev_merge_wide %>% dplyr::inner_join(dplyr::select(days_,Country,Year
  #                                                                          ,`Lifetime years lost (4 t1d basic care)`=lifetime_years_lost_complication_and_treatment
  #                                                                          ,`Lifetime years lost (4 t1d basic care) (complication)`=lifetime_years_lost_complication
  #                                                                          ,`Lifetime years lost (4 t1d basic care) (treatment)`   =lifetime_years_lost_treatment
  #                                                                          ), by=c("Country","Year"))
  #
  # days_ <- JDRF_calculations$days_4
  # prev_merge_wide   <- prev_merge_wide %>% dplyr::inner_join(dplyr::select(days_,Country,Year
  #                                                                          ,`Lifetime years lost (5 t1d best care)`=lifetime_years_lost_complication_and_treatment
  #                                                                          ,`Lifetime years lost (5 t1d best care) (complication)`=lifetime_years_lost_complication
  #                                                                          ,`Lifetime years lost (5 t1d best care) (treatment)`   =lifetime_years_lost_treatment
  #                                                                          ), by=c("Country","Year"))
  #
  # days_ <- JDRF_calculations$days_5
  # prev_merge_wide   <- prev_merge_wide %>% dplyr::inner_join(dplyr::select(days_,Country,Year
  #                                                                          ,`Lifetime years lost (6 t1d cure)`=lifetime_years_lost_complication_and_treatment
  #                                                                          ,`Lifetime years lost (6 t1d cure) (complication)`=lifetime_years_lost_complication
  #                                                                          ,`Lifetime years lost (6 t1d cure) (treatment)`   =lifetime_years_lost_treatment
  #                                                                          ), by=c("Country","Year"))

  prev_merge_wide <- prev_merge_wide %>% mutate_if(is.numeric, round, digits=2)





  # if(FALSE)
  # {
  #   prev_merge_wide_old <- read_parquet( "C:/DropboxT1D/reruns_scenario_2/rerun_0.4.15_0/AFG.binary")
  #   prev_merge_wide_old2 <- read_parquet( "C:/DropboxT1D/reruns_scenario_2/rerun_0.4.15.lever2023_0/AFG.binary")
  # }

  # for(i in 1:48)
  # {
  #   print(i)
  #  print( sum((prev_merge_wide_old!=prev_merge_wide_old2)[,i]) )
  # }


  time_taken <- proc.time() - ptm

  structure(with(prev,
    list(
      prev_merge_wide=prev_merge_wide
    )),
    class='prevalence')
}
