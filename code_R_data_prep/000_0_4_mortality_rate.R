
mortality_rate <- function(country,random_data_ci=FALSE)
{  # random_data_ci= TRUE

  if(FALSE)
  {


    # hba1c.csv ------------------------------------------------------------------------------

    hba1c <- read_excel('data_internal/country_parameters.xlsx', 'country_hba1c') %>%
      dplyr::select(-jdrf_region_name, -world_bank_income) %>%
      pivot_longer(
        -world_bank_name,
        names_to='year', names_transform   =list(year~as.integer(.)),
        values_to='hba1c', values_transform=list(hba1c~as.numeric(.))) %>%
      inner_join(country, by='world_bank_name') %>%
      transmute(
        loc_id,
        year,
        hba1c,
        smr = exp(-1.5274 + 0.3545 * hba1c)
      )
    # write_csv(hba1c, 'hba1c.csv')



    #classical_model_smr -------------------------------------------------------------------------------------------------------------

    classical_model_smr <- read.csv('data_mortality/classical_model_smr.csv', stringsAsFactors = F)
    classical_model_smr <- dplyr::select(classical_model_smr, -iso3c,-world_bank_name,-united_nations_name)
    classical_model_smr <-  gather(classical_model_smr, key = "year", value = "smr", -loc_id)
    classical_model_smr$year <- as.numeric(gsub('X','', classical_model_smr$year))
    classical_model_smr <- classical_model_smr[!is.na(classical_model_smr$smr),]
    classical_model_smr$age <- 1
    colnames(classical_model_smr)[colnames(classical_model_smr)=="smr"] <- "value"
    classical_model_smr <- imputation_year_age(classical_model_smr)

  }
  # machine learning model smr -------------------------------------------------------------------------------------------------------------
  # version_no <- "0.4.10.smr_age_cruve_flat_average"
  model_smr <- readRDS('data_mortality/machine_learning_model_smr.Rds')
  # model_smr <- readRDS('data_mortality/machine_learning_model_smr_flat_age.Rds')

  if(random_data_ci)
  {
    draw_level <- c("world_bank_name","year","standard_of_care")
    # draw_level <- c("world_bank_name","standard_of_care")

    model_smr_ci <- read_parquet('data_mortality/machine_learning_model_smr_ci.binary')
    # model_smr_ci$smr_predicted <- random_gen(mean=model_smr_ci$value_median, smr_ci_lower= model_smr_ci$lower,smr_ci_upper=model_smr_ci$upper )

    df_ran_draw_level <- setDT(model_smr_ci)[,list(value_median=median(value_median)
                                                         ,lower=median(lower)
                                                         ,upper=median(upper)
                                                         ),by=draw_level]

    df_ran_draw_level$smr_predicted <- random_gen(mean=df_ran_draw_level$value_median, smr_ci_lower= df_ran_draw_level$lower,smr_ci_upper=df_ran_draw_level$upper )
    df_ran_draw_level$deviation <- (df_ran_draw_level$smr_predicted-(df_ran_draw_level$upper+df_ran_draw_level$lower)/2 )/ (df_ran_draw_level$upper-df_ran_draw_level$lower)
    df_ran_draw_level <- dplyr::select(df_ran_draw_level,c(draw_level,"deviation"))


    model_smr_ci <- model_smr_ci %>% dplyr::left_join(df_ran_draw_level,by = draw_level)
    model_smr_ci$smr_predicted <-model_smr_ci$deviation *  (model_smr_ci$upper-model_smr_ci$lower) + (model_smr_ci$upper+model_smr_ci$lower)/2

    model_smr_ci<- setDF(model_smr_ci)[,colnames(model_smr)]
    model_smr <- model_smr_ci
  }

  model_smr$smr_predicted[model_smr$smr_predicted<=0] <- 0

  model_smr <- model_smr%>%
    inner_join(dplyr::select(country,world_bank_name, loc_id ), by='world_bank_name')

  colnames(model_smr)[colnames(model_smr)=="smr_predicted"] <- "value"
  colnames(model_smr)[colnames(model_smr)=="age_start"]     <- "age"
  model_smr$world_bank_name <- NULL

  model_smr_minimal_care <- model_smr[model_smr$standard_of_care=="Minimal Care",]
  model_smr_minimal_care$standard_of_care <- NULL

  # model_smr_minimal_care <- model_smr_minimal_care[model_smr_minimal_care$loc_id==250,]

  model_smr_minimal_care <- imputation_year_age(model_smr_minimal_care)

  model_smr_non_minimal_care <- model_smr[!model_smr$standard_of_care=="Minimal Care",]
  model_smr_non_minimal_care$standard_of_care <- NULL
  model_smr_non_minimal_care <- imputation_year_age(model_smr_non_minimal_care)


  machine_learning_model_smr <- model_smr_minimal_care
  machine_learning_model_smr$value_smr_minimal_care     <- machine_learning_model_smr$value
  machine_learning_model_smr$value_smr_non_minimal_care <- model_smr_non_minimal_care$value
  machine_learning_model_smr$value <- NULL




  # add value_percent_non_minimal_care  smr -------------------------------------------------------------------------------------------------------------

  Standards_of_care_mortality <- readxl::read_xlsx('data_mortality/Standards of care mortality 5.xlsx',sheet=2)
  Standards_of_care_mortality <- Standards_of_care_mortality[!is.na(Standards_of_care_mortality$year),]

  Standards_of_care_mortality$age <- 0
  Standards_of_care_mortality$value <- Standards_of_care_mortality$`non minimal care`
  Standards_of_care_mortality <- dplyr::select(Standards_of_care_mortality, year,age,value,loc_id=`Income class`)

  Standards_of_care_mortality <- imputation_year_age(Standards_of_care_mortality)

  Standards_of_care_mortality$income_class <- Standards_of_care_mortality$loc_id
  Standards_of_care_mortality$loc_id <- NULL
  Standards_of_care_mortality$value_percent_non_minimal_care <- Standards_of_care_mortality$value
  Standards_of_care_mortality$value <- NULL

  machine_learning_model_smr<- machine_learning_model_smr %>%
    inner_join(dplyr::select(country, loc_id,income_class= world_bank_classification ), by='loc_id') %>%
    inner_join(Standards_of_care_mortality, by=c('year','age','income_class') )

  return(list(machine_learning_model_smr=machine_learning_model_smr,Standards_of_care_mortality=Standards_of_care_mortality) )
}
