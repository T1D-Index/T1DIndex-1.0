
helathy_years_restored <- function(index_data)
{

  data_export_life_expec <- setDT( index_data)[,list(
    true_incidence_age_10= sum(true_incidence_age_10),
    `Life expectancy from age 10`                  = sum(`Life expectency (1 background)`[Year==2022& Age==10])+10
    ,`Healthy years lost when developing T1D`     = ((`Lifetime years lost (2 t1d base)`       + `Life expectency (1 background)` -`Life expectency (2 t1d base)`)[Year==2022 & Age==10] )

    ,`Healthy years restored with onset diagnosis` = ifelse(`Ann. onset deaths`[Year==2022 & Age==10]!=0, round( ((( `Life expectency (3 t1d diagnosis)` - `Lifetime years lost (3 t1d diagnosis)`  )[Year==2022 & Age==10] )
                                                                  - ((`Life expectency (2 t1d base)`     - `Lifetime years lost (2 t1d base)`    )[Year==2022 & Age==10] ))
                                                                 ,1),0
    )


    ,`Perctange of children with T1D undiagnosed` = paste0(round( sum(`Ann. onset deaths`[Year==2022& Age<=24]) / sum(`Incidence (2 diagnosis)`[Year==2022& Age<=24])*100,1),"%")
    ,`Uncelebrated birthdays this year` = sum(`Ghosts (onset death)`[Year==2022] )  # Missing prevalence
    ,`Educational poster cost in dollars` = 1 # for now.

    , `Healthy years restored with insulin and strips` =   round( (((`Life expectency (4 t1d basic care)`- `Lifetime years lost (4 t1d basic care)`  )[Year==2022 & Age==10] )
                                                                   - ((`Life expectency (3 t1d diagnosis)`- `Lifetime years lost (3 t1d diagnosis)`  )[Year==2022 & Age==10] ))
                                                                  ,1)

    ,`Lives lost without insulin and strips` = sum(`Ghosts (delta basic care)`[Year==2022])  # Missing prevalence due to device.
    ,`Uncelebrated birthdays this year2`     = sum(`Ghosts (onset death)`[Year==2022] )  # Missing prevalence

    # ,`Healthy hours restored with each test strip` = (round( ( `Life expectency (4 t1d basic care)` - `Lifetime years lost (4 t1d basic care)`)[Year==2022 & Age==10] -
    #                                                            (`Life expectency (3 t1d diagnosis)` - `Lifetime years lost (3 t1d diagnosis)`)[Year==2022 & Age==10]
    #                                                          ,1) * 365 * 24 )/ ((`Life expectency (4 t1d basic care)`[Year==2022 & Age==10] )*365*3)

    ,`Healthy hours restored with each test strip` = max(
                                                      (( round( (((`Life expectency (strip hig) `- `Lifetime years lost (strip hig) `  )[Year==2022 & Age==10] )
                                                               - ((`Life expectency (strip low) `- `Lifetime years lost (strip low) `  )[Year==2022 & Age==10] ))
                                                              ,1) * 365 * 24 )/ ((`Life expectency (strip hig) `[Year==2022 & Age==10] )*365*3)),
                                                      ((round( ( `Life expectency (4 t1d basic care)` - `Lifetime years lost (4 t1d basic care)`)[Year==2022 & Age==10] -
                                                                 (`Life expectency (3 t1d diagnosis)` - `Lifetime years lost (3 t1d diagnosis)`)[Year==2022 & Age==10]
                                                               ,1) * 365 * 24 )/ ((`Life expectency (4 t1d basic care)`[Year==2022 & Age==10] )*365*3))
                                                      )

    ,`Healthy years restored with device uptake` = round( (((`Life expectency (5 t1d best care)`- `Lifetime years lost (5 t1d best care)`  )[Year==2022 & Age==10] )
                                                             - ((`Life expectency (4 t1d basic care)`- `Lifetime years lost (4 t1d basic care)`  )[Year==2022 & Age==10] ))
                                                            ,1)

    # ,`Days per year managing T1D without devices` = 12 # constant from WHO
    ,`Days per year managing T1D without devices` = 18 # constant from WHO

    ,`Days per year restored through devices`  =  (round( (((`Life expectency (sensor hig) `- `Lifetime years lost (sensor hig) `  )[Year==2022 & Age==10] )
                                                           - ((`Life expectency (sensor low) `- `Lifetime years lost (sensor low) `  )[Year==2022 & Age==10] ))
                                                          ,1) * 365 * 24 )/ ((`Life expectency (sensor hig) `[Year==2022 & Age==10] )*12*2)

    #  calculated as “Healthy hours per sensor”

  ),by=c('Country')]

  data_export_life_expec
}

extract_for_purpose_v2 <- function(Country,include_gbd)
{

  # Country <- "" ; include_gbd=TRUE
  # Country <- "Lao People's Democratic Republic" ; include_gbd=TRUE

  filter <-  paste0('   ')

  if(Country !="")
  {
    filter <-  paste0('AND (   "Country"  = \'',gsub("\'","''", Country) ,'\')  ')
  }


  query <- paste0( 'SELECT *
                      FROM main_0_4_15_paper_median
                       WHERE 1=1
                      AND age_bracket = \'00_99\'
                      AND "Year" >=2000 AND "Year" <= 2022
                      ',filter,'
                   ORDER BY  "Country" ASC '
  )
  index_data <- Data_Run_query_return_df(query)

  query <- paste0( 'SELECT *
                       FROM main_0_4_15
                       WHERE 1=1
                      ',filter,'
                       AND "Age" = 10
                      AND "Year" >=2000 AND "Year" <= 2022
                       ORDER BY  "Country" ASC '
  )
  main_0_4_15 <- Data_Run_query_return_df(query)
  main_0_4_15 <- main_0_4_15[main_0_4_15$Country %in% index_data$Country,]
  main_0_4_15$true_incidence_age_10 <- main_0_4_15$`Incidence (1 base)`

  name_list_replace <-  c("true_incidence_age_10","Life expectency (3 t1d diagnosis)",                     "Life expectency (4 t1d basic care)"
                          ,"Life expectency (5 t1d best care)",                     "Life expectency (6 t1d cure)",                          "Lifetime years lost (2 t1d base) (complication)"
                          ,"Lifetime years lost (3 t1d diagnosis) (complication)",  "Lifetime years lost (4 t1d basic care) (complication)", "Lifetime years lost (5 t1d best care) (complication)"
                          ,"Lifetime years lost (6 t1d cure) (complication)",       "Lifetime years lost (2 t1d base) (treatment)",          "Lifetime years lost (3 t1d diagnosis) (treatment)"
                          ,"Lifetime years lost (4 t1d basic care) (treatment)",    "Lifetime years lost (5 t1d best care) (treatment)",     "Lifetime years lost (6 t1d cure) (treatment)"
                          ,"Life expectency (strip low) ",                          "Life expectency (strip hig) ",                          "Lifetime years lost (strip low) "
                          ,"Lifetime years lost (strip hig) ",                      "Life expectency (sensor low) ",                         "Life expectency (sensor hig) "
                          ,"Lifetime years lost (sensor low) ",                     "Lifetime years lost (sensor hig) ",                     "1 in x families"
                          ,"Ann. days lost (1 base)", "Ann. days lost (2 diagnosis)", "Ann. days lost (3 basic care)" ,"Ann. days lost (4 best care)", "Ann. days lost (5 cure)")
  index_data <- index_data[, setdiff(colnames(index_data), name_list_replace)]


  index_data <- index_data %>% dplyr::inner_join(main_0_4_15[,c("Country" ,"Year","Age",name_list_replace) ],by=c("Country" ,"Year","Age") )




  countries  <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country;")
  countries$Alpha.2.code[countries$world_bank_name=="Curaçao"] <- "CW"


  index_data_1 <- index_data[index_data$Country %in% countries$world_bank_name,]
  index_data_2 <- index_data[!index_data$Country %in% countries$world_bank_name,]


  index_data <- rbind( index_data_2,index_data_1)




  #---------------------------------------------------------------------------------------
  index_data$`Lifetime years lost (2 t1d base)`       <- index_data$`Lifetime years lost (2 t1d base) (complication)` + index_data$`Lifetime years lost (2 t1d base) (treatment)`
  index_data$`Lifetime years lost (3 t1d diagnosis)`  <- index_data$`Lifetime years lost (3 t1d diagnosis) (complication)` + index_data$`Lifetime years lost (3 t1d diagnosis) (treatment)`
  index_data$`Lifetime years lost (4 t1d basic care)` <- index_data$`Lifetime years lost (4 t1d basic care) (complication)` + index_data$`Lifetime years lost (4 t1d basic care) (treatment)`
  index_data$`Lifetime years lost (5 t1d best care)`  <- index_data$`Lifetime years lost (5 t1d best care) (complication)` + index_data$`Lifetime years lost (5 t1d best care) (treatment)`
  index_data$`Lifetime years lost (6 t1d cure)`       <- index_data$`Lifetime years lost (6 t1d cure) (complication)` + index_data$`Lifetime years lost (6 t1d cure) (treatment)`


  year_at <- 2022

  data_export_final <- data.frame()

  data_export <- setDT( index_data)[,list(
    `People Living with T1D`                          = sum(Prevalence[Year==year_at])
    ,`People who should still be alive today`         = sum(Ghosts[Year==year_at])
    ,`Healthy years of life lost per person`          = sum((`Lifetime years lost (2 t1d base)` + `Life expectency (1 background)` -`Life expectency (2 t1d base)`)[Year==year_at & Age==10] )

    ,`Healthy years of life lost to treatment`             = `Lifetime years lost (2 t1d base) (treatment)`[Year==year_at & Age==10]
    ,`Healthy years of life lost to treatment (Peers)`     = `Lifetime years lost (2 t1d base) (treatment)`[Year==year_at & Age==10]
    ,`Healthy years of life lost to complications`         = `Lifetime years lost (2 t1d base) (complication)`[Year==year_at & Age==10]
    ,`Healthy years of life lost to complications (Peers)` = `Lifetime years lost (2 t1d base) (complication)`[Year==year_at & Age==10]
    , `Healthy years of life lost to shorter life`         = `Life expectency (1 background)`[Year==year_at & Age==10] - `Life expectency (2 t1d base)`[Year==year_at & Age==10]
    , `Healthy years of life lost to shorter life (Peers)` = `Life expectency (1 background)`[Year==year_at & Age==10] - `Life expectency (2 t1d base)`[Year==year_at & Age==10]

    ,`Healthy years of life lost to cost of care`            = "TBD"
    ,`Healthy years of life lost to cost of care (Peers)`    = "TBD"
    ,`Healthy years of life lost to mental health`           = "TBD"
    ,`Healthy years of life lost to mental health (Peers)`   = "TBD"
    ,`Healthy years of life lost to quality of life`         = "TBD"
    ,`Healthy years of life lost to quality of life (Peers)` = "TBD"

    ,`Healthy years stolen`                            = sum((`Lifetime years lost (2 t1d base)` + `Life expectency (1 background)` -`Life expectency (2 t1d base)`)[Year==year_at & Age==10] )
    ,`Healthy years stolen (Peers)`                    = sum((`Lifetime years lost (2 t1d base)` + `Life expectency (1 background)` -`Life expectency (2 t1d base)`)[Year==year_at & Age==10] )
    ,`Healthy years remaining`                         = (`Life expectency (2 t1d base)` - `Lifetime years lost (2 t1d base)`)[Year==year_at & Age==10]+ 10
    ,`Healthy years remaining (Peers)`                 = (`Life expectency (2 t1d base)` - `Lifetime years lost (2 t1d base)`)[Year==year_at & Age==10]+ 10

    ,`Percentage of families impacted by T1D in 1990` = paste0(round(1/`1 in x families`[Year==1990 & Age==10]*100,2),'%')
    ,`Percentage of families impacted by T1D in 2022` = paste0(round(1/`1 in x families`[Year==year_at & Age==10]*100,2),'%')
    ,`Percentage of families impacted by T1D in 2040` = paste0(round(1/`1 in x families`[Year==2040 & Age==10]*100,2),'%')

  ),by=c('Country')]


  data_export  <-data_export %>% left_join(select(countries,Country=world_bank_name,Country_code=Alpha.2.code ,wd_income_category))


  data_export[, `Healthy years of life lost to treatment (Peers)`:=quantile(`Healthy years of life lost to treatment`,c(0.20)), by="wd_income_category"]
  data_export[, `Healthy years of life lost to complications (Peers)`:=quantile(`Healthy years of life lost to complications`,c(0.20)), by="wd_income_category"]
  data_export[, `Healthy years of life lost to shorter life (Peers)`:=quantile(`Healthy years of life lost to shorter life`,c(0.20)), by="wd_income_category"]
  data_export[, `Healthy years stolen (Peers)`:=quantile(`Healthy years stolen`,c(0.20)), by="wd_income_category"]
  data_export[, `Healthy years remaining (Peers)`:=quantile(`Healthy years remaining (Peers)`,c(0.80)), by="wd_income_category"]

  data_export$wd_income_category <- NULL

  data_export_final <- data_export
  #T1D growth from 1990 ---------------------------------------------------------------------------------------
  compute_compound_increase_rate <- function(value_start,value_end, period)
  {
    rate <- round(( ( (value_end-value_start)/value_start +1 ) ^(1/period)-1 ) * 100,2)
    if(period==0)
    {  rate <- 0 }
    rate
  }
  data_export_t1d_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2022)
  {
    code_temp <- paste0(code_temp, "\n,`T1D growth from 2000-",year,"` = paste0(round(sum(Prevalence[Year==",year,"]) /  sum(Prevalence[Year==",2000,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`T1D growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(Prevalence[Year==",2000,"]),sum(Prevalence[Year==",year,"]), year-2000 ),'%')")
  }

  code_text <- paste0( " data_export_t1d_growth <- setDT( index_data)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('Country')] ")

  eval(parse(text = code_text))

  #Population growth from 1990 ---------------------------------------------------------------------------------------

  data_export_population_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2022)
  {
    code_temp <- paste0(code_temp, "\n,`Population growth from 2000-",year,"` = paste0(round(sum(`Ann. background population`[Year==",year,"]) /  sum(`Ann. background population`[Year==",2001-1,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`Population growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(`Ann. background population`[Year==",2000,"]),sum(`Ann. background population`[Year==",year,"]), year-2000 ),'%')")

  }

  code_text <- paste0( " data_export_population_growth <- setDT( index_data)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('Country')] ")

  eval(parse(text = code_text))


  data_export_final  <-data_export_final %>% left_join(data_export_t1d_growth)
  data_export_final  <-data_export_final %>% left_join(data_export_population_growth)



if(include_gbd)
{

  #HIV T2D growth from 1990 ---------------------------------------------------------------------------------------
  gbd_data <- read.csv("code_quick_job_scripts/IHME-GBD_2019_DATA-959a6ce1-1.csv",stringsAsFactors = F)

  gbd_data$location_name[gbd_data$location_name=="Taiwan (Province of China)"] <-"China, Taiwan Province of China"
  gbd_data$location_name[gbd_data$location_name=="Democratic People's Republic of Korea"] <-"Dem. People's Republic of Korea"
  gbd_data$location_name[gbd_data$location_name=="Palestine"] <-"State of Palestine"
  gbd_data$location_name[gbd_data$location_name=="Micronesia (Federated States of)"] <-"Micronesia (Fed. States of)"
  gbd_data$location_name[gbd_data$location_name=="Dominica"] <-"Dominican Republic"

  gbd_data$Year <- gbd_data$year
  gbd_data$year <- NULL

  # unique(gbd_data$location_name)[!unique(gbd_data$location_name) %in% countries$united_nations_name]

  gbd_data_diabetes <- gbd_data[gbd_data$cause_name=="Diabetes mellitus type 2"
                                & gbd_data$metric_name=="Number" ,]

  data_export_gbd_t2d_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2019)
  {
    code_temp <- paste0(code_temp, "\n,`T2D growth from 2000-",year,"` = paste0(round(sum(val[Year==",year,"]) /  sum(val[Year==",2001-1,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`T2D growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(val[Year==",2000,"]),sum(val[Year==",year,"]), year-2000 ),'%')")
  }

  code_text <- paste0( " data_export_gbd_t2d_growth <- setDT( gbd_data_diabetes)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('location_name')] ")

  eval(parse(text = code_text))
  # data_export_gbd_t2d_growth$`T2D growth from 1990-1990` <- data_export_gbd_t2d_growth$`T2D growth from 1990-1991`
  data_export_gbd_t2d_growth$`T2D growth from 2000-2020` <- data_export_gbd_t2d_growth$`T2D growth from 2000-2019`
  data_export_gbd_t2d_growth$`T2D growth from 2000-2021` <- data_export_gbd_t2d_growth$`T2D growth from 2000-2019`
  data_export_gbd_t2d_growth$`T2D growth from 2000-2022` <- data_export_gbd_t2d_growth$`T2D growth from 2000-2019`


  data_export_gbd_t2d_growth  <-data_export_gbd_t2d_growth %>% left_join(select(countries,Country=world_bank_name,location_name=united_nations_name))
  data_export_gbd_t2d_growth$location_name <- NULL

  data_export_final  <-data_export_final %>% left_join(data_export_gbd_t2d_growth)

  #-------------------------------------------------------------------------------------------------------------------------
  gbd_data_hiv <- gbd_data[gbd_data$cause_name=="HIV/AIDS"& gbd_data$metric_name=="Number" ,]
  data_export_gbd_hiv_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2019)
  {
    code_temp <- paste0(code_temp, "\n,`HIV growth from 2000-",year,"` = paste0(round(sum(val[Year==",year,"]) /  sum(val[Year==",2001-1,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`HIV growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(val[Year==",2000,"]),sum(val[Year==",year,"]), year-2000 ),'%')")

  }

  code_text <- paste0( " data_export_gbd_hiv_growth <- setDT( gbd_data_hiv)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('location_name')] ")

  eval(parse(text = code_text))

  # data_export_gbd_hiv_growth$`HIV growth from 1990-1990` <- data_export_gbd_hiv_growth$`HIV growth from 1990-1991`
  data_export_gbd_hiv_growth$`HIV growth from 2000-2020` <- data_export_gbd_hiv_growth$`HIV growth from 2000-2019`
  data_export_gbd_hiv_growth$`HIV growth from 2000-2021` <- data_export_gbd_hiv_growth$`HIV growth from 2000-2019`
  data_export_gbd_hiv_growth$`HIV growth from 2000-2022` <- data_export_gbd_hiv_growth$`HIV growth from 2000-2019`

  data_export_gbd_hiv_growth  <-data_export_gbd_hiv_growth %>% left_join(select(countries,Country=world_bank_name,location_name=united_nations_name))
  data_export_gbd_hiv_growth$location_name <- NULL

  data_export_final  <-data_export_final %>% left_join(data_export_gbd_hiv_growth)

  #athema -------------------------------------------------------------------------------------------------------------------------
  gbd_data_hiv <- gbd_data[gbd_data$cause_name=="Asthma"& gbd_data$metric_name=="Number" ,]
  data_export_gbd_hiv_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2019)
  {
    code_temp <- paste0(code_temp, "\n,`Asthma growth from 2000-",year,"` = paste0(round(sum(val[Year==",year,"]) /  sum(val[Year==",2001-1,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`Asthma growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(val[Year==",2000,"]),sum(val[Year==",year,"]), year-2000 ),'%')")

  }

  code_text <- paste0( " data_export_gbd_hiv_growth <- setDT( gbd_data_hiv)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('location_name')] ")

  eval(parse(text = code_text))

  # data_export_gbd_hiv_growth$`HIV growth from 1990-1990` <- data_export_gbd_hiv_growth$`HIV growth from 1990-1991`
  data_export_gbd_hiv_growth$`Asthma growth from 2000-2020` <- data_export_gbd_hiv_growth$`Asthma growth from 2000-2019`
  data_export_gbd_hiv_growth$`Asthma growth from 2000-2021` <- data_export_gbd_hiv_growth$`Asthma growth from 2000-2019`
  data_export_gbd_hiv_growth$`Asthma growth from 2000-2022` <- data_export_gbd_hiv_growth$`Asthma growth from 2000-2019`

  data_export_gbd_hiv_growth  <-data_export_gbd_hiv_growth %>% left_join(select(countries,Country=world_bank_name,location_name=united_nations_name))
  data_export_gbd_hiv_growth$location_name <- NULL

  data_export_gbd_athema_growth <- data_export_gbd_hiv_growth

  data_export_final  <-data_export_final %>% left_join(data_export_gbd_athema_growth)

  #athema -------------------------------------------------------------------------------------------------------------------------
  gbd_data_hiv <- gbd_data[gbd_data$cause_name=="Tuberculosis"& gbd_data$metric_name=="Number" ,]

  data_export_gbd_hiv_growth <- data.frame()
  code_temp <- ""
  for(year in 2000:2019)
  {
    code_temp <- paste0(code_temp, "\n,`Tuberculosis growth from 2000-",year,"` = paste0(round(sum(val[Year==",year,"]) /  sum(val[Year==",2001-1,"]),4) * 100,'%')")
    # code_temp <- paste0(code_temp, "\n,`Tuberculosis growth from 2000-",year,"` = paste0( compute_compound_increase_rate(sum(val[Year==",2000,"]),sum(val[Year==",year,"]), year-2000 ),'%')")

  }

  code_text <- paste0( " data_export_gbd_hiv_growth <- setDT( gbd_data_hiv)[,list(
     ",substr(code_temp,3,nchar(code_temp)),"
  ),by=c('location_name')] ")

  eval(parse(text = code_text))

  # data_export_gbd_hiv_growth$`HIV growth from 1990-1990` <- data_export_gbd_hiv_growth$`HIV growth from 1990-1991`
  data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2020` <- data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2019`
  data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2021` <- data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2019`
  data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2022` <- data_export_gbd_hiv_growth$`Tuberculosis growth from 2000-2019`

  data_export_gbd_hiv_growth  <-data_export_gbd_hiv_growth %>% left_join(select(countries,Country=world_bank_name,location_name=united_nations_name))
  data_export_gbd_hiv_growth$location_name <- NULL

  data_export_gbd_tuberculosis_growth <- data_export_gbd_hiv_growth

  data_export_final  <-data_export_final %>% left_join(data_export_gbd_tuberculosis_growth)


}



    # unique(index_data$Country)[! unique(index_data$Country) %in% unique(gbd_data$location_name)]

  #data_export_life_expec ---------------------------------------------------------------------------------------



  data_export_life_expec <- helathy_years_restored (index_data)


  data_export_life_expec <- data_export_life_expec %>% left_join(select(countries,Country=world_bank_name,wd_income_category=wd_income_category))


  data_export_life_expec$wd_income_category[is.na(data_export_life_expec$wd_income_category)] <- 1:sum( is.na(data_export_life_expec$wd_income_category))
  setDT(data_export_life_expec)[,`Healthy hours restored with each test strip`:=sum(`Healthy hours restored with each test strip` * true_incidence_age_10) / sum(true_incidence_age_10)   ,by= c("wd_income_category")]
  setDT(data_export_life_expec)[,`Days per year restored through devices`     :=sum(`Days per year restored through devices`      * true_incidence_age_10) / sum(true_incidence_age_10)   ,by= c("wd_income_category")]



  data_export_life_expec$true_incidence_age_10 <- NULL
  data_export_life_expec$wd_income_category    <- NULL


  index_data_compare <- index_data[index_data$Country%in% c("United States","Canada"),]
  # data_export_life_expec2 <- data_export_life_expec[data_export_life_expec$Country %in% c("United States","Canada"),]

  data_export_final  <-data_export_final %>% left_join(data_export_life_expec)




  data_export_final$T1D_growth_rate             <-  round( ( (as.numeric(gsub("%","",data_export_final$`T1D growth from 2000-2022`))/100) ^(1/22)-1)*100 ,2)
  data_export_final$Population_growth_rate      <-  round( ( (as.numeric(gsub("%","",data_export_final$`Population growth from 2000-2022`))/100) ^(1/22)-1)*100,2)
  data_export_final$HIV_growth_rate             <-  round(  ((as.numeric(gsub("%","",data_export_final$`HIV growth from 2000-2022`))/100)^(1/22)-1)*100,2)
  data_export_final$T2D_growth_rate             <-  round(  ((as.numeric(gsub("%","",data_export_final$`T2D growth from 2000-2022`))/100)^(1/22)-1)*100,2)
  data_export_final$Asthma_growth_rate          <- round(  ((as.numeric(gsub("%","",data_export_final$`Asthma growth from 2000-2022`))/100)^(1/22)-1)*100,2)
  data_export_final$Tuberculosis_growth_rate    <- round(  ((as.numeric(gsub("%","",data_export_final$`Tuberculosis growth from 2000-2022`))/100)^(1/22)-1)*100,2)


  # V 13 changes
  # add dashboard url  and version  of shiny ----
  country  <- Data_Run_query_return_df ("SELECT * FROM country order by world_bank_name ;")
  country_parquet_list <- data.frame()
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name="GLOBAL", file_name=paste0("GLOBAL") ))
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(country$world_bank_classification), file_name=paste0(unique(country$world_bank_classification), "")) )
  country_parquet_list <- rbind(country_parquet_list, data.frame(country_name=unique(country$wd_region), file_name=paste0(unique(country$wd_region), "")))
  country_parquet_list$loc_id <- 1001:(1000 + nrow(country_parquet_list) )
  country_parquet_list <- rbind(country_parquet_list, dplyr::select(country, country_name=world_bank_name,file_name=world_bank_code,loc_id))

  data_export_final  <-data_export_final %>% left_join(select(country_parquet_list, Country=country_name,t1d_index_dashboar_url = loc_id))
  data_export_final  <- cbind(primary_key= data_export_final$t1d_index_dashboar_url, data_export_final)
  data_export_final$t1d_index_dashboar_url <- paste0("https://t1dindex.shinyapps.io/dashboard/?loc_id=",data_export_final$t1d_index_dashboar_url)
  data_export_final$t1d_index_version      <- "1.0.4.15"

  # Add Greenland (Denmark) as Denmark ------------------------------------
  data_export_final_greenland <- data_export_final[data_export_final$Country=="Denmark",]
  data_export_final_greenland$Country <- "Greenland (Denmark)"

  data_export_final <- rbind(data_export_final,data_export_final_greenland)






  data_export_final

}

if(FALSE)
{

  library(data.table)
  library(dplyr)
  library(RPostgreSQL)

  source("code_R_data_prep/DATA_CONSTANTS.R")
  library(readxl)


  data_export_final <-  extract_for_purpose_v2 (Country="",include_gbd=TRUE)

  # write_excel_csv(data_export_final,  "data_internal/export_scott_final.csv", col_names = TRUE)  # write as original_wolrd_bank_name


  # change country name to paper
  library(readr)

  code_site_country_name <- read.csv("data_internal/code_site_country_name.csv")
  for(i in 1:nrow(data_export_final))
  { # i <- 1
    if(data_export_final$Country[i] %in% code_site_country_name$code)
    {

      print(data_export_final$Country[i] )
      print(code_site_country_name$site[code_site_country_name$code == data_export_final$Country[i]] )
      data_export_final$Country[i] <- code_site_country_name$site[code_site_country_name$code ==data_export_final$Country[i]   ]
    }


  }

  # check with template
  # template <- read.csv("code_quick_job_scripts/JDRF v2 Data Template - CSV Template.csv")
  # colnames(template) %in% colnames(data_export_final)

  # write.csv(data_export_final,"temp/export_scott_v12.csv", row.names = F)
  # write_excel_csv(data_export_final,  "temp/export_scott_v14.csv", col_names = TRUE)



}
