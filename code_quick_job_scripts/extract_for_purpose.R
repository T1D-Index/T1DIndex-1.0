# library(data.table)
# library(dplyr)
# library(tidyr)
# version_no <- "0.4.10"
# t1d_index           <- readRDS( paste0("reruns/rerun_0.4.10_0/aggregates/t1dGlobalModel.Rds") )
# load(paste0("reruns/rerun_",input$in_version,"_0/",country_code,".Rda"))
# t1d_index <- spread(result$prev_merge_long, Value_type, Value )

extract_for_purpose <- function(t1d_index,age_start,age_end,current_year)
{  # t1d_index <- result ; current_year <- 2021 ; age_start <- 0; age_end <- 99

  t1d_index           <- t1d_index[t1d_index$Year>=1960 & t1d_index$Year<=2040,]
  # t1d_index           <- t1d_index[(t1d_index$Age>=0) & (t1d_index$Age<=19),]
  # t1d_index           <- t1d_index[t1d_index$Age>=20 & t1d_index$Age<=69,]
  # t1d_index           <- t1d_index[t1d_index$Age>=70 & t1d_index$Age<=99,]
  t1d_index           <- t1d_index[t1d_index$Age>=age_start & t1d_index$Age<=age_end,]

  # current_year= 2021
  data_everyone <- setDT(t1d_index)[,list(
    "Index"="?"
    ,"Living with T1D"= sum(Prevalence[Year==current_year])
    ,"Lives Stolen"   = sum(`Ghosts (onset death)`[Year==current_year]  + `Ghosts (early death)`[Year==current_year] )
    ,"Days Stolen"    = "?"
    ,"2040 Index"="?"
    ,"2040 Living with T1D"= sum(Prevalence[Year==2040])
    ,"2040 Lives Stolen"   = sum(`Ghosts (onset death)`[Year==2040]  + `Ghosts (early death)`[Year==2040] )
    ,"2040 Days Stolen"    = "?"
  ), by="Country"]

  Lives_Saved <-  dplyr:: select(t1d_index , Country, Year, Age,
                                       data1 ="Ghosts (onset death)",
                                       data2 ="Ghosts (delta basic care)",
                                       data3 ="Ghosts (delta best care)" ,
                                       data4 ="Ghosts (delta cure)"
                                       )

  Lives_Saved <- setDT(Lives_Saved)[,list(
    "Lives Saved (Diagnosis)" = sum (data1),
    "Lives Saved (Basic)"     = sum (data2),
    "Lives Saved (Best)"      = sum (data3),
    "Lives Saved (Cure)"      = sum (data4)
  ),by=c("Country","Year")]

  Lives_Saved_temp      <- dplyr::select(Lives_Saved,Country, Year, value= "Lives Saved (Diagnosis)")
  Lives_Saved_1         <- Lives_Saved_temp %>% spread(Year, value)
  colnames(Lives_Saved_1)[2:ncol(Lives_Saved_1)] <-  paste0(colnames(Lives_Saved_1)[2:ncol(Lives_Saved_1)]," Lives Saved (Diagnosis)")

  Lives_Saved_temp      <- dplyr::select(Lives_Saved,Country, Year, value= "Lives Saved (Basic)")
  Lives_Saved_2         <- Lives_Saved_temp %>% spread(Year, value)
  colnames(Lives_Saved_2)[2:ncol(Lives_Saved_2)] <-  paste0(colnames(Lives_Saved_2)[2:ncol(Lives_Saved_2)]," Lives Saved (Basic)")

  Lives_Saved_temp      <- dplyr::select(Lives_Saved,Country, Year, value= "Lives Saved (Best)")
  Lives_Saved_3         <- Lives_Saved_temp %>% spread(Year, value)
  colnames(Lives_Saved_3)[2:ncol(Lives_Saved_3)] <-  paste0(colnames(Lives_Saved_3)[2:ncol(Lives_Saved_3)]," Lives Saved (Best)")

  Lives_Saved_temp      <- dplyr::select(Lives_Saved,Country, Year, value= "Lives Saved (Cure)")
  Lives_Saved_4         <- Lives_Saved_temp %>% spread(Year, value)
  colnames(Lives_Saved_4)[2:ncol(Lives_Saved_4)] <-  paste0(colnames(Lives_Saved_4)[2:ncol(Lives_Saved_4)]," Lives Saved (Cure)")


  # 1 in ** families. country_indicator------------------------------------------------------------------------------------------------------------------------------------
  country_indicator <- dplyr::select(readRDS("data_wb/country_indicator_imputed_avg_0.4.13.Rds"),country,year,fertility)
  if(sum(country_indicator$country %in% t1d_index$Country))
  {
    country_indicator <- country_indicator[country_indicator$country %in% t1d_index$Country,]
    country_indicator_2001_2040 <- country_indicator[country_indicator$year>=2001,]
    country_indicator_2001_2040$year <- country_indicator_2001_2040$year + 20
    country_indicator_2001_2040$fertility <- NA
    country_indicator <- rbind(country_indicator, country_indicator_2001_2040)
    n_country <- unique(country_indicator_2001_2040$country)

    for(c in 1:length(n_country))
    { # c <- 1
      ci_temp <- country_indicator[country_indicator$country==n_country[c],]
      ratio <- (ci_temp$fertility[ci_temp$year==2020-0]/ci_temp$fertility[ci_temp$year==2020-1] +
                  ci_temp$fertility[ci_temp$year==2020-1]/ci_temp$fertility[ci_temp$year==2020-2] +
                  ci_temp$fertility[ci_temp$year==2020-2]/ci_temp$fertility[ci_temp$year==2020-3] +
                  ci_temp$fertility[ci_temp$year==2020-3]/ci_temp$fertility[ci_temp$year==2020-4] +
                  ci_temp$fertility[ci_temp$year==2020-4]/ci_temp$fertility[ci_temp$year==2020-5] +
                  ci_temp$fertility[ci_temp$year==2020-5]/ci_temp$fertility[ci_temp$year==2020-6] +
                  ci_temp$fertility[ci_temp$year==2020-6]/ci_temp$fertility[ci_temp$year==2020-7] +
                  ci_temp$fertility[ci_temp$year==2020-7]/ci_temp$fertility[ci_temp$year==2020-8] +
                  ci_temp$fertility[ci_temp$year==2020-8]/ci_temp$fertility[ci_temp$year==2020-9] +
                  ci_temp$fertility[ci_temp$year==2020-9]/ci_temp$fertility[ci_temp$year==2020-10] )/10
      for(d in 2021:2040)
      {
        ci_temp$fertility[ci_temp$year==d] <- ci_temp$fertility[ci_temp$year==d-1] *  ratio
      }
      country_indicator[country_indicator$country==n_country[c],] <- ci_temp
    }
  }

  # 1 in ** families country_indicator.------------------------------------------------------------------------------------------------------------------------------------

  # days lost -----------------------------------------------------------------------------------------------------------------------------------------
  calculate_days <- function(t1d_index_base)
  {
    t1d_index_base <- setDT(t1d_index_base)[,list(
      "background_population"=sum (`background_population`),
      "Diagnosed (living)"  =sum (`Diagnosed (living)`),
      "Diagnosed (ghost)"   =sum (`Diagnosed (ghost)`),
      "Undiagnosed (ghost)" =sum (`Undiagnosed (ghost)`),
      "Total"               =sum (`Diagnosed (living)`) + sum (`Diagnosed (ghost)`) + sum (`Undiagnosed (ghost)`),
      "Days lost (complications)"=sum (`Days lost (complications)`),
      "Life expectency"    =sum (`Life expectency`[Age==10])
    ),by=c("Country","Year")]

    t1d_index_base$`Days lost (diagnosed ghosts)`   <- t1d_index_base$`Diagnosed (ghost)` * 365
    t1d_index_base$`Days lost (undiagnosed ghosts)` <- t1d_index_base$`Undiagnosed (ghost)` * 365
    t1d_index_base$`Days lost (treatment)`          <- round(t1d_index_base$`Diagnosed (living)` * 365 * 0.033,2)
    t1d_index_base$`Days lost (complications)`      <- round(t1d_index_base$`Days lost (complications)` * 365 - t1d_index_base$`Days lost (treatment)`,2)

    t1d_index_base$`Days lost to complications` <- round( t1d_index_base$`Days lost (complications)` /t1d_index_base$`Diagnosed (living)`,0)
    t1d_index_base$`Days lost to treatment`     <- round( t1d_index_base$`Days lost (treatment)` /t1d_index_base$`Diagnosed (living)`,0)

    t1d_index_base$`Total days lost` <- t1d_index_base$`Days lost (diagnosed ghosts)` +
                                        t1d_index_base$`Days lost (undiagnosed ghosts)`+
                                        t1d_index_base$`Days lost (treatment)`+
                                        t1d_index_base$`Days lost (complications)`
    t1d_index_base$`(days left)` <-  round(t1d_index_base$Total * 365 - t1d_index_base$`Total days lost`,2)
    t1d_index_base$`index score` <-  round(t1d_index_base$`(days left)` / (t1d_index_base$`Total days lost` + t1d_index_base$`(days left)`) *100,0)

    t1d_index_base$lifetime_years_lost_complication_and_treatment <- (t1d_index_base$`Days lost (treatment)`+ t1d_index_base$`Days lost (complications)`)/365/t1d_index_base$`Diagnosed (living)` * t1d_index_base$`Life expectency`
    t1d_index_base$lifetime_years_lost_complication <- (t1d_index_base$`Days lost (complications)`)/365/t1d_index_base$`Diagnosed (living)`* t1d_index_base$`Life expectency`
    t1d_index_base$lifetime_years_lost_treatment    <- (t1d_index_base$`Days lost (treatment)`)    /365/t1d_index_base$`Diagnosed (living)`* t1d_index_base$`Life expectency`

    # 1 in x families
    t1d_index_base <- t1d_index_base %>% dplyr::left_join(dplyr::select(country_indicator,Country=country,Year=year,fertility),by=c("Country","Year"))
    t1d_index_base$`1 in x families` <- t1d_index_base$fertility^2 + t1d_index_base$fertility +2
    t1d_index_base$`1 in x families` <- 1/(1-((t1d_index_base$background_population/t1d_index_base$`Total`-1)/(t1d_index_base$background_population/t1d_index_base$`Total`) ) ^ t1d_index_base$`1 in x families`)

    return(t1d_index_base)
  }

  t1d_index_base <- t1d_index
  t1d_index_base <- dplyr::select(t1d_index_base,  Country, Year, Age,
                                  "background_population"       = `Ann. background population`,
                                  "Diagnosed (living)"          =  Prevalence,
                                  "Diagnosed (ghost)"           = `Ghosts (early death)`,
                                  "Undiagnosed (ghost)"         = `Ghosts (onset death)`,
                                  "Days lost (complications)"   = `Ann. days lost (1 base)`,
                                  "Life expectency"             = `Life expectency (2 t1d base)`)
days_base      <- calculate_days(t1d_index_base)
  #  levers  full diagnosis -=--------------------------------------------------------------------------------------------------------------

  t1d_index_base <- t1d_index
  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (onset death)`
  t1d_index_base$`Ghosts (onset death)` <- 0
  t1d_index_base <- dplyr::select(t1d_index_base,  Country, Year, Age,
                           "background_population"       = `Ann. background population`,
                           "Diagnosed (living)"          =  Prevalence,
                           "Diagnosed (ghost)"           = `Ghosts (early death)`,
                           "Undiagnosed (ghost)"         = `Ghosts (onset death)`,
                           "Days lost (complications)"   =  `Ann. days lost (2 diagnosis)`,
                           "Life expectency"             = `Life expectency (3 t1d diagnosis)`)
  days_diagnosis      <- calculate_days(t1d_index_base)
  #  levers  basic -=--------------------------------------------------------------------------------------------------------------

  t1d_index_base <- t1d_index
  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (onset death)`
  t1d_index_base$`Ghosts (onset death)` <- 0

  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta basic care)`
  t1d_index_base$`Ghosts (early death)` <- t1d_index_base$`Ghosts (early death)` - t1d_index_base$`Ghosts (delta basic care)`

  t1d_index_base <- dplyr::select(t1d_index_base,  Country, Year, Age,
                            "background_population"       = `Ann. background population`,
                           "Diagnosed (living)"          =  Prevalence,
                           "Diagnosed (ghost)"           = `Ghosts (early death)`,
                           "Undiagnosed (ghost)"         = `Ghosts (onset death)`,
                           "Days lost (complications)"   =  `Ann. days lost (3 basic care)`,
                           "Life expectency"             = `Life expectency (4 t1d basic care)`)
  days_basic      <- calculate_days(t1d_index_base)
  #  levers  best -=--------------------------------------------------------------------------------------------------------------

  t1d_index_base <- t1d_index
  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (onset death)`
  t1d_index_base$`Ghosts (onset death)` <- 0

  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta basic care)`
  t1d_index_base$`Ghosts (early death)` <- t1d_index_base$`Ghosts (early death)` - t1d_index_base$`Ghosts (delta basic care)`


  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta best care)`
  t1d_index_base$`Ghosts (early death)` <- t1d_index_base$`Ghosts (early death)` - t1d_index_base$`Ghosts (delta best care)`

  t1d_index_base <- dplyr::select(t1d_index_base,  Country, Year, Age,
                          "background_population"       = `Ann. background population`,
                           "Diagnosed (living)"          =  Prevalence,
                           "Diagnosed (ghost)"           = `Ghosts (early death)`,
                           "Undiagnosed (ghost)"         = `Ghosts (onset death)`,
                           "Days lost (complications)"   =  `Ann. days lost (4 best care)`,
                          "Life expectency"             = `Life expectency (5 t1d best care)`)
  days_best      <- calculate_days(t1d_index_base)
  #  levers  cure -=--------------------------------------------------------------------------------------------------------------

  t1d_index_base <- t1d_index
  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (onset death)`
  t1d_index_base$`Ghosts (onset death)` <- 0

  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta basic care)`
  t1d_index_base$`Ghosts (early death)` <- t1d_index_base$`Ghosts (early death)` - t1d_index_base$`Ghosts (delta basic care)`


  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta best care)`
  t1d_index_base$`Ghosts (early death)` <- t1d_index_base$`Ghosts (early death)` - t1d_index_base$`Ghosts (delta best care)`

  t1d_index_base$Prevalence             <- t1d_index_base$Prevalence + t1d_index_base$`Ghosts (delta cure)`
  t1d_index_base$`Ghosts (early death)` <- 0



  t1d_index_base <- dplyr::select(t1d_index_base,  Country, Year, Age,
                          "background_population"       = `Ann. background population`,
                           "Diagnosed (living)"          =  Prevalence,
                           "Diagnosed (ghost)"           = `Ghosts (early death)`,
                           "Undiagnosed (ghost)"         = `Ghosts (onset death)`,
                           "Days lost (complications)"   =  `Ann. days lost (5 cure)`,
                          "Life expectency"             = `Life expectency (6 t1d cure)`)
  days_cure      <- calculate_days(t1d_index_base)

  #  calculate levers -=--------------------------------------------------------------------------------------------------------------
  days_restored <- dplyr::select(days_base, Country,Year)

  days_restored$`Days Restored (Diagnosis)` <- days_base$`Total days lost`      -  days_diagnosis$`Total days lost`
  days_restored$`Days Restored (Basic)`     <- days_diagnosis$`Total days lost` -  days_basic$`Total days lost`
  days_restored$`Days Restored (Best)`      <- days_basic$`Total days lost`     -  days_best$`Total days lost`
  days_restored$`Days Restored (Cure)`      <- days_best$`Total days lost`      -  days_cure$`Total days lost`
  #  spread  levers -=--------------------------------------------------------------------------------------------------------------
  temp      <- dplyr::select(days_restored,Country, Year, value= "Days Restored (Diagnosis)")
  days_restored_1         <- temp %>% spread(Year, value)
  colnames(days_restored_1)[2:ncol(days_restored_1)] <-  paste0(colnames(days_restored_1)[2:ncol(days_restored_1)]," Days Restored (Diagnosis)")

  temp      <- dplyr::select(days_restored,Country, Year, value= "Days Restored (Basic)")
  days_restored_2         <- temp %>% spread(Year, value)
  colnames(days_restored_2)[2:ncol(days_restored_2)] <-  paste0(colnames(days_restored_2)[2:ncol(days_restored_2)]," Days Restored (Basic)")

  temp      <- dplyr::select(days_restored,Country, Year, value= "Days Restored (Best)")
  days_restored_3         <- temp %>% spread(Year, value)
  colnames(days_restored_3)[2:ncol(days_restored_3)] <-  paste0(colnames(days_restored_3)[2:ncol(days_restored_3)]," Days Restored (Best)")

  temp      <- dplyr::select(days_restored,Country, Year, value= "Days Restored (Cure)")
  days_restored_4         <- temp %>% spread(Year, value)
  colnames(days_restored_4)[2:ncol(days_restored_4)] <-  paste0(colnames(days_restored_4)[2:ncol(days_restored_4)]," Days Restored (Cure)")

  # Days Restored (Dignosis)
  # merge all sections ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
  Lives_Saved_1$Country <- NULL
  Lives_Saved_2$Country <- NULL
  Lives_Saved_3$Country <- NULL
  Lives_Saved_4$Country <- NULL
  days_restored_1$Country <- NULL
  days_restored_2$Country <- NULL
  days_restored_3$Country <- NULL
  days_restored_4$Country <- NULL

  data_everyone$Index                     <-  days_base$`index score`[days_base$Year==current_year]
  data_everyone$`Days Stolen`             <-  days_base$`Total days lost`[days_base$Year==current_year]
  data_everyone$`2040 Index`              <-  days_base$`index score`[days_base$Year==2040]
  data_everyone$`2040 Days Stolen`        <-  days_base$`Total days lost`[days_base$Year==2040]

  data_everyone$`Days lost to treatment`         <- days_base $`Days lost to treatment` [days_base$Year==current_year]
  data_everyone$`Days lost to treatment (Basic)` <- days_basic $`Days lost to treatment`[days_basic$Year==current_year]
  data_everyone$`Days lost to treatment (Best)`  <- days_best $`Days lost to treatment`[days_best$Year==current_year]
  data_everyone$`Days lost to treatment (Cure)`  <- days_cure $`Days lost to treatment`[days_cure$Year==current_year]

  data_everyone$`Days lost to complications`         <- days_base $`Days lost to complications` [days_base$Year==current_year]
  data_everyone$`Days lost to complications (Basic)` <- days_basic $`Days lost to complications`[days_basic$Year==current_year]
  data_everyone$`Days lost to complications (Best)`  <- days_best $`Days lost to complications`[days_best$Year==current_year]
  data_everyone$`Days lost to complications (Cure)`  <- days_cure $`Days lost to complications`[days_cure$Year==current_year]

  JDRF_calculations <- cbind(data_everyone, Lives_Saved_1,Lives_Saved_2,Lives_Saved_3,Lives_Saved_4,days_restored_1,days_restored_2,days_restored_3,days_restored_4)

  # write.csv(JDRF_calculations, "temp/JDRF_calculations_everyone.csv",row.names = F)
  # write.csv(JDRF_calculations, "temp/JDRF_calculations_young.csv",row.names = F)
  # write.csv(JDRF_calculations, "temp/JDRF_calculations_adults.csv",row.names = F)
  # write.csv(JDRF_calculations, "temp/JDRF_calculations_older.csv",row.names = F)



  return(list(JDRF_calculations=JDRF_calculations,
              days_1     =days_base,
              days_2     =days_diagnosis,
              days_3     =days_basic,
              days_4     =days_best,
              days_5     =days_cure
  ) )

}
