# incidence_curves.csv  and incidence_growth_rates.csv ------------------------------------------------------------------------------
# incidence.xlsx source : https://docs.google.com/spreadsheets/d/13Y0K0zppNs4sR5SqhLJBUROwSsptVhcT/edit#gid=120040738
get_Incidence_Full <- function(random_data_ci=FALSE)
{
  # random_data_ci <- TRUE
  # random_data_ci <- FALSE
  library(dplyr)
  library(readxl)
  library(arrow)

  tryCatch({

    country   <- read_country()



    source('code_R_data_prep/000_3_Incidence_over_age_curve.R')
    source('code_R/utils.R')

    if(FALSE)
    {
      incidence_curve <- read_excel('data_internal/incidence.xlsx', 'curves') %>%
        dplyr::select(-primary) %>%
        pivot_longer(-(1:2), names_to='age_range', values_to='incidence') %>%
        separate(age_range, sep='-', into=c('age_from','age_to')) %>%
        inner_join(country, by='world_bank_name') %>%
        dplyr::select(loc_id,year,age_from,age_to,incidence)
      incidence_curve$age_from <- as.numeric(incidence_curve$age_from)
      incidence_curve$age_to   <- as.numeric(incidence_curve$age_to)
    }

    # load and clean IDF estimates
    if(FALSE)
    { # old survey
      estimates <- data.frame(read_excel('data_internal/Country incidence.xlsx', '2019 IDF estimates',skip = 0))
      estimates[2,][is.na(estimates[2,])] <- estimates[1,][is.na(estimates[2,])]
      colnames(estimates) <-  estimates[2,]
      estimates <- estimates[3:nrow(estimates),]
      estimates <- estimates[,c(2,6,30:44,56,62) ]
    }

    # IDF ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    estimates <- data.frame(read_excel('data_incidence/IDF Atlas childhood T1DM 2021 estimates 6th July for Fei.xlsx', '2021 estimates',skip = 0))
    estimates_analysis_group <- data.frame(read_excel('data_incidence/IDF Atlas childhood T1DM 2021 estimates 6th July for Fei.xlsx', 'analysis_group',skip = 0))
    colnames(estimates) [!is.na(estimates[1,])] <-  estimates[1,][!is.na(estimates[1,])]
    estimates <- estimates[2:nrow(estimates),]
    estimates <- estimates[,c(2,5,6,30:44,60,66) ]
    estimates <- estimates[!is.na(estimates$Country),]

    estimates <- cbind(analysis_group=estimates_analysis_group$analysis.group,estimates)


    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Czech Republic"] <- "Czechia"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Federated States of Micronesia"] <- "Micronesia (Federated States of)"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Hong Kong China"] <- "Hong Kong"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Islamic Republic of Iran"] <- "Iran (Islamic Republic of)"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Lao People's Democratic Republic"] <- "Laos"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Macau China"] <- "Macao"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Macedonia"] <- "North Macedonia"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Republic of Congo"] <- "Democratic Republic of the Congo"
    # estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Sint Maarten"] <- "Czechia"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Swaziland"] <- "Eswatini"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Syrian Arab Republic"] <- "Syria"
    estimates$Country.used.to.make.extrapolations..0.14.[estimates$Country.used.to.make.extrapolations..0.14.== "Venezuela"] <- "Venezuela (Bolivarian Republic of)"

    estimates$Period[estimates$Country=='Algeria'] <- '2013-2019'

    # ADD CIs from T1D Index CI mapping - CI_Incidence --------------------------
    estimates$`M         0-4_lower` <- NA
    estimates$`M         0-4_upper` <- NA
    estimates$`M        5-9_lower` <- NA
    estimates$`M        5-9_upper` <- NA
    estimates$`M       10-14_lower` <- NA
    estimates$`M       10-14_upper` <- NA

    estimates$`F         0-4_lower` <- NA
    estimates$`F         0-4_upper` <- NA
    estimates$`F        5-9_lower` <- NA
    estimates$`F        5-9_upper` <- NA
    estimates$`F       10-14_lower` <- NA
    estimates$`F       10-14_upper` <- NA

    estimates$`M 15-19 (ratio) estimate_lower` <- NA
    estimates$`M 15-19 (ratio) estimate_upper` <- NA
    estimates$`F 15-19 (ratio) estimate_lower` <- NA
    estimates$`F 15-19 (ratio) estimate_upper` <- NA

    estimates$`Age-stand  M&F      0-14_lower` <- NA
    estimates$`Age-stand  M&F      0-14_upper` <- NA


    if(random_data_ci)
    {
      CI_Incidence <- read.csv("data_incidence/T1D Index CI mapping - CI_Incidence.csv",skip=1)
      CI_Incidence$Min.1 <- as.numeric(CI_Incidence$Min.1)
      CI_Incidence$Max.1 <- as.numeric(CI_Incidence$Max.1)
      CI_Incidence <- CI_Incidence[!is.na(CI_Incidence$Min.1),]
      CI_Incidence <- CI_Incidence[!is.na(CI_Incidence$Max.1),]

      ratio_upper_list <- c()
      ratio_lower_list <- c()
      for(i in 1:nrow(estimates))
      {  # i <- 91
        # M
        if(!is.na(estimates$`M         0-4`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Max==4 & CI_Incidence$Gender=='M',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M         0-4`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`M         0-4_lower`[i] <- as.numeric(estimates$`M         0-4`[i])* ratio_lower
            estimates$`M         0-4_upper`[i] <- as.numeric(estimates$`M         0-4`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }

        if(!is.na(estimates$`F         0-4`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Max==4 & CI_Incidence$Gender=='F',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M         0-4`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`F         0-4_lower`[i] <- as.numeric(estimates$`F         0-4`[i])* ratio_lower
            estimates$`F         0-4_upper`[i] <- as.numeric(estimates$`F         0-4`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }

        if(!is.na(estimates$`M        5-9`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Max==9 & CI_Incidence$Gender=='M',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M        5-9`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`M        5-9_lower`[i] <- as.numeric(estimates$`M        5-9`[i])* ratio_lower
            estimates$`M        5-9_upper`[i] <- as.numeric(estimates$`M        5-9`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }

        if(!is.na(estimates$`F        5-9`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Max==9 & CI_Incidence$Gender=='F',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M        5-9`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`F        5-9_lower`[i] <- as.numeric(estimates$`F        5-9`[i])* ratio_lower
            estimates$`F        5-9_upper`[i] <- as.numeric(estimates$`F        5-9`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }


        if(!is.na(estimates$`M       10-14`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Min==10 &CI_Incidence$Max==14 & CI_Incidence$Gender=='M',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M       10-14`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`M       10-14_lower`[i] <- as.numeric(estimates$`M       10-14`[i])* ratio_lower
            estimates$`M       10-14_upper`[i] <- as.numeric(estimates$`M       10-14`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }



        if(!is.na(estimates$`F       10-14`[i]))
        {
          # print(i)
          CI_Incidence_t <- CI_Incidence[CI_Incidence$Min==10 &CI_Incidence$Max==14 & CI_Incidence$Gender=='F',]
          index_value <- CI_Incidence_t$world_bank_name ==estimates$Country[i]
          if(sum(index_value))
          {
            # print(i)
            # print(estimates[i,]$`M       10-14`)
            # print( estimates[i,]$Period)
            ratio_lower <-  median(CI_Incidence_t[index_value,]$Min.1/CI_Incidence_t[index_value,]$Mean)
            ratio_upper <-  median(CI_Incidence_t[index_value,]$Max.1/CI_Incidence_t[index_value,]$Mean)
            estimates$`F       10-14_lower`[i] <- as.numeric(estimates$`F       10-14`[i])* ratio_lower
            estimates$`F       10-14_upper`[i] <- as.numeric(estimates$`F       10-14`[i])* ratio_upper
            ratio_lower_list <- c(ratio_lower_list,ratio_lower)
            ratio_upper_list <- c(ratio_upper_list,ratio_upper)
          }
        }
      }

      ratio_lower_median <- median(ratio_lower_list)
      ratio_upper_median <- median(ratio_upper_list)

      #----  impute lower for all M F
      index_impute <- (is.na(estimates$`M         0-4_lower`)) & (!is.na(estimates$`M         0-4`))
      estimates$`M         0-4_lower`[index_impute] <- as.numeric(estimates$`M         0-4`[index_impute]) * ratio_lower_median
      index_impute <- (is.na(estimates$`F         0-4_lower`)) & (!is.na(estimates$`F         0-4`))
      estimates$`F         0-4_lower`[index_impute] <- as.numeric(estimates$`F         0-4`[index_impute]) * ratio_lower_median

      index_impute <- (is.na(estimates$`M        5-9_lower`)) & (!is.na(estimates$`M        5-9`))
      estimates$`M        5-9_lower`[index_impute] <- as.numeric(estimates$`M        5-9`[index_impute]) * ratio_lower_median
      index_impute <- (is.na(estimates$`F        5-9_lower`)) & (!is.na(estimates$`F        5-9`))
      estimates$`F        5-9_lower`[index_impute] <- as.numeric(estimates$`F        5-9`[index_impute]) * ratio_lower_median

      index_impute <- (is.na(estimates$`M       10-14_lower`)) & (!is.na(estimates$`M       10-14`))
      estimates$`M       10-14_lower`[index_impute] <- as.numeric(estimates$`M       10-14`[index_impute]) * ratio_lower_median
      index_impute <- (is.na(estimates$`F       10-14_lower`)) & (!is.na(estimates$`F       10-14`))
      estimates$`F       10-14_lower`[index_impute] <- as.numeric(estimates$`F       10-14`[index_impute]) * ratio_lower_median

      index_impute <- (is.na(estimates$`M 15-19 (ratio) estimate_lower`)) & (!is.na(estimates$`M 15-19 (ratio) estimate`))
      estimates$`M 15-19 (ratio) estimate_lower`[index_impute] <- as.numeric(estimates$`M 15-19 (ratio) estimate`[index_impute]) * ratio_lower_median
      index_impute <- (is.na(estimates$`F 15-19 (ratio) estimate_lower`)) & (!is.na(estimates$`F 15-19 (ratio) estimate`))
      estimates$`F 15-19 (ratio) estimate_lower`[index_impute] <- as.numeric(estimates$`F 15-19 (ratio) estimate`[index_impute]) * ratio_lower_median

      index_impute <- (is.na(estimates$`Age-stand  M&F      0-14_lower`)) & (!is.na(estimates$`Age-stand  M&F      0-14`))
      estimates$`Age-stand  M&F      0-14_lower`[index_impute] <- as.numeric(estimates$`Age-stand  M&F      0-14`[index_impute]) * ratio_lower_median

      #----  impute upper for all M F
      index_impute <- (is.na(estimates$`M         0-4_upper`)) & (!is.na(estimates$`M         0-4`))
      estimates$`M         0-4_upper`[index_impute] <- as.numeric(estimates$`M         0-4`[index_impute]) * ratio_upper_median
      index_impute <- (is.na(estimates$`F         0-4_upper`)) & (!is.na(estimates$`F         0-4`))
      estimates$`F         0-4_upper`[index_impute] <- as.numeric(estimates$`F         0-4`[index_impute]) * ratio_upper_median

      index_impute <- (is.na(estimates$`M        5-9_upper`)) & (!is.na(estimates$`M        5-9`))
      estimates$`M        5-9_upper`[index_impute] <- as.numeric(estimates$`M        5-9`[index_impute]) * ratio_upper_median
      index_impute <- (is.na(estimates$`F        5-9_upper`)) & (!is.na(estimates$`F        5-9`))
      estimates$`F        5-9_upper`[index_impute] <- as.numeric(estimates$`F        5-9`[index_impute]) * ratio_upper_median

      index_impute <- (is.na(estimates$`M       10-14_upper`)) & (!is.na(estimates$`M       10-14`))
      estimates$`M       10-14_upper`[index_impute] <- as.numeric(estimates$`M       10-14`[index_impute]) * ratio_upper_median
      index_impute <- (is.na(estimates$`F       10-14_upper`)) & (!is.na(estimates$`F       10-14`))
      estimates$`F       10-14_upper`[index_impute] <- as.numeric(estimates$`F       10-14`[index_impute]) * ratio_upper_median

      index_impute <- (is.na(estimates$`M 15-19 (ratio) estimate_upper`)) & (!is.na(estimates$`M 15-19 (ratio) estimate`))
      estimates$`M 15-19 (ratio) estimate_upper`[index_impute] <- as.numeric(estimates$`M 15-19 (ratio) estimate`[index_impute]) * ratio_upper_median
      index_impute <- (is.na(estimates$`F 15-19 (ratio) estimate_upper`)) & (!is.na(estimates$`F 15-19 (ratio) estimate`))
      estimates$`F 15-19 (ratio) estimate_upper`[index_impute] <- as.numeric(estimates$`F 15-19 (ratio) estimate`[index_impute]) * ratio_upper_median

      index_impute <- (is.na(estimates$`Age-stand  M&F      0-14_upper`)) & (!is.na(estimates$`Age-stand  M&F      0-14`))
      estimates$`Age-stand  M&F      0-14_upper`[index_impute] <- as.numeric(estimates$`Age-stand  M&F      0-14`[index_impute]) * ratio_upper_median

      # impute
      index_imp <- !is.na(estimates$`M         0-4`)
      estimates$`M         0-4`[index_imp] <- random_gen(mean= as.numeric(estimates$`M         0-4`[index_imp])
                                                         ,smr_ci_lower = as.numeric(estimates$`M         0-4_lower`[index_imp])
                                                         ,smr_ci_upper = as.numeric(estimates$`M         0-4_upper`[index_imp]) )
      index_imp <- !is.na(estimates$`F         0-4`)
      estimates$`F         0-4`[index_imp] <- random_gen(mean= as.numeric(estimates$`F         0-4`[index_imp])
                                                         ,smr_ci_lower = as.numeric(estimates$`F         0-4_lower`[index_imp])
                                                         ,smr_ci_upper = as.numeric(estimates$`F         0-4_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`M        5-9`)
      estimates$`M        5-9`[index_imp] <- random_gen(mean= as.numeric(estimates$`M        5-9`[index_imp])
                                                        ,smr_ci_lower = as.numeric(estimates$`M        5-9_lower`[index_imp])
                                                        ,smr_ci_upper = as.numeric(estimates$`M        5-9_upper`[index_imp]) )
      index_imp <- !is.na(estimates$`F        5-9`)
      estimates$`F        5-9`[index_imp] <- random_gen(mean= as.numeric(estimates$`F        5-9`[index_imp])
                                                        ,smr_ci_lower = as.numeric(estimates$`F        5-9_lower`[index_imp])
                                                        ,smr_ci_upper = as.numeric(estimates$`F        5-9_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`M       10-14`)
      estimates$`M       10-14`[index_imp] <- random_gen(mean= as.numeric(estimates$`M       10-14`[index_imp])
                                                         ,smr_ci_lower = as.numeric(estimates$`M       10-14_lower`[index_imp])
                                                         ,smr_ci_upper = as.numeric(estimates$`M       10-14_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`F       10-14`)
      estimates$`F       10-14`[index_imp] <- random_gen(mean= as.numeric(estimates$`F       10-14`[index_imp])
                                                         ,smr_ci_lower = as.numeric(estimates$`F       10-14_lower`[index_imp])
                                                         ,smr_ci_upper = as.numeric(estimates$`F       10-14_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`M 15-19 (ratio) estimate`)
      estimates$`M 15-19 (ratio) estimate`[index_imp] <- random_gen(mean= as.numeric(estimates$`M 15-19 (ratio) estimate`[index_imp])
                                                                    ,smr_ci_lower = as.numeric(estimates$`M 15-19 (ratio) estimate_lower`[index_imp])
                                                                    ,smr_ci_upper = as.numeric(estimates$`M 15-19 (ratio) estimate_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`F 15-19 (ratio) estimate`)
      estimates$`F 15-19 (ratio) estimate`[index_imp] <- random_gen(mean= as.numeric(estimates$`F 15-19 (ratio) estimate`[index_imp])
                                                                    ,smr_ci_lower = as.numeric(estimates$`F 15-19 (ratio) estimate_lower`[index_imp])
                                                                    ,smr_ci_upper = as.numeric(estimates$`F 15-19 (ratio) estimate_upper`[index_imp]) )

      index_imp <- !is.na(estimates$`Age-stand  M&F      0-14`)
      estimates$`Age-stand  M&F      0-14`[index_imp] <- random_gen(mean= as.numeric(estimates$`Age-stand  M&F      0-14`[index_imp])
                                                                    ,smr_ci_lower = as.numeric(estimates$`Age-stand  M&F      0-14_lower`[index_imp])
                                                                    ,smr_ci_upper = as.numeric(estimates$`Age-stand  M&F      0-14_upper`[index_imp]) )

    }

    # Adult curve ratios ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    adult_curve_ratios <- read_excel('data_internal/Country incidence.xlsx', 'Adult curve ratios',skip = 0)  # only using format , overrtie content.


    global_curve              <- run_adult_onset_surve(1980,3,FALSE,run_CI=random_data_ci)$global_z
    global_curve_sub_saharan  <- run_adult_onset_surve(1980,3,TRUE,run_CI=random_data_ci)$global_z

    # write.csv(data.frame(x= 0 :(length(global_curve)-1), y= global_curve ), "temp/global_curve.csv",row.names = FALSE)
    # adult_incidence_all_studies
    # global_curve              <- run_adult_onset_surve(1968,2,FALSE,run_CI=random_data_ci)$global_z
    # global_curve_sub_saharan  <- run_adult_onset_surve(1968,2,TRUE,run_CI=random_data_ci)$global_z

    # Extend for age 25 + with global curve -----,
    global_curve_sub_saharan[26:80] <- global_curve[26:80] * (sum(global_curve_sub_saharan[1:25]) / sum(global_curve[1:25]) )
    global_curve_sub_saharan        <- round(global_curve_sub_saharan/max(global_curve_sub_saharan)*100,0)
    # data.frame(x=1:length(global_curve), y= global_curve) %>%echarts4r::e_chart(x) %>%e_line(y)
    # data.frame(x=1:length(global_curve_sub_saharan), y= global_curve_sub_saharan) %>%echarts4r::e_chart(x) %>%e_line(y)
    adult_curve_ratios[,3:102]  <- 0
    adult_curve_ratios[,3:102]  <- data.frame(t(data.frame(global_curve)))
    adult_curve_ratios[3,3:82]  <- data.frame(t(data.frame(global_curve_sub_saharan)))

    colnames(adult_curve_ratios)[3:102] <- 0:99

    adult_curve_ratios[,83:102]  <- 0

    # version_no <- "0.4.10.adult_onset_zero"
    # print(paste0("sensitivity case: set adult onset ( >= 20) to 0" ))
    # adult_curve_ratios[,23:102] <- 0   # set adult onset to 0
    # write.csv(data.frame(x= 0 : 79 , y= as.numeric(t(adult_curve_ratios[3,]))[3:82] ), "temp/global_curve_sub.csv",row.names = FALSE)

    incidence_calcs <- read_excel('data_internal/Country incidence.xlsx', 'Incidence calcs',skip = 0)
    incidence_calcs <- incidence_calcs[!is.na(incidence_calcs$`World Bank name`),]

    incidence_calcs_new <- incidence_calcs[,c(1:7,17:23) ]

    # country rename
    incidence_calcs_new$`IDF Country Name`[!incidence_calcs_new$`IDF Country Name` %in% estimates$Country]
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Czech Republic"] <- "Czechia"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Federated States of Micronesia"] <- "Micronesia (Federated States of)"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Hong Kong China"] <- "Hong Kong"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Islamic Republic of Iran"] <- "Iran (Islamic Republic of)"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Lao People's Democratic Republic"] <- "Laos"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Macau China"] <- "Macao"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Macedonia"] <- "North Macedonia"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Republic of Congo"] <- "Democratic Republic of the Congo"
    # incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Sint Maarten"] <- "Czechia"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Swaziland"] <- "Eswatini"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Syrian Arab Republic"] <- "Syria"
    incidence_calcs_new$`IDF Country Name`[incidence_calcs_new$`IDF Country Name`== "Venezuela"] <- "Venezuela (Bolivarian Republic of)"


    # add country wd region wd income level
    incidence_calcs_new$world_bank_name <- incidence_calcs_new$`World Bank name`

    # ATLAS Add longitudinal data ---------------------------------------------------------------------------------------------
    # eddits on file,
    # 1.   column name  DJ change value from 1921 to 1925
    source('code_tools/impute_By_Age_Ratio_Naive.R')

    # Atlas     <- data.frame(read_excel('data_incidence/Atlas incidence studies v19 for Gabriel.xlsx', '2021 estimates',skip = 1),stringsAsFactors = F)
    # Atlas_fit <- read.csv('data_incidence/fit.csv', stringsAsFactors = F)
    # Atlas_fit <- Atlas_fit[Atlas_fit$Time>= Atlas_fit$year_2study_covered_start,]
    # Atlas_fit <- Atlas_fit[Atlas_fit$Time<= Atlas_fit$year_2study_covered_end,]
    #
    #
    # colnames(Atlas)[grepl("\\.\\.\\.",colnames(Atlas))] <- Atlas[1,grepl("\\.\\.\\.",colnames(Atlas))]
    # Atlas <- Atlas[2:nrow(Atlas),5:ncol(Atlas)]
    # Atlas$`Analysis group` <- as.numeric(Atlas$`Analysis group`)
    #
    #
    # if(FALSE)
    # {
    #   # check world bank name
    #   Atlas$`world bank name` [!tolower(Atlas$`world bank name`) %in% tolower(country$world_bank_name) ]
    # }


    Atlas_fit <- read.csv('data_incidence/fit.csv', stringsAsFactors = F)
    Atlas_fit <- Atlas_fit[Atlas_fit$Time>= Atlas_fit$year_2study_covered_start,]
    Atlas_fit <- Atlas_fit[Atlas_fit$Time<= Atlas_fit$year_2study_covered_end,]

    Atlas_fit$Time <- floor(Atlas_fit$Time)
    Atlas_fit <- data.frame(setDT(Atlas_fit)[,list(Ratio=mean(Ratio)),by=c("Time","analysis.group","Region","year_2study_covered_start","year_2study_covered_end") ])

    # extrapolate growth rate of all groups 1- 10  to 2021 -----
    for(i in c(1:10,22) )
    { # i <- 1
      Atlas_fit_t <- Atlas_fit[Atlas_fit$analysis.group==i,]
      Atlas_fit[Atlas_fit$analysis.group==i,]$year_2study_covered_end <- 2021
      years <- (max(Atlas_fit_t$Time)+1):2021
      Atlas_fit_add <- as.data.frame(lapply(Atlas_fit_t[1,], rep, length(years)))
      Atlas_fit_add$Time <- years
      Atlas_fit_add$year_2study_covered_end <- 2021
      Atlas_fit_add$Ratio <- mean(Atlas_fit_t$Ratio[Atlas_fit_t$Time>=min(years)-5])
      Atlas_fit <- rbind(Atlas_fit, Atlas_fit_add)
    }
    Atlas_fit <- Atlas_fit[order(Atlas_fit$Time),]
    Atlas_fit <- Atlas_fit[order(Atlas_fit$analysis.group),]

    if(FALSE)
    {
      # derive  global curve
      Atlas_fit_22 <- data.frame(setDT(Atlas_fit)[Time<=2021 & Time>=1985& analysis.group<=10,list(
        # Ratio_mean=mean(Ratio)
        #,Ratio_median=median(Ratio)
        Ratio=median(Ratio)
        # , analysis_group_used       = paste0(analysis.group,collapse=",")
        # , analysis_group_used_ratio = paste0(round(Ratio,3),collapse=",")
        ,year_2study_covered_start = 1985
        ,year_2study_covered_end   = 2021
      ),by="Time"])
      Atlas_fit_22$Region <- "All other countries"
      Atlas_fit_22$analysis.group <- 22

    }

    Atlas_fit_22 <- Atlas_fit[Atlas_fit$analysis.group==22,]
    # apply 22 curve  to groups 11+ -----------------------+ -----------------------+ -----------------------
    Atlas_fit_apply22_to_11_14 <- data.frame()
    for(i in 11:14 )
    {  # i <- 11
      Atlas_fit_t <- Atlas_fit[Atlas_fit$analysis.group==i,]
      range_t  <- range(Atlas_fit_t$Time)
      range_22 <- range(Atlas_fit_22$Time)
      range_together <- range(c(range_t,range_22))
      temp_rows <- data.frame()
      for(t in min(range_together) : max(range_together) )
      {
        # t <- 1980; print(t)
        if(sum(Atlas_fit_t$Time==t))
        {
          temp_rows <- rbind(temp_rows,Atlas_fit_t[Atlas_fit_t$Time==t,])
        }else if( (sum(Atlas_fit_22$Time==t) & t < min(range_t)) | (sum(Atlas_fit_22$Time==t) & t > max(range_t)))
        {
          temp_rows <-  rbind(temp_rows,Atlas_fit_22[Atlas_fit_22$Time==t,])
        }
        temp_rows$year_2study_covered_start <- min(temp_rows$year_2study_covered_start)
        temp_rows$year_2study_covered_end   <- max(temp_rows$year_2study_covered_end)
        temp_rows$Region <- unique(Atlas_fit_t$Region)
        temp_rows$analysis.group <- i
      }
      Atlas_fit_apply22_to_11_14 <- rbind(Atlas_fit_apply22_to_11_14,temp_rows)
    }

    Atlas_fit <- Atlas_fit[!Atlas_fit$analysis.group %in% 11:14,]
    Atlas_fit <- rbind(Atlas_fit, Atlas_fit_apply22_to_11_14)


    # Atlas_fit$Ratio <- 1  # 0.4.12.iot_no_growth no out of sample growth assumption

    #    # # version_no <- "0.4.10.iot_global_curve_for_all" ---------------------------------------------------------------------------------
    # Atlas_fit_22_all <- Atlas_fit_22
    #   for(i in 1:14)
    #   {
    #     Atlas_fit_22_t <- Atlas_fit_22
    #     Atlas_fit_22_t$analysis.group <- i
    #     Atlas_fit_22_all <- rbind(Atlas_fit_22_all, Atlas_fit_22_t)
    #   }
    # Atlas_fit <- Atlas_fit_22_all

    if(FALSE)
    {
      write.csv(Atlas_fit_22, "temp/Atlas_fit_22.csv")
      # plot for IOT Supp material draft 1, 27 Sep.
      Atlas_fit_22$Time <- as.character(Atlas_fit_22$Time)
      Atlas_fit_22$Ratio <- 100*((Atlas_fit_22$Ratio)-1)
      # data <- Atlas_fit_22
      data <- Atlas_fit[Atlas_fit$Region=="Northern Western Europe",]
      data  %>%
        e_charts(Time)  %>%
        e_step(Ratio,legend=FALSE)  %>%
        # e_title(unique(data_plot$world_bank_name)) %>%
        e_y_axis(min = 0.95,max = 1.05 )%>%
        e_x_axis(min = 1953,max = 2020 )%>%
        e_axis_labels( x = 'Year', y = 'Incidence change %'
        )
    }

    # write.csv(Atlas_fit_22,"temp/Atlas_fit_22.csv")
    # Atlas_fit <- rbind(Atlas_fit,Atlas_fit_22[,colnames(Atlas_fit)])
    # load Atlas incidence studies ----------------------------------------------------------------------------------------------------------------------------------------------------------

    Atlas     <- data.frame(read_excel('data_incidence/Atlas incidence studies v18 for indiv countries in Index JM fix for 0.4.5 CI.xlsx',skip = 1),stringsAsFactors = F)

    colnames(Atlas)[grepl("\\.\\.\\.",colnames(Atlas))] <- Atlas[1,grepl("\\.\\.\\.",colnames(Atlas))]
    Atlas <- Atlas[2:nrow(Atlas),5:ncol(Atlas)]
    Atlas$`Analysis group` <- as.numeric(Atlas$`Analysis group`)

    Atlas <- Atlas[!is.na(Atlas$`Analysis group`),-1]

    # replace analysis group from IDF -----------------
    for(i in 1:nrow(Atlas))
    {  # i <- 1
      index <- which(estimates_analysis_group$world_bank_name == Atlas[i,]$`world bank name`)
      Atlas$`Analysis group`[i] <- estimates_analysis_group$analysis.group[index]
      # print(paste0(i, " ",estimates_analysis_group$analysis.group[index]) )
    }

    if(FALSE)
    {# check world bank name
      Atlas$`world bank name` [!tolower(Atlas$`world bank name`) %in% tolower(country$world_bank_name) ]
    }

    # add interval to Atlas -----
    if(random_data_ci)
    {
      CI_Incidence <- read.csv("data_incidence/T1D Index CI mapping - CI_Incidence.csv",skip=1)
      CI_Incidence$row_id <- 1:nrow(CI_Incidence)
      CI_Incidence$Min.1 <- as.numeric(CI_Incidence$Min.1)
      CI_Incidence$Max.1 <- as.numeric(CI_Incidence$Max.1)
      CI_Incidence <- CI_Incidence[!is.na(CI_Incidence$Min.1),]
      CI_Incidence <- CI_Incidence[!is.na(CI_Incidence$Max.1),]
      CI_Incidence <- CI_Incidence[!is.na(CI_Incidence$Mean),]
      # CI_Incidence$world_bank_name <- CI_Incidence$world_bank_name

      # for(i in 1:nrow(CI_Incidence))
      # {
      #   index_match <- sapply(country$world_bank_name, grepl,CI_Incidence$Country[i])
      #   CI_Incidence$world_bank_name[i]<-  ifelse(sum(index_match), country$world_bank_name[ index_match], "NO MATCH")
      #   # CI_Incidence$world_bank_name[i]<-  ifelse(sum(index_match), country$world_bank_name[ index_match], CI_Incidence$world_bank_name[i])
      # }
      # write.csv(CI_Incidence,"CI_Incidence.csv")

      column_names <- c("world bank name",as.character(1920:2020) )
      Atlas_s <- Atlas[,column_names]
      Atlas_matching_report <- Atlas[,column_names]
      Atlas_matching_report[is.na(Atlas_matching_report)] <- ""
      Atlas_lower <- Atlas[,column_names]
      Atlas_upper <- Atlas[,column_names]
      Atlas_lower[] <- NA
      Atlas_upper[] <- NA

      ratio_lower_list <- c()
      ratio_upper_list <- c()

      for(i in 1:nrow(Atlas_s))
      {
        for(j in 2:ncol(Atlas_s))
        {  # i <- 62 ; j <- 64
          if(!is.na(Atlas_s[i,j]))
          {

            index_match <- CI_Incidence$Source %in% trimws(unlist(strsplit(Atlas$Source[i],","))) &
              CI_Incidence$world_bank_name == Atlas$`world bank name`[i] &
              CI_Incidence$Start   <= colnames(Atlas_s)[j]&
              CI_Incidence$End     >= colnames(Atlas_s)[j] &
              CI_Incidence$Min   ==0&
              CI_Incidence$Max   ==as.numeric(gsub("<","",Atlas$`Age range`))[i]-1 &
              CI_Incidence$Gender   =="M+F"

            if(sum(index_match)==0)
            {
              index_match <- CI_Incidence$Source %in% trimws(unlist(strsplit(Atlas$Source[i],","))) &
                CI_Incidence$world_bank_name == Atlas$`world bank name`[i] &
                CI_Incidence$Start   <= colnames(Atlas_s)[j]&
                CI_Incidence$End     >= colnames(Atlas_s)[j] &
                CI_Incidence$Min   ==0&
                CI_Incidence$Max   ==as.numeric(gsub("<","",Atlas$`Age range`))[i]-1
            }

            if(sum(index_match)==0)
            {
              index_match <- CI_Incidence$Source %in% trimws(unlist(strsplit(Atlas$Source[i],","))) &
                CI_Incidence$world_bank_name == Atlas$`world bank name`[i] &
                CI_Incidence$Start   <= colnames(Atlas_s)[j]&
                CI_Incidence$End     >= colnames(Atlas_s)[j]
            }
            if(sum(index_match)==0)
            {
              index_match <- CI_Incidence$Source %in% trimws(unlist(strsplit(Atlas$Source[i],","))) &
                CI_Incidence$world_bank_name == Atlas$`world bank name`[i]
            }

            if(sum(index_match)==0)
            {
              index_match <- CI_Incidence$Source %in% trimws(unlist(strsplit(Atlas$Source[i],",")))
            }

            if(sum(index_match)>0)
            {            # print(paste0("i: ",i) ); print(paste0("j: ",j) )
              CI_Incidence_t <- CI_Incidence[index_match,]
              ratio_lower <-  median(CI_Incidence_t$Min.1/CI_Incidence_t$Mean)
              ratio_upper <-  median(CI_Incidence_t$Max.1/CI_Incidence_t$Mean)
              # if(is.na(ratio_lower)){print( paste0("NA ratio: ",i," ",j) )}
              Atlas_lower[i,j] <- Atlas_s[i,j] * ratio_lower
              Atlas_upper[i,j] <- Atlas_s[i,j] * ratio_upper
              ratio_lower_list <- c(ratio_lower_list,ratio_lower)
              ratio_upper_list <- c(ratio_upper_list,ratio_upper)
              Atlas_matching_report[i,j] <- paste0(CI_Incidence[index_match,]$row_id+2,collapse = " | ")
            }else
            {
              Atlas_matching_report[i,j] <- 'NO'
            }
          }

        }
      }
      Atlas_matching_output <- Atlas
      Atlas_matching_output[,as.character(1920:2020)] <- Atlas_matching_report[, as.character(1920:2020)]
      # write.csv(Atlas_matching_output,"../temp/Atlas_matching_output.csv")

      # print(paste0(" Number matched : "  , sum(Atlas_matching_report!="NO" & Atlas_matching_report!="")  ))
      # print(paste0(" Number notched : "  , sum(Atlas_matching_report=="NO" )  ))




      ratio_lower_median <- median(ratio_lower_list)
      ratio_upper_median <- median(ratio_upper_list)
      for(i in 1:nrow(Atlas_s))
      {
        for(j in 2:ncol(Atlas_s))
        {  # i <- 65 ; j <- 92
          if(!is.na(Atlas_s[i,j]) & is.na(Atlas_lower[i,j]) )
          {
            Atlas_lower[i,j] <- Atlas_s[i,j] * ratio_lower_median
            Atlas_upper[i,j] <- Atlas_s[i,j] * ratio_upper_median
          }
        }
      }


      # impute ---
      Atlas_s$`world bank name` <- NA
      index_impute <- !is.na((Atlas_s) )

      Atlas_s[index_impute] <- random_gen(mean =Atlas_s[index_impute], smr_ci_lower = Atlas_lower[index_impute],smr_ci_upper = Atlas_upper[index_impute]  )
      Atlas[,as.character(1920:2020)] <- Atlas_s[,as.character(1920:2020)]

    }





    # Impute missing values incidence over time Graham, Gabriel ------------------------
    Atlas_imputed <- data.frame()
    analysis_group <- unique(Atlas$`Analysis group`)

    for(i in 1:length(analysis_group))
    {
      # i <- 11
      Atlas_temp <- Atlas[Atlas$`Analysis group`==i,]
      Atlas_matrix <- Atlas_temp[,as.character(1920:2020)]
      matrix <- data.frame(matrix(NA, nrow = nrow(Atlas_matrix), ncol = length(1900:2040)))
      colnames(matrix) <- as.character(1900:2040)
      matrix[,as.character(1920:2020)] <- Atlas_matrix
      # plot_Heatmap(matrix,"raw data")
      # spline extrapolate intermidiate values ---------------------
      for(p in 1:nrow(matrix))
      {
        # p <- 17
        row_filled <- spline(x=colnames(matrix),y=matrix[p,],method="natural") # use this one to define window
        row_filled <- spline(x=colnames(matrix),y=matrix[p,],method="natural",xout= range(row_filled$x)[1]:range(row_filled$x)[2])

        row_filled <- data.frame(x=row_filled$x,y=row_filled$y)
        row_filled$year <- round(row_filled$x,0)
        row_filled <- setDT(row_filled)[,list(value= y[which.min(abs(x-year))]),by="year"]

        matrix[p, as.character(row_filled$year) ] <-  row_filled$value
      }
      # plot_Heatmap(matrix,"fill intermidiate values")
      # extrapolate backward and forward based on fitted line  ---------------------

      Atlas_fit_temp <- Atlas_fit[Atlas_fit$analysis.group==i,]
      if(nrow(Atlas_fit_temp) >0)
      {
        Atlas_fit_temp$Time <- floor(Atlas_fit_temp$Time)
        Atlas_fit_temp <- data.frame(setDT(Atlas_fit_temp)[,list(Ratio=mean(Ratio)),by="Time"])
        rownames(Atlas_fit_temp) <- as.character(Atlas_fit_temp$Time)

        row_filled <- spline(x=Atlas_fit_temp$Time,y=Atlas_fit_temp$Ratio,method="natural") # use this one to define window
        row_filled <- spline(x=Atlas_fit_temp$Time,y=Atlas_fit_temp$Ratio,method="natural",xout= range(row_filled$x)[1]:range(row_filled$x)[2]) # use this one to define window

        Atlas_fit_temp <- data.frame(Time = row_filled$x, Ratio = row_filled$y  )
        rownames(Atlas_fit_temp) <- as.character(Atlas_fit_temp$Time)

        Atlas_fit_temp$Ratio_projected  <- NA
        for(r in 1:nrow(Atlas_fit_temp))
        {
          Atlas_fit_temp$Ratio_projected[r] <- prod(Atlas_fit_temp$Ratio[1:r])
        }
        # Atlas_fit_temp$rate <- NA
        for(p in 1:nrow(matrix))
        {
          # p <- 1

          matrix_t <- matrix[p,]
          matrix_t_names <- names(matrix_t)[!is.na(matrix_t)]
          Atlas_fit_temp$rate <- NA
          Atlas_fit_temp[matrix_t_names,]$rate <- (matrix_t)[!is.na(matrix_t)]
          Atlas_fit_temp <- Atlas_fit_temp[!is.na(Atlas_fit_temp$Time),]

          year_min <- min(matrix_t_names)
          year_max <- max(matrix_t_names)

          value_min  <-  mean( Atlas_fit_temp[as.character( (as.numeric(year_min)+5): (as.numeric(year_min))  ),]$rate)
          value_max  <-  mean( Atlas_fit_temp[as.character( (as.numeric(year_max)-5): (as.numeric(year_max))  ),]$rate)

          Atlas_fit_temp$rate_extrapolate <- ifelse(Atlas_fit_temp$Time<year_min
                                                    , value_min * Atlas_fit_temp$Ratio_projected/Atlas_fit_temp[as.character( as.numeric(year_min)+2),]$Ratio_projected
                                                    , Atlas_fit_temp$rate )


          Atlas_fit_temp$rate_extrapolate <- ifelse(Atlas_fit_temp$Time>year_max
                                                    , value_max * Atlas_fit_temp$Ratio_projected/Atlas_fit_temp[as.character( as.numeric(year_max)-2),]$Ratio_projected
                                                    , Atlas_fit_temp$rate_extrapolate )
          matrix[p, as.character(Atlas_fit_temp$Time) ] <-  Atlas_fit_temp$rate_extrapolate
        }
        # plot_Heatmap(matrix,"fill backward/forward in curve")
      }
      # applty Global curve 22  --------------

      # Keep values before and after fit curve constant --------------

      for(p in 1:nrow(matrix))
      {
        # p <- 1
        matrix_t <- matrix[p,]
        matrix_t_names <- names(matrix_t)[!is.na(matrix_t)]
        year_min <- min(matrix_t_names)
        year_max <- max(matrix_t_names)

        matrix_t[names(matrix_t) <= year_min] <-   matrix_t[year_min]
        matrix_t[names(matrix_t) >= year_max] <-   matrix_t[year_max]

        matrix[p,] <- matrix_t
      }

      # average by country --------------------------------------------------
      matrix$world_bank_name <-Atlas_temp$`world bank name`
      # plot_Heatmap(matrix,"average by country")
      # print(i)
      # print(sum(is.na(matrix_avg)))
      Atlas_imputed <- rbind(Atlas_imputed, matrix)
    }

    Atlas_imputed <- Atlas_imputed %>%
      group_by(world_bank_name) %>%
      summarise_all(mean)

    # write.csv(Atlas_imputed,"temp/Atlas_imputed.csv")
    # plot_Heatmap((dplyr::select(Atlas_imputed,-world_bank_name)),"Atlas_imputed")
    # incidence_calcs_new$`Start year` <- NA
    # incidence_calcs_new$`End year`   <- NA
    # Generate adult rate  -----------------------------  -----------------------------  -----------------------------  -----------------------------  -----------------------------
    incidence_rate_year_age <- data.frame()
    rownames(estimates) <- 1:nrow(estimates)
    estimates$analysis_group[is.na(estimates$analysis_group) ] <- 22

    for(i in 1:nrow(estimates))
    { # i <- 42
      # print(paste0(i," " ,estimates$Country[i] ))
      estimates_index            <- i
      estimates_index_reference  <- which(estimates$Country== estimates$Country.used.to.make.extrapolations..0.14.[estimates_index])

      if(length(estimates_index ) |  length(estimates_index_reference ))
      {
        yea_range <- ifelse(length(estimates_index_reference),estimates$Period[estimates_index_reference],estimates$Period[estimates_index])
        year <-ceiling( median( as.numeric(unlist(strsplit(yea_range,'-')))) )
        # incidence_calcs_new$year[i] <- year

        m_0_4 <-  ifelse(length(estimates_index_reference),estimates$`M         0-4`[estimates_index_reference],estimates$`M         0-4`[estimates_index])
        f_0_4 <-  ifelse(length(estimates_index_reference),estimates$`F         0-4`[estimates_index_reference],estimates$`F         0-4`[estimates_index])

        m_5_9   <-  ifelse(length(estimates_index_reference),estimates$`M        5-9`[estimates_index_reference],estimates$`M        5-9`[estimates_index])
        f_5_9   <-  ifelse(length(estimates_index_reference),estimates$`F        5-9`[estimates_index_reference],estimates$`F        5-9`[estimates_index])

        m_10_14 <-  ifelse(length(estimates_index_reference),estimates$`M       10-14`[estimates_index_reference],estimates$`M       10-14`[estimates_index])
        f_10_14 <-  ifelse(length(estimates_index_reference),estimates$`F       10-14`[estimates_index_reference],estimates$`F       10-14`[estimates_index])

        m_15_19 <-  ifelse(length(estimates_index_reference),estimates$`M 15-19 (ratio) estimate`[estimates_index_reference],estimates$`M 15-19 (ratio) estimate`[estimates_index])
        f_15_19 <-  ifelse(length(estimates_index_reference),estimates$`F 15-19 (ratio) estimate`[estimates_index_reference],estimates$`F 15-19 (ratio) estimate`[estimates_index])

        mf_0_14 <- ifelse(length(estimates_index_reference),estimates$`Age-stand  M&F      0-14`[estimates_index_reference],estimates$`Age-stand  M&F      0-14`[estimates_index])

        mf_0_4   <- ifelse(!is.na(m_0_4)  , (as.numeric(m_0_4)   + as.numeric(f_0_4))/2  , as.numeric(mf_0_14)  )
        mf_5_9   <- ifelse(!is.na(m_5_9)  , (as.numeric(m_5_9)   + as.numeric(f_5_9))/2  , as.numeric(mf_0_14)  )
        mf_10_14 <- ifelse(!is.na(m_10_14), (as.numeric(m_10_14) + as.numeric(f_10_14))/2, as.numeric(mf_0_14)  )
        mf_15_19 <- ifelse(!is.na(m_15_19), (as.numeric(m_15_19) + as.numeric(f_15_19))/2, as.numeric(mf_0_14)  )

        #  calculate adult incidence
        mf_0_14 <- round(mean(c(mf_0_4,mf_5_9,mf_10_14)),2)
        mf_0_19 <- round(mean(c(mf_0_4,mf_5_9,mf_10_14,mf_15_19)),2)

        wb_Region <- ifelse(length(estimates_index_reference),estimates$`World Bank Region`[estimates_index_reference],estimates$`World Bank Region`[estimates_index])

        adult_curve_index <- which(adult_curve_ratios$Region == wb_Region)
        base              <- adult_curve_ratios$Base[adult_curve_index]
        # print(base)
        curve_base        <- ifelse(base=="0-19", mf_0_19,mf_0_14 )
        # incidence_calcs_new$curve_base[i] <- curve_base
        adult_rate_temp <- adult_curve_ratios[adult_curve_index,c(-1,-2)]

        if(base=="0-14")
        {
          adult_rate_temp[16:length(adult_rate_temp)] <- adult_rate_temp[16:length(adult_rate_temp)]/ (sum(adult_rate_temp[1:15])/15)
          adult_rate_temp <- round(adult_rate_temp * curve_base,2)
          adult_rate_temp[as.character(0:4)] <- mf_0_4
          adult_rate_temp[as.character(5:9)] <- mf_5_9
          adult_rate_temp[as.character(10:14)] <- mf_10_14

        }else if (base=="0-19")
        {
          adult_rate_temp[21:length(adult_rate_temp)] <- adult_rate_temp[21:length(adult_rate_temp)]/ (sum(adult_rate_temp[1:20])/20)
          adult_rate_temp <- round(adult_rate_temp * curve_base,2)
          adult_rate_temp[as.character(0:4)] <- mf_0_4
          adult_rate_temp[as.character(5:9)] <- mf_5_9
          adult_rate_temp[as.character(10:14)] <- mf_10_14
          adult_rate_temp[as.character(15:19)] <- mf_15_19
        }

        # write.csv(data.frame(x= 0 :79 , y= t(adult_rate_temp)[1:80] ), "temp/global_curve_Rwanda.csv",row.names = FALSE)

        # --- add incidence over time curve -----------------------------
        Country_to_use         <-  ifelse(length(estimates_index_reference), estimates$Country[estimates_index_reference ], estimates$Country[estimates_index ] )
        analysis_group_to_use  <-  ifelse(length(estimates_index_reference), estimates$analysis_group[estimates_index_reference ], estimates$analysis_group[estimates_index ] )
        world_bank_name_to_use <- ifelse(length(estimates_index_reference)
                                         , estimates_analysis_group$world_bank_name[estimates_index_reference ]
                                         , estimates_analysis_group$world_bank_name[estimates_index ] )
        # Country_to_use         <-  estimates$Country[estimates_index ]
        # analysis_group_to_use  <-  estimates$analysis_group[estimates_index ]
        # world_bank_name_to_use <-  estimates_analysis_group$world_bank_name[estimates_index ]

        # estimates_analysis_group$world_bank_name[i]

        index_atlas <- which(Atlas_imputed$world_bank_name == world_bank_name_to_use )

        if(estimates$analysis_group[estimates_index ]==22)
        {
          index_atlas <- NULL
          analysis_group_to_use <- 22
        }

        adult_rate_full <- data.frame(matrix(NA, nrow = length(1900:2040), ncol = 100))
        colnames(adult_rate_full) <- 0:99
        rownames(adult_rate_full) <- 1900:2040

        if(length(index_atlas>0))
        {
          Atlas_temp <- Atlas_imputed[index_atlas,-1]

          ratio <- Atlas_temp / curve_base

          for( q in 1:nrow(adult_rate_full) )
          {
            adult_rate_full[q,] <- adult_rate_temp * as.numeric( ratio[q])
          }

        }else if( (analysis_group_to_use %in% Atlas_fit$analysis.group)  )
        {

          # print("in group")
          Atlas_fit_temp <- Atlas_fit[Atlas_fit$analysis.group==analysis_group_to_use,]

          if(! year %in% Atlas_fit_temp$Time)
          {
            Atlas_fit_temp <- Atlas_fit[Atlas_fit$analysis.group==22,]

          }

          Atlas_fit_temp$Time <- floor(Atlas_fit_temp$Time)
          Atlas_fit_temp <- data.frame(setDT(Atlas_fit_temp)[,list(Ratio=mean(Ratio)),by="Time"])
          rownames(Atlas_fit_temp) <- as.character(Atlas_fit_temp$Time)

          row_filled <- spline(x=Atlas_fit_temp$Time,y=Atlas_fit_temp$Ratio,method="natural") # use this one to define window
          row_filled <- spline(x=Atlas_fit_temp$Time,y=Atlas_fit_temp$Ratio,method="natural",xout= range(row_filled$x)[1]:range(row_filled$x)[2]) # use this one to define window

          Atlas_fit_temp <- data.frame(Time = row_filled$x, Ratio = row_filled$y  )
          rownames(Atlas_fit_temp) <- as.character(Atlas_fit_temp$Time)

          # keep constant till 2040 ---
          Atlas_fit_2040 <- data.frame(Time=(max(Atlas_fit_temp$Time)+1):2040,Ratio=1 )
          rownames(Atlas_fit_2040) <- Atlas_fit_2040$Time
          Atlas_fit_temp <- rbind(Atlas_fit_temp, Atlas_fit_2040)
          #  ---

          Atlas_fit_temp$Ratio_projected  <- NA

          for(r in 1:nrow(Atlas_fit_temp))
          {
            Atlas_fit_temp$Ratio_projected[r] <- prod(Atlas_fit_temp$Ratio[1:r])
          }

          Atlas_fit_temp$rate <- NA
          Atlas_fit_temp[as.character(year),]$rate <- curve_base
          Atlas_fit_temp <- Atlas_fit_temp[!is.na(Atlas_fit_temp$Time),]

          year_min <- as.character(min(year))
          year_max <- as.character(max(year))

          Atlas_fit_temp$rate_extrapolate <- ifelse(Atlas_fit_temp$Time<year_min, Atlas_fit_temp[year_min,]$rate* Atlas_fit_temp$Ratio_projected/Atlas_fit_temp[year_min,]$Ratio_projected,Atlas_fit_temp$rate )
          Atlas_fit_temp$rate_extrapolate <- ifelse(Atlas_fit_temp$Time>year_max, Atlas_fit_temp[year_max,]$rate* Atlas_fit_temp$Ratio_projected/Atlas_fit_temp[year_max,]$Ratio_projected,Atlas_fit_temp$rate_extrapolate )

          # Keep values before and after fit curve constant --------------
          matrix <- data.frame(matrix(NA, nrow = 1, ncol = length(1900:2040)))
          colnames(matrix) <- 1900:2040
          matrix[,as.character(Atlas_fit_temp$Time)] <- Atlas_fit_temp$rate_extrapolate

          for(p in 1:nrow(matrix))
          {
            # p <- 1
            matrix_t <- matrix[p,]
            matrix_t_names <- names(matrix_t)[!is.na(matrix_t)]
            year_min <- min(matrix_t_names)
            year_max <- max(matrix_t_names)
            matrix_t[names(matrix_t) <= year_min] <-   matrix_t[year_min]
            matrix_t[names(matrix_t) >= year_max] <-   matrix_t[year_max]
            matrix[p,] <- matrix_t
          }

          ratio <- matrix / curve_base

          for( q in 1:nrow(adult_rate_full) )
          {
            adult_rate_full[q,] <- adult_rate_temp * as.numeric( ratio[q])
          }

        }else
        {
          for( q in 1:nrow(adult_rate_full) )
          {
            adult_rate_full[q,] <- adult_rate_temp
          }

          # print(paste0(i," country : ",estimates_analysis_group$world_bank_name[i], "  using replacement country: ",Country_to_use , " year ",year) )
        }

        adult_rate_full <- cbind(idf_country_name=estimates_analysis_group$Country[i]
                                 ,world_bank_name=estimates_analysis_group$world_bank_name[i]
                                 ,year= row.names(adult_rate_full)
                                 ,adult_rate_full)

        incidence_rate_year_age <- rbind(incidence_rate_year_age,adult_rate_full)

      }else
      {
        # print(paste0(estimates[i,]$Country,"  not in estimates 2021"))
      }

    }

    # write.csv(incidence_rate_year_age[incidence_rate_year_age$year>=1920,], "incidence_over_year_age.csv")
    # saveRDS(incidence_calcs_new,"data_incidence/incidence_calcs_new.Rds")
    # incidence_calcs_new <- dplyr::select(incidence_calcs_new,"World Bank name",year,"0-4", "5-9","10-14","15-19" )

    incidence_curve <-  incidence_rate_year_age %>%
      pivot_longer(-(1:3), names_to='age', values_to='incidence_rate') %>%
      # separate(age_range, sep='-', into=c('age_from','age_to')) %>%
      inner_join(dplyr::select(country,world_bank_name,loc_id, wd_region), by='world_bank_name') %>%
      dplyr::select(loc_id,wd_region,year,age,incidence_rate)

    incidence_curve$year   <- as.numeric(incidence_curve$year)
    incidence_curve$loc_id <- as.numeric(incidence_curve$loc_id)
    incidence_curve$age    <- as.numeric(incidence_curve$age)


    # incidence_curve_thinkcell <- setDT(incidence_curve)[incidence_curve$year==2021,list(incidence_rate=mean(incidence_rate)),by=c("wd_region","age") ]
    # incidence_curve_thinkcell <- spread(incidence_curve_thinkcell, wd_region, incidence_rate)
    # write.csv(incidence_curve_thinkcell,"temp/incidence_curve_thinkcell_region.csv",row.names = FALSE)


    incidence_curve$wd_region    <- NULL

    # expand to single age ---------------------------------------------------------------------------
    # incidence_calcs_new$`JDRF model <20y incidence rates` <- round(as.numeric(incidence_calcs_new$`JDRF model <20y incidence rates`),2)
    # incidence_calcs_new$...18 <- round(as.numeric(incidence_calcs_new$...18),2)
    # incidence_calcs_new$...19 <- round(as.numeric(incidence_calcs_new$...19),2)
    # incidence_calcs_new$...20 <- round(as.numeric(incidence_calcs_new$...20),2)
    # Generate Adult incidence    -----------------------------

    # incidence_growth_rate <- read_excel('data_internal/incidence.xlsx', 'growth_rates') %>%
    #   inner_join(country, by='world_bank_name') %>%
    #   dplyr::select(loc_id, past_growth_rate, future_growth_rate)
    # Dump out to disk
    if(FALSE)
    {
      write_parquet(incidence_curve,"data_incidence/incidence_curve.binary")
      # write_parquet(country        ,"data_incidence/country.binary")
    }
  },
  error = function(cond) {
    sink("log.txt",append=TRUE);cat(paste0("  ",  cond, " \n") );sink()
  },
  finally = {
  })
  return(list(incidence_curve=incidence_curve,global_curve=global_curve,global_curve_sub_saharan=global_curve_sub_saharan) )
}


if(FALSE)
{
  library(arrow)
  library(data.table)
  library(parallel)
  library(dplyr)
  library(echarts4r)
  # MCMC incidenc run ----

  sink("log.txt",append=TRUE);cat(paste0(Sys.time()," mcmc_incidence  Start \n") );sink()
  mcmc_incidence <- function(i)
  { # i <- 1
    tryCatch({

      # sink("log.txt",append=TRUE);cat(paste0(Sys.time()," mcmc_incidence, process id: ", i ," \n") );sink()
      library(arrow)
      # i <- 1
      source('code_R_data_prep/000_0_3_incidence_rate.R')
      get_Incidence_Full_result <- get_Incidence_Full(random_data_ci=TRUE)
      incidence_curve                  <- get_Incidence_Full_result$incidence_curve
      global_curve                     <- get_Incidence_Full_result$global_curve
      global_curve_sub_saharan         <- get_Incidence_Full_result$global_curve_sub_saharan
      write_parquet(incidence_curve           ,paste0("../../mcmc_incidence/incidence_curve_mcmc_",i,".binary"))
      write_parquet(data.frame(curve=global_curve)              ,paste0("../../mcmc_incidence/global_curve_",i,".binary"))
      write_parquet(data.frame(curve=global_curve_sub_saharan)  ,paste0("../../mcmc_incidence/global_curve_sub_saharan_",i,".binary"))

      # sink("log.txt",append=TRUE);cat(paste0(Sys.time()," mcmc_incidence, process id: ", i ," finished \n") );sink()
    },
    error = function(cond) { sink("log.txt",append=TRUE);cat(paste0("  ",  cond, " \n") );sink() }, finally = {})

  }

  num_runs <- 1000  ;  num_cors <- 120   # has to be dividable by 10.
  # num_runs <- 1000  ;  num_cors <- 30   # for local desktop

  num_thread <- num_cors
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c())
  system.time({a <- clusterApply(clust, 361:num_runs, mcmc_incidence)})
  stopCluster(clust)
  Sys.time()

  # if(FALSE)
  # {
  #   #  SparkR install guild: https://phoenixnap.com/kb/install-spark-on-windows-10
  #   library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
  #   sparkR.session(master = "local[30]", sparkConfig = list(
  #     spark.executor.memory='1G'
  #     ,spark.driver.memory='2G'
  #     # ,spark.executor.rpc.netty.dispatcher.numThreads = 10
  #     ,spark.serializer="org.apache.spark.serializer.KryoSerializer"
  #     # ,spark.sql.execution.arrow.sparkr.enabled    =TRUE
  #   ))
  #   # sparkR.session.stop()
  #   system.time({SparkR::spark.lapply(1:30,mcmc_incidence)})
  #   detach("package:SparkR", unload = TRUE)
  # }

  # partition to by country -----
  num_thread <- 25
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c())
  system.time({a <- parLapply(clust, 1:(1000/20), function(i){
    # i <- 10
    library(arrow)
    library(data.table)
    files_db       <- c(paste0("../../mcmc_incidence/incidence_curve_mcmc_",( (i-1)*20+1 ):((i-1)*20+20),".binary"))
    data_partition <- rbindlist( lapply(files_db, read_parquet))

    loc_id_list <- unique(data_partition$loc_id)
    for(j in 1:length(loc_id_list))
    {
      write_parquet(data_partition[data_partition$loc_id==loc_id_list[j],]           ,paste0("../../mcmc_incidence_by_country/incidence_curve_mcmc_",i,"_loc_id_",j,".binary"))
    }
  })})
  stopCluster(clust)
  Sys.time()

  # aggregate  by country -----

  num_thread <- 50
  clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
  clusterExport(cl=clust, varlist=c())
  system.time({a <- parLapply(clust, 1:201, function(i){
    # i <- 10
    library(arrow)
    library(data.table)
    files_db       <- c(paste0("../../mcmc_incidence_by_country/incidence_curve_mcmc_",1:50,"_loc_id_",i,".binary"))
    data_partition <- rbindlist( lapply(files_db, read_parquet))
    incidence_curve_ci  <- setDT(data_partition)[,as.list(quantile(incidence_rate,probs=c(.025,.5,.975))),by=c("loc_id","year","age") ]
    write_parquet(incidence_curve_ci  ,paste0("../../mcmc_incidence_c_aggregate/incidence_curve_mcmc_",unique(incidence_curve_ci$loc_id),".binary"))
  })})
  stopCluster(clust)
  Sys.time()

 # merge country and save ------
  file_list <- list.files("../../mcmc_incidence_c_aggregate/",full.names = TRUE)
  incidence_curve_ci <- rbindlist( lapply(file_list, read_parquet))
  colnames(incidence_curve_ci)[4] <- "lower"
  colnames(incidence_curve_ci)[5] <- "value_median"
  colnames(incidence_curve_ci)[6] <- "upper"

  # write_parquet(incidence_curve_ci  ,paste0("data_incidence/incidence_curve_ci.binary"))

  if(FALSE)
  {

    incidence_curve_ci <- arrow::read_parquet(paste0("data_incidence/incidence_curve_ci.binary"))
    # plottings _------------------------------------------------------------------------------------------------------------------
    country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')

    # country_name <- "United States" ; year_at <- 2015
    # country_name <- "Brazil" ; year_at <- 2011
    # country_name <- "India" ; year_at <- 2009
    # country_name <- "Rwanda" ; year_at <- 2009
    # loc_id <- country$loc_id[country$world_bank_nam=="India"]
    # loc_id <- country$loc_id[country$world_bank_nam=="India"]
    # loc_id <- country$loc_id[country$world_bank_nam=="India"]
    # loc_id <- country$loc_id[country$world_bank_nam=="India"]
    loc_id <- country$loc_id[country$world_bank_nam==country_name]
    data_plot  <- setDF(incidence_curve_ci)[incidence_curve_ci$loc_id==loc_id & incidence_curve_ci$year==year_at,]


    e3 <- data_plot %>%
      e_charts(age) %>%
      e_line(value_median,name="Value") %>%
      e_band(lower,upper,
             stack = "confidence-band",
             symbol = c("none", "none"),)%>%e_x_axis(name="Age")%>%e_y_axis(name=country_name) %>%
      e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)

    write.csv(data_plot,paste0("temp/adult_incidence_",country_name,"_ci.csv") ,row.names = FALSE)

     # GLOBAL curve global ---------------------------------------------------------------------------------------------------------------------------------

    file_list <- list.files("../../mcmc_incidence/",full.names = TRUE)
    file_list <- file_list[grepl("global_curve_",file_list)]
    file_list <- file_list[!grepl("global_curve_sub_saharan_",file_list)]
    data      <- setDF(rbindlist( lapply(file_list,function(x){df <- read_parquet(x);df$age<- 0:99;df$run<- x; df} )))
    country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')

    data_plot  <- setDT(data)[,list(value_median= median(curve)
                                    ,lower= quantile(curve,probs=c(.025))
                                    ,upper= quantile(curve,probs=c(.975))
                                    ,n_run=.N),by="age"]

    e_age <- data_plot %>%
      e_charts(age) %>%
      e_line(value_median,name="Value") %>%
      e_band(lower,upper,
             stack = "confidence-band",
             symbol = c("none", "none"),)%>%e_x_axis(name="Age")%>%e_y_axis(name="GLOBAL age curve") %>%
      e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)

    write.csv(data_plot,paste0("temp/adult_incidence_global_curve_ci.csv") ,row.names = FALSE)

    # GLOBAL curve sub saharan ---------------------------------------------------------------------------------------------------------------------------------
    file_list <- list.files("../../mcmc_incidence/",full.names = TRUE)
    # file_list <- file_list[grepl("global_curve_",file_list)]
    file_list <- file_list[grepl("global_curve_sub_saharan_",file_list)]
    data      <- setDF(rbindlist( lapply(file_list,function(x){df <- read_parquet(x);df$age<- 0:99;df$run<- x; df} )))
    country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')

    data_plot  <- setDT(data)[,list(value_median= median(curve)
                                    ,lower= quantile(curve,probs=c(.025))
                                    ,upper= quantile(curve,probs=c(.975))
                                    ,n_run=.N),by="age"]

    e_age <- data_plot %>%
      e_charts(age) %>%
      e_line(value_median,name="Value") %>%
      e_band(lower,upper,
             stack = "confidence-band",
             symbol = c("none", "none"),)%>%e_x_axis(name="Age")%>%e_y_axis(name="GLOBAL age curve") %>%
      e_tooltip(trigger = "axis") %>% e_legend(bottom = 0)

    write.csv(data_plot,paste0("temp/adult_incidence_subsaharan_curve_ci.csv") ,row.names = FALSE)




  }


}
