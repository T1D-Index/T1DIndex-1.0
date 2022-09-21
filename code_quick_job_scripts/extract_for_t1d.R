extract_for_t1d <- function(result,young_range,  adults_range,  older_range , years ,input_lever )
{
  # years <- c(seq(from = 1960, to = 2020, by = 5),2021)
 # young_range=c(0,19); adults_range= c(20,66); older_range=c(67,99); years= c(seq(from = 1960, to = 2020, by = 5),2021) ; input_lever <-
  input_lever <-  as.numeric(substring(input_lever,1,1))
  aggregate_by_year <- function(data_frame,prefix)
  {
    data_frame_output <- t(setDT( data_frame)[Year %in% years,list(
      Total  = sum(Value[Age >= 0  & Age <=99])
      ,Young = sum(Value[Age >= young_range[1]   & Age <=young_range[2]])
      ,Adults= sum(Value[Age >= adults_range[1]  & Age <=adults_range[2]])
      ,Older = sum(Value[Age >= older_range[1]   & Age <=older_range[2]])
    ), by = c("Year")])

    colnames(data_frame_output) <- data_frame_output[1,]
    data_frame_output <- as.data.frame(data_frame_output[-1,])

    data_frame_output <- cbind(` `=rownames(data_frame_output) ,data_frame_output )
    data_frame_output <- cbind(` ` = prefix,data_frame_output )
    data_frame_output <- rbind(data_frame_output, "")
    return(data_frame_output)
  }

  aggregate_by_year_per100k <- function(data_frame,prefix,per100k=100000)
  {
    data_frame_output <- round(t(setDT( data_frame)[Year %in% years,list(
      Total=  sum(Value1[Age >= 0  & Age <=99])/sum(Value2[Age >= 0  & Age <=99])
      ,Young= sum(Value1[Age >= young_range[1]  & Age <=young_range[2]])/sum(Value2[Age >= young_range[1]  & Age <=young_range[2]])
      ,Adults= sum(Value1[Age >= adults_range[1]  & Age <=adults_range[2]])/sum(Value2[Age >= adults_range[1]  & Age <=adults_range[2]])
      ,Older= sum(Value1[Age >= older_range[1]   & Age <=older_range[2]])/sum(Value2[Age >= older_range[1]   & Age <=older_range[2]])
    ), by = c("Year")]) * per100k,2)

    colnames(data_frame_output) <- years
    data_frame_output <- as.data.frame(data_frame_output[-1,])

    data_frame_output <- cbind(` `=rownames(data_frame_output) ,data_frame_output )
    data_frame_output <- cbind(` ` = prefix,data_frame_output )
    data_frame_output <- rbind(data_frame_output, "")
    return(data_frame_output)
  }


  export_csv <- data.frame()

  # Burden --------------- --------------- --------------- --------------- --------------- --------------- ---------------

  JDRF_calculations <- extract_for_purpose(result,0,99,2021)
  Burden <- JDRF_calculations$days_1
  # eval(parse(text = paste0("Burden <- JDRF_calculations$days_",input_lever)))
  Burden_Total <- Burden[Burden$Year %in% years,]

  JDRF_calculations <- extract_for_purpose(result,young_range[1],young_range[2],2021)
  # eval(parse(text = paste0("Burden <- JDRF_calculations$days_",input_lever)))
  Burden <- JDRF_calculations$days_1

  Burden_Young <- Burden[Burden$Year %in% years,]

  JDRF_calculations <- extract_for_purpose(result,adults_range[1],adults_range[2],2021)
  # eval(parse(text = paste0("Burden <- JDRF_calculations$days_",input_lever)))
  Burden <- JDRF_calculations$days_1

  Burden_Adults <- Burden[Burden$Year %in% years,]

  JDRF_calculations <- extract_for_purpose(result,older_range[1],older_range[2],2021)
  # eval(parse(text = paste0("Burden <- JDRF_calculations$days_",input_lever)))
  Burden <- JDRF_calculations$days_1

  Burden_Older <- Burden[Burden$Year %in% years,]


  Burden_days <- as.data.frame( t(data.frame(Total = Burden_Total$`Days lost (diagnosed ghosts)`
                                             ,Young = Burden_Young$`Days lost (diagnosed ghosts)`
                                             ,Adults = Burden_Adults$`Days lost (diagnosed ghosts)`
                                             ,Older = Burden_Older$`Days lost (diagnosed ghosts)`
  )))
  colnames(Burden_days) <- years
  Burden_days <- cbind(` `=rownames(Burden_days) ,Burden_days )
  Burden_days <- cbind(` ` = "Days lost (diagnosed ghosts)",Burden_days )
  Burden_days_lost_diagnosed_ghost <- rbind(Burden_days, "")

  Burden_days <- as.data.frame( t(data.frame(Total = Burden_Total$`Days lost (undiagnosed ghosts)`
                                             ,Young = Burden_Young$`Days lost (undiagnosed ghosts)`
                                             ,Adults = Burden_Adults$`Days lost (undiagnosed ghosts)`
                                             ,Older = Burden_Older$`Days lost (undiagnosed ghosts)`
  )))
  colnames(Burden_days) <- years
  Burden_days <- cbind(` `=rownames(Burden_days) ,Burden_days )
  Burden_days <- cbind(` ` = "Days lost (undiagnosed ghosts)",Burden_days )
  Burden_days_lost_undiagnosed_ghost <- rbind(Burden_days, "")

  Burden_days <- as.data.frame( t(data.frame(Total = Burden_Total$`Days lost (treatment)`
                                             ,Young = Burden_Young$`Days lost (treatment)`
                                             ,Adults = Burden_Adults$`Days lost (treatment)`
                                             ,Older = Burden_Older$`Days lost (treatment)`
  )))
  colnames(Burden_days) <- years
  Burden_days <- cbind(` `=rownames(Burden_days) ,Burden_days )
  Burden_days <- cbind(` ` = "Days lost (treatment)",Burden_days )
  Burden_days_lost_treatment <- rbind(Burden_days, "")


  Burden_days <- as.data.frame( t(data.frame(Total = Burden_Total$`Days lost (complications)`
                                             ,Young = Burden_Young$`Days lost (complications)`
                                             ,Adults = Burden_Adults$`Days lost (complications)`
                                             ,Older = Burden_Older$`Days lost (complications)`
  )))
  colnames(Burden_days) <- years
  Burden_days <- cbind(` `=rownames(Burden_days) ,Burden_days )
  Burden_days <- cbind(` ` = "Days lost (complications)",Burden_days )
  Burden_days_lost_complications <- rbind(Burden_days, "")


  Burden_days <- as.data.frame( t(data.frame(Total = Burden_Total$`(days left)`
                                             ,Young = Burden_Young$`(days left)`
                                             ,Adults = Burden_Adults$`(days left)`
                                             ,Older = Burden_Older$`(days left)`
  )))
  colnames(Burden_days) <- years
  Burden_days <- cbind(` `=rownames(Burden_days) ,Burden_days )
  Burden_days <- cbind(` ` = "Days left",Burden_days )
  Burden_days_lest <- rbind(Burden_days, "")







  Burden <- rbind(Burden_days_lost_diagnosed_ghost,Burden_days_lost_undiagnosed_ghost,Burden_days_lost_treatment,Burden_days_lost_complications,Burden_days_lest)

  Burden <- rbind("Burden",Burden); Burden[1,2:ncol(Burden)] <- "---"
  Burden <- cbind("Burden",Burden); Burden[2:nrow(Burden),1] <- "" ; colnames(Burden)[1]<- ""

  export_csv <- rbind(export_csv,Burden)

  # Population --------------- --------------- --------------- --------------- --------------- --------------- ---------------

  data_frame <- result
  data_frame$Value  <- data_frame$Prevalence + data_frame$`Ghosts (early death)` + data_frame$`Ghosts (onset death)`
  Population_Total     <- aggregate_by_year(data_frame,"Total")

  data_frame <- select(result, Year,Age, Value= Prevalence)
  Population_diagnosed_living     <- aggregate_by_year(data_frame,"Diagnosed (living prevalence)")

  data_frame <- select(result, Year,Age, Value= `Ghosts (early death)`)
  Population_diagnosed_missing   <- aggregate_by_year(data_frame,"Diagnosed (mising prevalence)")

  data_frame <- select(result, Year,Age, Value= `Ghosts (onset death)`)
  Population_undiagnosed_missing <- aggregate_by_year(data_frame,"Undiagnosed (missing prevalence)")


  data_frame <- select(result, Year,Age, Value1= Prevalence,Value2 = `Ann. background population` )
  Population_living_prevalence_per_100k <- aggregate_by_year_per100k(data_frame,"Living prevalence per 100k")

  data_frame <- select(result, Year,Age, Value1= Prevalence,Value2 = Ghosts )
  Population_ratio_living_missing <- aggregate_by_year_per100k(data_frame,"Ratio living:missing",1)



  Population <- rbind(Population_Total,Population_diagnosed_living,Population_diagnosed_missing,Population_undiagnosed_missing,Population_living_prevalence_per_100k,Population_ratio_living_missing)

  Population <- rbind("Population",Population); Population[1,2:ncol(Population)] <- "---"

  Population <- cbind("Population",Population); Population[2:nrow(Population),1] <- "" ; colnames(Population)[1]<- ""

  export_csv <- rbind(export_csv,Population)



  # Incidence  --------------- --------------- --------------- --------------- --------------- --------------- ---------------
  data_frame <- select(result, Year,Age, Value= `Incidence (1 base)`)
  Incidence_Diagnosed     <- aggregate_by_year(data_frame,"Diagnosed")

  data_frame <- select(result, Year,Age, Value= `Ann. onset deaths`)
  Incidence_Undiagnosed     <- aggregate_by_year(data_frame,"Undiagnosed")

  data_frame <- select(result, Year,Age, Value1= `Incidence (1 base)`,Value2= `Ann. onset deaths` )
  data_frame$Value <- data_frame$Value1 + data_frame$Value2
  Incidence_Total     <- aggregate_by_year(data_frame,"Total")


  data_frame <- select(result, Year,Age, Value1= `Incidence (1 base)`,Value2= `Ann. onset deaths` , Value3= `Ann. background population`)
  data_frame$Value1 <- data_frame$Value1 + data_frame$Value2
  data_frame$Value2 <- data_frame$Value3
  Incidence_Total_per_100k     <- aggregate_by_year_per100k(data_frame,"Total per 100k")

  data_frame <- select(result, Year,Age, Value1= `Incidence (1 base)`,Value2= `Ann. onset deaths` , Value3= `Ann. background population`)
  data_frame$Value1 <- data_frame$Value1
  data_frame$Value2 <- data_frame$Value3
  Incidence_diagnosed_per_100k     <- aggregate_by_year_per100k(data_frame,"Diagnosed per 100k")


  data_frame <- select(result, Year,Age, Value1= `Incidence (1 base)`,Value2= `Ann. onset deaths` , Value3= `Ann. background population`)
  data_frame$Value1 <- data_frame$Value2
  data_frame$Value2 <- data_frame$Value3
  Incidence_undiagnosed_per_100k     <- aggregate_by_year_per100k(data_frame,"Undiagnosed per 100k")

  data_frame <- select(result, Year,Age, Value1= `Incidence (1 base)`,Value2= `Ann. onset deaths` )
  data_frame$Value1 <- data_frame$Value1
  data_frame$Value2 <- data_frame$Value1 +data_frame$Value2
  Incidence_diagnosed_rate     <- aggregate_by_year_per100k(data_frame,"Undiagnosed per 100k",1)




  Incidence <- rbind(Incidence_Total,Incidence_Diagnosed,Incidence_Undiagnosed,Incidence_Total_per_100k,Incidence_diagnosed_per_100k,Incidence_undiagnosed_per_100k ,Incidence_diagnosed_rate)
  Incidence <- rbind("Incidence",Incidence); Incidence[1,2:ncol(Incidence)] <- "----"
  Incidence <- cbind("Incidence",Incidence); Incidence[2:nrow(Incidence),1] <- ""; colnames(Incidence)[1]<- ""

  export_csv <- rbind(export_csv,Incidence)



  # Mortality --------------- --------------- --------------- --------------- --------------- ---------------
  data_frame <- select(result, Year,Age, Value1= `Ann. early deaths`,Value2= `Ann. onset deaths` )
  data_frame$Value <- data_frame$Value1 + data_frame$Value2
  Mortality_Total  <- aggregate_by_year(data_frame,"Total")

  data_frame <- select(result, Year,Age, Value= `Ann. early deaths`)
  Mortality_Diagnosed  <- aggregate_by_year(data_frame,"Diagnosed")

  data_frame <- select(result, Year,Age, Value= `Ann. onset deaths`)
  Mortality_Undiagnosed <- aggregate_by_year(data_frame,"Undiagnosed")

  # need confirmation . ???????????????????????????????????
  data_frame <- select(result, Year,Age, Value1= `Ann. early deaths`,Value2= `Ann. onset deaths`, Value3 = Prevalence, Value4 = `Incidence (1 base)`)
  data_frame$Value1 <- data_frame$Value1 + data_frame$Value2
  data_frame$Value2 <- data_frame$Value3 - (data_frame$Value4)
  Mortality_total_t1ds_per1k <- aggregate_by_year_per100k(data_frame,"Total T1Ds (per thousand)",1000)


  # need confirmation . ???????????????????????????????????
  data_frame <- select(result, Year,Age, Value1= `Ann. early deaths`,Value2= `Ann. onset deaths`, Value3 = Prevalence, Value4 = `Incidence (1 base)`)
  data_frame$Value1 <- data_frame$Value1
  data_frame$Value2 <- data_frame$Value3 - (data_frame$Value4)
  Mortality_diagnosed_t1ds_per1k <- aggregate_by_year_per100k(data_frame,"Diagnosed T1Ds (per thousand)",1000)


  data_frame <- select(result, Year,Age, Value1= `Ann. background mortality`, Value2 = `Ann. background population`)
  Mortality_Background_per1k <- aggregate_by_year_per100k(data_frame,"Background (per thousand)",1000)


  matrix1 <- (as.matrix(Mortality_diagnosed_t1ds_per1k[-5,c(-1,-2)])) ;  class(matrix1) <- "numeric"
  matrix2 <- (as.matrix(Mortality_Background_per1k[-5,c(-1,-2)])) ;  class(matrix2) <- "numeric"
  matrix3 <- round(matrix1 / matrix2,2)
  matrix3 <- cbind(` `=rownames(matrix3) ,matrix3 )
  matrix3 <- cbind(` ` = "SMR",matrix3 )
  matrix3 <- rbind(matrix3, "")
  Mortality_SMR <- matrix3

  Mortality <- rbind(Mortality_Total,Mortality_Diagnosed, Mortality_Undiagnosed,Mortality_total_t1ds_per1k,Mortality_diagnosed_t1ds_per1k,Mortality_Background_per1k,Mortality_SMR)
  Mortality <- rbind("Mortality",Mortality); Mortality[1,2:ncol(Mortality)] <- "----"
  Mortality <- cbind("Mortality",Mortality); Mortality[2:nrow(Mortality),1] <- ""; colnames(Mortality)[1]<- ""

  export_csv <- rbind(export_csv,Mortality)



  # Profile --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- --------------- ---------------
  data_frame <- select(result, Year,Age, Value= `Ann. background population`)
  profile_population <- aggregate_by_year(data_frame,"Population")

  data_frame <- select(result, Year,Age, Value= `Ann. background mortality`)
  profile_mortality <- aggregate_by_year(data_frame,"Mortality")

  profile <- rbind(profile_population,profile_mortality)
  profile <- rbind("Profile",profile); profile[1,2:ncol(profile)] <- "----"
  profile <- cbind("profile",profile); profile[2:nrow(profile),1] <- ""; colnames(profile)[1]<- ""

  export_csv <- rbind(export_csv,profile)
  colnames(export_csv)[1] <- "Type"
  colnames(export_csv)[2] <- "Sub Type"
  colnames(export_csv)[3] <- "Age"
  rownames(export_csv) <- NULL

  # young_range=c(0,19); adults_range= c(20,66); older_range=c(67,99); years= c(seq(from = 1960, to = 2020, by = 5),2021)

  export_csv$Age <- gsub("Young", paste0("Young [",paste0(young_range,collapse = "-"),"]" ) ,export_csv$Age )
  export_csv$Age <- gsub("Adults", paste0("Adults [",paste0(adults_range,collapse = "-"),"]" ) ,export_csv$Age )
  export_csv$Age <- gsub("Older", paste0("Older [",paste0(older_range,collapse = "-"),"]" ) ,export_csv$Age )

  export_csv[,4:ncol(export_csv)] <- suppressWarnings(as.data.frame(sapply(export_csv[,4:ncol(export_csv)], as.numeric)))

  return(export_csv)

}
