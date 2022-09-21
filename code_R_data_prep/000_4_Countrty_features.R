
library(wbstats)  # remotes::install_github("nset-ornl/wbstats")
library(tidyverse)
library(reactable) # cell level rendering https://glin.github.io/reactable/articles/examples.html#cell-rendering-1
library(echarts4r)
library(sparkline)
library(data.table)
source('code_tools/imputation_matrix_completion.R')
source('code_tools/impute_By_Age_Ratio_Naive.R')
source('code_tools/utils_plot.R')

country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')
# country    <- read_rds('data_internal/country.Rds')
world_bank_region <- read.csv("data_internal/world_bank_region.csv",stringsAsFactors = F,encoding='UTF-8')

country <- country %>% left_join(dplyr::select(world_bank_region,world_bank_code,Region),by="world_bank_code" )

# commented out indicators don't have full country coverage
# obtain indicators from World Bank
if(FALSE)
{
  country_indicator <- wb_data( list(
    nurses             ="SH.MED.NUMW.P3",
    physicians_per_1000="SH.MED.PHYS.ZS",
    health_exp          ="SH.XPD.CHEX.GD.ZS",
    imr                ="SP.DYN.IMRT.IN",
    imr_u5                ="SH.DYN.MORT",

    #gdp_pc_ppp="NY.GDP.PCAP.PP.KD",
    gdp_pc_current     ="NY.GDP.PCAP.CD",
    gdp_pc_2010        ="NY.GDP.PCAP.KD",

    hosp_beds          ="SH.MED.BEDS.ZS",
    pop                ="SP.POP.TOTL",
    pop_urban               ="SP.URB.TOTL.IN.ZS",

    gni                ="NY.GNP.PCAP.PP.CD",
    gini                ="SI.POV.GINI",
    literacy_rate               ="SE.ADT.LITR.ZS",
    hiv_i               ="SH.HIV.INCD.TL.P3",
    fertility = "SP.DYN.TFRT.IN"
    #cty_hwkr="SH.MED.CMHW.P3",  # community health workers
    # cr_mort="SP.DYN.CDRT.IN", # crude mortality rate
    # mat_mort="SH.STA.MMRT", # maternal mortality rate
    # life_exp = "SP.DYN.LE00.IN",
  ) ,start_date=1950,end_date=2021,country = "all")




  write_rds(country_indicator,"data_wb/country_indicator.Rds")

}

country_indicator <- readRDS("data_wb/country_indicator.Rds")

# add fertility ----
country_indicator_fertility <- wb_data( list(  fertility = "SP.DYN.TFRT.IN") ,start_date=1950,end_date=2021,country = "all")

country_indicator <- country_indicator %>% dplyr::left_join(dplyr::select(country_indicator_fertility,iso3c,date,fertility),by=c("iso3c","date"))



country_indicator$mr_u5 <- country_indicator$imr_u5
country_indicator$doctors_per_capital <- country_indicator$physicians_per_1000
country_indicator$year <- country_indicator$date

country_indicator_pop  <- readRDS("data_un/population_single_year.Rds")
country_indicator_pop  <- data.frame(setDT(country_indicator_pop)[(year<=2020) & (year >= 1960),list(population= sum(population)),by=c("loc_id", "year" )])
country_indicator_pop  <- country_indicator_pop%>% inner_join(dplyr::select(country, loc_id,world_bank_name,iso3c=world_bank_code,region=Region ,income_classification=world_bank_classification ),by="loc_id")
country_indicator_pop$date  <- country_indicator_pop$year
country_indicator_pop$year  <- NULL

country_indicator <- country_indicator_pop %>% left_join(country_indicator,by=c("iso3c","date") )

country_indicator$country <- country_indicator$world_bank_name
# matrix_long <-( select(country_indicator, x= date  , y=country , z =  gdp_pc_2010  ))
# plot_hm_raw <- plot_Heatmap(matrix_long,"longitudinal raw","Calender year","Country")
# plot_hm_raw



Impute_country_indicator <- function(country_indicator, col_name )
{
# country_indicator <- country_indicator  ,   col_name <- "gdp_pc_2010"
  country_indicator_wide           <-  spread(dplyr::select(country_indicator, x= date  , y=country , z = col_name ),  x, z)
  country_indicator_wide_df        <-  data.frame(country_indicator_wide)

  rownames(country_indicator_wide_df) <- country_indicator_wide_df$y
  colnames(country_indicator_wide_df) <- colnames(country_indicator_wide)
  country_indicator_wide_df$y <- NULL

  filter_all_na_row <- rowSums(!is.na(country_indicator_wide_df))> 0

  country_indicator_wide_df <- country_indicator_wide_df[filter_all_na_row,]

  filter_all_na_col <- colSums(!is.na(country_indicator_wide_df))> 0

  country_indicator_wide_df <- country_indicator_wide_df[,filter_all_na_col]

  # spline extrapolate intermidiate values ---------------------
  matrix <- country_indicator_wide_df
  for(p in 1:nrow(matrix))
  {
    # p <- 10
    row_filled <- spline(x=colnames(matrix),y=matrix[p,],method="fmm") # use this one to define window
    row_filled <- spline(x=colnames(matrix),y=log(matrix[p,]),method="fmm",xout= range(row_filled$x)[1]:range(row_filled$x)[2])
    row_filled$y <- exp(row_filled$y)
    # data.frame(x=colnames(matrix) , y = c(t(matrix[p,])) ) %>% e_charts(x) %>% e_scatter(y)
    # data.frame(x=as.character(row_filled$x), y= row_filled$y) %>% e_charts(x) %>% e_scatter(y)
    row_filled <- data.frame(x=row_filled$x,y=row_filled$y)
    row_filled$year <- round(row_filled$x,0)
    row_filled <- setDT(row_filled)[,list(value= y[which.min(abs(x-year))]),by="year"]
    matrix[p, as.character(row_filled$year) ] <-  row_filled$value

  }
  country_indicator_wide_df <- matrix


  # country_indicator_wide_df <- impute_By_Age_Ratio_Naive (matrix = country_indicator_wide_df)
  # country_indicator_wide_df1 <- impute_matrix_completion (Y = country_indicator_wide_df,lambda=0.001)


  country_indicator_long <- gather(cbind(country=rownames(country_indicator_wide_df),country_indicator_wide_df),key = year, value = value,-country)
  country_indicator_long$year <- as.numeric(gsub('X','',country_indicator_long$year) )

  return(country_indicator_long)
}

col_name = "gdp_pc_2010"
country_indicator_gdp <- Impute_country_indicator ( country_indicator,col_name )

col_name = "imr"
country_indicator_imr <- Impute_country_indicator ( country_indicator,col_name )

col_name = "mr_u5"
country_indicator_imr_u5 <- Impute_country_indicator ( country_indicator,col_name )

col_name = "hiv_i"
country_indicator_imr_hiv_i <- Impute_country_indicator ( country_indicator,col_name )

col_name = "doctors_per_capital"
country_indicator_doctors_per_capital <- Impute_country_indicator ( country_indicator,col_name )

col_name = "hosp_beds"
country_indicator_hosp_beds <- Impute_country_indicator ( country_indicator,col_name )

col_name = "pop"
country_indicator_pop <- Impute_country_indicator ( country_indicator,col_name )

col_name = "pop_urban"
country_indicator_pop_urban <- Impute_country_indicator ( country_indicator,col_name )

col_name = "gini"
country_indicator_gini <- Impute_country_indicator ( country_indicator,col_name )

col_name = "gni"
country_indicator_gni <- Impute_country_indicator ( country_indicator,col_name )

col_name = "literacy_rate"
country_indicator_literacy_rate <- Impute_country_indicator ( country_indicator,col_name )

col_name = "health_exp"
country_indicator_health_exp <- Impute_country_indicator ( country_indicator,col_name )

col_name = "fertility"
country_indicator_fertility <- Impute_country_indicator ( country_indicator,col_name )

# matrix_long <-( select(country_indicator_long, x= year  , y=iso3c , z =  gdp_pc_2010  ))
# plot_hm_raw <- plot_Heatmap(matrix_long,"longitudinal raw","Calender year","Country")
# plot_hm_raw



# merge all imputed values ------------------
country_indicator_imputed <- dplyr::select(country_indicator,country, year=date,region,income_classification,population)
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_gdp,country,year,gdp_pc_2010=value)                                ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_imr,country,year,imr=value)                                        ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_imr_u5,country,year,mr_u5=value)                                   ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_doctors_per_capital,country,year,doctors_per_capital=value)        ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_hosp_beds,country,year,hosp_beds=value)                            ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_pop_urban,country,year,pop_urban=value)                            ,by=c("country","year"))
country_indicator_imputed <- country_indicator_imputed %>% left_join(dplyr::select(country_indicator_fertility,country,year,fertility=value)                            ,by=c("country","year"))


# write.csv(country_indicator_imputed,"data_wb/country_indicator_imputed.csv",row.names = F)
# country_indicator      <- country_indicator[,colnames(country_indicator_imputed)]
# write.csv(country_indicator,"data_wb/country_indicator.csv",row.names = F)





library(missForest)
country_indicator_imputed$income_classification <- as.factor(country_indicator_imputed$income_classification)
country_indicator_imputed$region                <- as.factor(country_indicator_imputed$region)

# impute data point ,  random forest impute for each country ----------------------------------------

country_list <- unique(country_indicator_imputed$country )
country_indicator_imputed_all <- data.frame()

for(i in 1:length(country_list))
{
  # i <- 190
  print(i)
  data_input <-  data.frame( country_indicator_imputed[country_indicator_imputed$country==country_list[i],c(-1,-2)])
  NAs <- colSums(is.na(data_input))
  if(sum(NAs >= nrow(data_input)) <6)
  {
    impute <- missForest(data_input)
    impute <- impute$ximp
    data_input[,colnames(impute)] <- impute
  }
  data_input <- cbind(country_indicator_imputed[country_indicator_imputed$country==country_list[i],c(1,2)], data_input)
  country_indicator_imputed_all <- rbind(country_indicator_imputed_all,data_input)
}


# impute data point , no data in any year of a  country ------------------------
country_indicator_input <- country_indicator_imputed_all
region_list <- unique(country_indicator_input$region )
country_indicator_imputed_by_region <- data.frame()
for(i in 1:length(region_list))
{
  # i <-16
  print(i)
  data_input <-  as.data.frame( country_indicator_input[country_indicator_input$region==region_list[i],c(-1,-2)])
  NAs <- colSums(is.na(data_input))
  if(sum(NAs >= nrow(data_input)) <6)
  {
    impute <- missForest(data_input)
    impute <- impute$ximp
  }
  data_input[,colnames(impute)] <- impute
  data_input <- cbind(country_indicator_input[country_indicator_input$region==region_list[i],c(1,2)], data_input)
  country_indicator_imputed_by_region <- rbind(country_indicator_imputed_by_region,data_input)
}

# # impute data point , no data in any year of a  country ------------------------
# country_indicator_input <- country_indicator_imputed_all
# income_classification <- unique(country_indicator_input$income_classification )
# country_indicator_imputed_by_income_class <- data.frame()
# for(i in 1:length(income_classification))
# {
#   # i <-16
#   print(i)
#   data_input <-  as.data.frame( country_indicator_input[country_indicator_input$income_classification==income_classification[i],c(-1,-2)])
#   NAs <- colSums(is.na(data_input))
#   if(sum(NAs >= nrow(data_input)) <6)
#   {
#     impute <- missForest(data_input)
#     impute <- impute$ximp
#   }
#   data_input[,colnames(impute)] <- impute
#   data_input <- cbind(country_indicator_input[country_indicator_input$income_classification==income_classification[i],c(1,2)], data_input)
#   country_indicator_imputed_by_income_class <- rbind(country_indicator_imputed_by_income_class,data_input)
# }
#
# country_indicator_imputed_by_income_class <- country_indicator_imputed_by_income_class[with(country_indicator_imputed_by_income_class, order(year)),]
# country_indicator_imputed_by_income_class <- country_indicator_imputed_by_income_class[with(country_indicator_imputed_by_income_class, order(country)),]
#
# country_indicator_imputed_by_region <- country_indicator_imputed_by_region[with(country_indicator_imputed_by_region, order(year)),]
# country_indicator_imputed_by_region <- country_indicator_imputed_by_region[with(country_indicator_imputed_by_region, order(country)),]
#
#
# country_indicator_imputed_avg <-  country_indicator_imputed_by_income_class[,c(1,2,3,4)]
# country_indicator_imputed_avg <-  cbind(country_indicator_imputed_avg,
#                                         (country_indicator_imputed_by_income_class[,c(-1,-2,-3,-4)] + country_indicator_imputed_by_region[,c(-1,-2,-3,-4)])/2  )


country_indicator_imputed_avg <- country_indicator_imputed_by_region



# write_rds(country_indicator_imputed_avg,"data_wb/country_indicator_imputed_avg_0.4.5.Rds")
write_rds(country_indicator_imputed_avg,"data_wb/country_indicator_imputed_avg_0.4.13.Rds")


# Onset data ------------------------------------------------------------------------------------------------------------------------------
if(FALSE)
{
  load("data_onset/model_data.Rda")
  model_data <- dplyr::select(model_data, country= world_bank_name,year= period ,midpoint  )
  model_data <- model_data%>% inner_join(select(country, loc_id,country=world_bank_name,region=jdrf_region_name ,income_classification=world_bank_classification ),by="country")
  # write.csv(model_data,"data_wb/model_data.csv",row.names = F)

  model_data$year <- as.character(model_data$year)
  model_data <- data.frame(setDT(model_data)[,list(death_undiagnosed=mean(midpoint)),by=c("country","year")])

  model_data$year[model_data$year==1] <- 2000
  model_data$year[model_data$year==2] <- 2010
  model_data$year[model_data$year==3] <- 2020
  model_data$year <- as.numeric(model_data$year)


  country_indicator_imputed_avg <- country_indicator_imputed_avg  %>% left_join(model_data , by=c("country","year"))
  country_indicator_imputed_avg$death_undiagnosed[country_indicator_imputed_avg$income_classification=="HIC"] <- 0



  # impute data point ,  random forest impute for each country ----------------------------------------
  country_indicator_input <- country_indicator_imputed_avg
  country_list <- unique(country_indicator_input$country )
  country_indicator_imputed_all <- data.frame()
  for(i in 1:length(country_list))
  {
    # i <- 213
    print(i)
    data_input <-  as.data.frame( country_indicator_input[country_indicator_input$country==country_list[i],c(-1,-2)])
    NAs <- colSums(is.na(data_input))
    if(sum(NAs >= nrow(data_input)) <6)
    {
      impute <- missForest(data_input)
      impute <- impute$ximp
    }
    data_input[,colnames(impute)] <- impute
    data_input <- cbind(country_indicator_input[country_indicator_input$country==country_list[i],c(1,2)], data_input)
    country_indicator_imputed_all <- rbind(country_indicator_imputed_all,data_input)
  }



  # impute data point , no data in any year of a  country ---------------------------------------------------
  country_indicator_input <- country_indicator_imputed_all
  region_list <- unique(country_indicator_input$region )
  country_indicator_imputed_by_region <- data.frame()
  for(i in 1:length(region_list))
  {
    # i <-16
    print(i)
    data_input <-  as.data.frame( country_indicator_input[country_indicator_input$region==region_list[i],c(-1,-2)])
    NAs <- colSums(is.na(data_input))
    if(sum(NAs >= nrow(data_input)) <6)
    {
      impute <- missForest(data_input)
      impute <- impute$ximp
    }
    data_input[,colnames(impute)] <- impute
    data_input <- cbind(country_indicator_input[country_indicator_input$region==region_list[i],c(1,2)], data_input)
    country_indicator_imputed_by_region <- rbind(country_indicator_imputed_by_region,data_input)
  }

  # impute data point , no data in any year of a  country ------------------------
  country_indicator_input <- country_indicator_imputed_all
  income_classification <- unique(country_indicator_input$income_classification )
  country_indicator_imputed_by_income_class <- data.frame()
  for(i in 1:length(income_classification))
  {
    # i <-16
    print(i)
    data_input <-  as.data.frame( country_indicator_input[country_indicator_input$income_classification==income_classification[i],c(-1,-2)])
    NAs <- colSums(is.na(data_input))
    if(sum(NAs >= nrow(data_input)) <6)
    {
      impute <- missForest(data_input)
      impute <- impute$ximp
    }
    data_input[,colnames(impute)] <- impute
    data_input <- cbind(country_indicator_input[country_indicator_input$income_classification==income_classification[i],c(1,2)], data_input)
    country_indicator_imputed_by_income_class <- rbind(country_indicator_imputed_by_income_class,data_input)
  }

  country_indicator_imputed_avg <-  country_indicator_imputed_by_income_class[,c(1,2,3,4)]
  country_indicator_imputed_avg <-  cbind(country_indicator_imputed_avg,  (country_indicator_imputed_by_income_class[,c(-1,-2,-3,-4)] + country_indicator_imputed_by_region[,c(-1,-2,-3,-4)])/2  )


  country_indicator_imputed_avg <- country_indicator_imputed_by_region

  write.csv(country_indicator_imputed_avg,"data_wb/country_indicator_imputed_avg.csv",row.names = F)
  write_rds(country_indicator_imputed_avg,"data_wb/country_indicator_imputed_avg.Rds")




}

