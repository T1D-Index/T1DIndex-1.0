suppressPackageStartupMessages(library(tidyverse))
# library(wbstats)  # remotes::install_github("nset-ornl/wbstats")
# library(WHO)      # remotes::install_github("expersso/WHO")
# library(rJava)
library(modelr)
library(randomForest)
library(data.table)
# library(mice)
library(Hmisc)
library(missForest)
library(echarts4r)
library(sparkline)
library(reactable)
library(MASS)
library(dplyr)
# library(xlsx)
library(readxl)
library(Metrics)
library(fst)
library(RPostgres)
source('code_tools/imputation_matrix_completion.R')
source('code_tools/impute_By_Age_Ratio_Naive.R')
source('code_tools/missForest_t1d.R')
# source('code_tools/CV_randomForest.R')
source('code_R_data_prep/000_2_Mortality_Diagnosed_046_data_clean.R')

CV_randomForest <- function(model.data,features_for_training, response_name="smr",Nfold = 10,nodesize = 5,ntrees = 100,mtry)
{

  # model.data <- d_trian; # model.data$smr <- log(model.data$smr+1)
  # model.data <- model_data_global_studies
  # CV_randomForest(d_trian, features_for_training,response= "smr",10,nodesize = 5)

  # fold_key="unique_key";response_name="smr";  model.data          <- model_data_global_studies  ;nodesize = 5 ,Nfold = 10
  model.data$response <- model.data[,response_name]
  # set.seed(11)
  # mtry <- 20
  # Nfold    <- floor(length(unique(model.data[,fold_key])) /7)
  Nfold_df <- unique(model.data[,"unique_key",drop=FALSE] )

  Nfold_df$Nfold <- (1:nrow(Nfold_df) %% Nfold)+1
  model.data <- model.data  %>% left_join(Nfold_df, by='unique_key')

  valid_predicted_all  <- data.frame()
  train_predicted_all  <- data.frame()
  R2list_train   <- c();    R2list_predict <- c()
  RMSElist_train   <- c();    RMSElist_predict <- c()
  MAPElist_predict <- c()

  for(i in 1:Nfold)
  {
    # print(i)
    # i <- 4;  set.seed(11)
    # paste0("CV fold: ",print(i))

    train.data <- model.data[model.data$Nfold!=i,] ; train.data$Nfold <- NULL
    valid.data <- model.data[model.data$Nfold==i,] ; valid.data$Nfold <- NULL

    train.data_ <-  train.data[,features_for_training]

    rf <- randomForest(train.data_, train.data$response, ntree = ntrees,nodesize = nodesize,mtry=mtry)

    # rf <- glm.nb(response ~ imr , data = train.data)

    valid.data$predicted <- predict(rf,valid.data[,features_for_training],type="response")
    valid_predicted_all  <- rbind(valid_predicted_all,valid.data)
    train.data$predicted <- predict(rf,setDF(train.data[,features_for_training]),type="response")
    train_predicted_all  <- rbind(train_predicted_all,train.data)

    RMSElist_train   <-  c(RMSElist_train    ,rmse(train.data$response, train.data$predicted))
    RMSElist_predict <-  c(RMSElist_predict  ,rmse(valid.data$response, valid.data$predicted))
    MAPElist_predict <-  c(MAPElist_predict  ,mean(abs(( (valid.data$response+1)- (valid.data$predicted+1) )/(valid.data$response+1) )) * 100)


    R2list_train     <-  c(R2list_train  ,cor(train.data$response, train.data$predicted) ^ 2*100)
    R2list_predict   <-  c(R2list_predict,cor(valid.data$response, valid.data$predicted) ^ 2*100)

  }

  # r squared
  R2_t <- cor(train_predicted_all$response, train_predicted_all$predicted) ^ 2*100
  R2_p <- cor(valid_predicted_all$response, valid_predicted_all$predicted) ^ 2*100


  index_non_zero  <- train_predicted_all$response !=0
  list_x <- ( train_predicted_all$predicted[index_non_zero] )/ train_predicted_all$response[index_non_zero]
  ci_lower_train  <- median((list_x[list_x<1]) )
  ci_upper_train  <- median(abs(list_x[list_x>1]) )

  index_non_zero  <- valid_predicted_all$response !=0
  list_x <- ( valid_predicted_all$predicted[index_non_zero] )/ valid_predicted_all$response[index_non_zero]

  ci_lower_predict  <- median((list_x[list_x<1]) )
  ci_upper_predict  <- median((list_x[list_x>1]) )

  list_x <- ( valid_predicted_all$predicted[] )/ valid_predicted_all$response[]

  valid_predicted_all$list_x <- list_x

  quantile(list_x[list_x>1],probs=c(0.025,0.5,.975))

  # quantile(list_x,probs=c(.025,0.5,.975))
  # print(paste0("ci_lower_predict: ",ci_lower_predict))
  # print(paste0("ci_upper_predict: ",ci_upper_predict))
  #  rmse

  rmse(train_predicted_all$response, train_predicted_all$predicted)
  rmse(valid_predicted_all$response, valid_predicted_all$predicted)
  #  variance

  median(1-abs(train_predicted_all$response - train_predicted_all$predicted) / train_predicted_all$predicted)
  median(1-abs(valid_predicted_all$response - valid_predicted_all$predicted) / valid_predicted_all$predicted)

  # randomForest::importance(rf)
  # print(paste0("CV R2 train   "  ,R2_t ))
  # print(paste0("CV R2 predict   ",R2_p ))

  # randomForest::importance(rf)
  # print(paste0("CV RMSE train   ",round(mean(RMSElist_train) ,2) ))
  # print(paste0("CV RMSE predict   ",round(mean(RMSElist_predict) ,2) ))
  return(list(R2_t=R2_t,R2_p=R2_p,ci_lower_train=ci_lower_train,ci_upper_train=ci_upper_train,ci_lower_predict=ci_lower_predict ,ci_upper_predict=ci_upper_predict))
}

# expand to all age bins  5 years, fill NA  ,  need authors, country, year, age column
expand_age_bin <- function(model_data_s2)
{
  # model_data_s2 <- model_data_raw_age_curve
  model_data_s2_author_country_year <- unique(dplyr::select(model_data_s2,authors,country,year))
  for(i in 1:nrow(model_data_s2_author_country_year))
  { # i <- 1
    model_data_s2_t <- model_data_s2[  model_data_s2$authors %in% model_data_s2_author_country_year$authors[i]
                                       & model_data_s2$year    %in% model_data_s2_author_country_year$year[i]
                                       & model_data_s2$country %in% model_data_s2_author_country_year$country[i] ,]
    age_all <- (0:19)*5
    age_all <- age_all [!age_all %in%  model_data_s2_t$age ]
    if(length(age_all))
    { model_data_s2_t_add <- as.data.frame(lapply(model_data_s2_t[1,], rep, length(age_all)))
    model_data_s2_t_add$age <- age_all
    model_data_s2_t_add$smr <- NA
    model_data_s2 <- rbind(model_data_s2, model_data_s2_t_add)
    }
  }
  return(model_data_s2)
}

load_clean_data <- function()
{
  model_data_raw_ci <- read.csv('data_mortality/T1D Index CI mapping - CI_Mortality.csv',skip = 1)
  model_data_raw_ci <- model_data_raw_ci[!is.na(as.numeric(model_data_raw_ci$Start)),]
  model_data_raw_ci <- model_data_raw_ci[!is.na(as.numeric(model_data_raw_ci$Min.1)),]
  model_data_raw_ci <- model_data_raw_ci[model_data_raw_ci$Assigned.to!="",]
  model_data_raw_ci$Start <- as.numeric(model_data_raw_ci$Start)
  model_data_raw_ci$End   <- as.numeric(model_data_raw_ci$End)
  model_data_raw_ci$year  <- floor(apply(dplyr::select(model_data_raw_ci,Start,End), 1, median))
  model_data_raw_ci$Min[model_data_raw_ci$Min=="1"] <- "0"

  # S1 Load data, Tom's edit -------------------------------------------------------
  model_data_raw    <- data.frame(read_excel('data_mortality/Mortality_v15.xlsx', 'Sheet1',skip = 0))
  model_data_raw <- model_data_raw[!is.na(model_data_raw$SMR),]
  model_data_raw <- dplyr::select(model_data_raw,authors, country=world_bank_name

                                  ,Start=start_diagnosed
                                  ,End=end_diagnosed
                                  ,Min=min_diagnosis_calc
                                  ,Max=max_diagnosis_calc
                                  ,censor_year

                                  ,year= mid_year
                                  ,person_years
                                  ,age_start= min_attained_calc
                                  ,age_end= max_attained_calc
                                  ,smr=SMR)
  model_data_raw$standard_of_care <- "Non Minimal Care"
  model_data_raw$standard_of_care[model_data_raw$country=="Mali" & model_data_raw$year==1994] <- "Minimal Care"
  model_data_raw$standard_of_care[model_data_raw$country=="Uzbekistan" & model_data_raw$year==1991] <- "Minimal Care"
  model_data_raw$age_end <- sapply(model_data_raw$age_end,min,99)
  model_data_raw$person_years <- as.numeric(model_data_raw$person_years)

  model_data_raw$country[model_data_raw$country=="Taiwan"] <- "China, Taiwan Province of China"
  model_data_raw$country[model_data_raw$country=="USA"] <- "United States"
  model_data_raw$country[model_data_raw$country=="United States of America"] <- "United States"
  model_data_raw$country[model_data_raw$country=="Virgin Islands (U.S.)"]    <- "United States Virgin Islands"
  model_data_raw$country[model_data_raw$country=="Scotland, UK"]    <- "United Kingdom"

  model_data_raw_ci$Country[model_data_raw_ci$Country=="Taiwan"] <- "China, Taiwan Province of China"
  model_data_raw_ci$Country[model_data_raw_ci$Country=="USA"] <- "United States"
  model_data_raw_ci$Country[model_data_raw_ci$Country=="United States of America"] <- "United States"
  model_data_raw_ci$Country[model_data_raw_ci$Country=="Virgin Islands (U.S.)"]    <- "United States Virgin Islands"
  model_data_raw_ci$Country[model_data_raw_ci$Country=="Scotland, UK"]    <- "United Kingdom"

  # model_data_raw

  model_data_raw$smr_ci_lower <- NA
  model_data_raw$smr_ci_upper <- NA
  model_data_raw$smr_phds <- NA
  model_data_raw$phd_row_number <- NA
  for(i in 1:nrow(model_data_raw))
  { # i <- 1
    index <-          model_data_raw$country[i] == model_data_raw_ci$Country | grepl(model_data_raw$country[i],  model_data_raw_ci$Country)
    index <- index &  model_data_raw$Start[i] == model_data_raw_ci$Start
    index <- index &  (model_data_raw$Min[i] == model_data_raw_ci$Min | model_data_raw$age_start[i] == model_data_raw_ci$Min)
    if(sum(model_data_raw_ci[index,]$Gender=="M+F")>0)
    {
      index <- index &  model_data_raw_ci$Gender== "M+F"

    }

    # print( paste0(i," ",model_data_raw$authors[i] ) )
    # print(model_data_raw_ci[index,] )
    # matched_rows <- model_data_raw_ci[index,]

    model_data_raw$smr_phds[i]     <- mean(model_data_raw_ci[index,]$SMR)
    model_data_raw$smr_ci_lower[i] <- mean(model_data_raw_ci[index,]$Min.1)
    model_data_raw$smr_ci_upper[i] <- mean(model_data_raw_ci[index,]$Max.1)


    model_data_raw$phd_row_number[i]  <- paste0(as.numeric(rownames(model_data_raw_ci[index,]))+2,collapse = ",")

  }


  # plug in dianna
  diana_spain_cis <- get_ci_diana_spain()
  model_data_raw[model_data_raw$authors=="Diana",]$smr_ci_lower  <- diana_spain_cis$smr_ci_lower
  model_data_raw[model_data_raw$authors=="Diana",]$smr_ci_upper  <- diana_spain_cis$smr_ci_upper
  model_data_raw[model_data_raw$authors=="Diana",]$smr_phds      <- diana_spain_cis$smr


  index_miss_matches <- is.nan(model_data_raw$smr_ci_lower) |
    model_data_raw$smr <=model_data_raw$smr_ci_lower |
    model_data_raw$smr >=model_data_raw$smr_ci_upper |
    round(model_data_raw$smr_phds,0)  !=round(model_data_raw$smr,0)

  df_check <- (model_data_raw[index_miss_matches,])


  model_data_raw$smr_ci_upper[index_miss_matches] <- NA
  model_data_raw$smr_ci_lower[index_miss_matches] <- NA
  # write.csv(df_check,"temp/check2.csv")

  model_data_raw$Start <- NULL
  model_data_raw$End <- NULL
  model_data_raw$Min <- NULL
  model_data_raw$Max <- NULL
  model_data_raw$censor_year <- NULL
  model_data_raw$smr_phds <- NULL
  model_data_raw$phd_row_number <- NULL
  model_data_raw

}

data_age_curve <- function(model_data_raw)
{
  # S2 build global age curve using 5 studies selected by Tom ------ ------ ------ ------ ------ ------ ------ ------ ------
  model_data_raw_age_curve1 <- model_data_raw[model_data_raw$authors=="Laing (1999)",]
  model_data_raw_age_curve1$id <- 1
  model_data_raw_age_curve2 <- model_data_raw[model_data_raw$authors=="Harding (2014)" & model_data_raw$year==2000,]
  model_data_raw_age_curve2$id <- 2
  model_data_raw_age_curve3 <- model_data_raw[model_data_raw$authors=="Lin",]
  model_data_raw_age_curve3$id <- 3
  model_data_raw_age_curve4 <- model_data_raw[model_data_raw$authors=="Harding (2014)" & model_data_raw$year==2007,]
  model_data_raw_age_curve4$id <- 4
  # model_data_raw_age_curve5 <- model_data_raw[model_data_raw$authors=="Diana",]
  # model_data_raw_age_curve5 <- setDT(model_data_raw_age_curve5)[,list(year=2012,smr=mean(smr)),by=c('authors', 'country', 'standard_of_care', 'person_years', 'age_start', 'age_end'  )]
  # model_data_raw_age_curve5$id <- 5
  # model_data_raw_age_curve5 <- setDF(model_data_raw_age_curve5)[,colnames(model_data_raw_age_curve1)]
  model_data_raw_age_curve <- rbind(model_data_raw_age_curve1
                                    ,model_data_raw_age_curve2
                                    ,model_data_raw_age_curve3
                                    ,model_data_raw_age_curve4
                                    # ,model_data_raw_age_curve5
  )
  model_data_raw_age_curve
}



get_age_curve <- function(model_data_raw_age_curve)
{ # model_data_raw_age_curve <- model_data_raw_random_p
  # row expand to single year , shrink to 5 year bin, merge bin
  model_data_raw_age_curve <- setDT(model_data_raw_age_curve)[ , list(age = age_start: age_end), by = c(colnames(model_data_raw_age_curve))]
  model_data_raw_age_curve <- setDT(model_data_raw_age_curve)[ , list(age = unique ( 5*floor((age)/5))), by = c(colnames(model_data_raw_age_curve)[colnames(model_data_raw_age_curve)!="age"] )]
  model_data_raw_age_curve <- setDT(model_data_raw_age_curve)[ , list(smr = mean(smr)), by = c(colnames(model_data_raw_age_curve)[!colnames(model_data_raw_age_curve) %in% c("age_start","age_end","smr") ] )]
  model_data_raw_age_curve <- expand_age_bin(model_data_raw_age_curve)

  # impute
  impute_input <- dplyr::select(model_data_raw_age_curve,  country,year,age,smr)
  impute_input$country <- as.factor(impute_input$country)
  if(sum(is.na(impute_input$smr))){
    set.seed(0)
    impute_output <- missForest_t1d(impute_input,iteration=9)
    model_data_raw_age_curve$smr[is.na(model_data_raw_age_curve$smr)] <- impute_output$smr_imputed[is.na(model_data_raw_age_curve$smr)]
  }

  model_data_raw_age_curve$legend <- paste0(model_data_raw_age_curve$country," ",model_data_raw_age_curve$year)
  model_data_raw_age_curve_merge <- setDT(model_data_raw_age_curve)[,list(smr=mean(smr,na.rm = T)),by=c("age")]
  model_data_raw_age_curve_merge$legend <- "Global Age Curve"


  data_plot <- rbind(model_data_raw_age_curve_merge, setDF(model_data_raw_age_curve)[,colnames(model_data_raw_age_curve_merge)] )
  model_data_raw_age_curve_merge$smr_percentage <- model_data_raw_age_curve_merge$smr/sum(model_data_raw_age_curve_merge$smr)
  global_age_curve <- data_plot %>%
    group_by(legend ) %>%
    e_charts(age) %>%
    e_line(smr)%>%
    e_x_axis(name="Age")%>%
    e_y_axis(name="Smr")%>%
    e_title("SMR Global Age Curve VS 5 Studies Used",position="bottom")%>%
    e_tooltip(trigger = "item") %>% e_legend(bottom = 0)
  # global_age_curve

  data_plot_thinkcell <- spread(data_plot, legend, smr )
  # write.csv(data_plot_thinkcell,"temp/smr_global_age_curve.csv",row.names = FALSE)
  model_data_raw_age_curve_merge
}



inpute_studies <- function(model_data_raw,model_data_raw_age_curve_merge,run_cv=FALSE)
{
  # model_data_raw_age_curve_merge <- model_data_raw_age_curve_merge_t
  # S3 Impute studies --------------------------------------------------------------------------------------------------------------------
  # Step1 put into age bins --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # row expand to single year , shrink to 5 year bin, merge bin
  model_data_raw <- setDT(model_data_raw)[ , list(age = age_start: age_end), by = c(colnames(model_data_raw))]
  model_data_raw <- setDT(model_data_raw)[ , list(age = unique ( 5*floor((age)/5))), by = c(colnames(model_data_raw)[colnames(model_data_raw)!="age"] )]
  model_data_raw <- setDT(model_data_raw)[ , list(smr = mean(smr)), by = c(colnames(model_data_raw)[!colnames(model_data_raw) %in% c("age_start","age_end","smr") ] )]

  #Apply Global age curve to all studies, get SMR for
  author_country_year <- setDT(model_data_raw)[,list(smr_sum=sum(smr), age_list=list(age), person_years=mean(person_years))
                                               , by = c('authors','country','year','standard_of_care' )]
  author_country_year$person_years[is.na(author_country_year$person_years)] <- mean(author_country_year$person_years,na.rm = T)

  for(i in 1:nrow(author_country_year))
  { # i <- 1
    percentage_out_of_20 <- model_data_raw_age_curve_merge$age %in% unlist(author_country_year$age_list[i])
    percentage_out_of_20 <- model_data_raw_age_curve_merge$smr_percentage[percentage_out_of_20]
    smr_sum_20 <- author_country_year$smr_sum[i]/sum(percentage_out_of_20)
    author_country_year$smr_avg[i] <- smr_sum_20/20
  }


  # country_year1 <- setDT(author_country_year)[,list(smr=mean(smr_avg)),by=c('country','year','standard_of_care' )]
  # merge studies weighting by person_year
  country_year <- setDT(author_country_year)[,list(smr=sum(smr_avg * log(person_years) ) / (sum( log(person_years) )))
                                             ,by=c('country','year','standard_of_care' )]



  country  <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')
  country  <- country[country$world_bank_classification== "HIC",]
  country_year_thinkcell <- setDF(country_year)[country_year$country %in% country$world_bank_name,]
  country_year_thinkcell <- setDT(country_year_thinkcell)[,list(smr=mean(smr)),by="year"]
  country_year_thinkcell <- country_year_thinkcell[order(country_year_thinkcell$year),]
  # write.csv(country_year_thinkcell,"temp/smr_figure2.csv",row.names = FALSE)



  country_year_thinkcell <- setDF(country_year)[country_year$country %in% c("Australia","Finland","Japan","Sweden","United Kingdom","United States"),]
  country_year_thinkcell <- spread(country_year_thinkcell, country , smr )
  # write.csv(country_year_thinkcell,"temp/smr_figure3.csv",row.names = FALSE)



  # attach country indicators
  country_indicator_imputed_avg <- readRDS("data_wb/country_indicator_imputed_avg_0.4.5.Rds")
  country_indicator_imputed_avg$hosp_beds <- NULL
  country_indicator_imputed_avg$doctors_per_capita <- country_indicator_imputed_avg$doctors_per_capita

  # country_indicator_imputed_avg$doctors_per_capital <- NULL

  # Build Global data set
  country_list               <- unique(country_indicator_imputed_avg$country)
  standard_of_care_list      <- unique(model_data_raw$standard_of_care)
  year_list         <- 1960:2020
  year_list         <- range(country_year$year)[1] : range(country_year$year)[2]    # use studies year range
  # age_list          <- sort(unique(model_data_raw$age))
  model_data_global <-        data.frame(country=country_list) %>%
    full_join(data.frame(year=year_list)  , by=character()) %>%
    full_join(data.frame(standard_of_care=standard_of_care_list), by=character())
  # full_join(data.frame(age=age_list)    , by=character())
  model_data_global <- model_data_global %>% left_join(country_indicator_imputed_avg, by=c("country","year"))
  model_data_global <- model_data_global %>% left_join(country_year, by=c("country","standard_of_care","year"))
  model_data_global$standard_of_care   <- as.factor(model_data_global$standard_of_care)

  # impute countries from studies first --
  index_studies_from_global <- model_data_global$country %in% country_year$country

  model_data_global_studies  <- model_data_global[index_studies_from_global,]

  impute_input_train         <- model_data_global_studies
  # impute_input_train$country <- as.factor(impute_input_train$country)
  # impute_input_train$country <- NULL
  impute_input_train$population <- NULL
  impute_input_train$year_flag <- as.factor(impute_input_train$year>=1982)
  impute_input_train$year <- NULL
  features_for_training <- c( 'year_flag', 'standard_of_care',
                              'region',
                              'income_classification',
                              'doctors_per_capita',
                              'gdp_pc_2010',
                              'imr',
                              'mr_u5',
                              'pop_urban'  )
  d_trian <- impute_input_train[!is.na(impute_input_train$smr),]
  cv_ci <- NULL
  if(run_cv)
  {


    d_trian$unique_key <- 1:nrow(d_trian); cv_ci_1 <-  CV_randomForest(d_trian, features_for_training,response= "smr",Nfold=10,nodesize = 2,ntrees=3000,mtry=4)

    d_trian$unique_key <- d_trian$country ;cv_ci_2 <-  CV_randomForest(d_trian, features_for_training,response= "smr",Nfold=37,nodesize = 10,ntrees=3000,mtry=4)

    cv_ci <- list(ci_lower_train=cv_ci_1$ci_lower_train, ci_upper_train=cv_ci_1$ci_upper_train,
                  ci_lower_predict=cv_ci_1$ci_lower_predict, ci_upper_predict=cv_ci_1$ci_upper_predict,
                  ci_lower_predict2=cv_ci_2$ci_lower_predict, ci_upper_predict2=cv_ci_2$ci_upper_predict)
  }
  set.seed(0)
  rf <- randomForest( d_trian[,features_for_training ]   , d_trian$smr, ntree = 1000,nodesize = 10)
  # randomForest::varImpPlot(rf)
  model_data_global_studies$smr_imputed <-  predict(rf,impute_input_train[,features_for_training ],type="response")
  model_data_global_studies
  return(list(model_data_global_studies=model_data_global_studies
              ,model_data_global=model_data_global
              ,index_studies_from_global=index_studies_from_global
              ,cv_ci=cv_ci
  ))


}



# S4 build global longitudinal model --------------------------------- --------------------------------- --------------------------------- ---------------------------------
inpute_logitudinal <- function(model_data_global_studies, model_data_global,index_studies_from_global)
{
  features_for_training <- c( 'year', 'standard_of_care',
                              'region',
                              'income_classification',
                              # 'population',
                              'gdp_pc_2010',
                              'imr',
                              'mr_u5',
                              'pop_urban'  )
  response <- "smr_imputed"

  model_data_global_studies$unique_key <-  model_data_global_studies$country
  # CV_randomForest(model_data_global_studies, features_for_training,response= "smr_imputed",Nfold = 18,nodesize = 50)


  model_data_trian   <- data.frame(model_data_global_studies)[,features_for_training]
  set.seed(0)
  rf <- randomForest( model_data_trian    , setDF(model_data_global_studies)[,response], ntree = 1000,nodesize = 50)
  feature_importance= rf
  # randomForest::varImpPlot(feature_importance)

  model_data_global_studies$smr_predicted <-  predict(rf,data.frame(model_data_global_studies[,features_for_training] ),type="response")
  R2_fiting <-  cor(setDF(model_data_global_studies)[,response], model_data_global_studies$smr_predicted) ^ 2
  R2_fiting

  model_data_global$smr_predicted <-  predict(rf,data.frame(model_data_global[,features_for_training] ),type="response")
  # make imputed as predicted for countries in the study
  model_data_global$smr_predicted[index_studies_from_global] <- model_data_global_studies$smr_imputed

  list(model_data_global=model_data_global,rf=rf)
}

