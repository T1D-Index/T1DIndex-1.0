library(parallel)
library(arrow)
set.seed(0) # very important , don't change.
source('code_R_data_prep/000_2_Mortality_Diagnosed_046.R')
# source('code_R_data_prep/000_2_Mortality_Diagnosed_046_single_run.R')
# result_default_run

model_data_raw <- load_clean_data()

# Impute NA CIs with median deviations from mean.
ratio_lower <- median(model_data_raw$smr_ci_lower[!is.na(model_data_raw$smr_ci_lower)]/model_data_raw$smr[!is.na(model_data_raw$smr_ci_lower)])
ratio_upper <- median(model_data_raw$smr_ci_upper[!is.na(model_data_raw$smr_ci_upper)]/model_data_raw$smr[!is.na(model_data_raw$smr_ci_lower)])

model_data_raw$smr_ci_lower[is.na(model_data_raw$smr_ci_lower)] <- model_data_raw$smr[is.na(model_data_raw$smr_ci_lower)] * ratio_lower
model_data_raw$smr_ci_upper[is.na(model_data_raw$smr_ci_upper)] <- model_data_raw$smr[is.na(model_data_raw$smr_ci_upper)] * ratio_upper


# run_folder <- "temp/mcmc_mortality_log_normal"
# run_folder <- "temp/mcmc_mortality_log_normal_rds"
run_folder <- "../t1dGlobalModel_data/mcmc_mortality_log_normal_parquet"
# run_folder <- "temp/mcmc_mortality_gamma"
# run_folder <- "temp/mcmc_mortality_normal2"

random_gen <- function(mean,smr_ci_lower, smr_ci_upper)
{
  smr <- NULL
  smr_ci_upper <- sapply(smr_ci_upper, max,0)  +1
  smr_ci_lower <- sapply(smr_ci_lower, max,0)+1
  mean         <- sapply(mean, max,0)+1
  # # normal distribution
  #  sd <-                    (smr_ci_upper - smr_ci_lower)/(1.96*2)
  #  smr1                      <- rnorm(length(mean), mean=mean, sd= sd )/1000000
  #  smr1[smr1<0] <- 0
  #  smr <- smr1
  #  # log normal distribution
  sd <-                    (log(smr_ci_upper) - log(smr_ci_lower) )/(1.96*2)
  smr3                      <- round(exp(rnorm(length(mean), mean=log(mean), sd= sd ))-1,4)
  smr <- smr3
  # # gamma
  # mean[mean==0] <-0.0001
  # smr2   <- rgamma(length(mean),shape=mean^2/sd^2,scale=sd^2/mean)/1000000
  # smr2[is.nan(smr2)] <- 0
  # # hist(smr,breaks = 200)
  # smr <- smr2
  return(smr)
}
# Monte carlo 1 :


mcmc_one_loop <- function(run_id)
  # for(i in 1:100)
{ # run_id <- 1
  Debug <- TRUE
  Debug <- FALSE
  source('code_R_data_prep/000_2_Mortality_Diagnosed_046.R')
  list_countries <- c("United States","United Kingdom","Australia","Nigeria","Qatar","Canada")

  model_data_global_all                   <- list()
  model_data_global_age_all               <- list()
  model_data_raw_age_curve_merge_all      <- list()

  print(run_id)
  model_data_raw_random        <- model_data_raw
  model_data_raw_random$smr_ci_lower <- NULL
  model_data_raw_random$smr_ci_upper <- NULL
  model_data_raw_random$smr    <- random_gen(mean=model_data_raw$smr
                                             ,smr_ci_lower=model_data_raw$smr_ci_lower
                                             ,smr_ci_upper=model_data_raw$smr_ci_upper
  )
  model_data_raw_age_curve <- data_age_curve (model_data_raw_random)
  # run_result <- get_age_curve(model_data_raw_random)
  # run_result$run <-  i
  # run_result_all <- rbind(run_result_all,run_result )
  # for(p in 0:4)
  for(p in 1:5)
  { # p <- 4

    model_data_raw_age_curve_t <- model_data_raw_age_curve[model_data_raw_age_curve$id!=(p-1),]
    model_data_raw_age_curve_merge <- get_age_curve(model_data_raw_age_curve_t)

    result                    <- inpute_studies(model_data_raw_random,model_data_raw_age_curve_merge )
    model_data_global_studies <- result$model_data_global_studies
    model_data_global         <- result$model_data_global
    index_studies_from_global <- result$index_studies_from_global

    model_data_global       <- inpute_logitudinal(model_data_global_studies, model_data_global,index_studies_from_global)
    model_data_global$run              <-  paste0(run_id,"_",p)


    model_data_global_age <- data.frame()
    all_ages  <- 0:19 *5
    for(i in 1:length(all_ages))
    {  # i <- 1
      model_data_global_studies_t <- model_data_global
      model_data_global_studies_t$age <- all_ages[i]
      model_data_global_age <- rbind(model_data_global_age,model_data_global_studies_t)
    }
    model_data_global_age <- model_data_global_age %>% left_join(dplyr::select(model_data_raw_age_curve_merge,age,smr_percentage), by="age")

    model_data_global_age$smr_predicted <- model_data_global_age$smr_predicted*20* model_data_global_age$smr_percentage

    model_data_global_age$run           <-  paste0(run_id,"_",p)

    model_data_global_age <- dplyr::select(  model_data_global_age ,world_bank_name=country,standard_of_care,year,age_start=age,smr_predicted,run)

    if(FALSE)
    {
      model_data_global_t$smr_ci_upper <- NA
      model_data_global_t$smr_ci_lower <- NA
      index_train   <- !is.na(model_data_global_t$smr)
      index_by_year <- index_studies_from_global & is.na(model_data_global_t$smr)
      index_by_170  <- !(index_train|index_by_year )
      model_data_global_t$smr_ci_lower[index_train] <- model_data_global_t$smr_predicted[index_train] * result_default_run$ci_lower_train
      model_data_global_t$smr_ci_upper[index_train] <- model_data_global_t$smr_predicted[index_train] * result_default_run$ci_upper_train
      model_data_global_t$smr_ci_lower[index_by_year] <- model_data_global_t$smr_predicted[index_by_year] * result_default_run$ci_lower_predict
      model_data_global_t$smr_ci_upper[index_by_year] <- model_data_global_t$smr_predicted[index_by_year] * result_default_run$ci_upper_predict
      model_data_global_t$smr_ci_lower[index_by_170] <- model_data_global_t$smr_predicted[index_by_170] * result_default_run$ci_lower_predict2
      model_data_global_t$smr_ci_upper[index_by_170] <- model_data_global_t$smr_predicted[index_by_170] * result_default_run$ci_upper_predict2
      sd  <- (model_data_global_t$smr_ci_upper - model_data_global_t$smr_ci_lower)/(1.96*2)
      model_data_global_all_q <- list()
      for(q in 1:90)
      {
        # q <- 1
        if(Debug) print(q)
        model_data_global_t$run              <-  paste0(i,"_",p,"_",q)
        model_data_global_t$smr_predicted2   <-       random_gen( mean= model_data_global_t$smr_predicted
                                                                  ,smr_ci_lower = model_data_global_t$smr_ci_lower
                                                                  ,smr_ci_upper = model_data_global_t$smr_ci_upper)
        # model_data_global_all                <- rbind(model_data_global_all, model_data_global_t )
        model_data_global_t_save <- dplyr::select(model_data_global_t,country, year,standard_of_care,smr_predicted2,run)
        model_data_global_t_save <- model_data_global_t_save[model_data_global_t_save$country %in% list_countries,]
        model_data_global_all_q[[q]] <- model_data_global_t_save
      }
      model_data_global_all_q    <- rbindlist(model_data_global_all_q)
    }


    model_data_global_all[[p]] <- dplyr::select(model_data_global,country ,year,standard_of_care,smr_predicted, smr_predicted ,run)
    model_data_global_age_all[[p]] <- model_data_global_age

    model_data_raw_age_curve_merge$run <- paste0(run_id,"_",p)
    # write_rds(model_data_raw_age_curve_merge, paste0(run_folder,"/model_data_raw_age_curve_merge_t_",run_id,"_",p,".binary"))
    arrow::write_parquet(model_data_raw_age_curve_merge, paste0(run_folder,"/model_data_raw_age_curve_merge_t_",run_id,"_",p,".binary"))
  }
  model_data_global_all        <- rbindlist(model_data_global_all)
  model_data_global_age_all    <- rbindlist(model_data_global_age_all)
  # write_rds(model_data_global_all,     paste0(run_folder,"/model_data_global_t_",run_id,".binary"))
  # write_rds(model_data_global_age_all, paste0(run_folder,"/model_data_global_age_t_",run_id,".binary"))
  arrow::write_parquet(model_data_global_all,     paste0(run_folder,"/model_data_global_t_",run_id,".binary"))
  arrow::write_parquet(model_data_global_age_all, paste0(run_folder,"/model_data_global_age_t_",run_id,".binary"))

  # return(list(model_data_global_all=model_data_global_all,model_data_raw_age_curve_merge_all=model_data_raw_age_curve_merge_all ))
  return()
}

# unlink(run_folder,recursive=TRUE)

dir.create(run_folder)




num_thread <- floor(parallel::detectCores()*0.9)
num_thread <- 20
clust      <- parallel::makeCluster(num_thread, type = "PSOCK")  # stopCluster(clust)
result_default_run <- NULL
clusterExport(cl=clust, varlist=c("random_gen","model_data_raw","result_default_run","run_folder"))

system.time({a <- parLapply(clust, 1:200, mcmc_one_loop)})

stopCluster(clust)
Sys.time()

# refresh_one_country_file(country_wb_name="Rwanda")

files_db <- list.files(run_folder,full.names=TRUE)
# files_db <- list.files("temp/mcmc_mortality_log_normal_parquet",full.names=TRUE)
# files_db <- list.files("temp/mcmc_mortality_gamma",full.names=TRUE)

model_data_global_all                   <- rbindlist( lapply(files_db[grepl("model_data_global_t_",files_db)], read_parquet))
model_data_global_age_all               <- rbindlist( lapply(files_db[grepl("model_data_global_age_t_",files_db)], read_parquet))
model_data_raw_age_curve_merge_all      <- rbindlist( lapply(files_db[grepl("model_data_raw_age_curve_merge_t_",files_db)], read_parquet))
####################------------------------------------------------------------------------------------------------

run_result_all_plot <- setDT(model_data_raw_age_curve_merge_all)[,list(value_mean= mean(smr)
                                                                       ,lower= quantile(smr,probs=c(.025))
                                                                       ,upper= quantile(smr,probs=c(.975))
),by=c("age")]

run_result_all_plot %>%
  e_charts(age) %>%
  e_line(value_mean,name="Average SMR") %>%
  e_band(lower,upper,
         stack = "confidence-band",
         symbol = c("none", "none"),)%>%e_x_axis(name="Age")%>%e_y_axis(name="SMR")%>%
  e_tooltip(trigger = "item") %>% e_legend(bottom = 0)

write.csv(run_result_all_plot,paste0("temp/smr_age_curve_ci.csv") ,row.names = FALSE)

####################------------------------------------------------------------------------------------------------
name_list <- c("United States","United Kingdom","Australia","Nigeria","Qatar","Canada")
for(i in 1:length(name_list))
{  # i <- 3
  model_data_global_all_plot <- model_data_global_all[model_data_global_all$country==name_list[i]&model_data_global_all$standard_of_care=="Non Minimal Care",]
  model_data_global_all_plot$smr_predicted <- round(model_data_global_all_plot$smr_predicted,2)
  model_data_global_all_plot <- setDT(model_data_global_all_plot)[,list(value_mean= quantile(smr_predicted,probs=c(.5))
                                                                        ,lower= quantile(smr_predicted,probs=c(.025))
                                                                        ,upper= quantile(smr_predicted,probs=c(.975))
                                                                        ,num_runs = .N),by=c("year")]

  eval(parse(text = paste0("e_",i," <- model_data_global_all_plot %>%
  e_charts(year) %>%
  e_line(value_mean,name='Average SMR') %>%
  e_band(lower,upper,
         # stack = 'confidence-band',
         symbol = c('none', 'none'),)%>%e_x_axis(name='year')%>%e_y_axis(name='SMR')%>%
  e_tooltip(trigger = 'item') %>% e_legend(bottom = 0)%>%
  e_title(paste0(name_list[i]))  %>%   e_tooltip(trigger = 'axis') %>% e_legend(bottom = 0)
  "
  )))
  # data_plot_thinkcell <- dplyr::select(data_plot, year,smr,smr_imputed)
  write.csv(model_data_global_all_plot,paste0("temp/smr_",name_list[i],"_ci.csv") ,row.names = FALSE)
}
# e_gamma <-  e_arrange( e_1,e_2,e_3,e_4,e_5,e_6,   cols = 3)
e_normal <-  e_arrange( e_1,e_2,e_3,e_4,e_5,e_6,   cols = 3)
# e_arrange( e_1,e_2,e_3,   cols = 4)

####################------------------------------------------------------------------------------------------------
model_data_global_age_all_ci <- setDT(model_data_global_age_all)[,list(value_median= quantile(smr_predicted,probs=c(.5))
                                                                       ,value_mean= mean(smr_predicted)
                                                                       ,lower= quantile(smr_predicted,probs=c(.025))
                                                                       ,upper= quantile(smr_predicted,probs=c(.975))
                                                                       ,num_runs = .N),by=c("world_bank_name","standard_of_care","year","age_start")]




write_parquet(model_data_global_age_all_ci,"data_mortality/machine_learning_model_smr_ci.binary")

Sys.time()

