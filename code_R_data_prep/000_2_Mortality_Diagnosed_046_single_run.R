# set.seed(0) # important   --------------------------------------------------------------------------------

source('code_R_data_prep/000_2_Mortality_Diagnosed_046.R')
source('code_R_data_prep/DATA_CONSTANTS.R')


model_data_raw <- load_clean_data()
model_data_raw$smr_ci_lower <- NULL
model_data_raw$smr_ci_upper <- NULL
# model_data_raw$sd <-
# rnorm(4000000, mean=c(1:4000000), sd=c(1:4000000) )
# https://math.stackexchange.com/questions/2873763/is-it-possible-to-determine-shape-and-scale-for-a-gamma-distribution-from-a-mean
# model_data_raw$country[!model_data_raw$country %in% country$world_bank_name]
# model_data_raw <- model_data_raw[!model_data_raw$country=="France",]   # try take france out

model_data_raw_age_curve       <- data_age_curve (model_data_raw)
model_data_raw_age_curve_merge <- get_age_curve  (model_data_raw_age_curve)


result <- inpute_studies(model_data_raw,model_data_raw_age_curve_merge ,run_cv = FALSE)
model_data_global_studies <- result$model_data_global_studies
model_data_global         <- result$model_data_global
index_studies_from_global         <- result$index_studies_from_global

result_default_run <- result$cv_ci
# Plot few countries --------------------------------------------------------------------------------------------
# impute <- missForest_t1d(impute_input_train,iteration=1,node_size=10)
# model_data_global_studies$smr_imputed <- impute$smr_imputed

# dump out put
# write.csv(model_data_global_studies,"temp/model_data_global_studies.csv",row.names = F)

# name_list <- c("United Kingdom","United States","Australia","Finland","Sweden","Japan","Mali","Rwanda")
# name_list <- c("United Kingdom","United States","Australia","Finland","Sweden","Japan","Germany","France")
name_list <- c("United Kingdom","United States","Australia")

for(i in 1:length(name_list))
{  # i <-2
  data_plot <- model_data_global_studies[model_data_global_studies$country==name_list[i] & model_data_global_studies$standard_of_care=="Non Minimal Care",]
  eval(parse(text = paste0("e_",i," <- data_plot %>%
    e_charts(year) %>%
    e_line(smr,name='raw smr')%>%
    e_line(smr_imputed,name='imputed smr')%>%
    e_x_axis(name='Age', min=1960)%>%
    e_y_axis(name='Smr', max= 10)%>%
    e_title(paste0('SMR Raw VS Imputed, ',name_list[i]),'Non Minimal Care',left=100)%>%
    e_tooltip(trigger = 'item') %>% e_legend(bottom = 0)"
  )))
  data_plot_thinkcell <- dplyr::select(data_plot, year,smr,smr_imputed)
  # write.csv(data_plot_thinkcell,paste0("temp/smr_",name_list[i],".csv") ,row.names = FALSE)

}
# e_arrange( e_1,e_2,e_3,e_4,e_5,e_6,e_7,e_8,   cols = 4)
# e_arrange( e_1,e_2,e_3,   cols = 4)


results <- inpute_logitudinal(model_data_global_studies, model_data_global,index_studies_from_global)
model_data_global <- results$model_data_global
rf                <- results$rf



# Get States data predicted :
country_region_mapping <- Data_Run_query_return_df ("SELECT * FROM index_parameters.country_region where sub_nation_id !=0 ;")

country_region_mapping$wd_income_category <- as.factor(country_region_mapping$wd_income_category)

model_data_global_states <- data.frame()
for(i in 1:nrow( country_region_mapping))
{ # i <- 1
  model_data_global_states_t <- model_data_global[model_data_global$country == country_region_mapping$world_bank_name_parent[i],]
  model_data_global_states_t$smr <- NULL
  model_data_global_states_t$country <- country_region_mapping$world_bank_name[i]
  model_data_global_states_t$world_bank_name <- country_region_mapping$world_bank_name_parent[i]

  model_data_global_states_t$gdp_pc_2010           <- model_data_global_states_t$gdp_pc_2010 * country_region_mapping$gdp_ratio[i]
  model_data_global_states_t$imr                   <-model_data_global_states_t$imr * country_region_mapping$background_mortality_ratio[i]
  model_data_global_states_t$income_classification <-  country_region_mapping$wd_income_category[i]
  # model_data_global_states_t$income_classification <- country_region_mapping$`Income class...12`[i]
  model_data_global_states_t$smr_predicted <-  predict(rf,data.frame(model_data_global_states_t ),type="response")
  model_data_global_states <- rbind(model_data_global_states, model_data_global_states_t)
}

# saveRDS(model_data_global_states,"data_mortality/machine_learning_model_smr_states.Rds")




# Plot few countries --------------------------------------------------------

# name_list <- c("United Kingdom","United States","Australia","Finland","Sweden","Japan","Mali","Rwanda")
# name_list <- c("United Kingdom","United States","Australia","Finland","Sweden","Japan","Germany","France")
name_list <- c("Qatar","Canada","Nigeria")
for(i in 1:length(name_list))
{  # i <- 2
  data_plot <- model_data_global[model_data_global$country==name_list[i] & model_data_global$standard_of_care=="Non Minimal Care",]
  # data_plot$imr <- data_plot$imr
  # data_plot$mr_u5 <- data_plot$mr_u5
  # data_plot$gdp_pc_2010 <- data_plot$gdp_pc_2010/ 10000
  # data_plot$pop_urban <- data_plot$pop_urban/ 10
  eval(parse(text = paste0("e_",i," <- data_plot %>%
    e_charts(year) %>%
    e_line(smr,name='raw smr')%>%
    e_line(smr_predicted,name='imputed smr')%>%
    # e_line(imr,name='imr')%>%
    # e_line(mr_u5,name='mr_u5')%>%
    # e_line(pop_urban,name='pop_urban')%>%
    # e_line(gdp_pc_2010,name='gdp_pc_2010')%>%
    e_x_axis(name='Age', min=1960)%>%
    e_y_axis(name='Smr',max=10)%>%
    e_title(paste0('SMR Raw VS Imputed, ',name_list[i]),'Non Minimal Care',left=100)%>%
    e_tooltip(trigger = 'item') %>% e_legend(bottom = 0)"
  )))
  data_plot_thinkcell <- dplyr::select(data_plot, year,smr,smr_predicted)
  # write.csv(data_plot_thinkcell,paste0("temp/smr_",name_list[i],".csv") ,row.names = FALSE)


}
# e_arrange( e_1,e_2,e_3,e_4,e_5,e_6,e_7,e_8,   cols = 4)
# e_arrange( e_1,e_2,e_3,   cols = 4)



# expand to age bins  ------------------------------------
model_data_global_age <- data.frame()
all_ages  <- 0:19 *5
for(i in 1:length(all_ages))
{  # i <- 1
  model_data_global_studies_t <- model_data_global
  model_data_global_studies_t$age <- all_ages[i]
  model_data_global_age <- rbind(model_data_global_age,model_data_global_studies_t)
}
model_data_global_age <- model_data_global_age %>% left_join(dplyr::select(model_data_raw_age_curve_merge,age,smr_percentage), by="age")

# comment out when machine_learning_model_smr_flat_age.Rds
model_data_global_age$smr_predicted <- model_data_global_age$smr_predicted*20* model_data_global_age$smr_percentage




# plot 3d for one country ---------------------------------
data_plot <- model_data_global_age[model_data_global_age$country=="Germany"&  model_data_global_age$standard_of_care=="Non Minimal Care",]
data_plot %>%
  # group_by(standard_of_care) %>%
  e_charts(age ) %>%
  e_scatter_3d(year, smr_predicted   )%>%
  # e_scatter_3d(year, smr  )%>%
  # e_scatter_3d(year, imr             )%>%
  e_x_axis_3d(axisLine = list(),name="Age")%>%e_y_axis_3d(axisLine = list(),name="Year",min=min(data_plot$year))%>%e_z_axis_3d(axisLine = list(),name= "Smr") %>%
  e_tooltip(trigger = "item") %>% e_legend()


model_data_global_age <- dplyr::select(  model_data_global_age ,world_bank_name=country,standard_of_care,year,age_start=age,smr_predicted)
model_data_global_age[1:10,]
# saveRDS(model_data_global_age,"data_mortality/machine_learning_model_smr.Rds")
# saveRDS(model_data_global_age,"data_mortality/machine_learning_model_smr_flat_age.Rds")




