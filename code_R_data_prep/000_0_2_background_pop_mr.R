background_pop_mr <- function(country,data_un_path="../data_un")
{



  # population_single_year  which countries (LocIDs) to keep ------------------------------------------------------------------------------

  population1 <- readRDS(paste0(data_un_path,'/WPP2019_PopulationBySingleAgeSex_1950-2019.Rds'))
  population2 <- readRDS(paste0(data_un_path,'/WPP2019_PopulationBySingleAgeSex_2020-2100.Rds'))
  population  <- rbind(population1,population2)



  index_keep <- population$LocID %in% country$loc_id
  # index_keep <- index_keep & (population$Time %% 5 ==0)
  index_keep <- index_keep & (population$AgeGrpStart  != 100)
  population <- population[index_keep,]
  population$trajectory <- 0
  population$PopTotal   <-  population$PopTotal  * 1000
  population_single_year <- dplyr::select(population
                                   , loc_id = LocID
                                   , year = Time
                                   , age_grp_start = AgeGrpStart
                                   , trajectory
                                   , population = PopTotal
  )

  population_single_year <- population_single_year[population_single_year$year<=2040,]

  # keep population constant  <=1950 -----------------
  population_single_year_temp <- population_single_year[population_single_year$year==1950,]
  population_single_year_backward <- data.frame()
  for(i in 1900:1949)
  {
    population_single_year_temp$year <- i
    population_single_year_backward <- rbind(population_single_year_backward,population_single_year_temp )
  }

  population_single_year <- rbind(population_single_year_backward, population_single_year )
  write_rds(population_single_year,paste0(paste0(data_un_path,'/population_single_year.Rds') ))


  # population.csv which countries (LocIDs) to keep ------------------------------------------------------------------------------
  population <- readRDS(paste0(data_un_path,'/WPP2019_PopulationByAgeSex_Medium.Rds'))
  index_keep <- population$LocID %in% country$loc_id
  index_keep <- index_keep & (population$Time %% 5 ==0)
  index_keep <- index_keep & (population$AgeGrpStart  != 100)
  population <- population[index_keep,]
  population$trajectory <- 0
  population$PopTotal   <-  population$PopTotal  * 1000
  population <- dplyr::select(population
                       , loc_id = LocID
                       , year = Time
                       , age_grp_start = AgeGrpStart
                       , trajectory
                       , population = PopTotal
  )%>%
    left_join(dplyr::select(country, world_bank_name, loc_id), by='loc_id')

  write_rds(population,paste0(data_un_path,'/population.Rds'))

  # life_table.csv which countries (LocIDs) to keep ------------------------------------------------------------------------------
  life_table <- readRDS(paste0(data_un_path,'/WPP2019_Life_Table_Medium.Rds'))

  life_table$start_year <-  life_table$MidPeriod-3
  index_keep <- life_table$LocID %in% country$loc_id
  # index_keep <- index_keep & (life_table$AgeGrpStart  != 100)
  index_keep <- index_keep & (life_table$SexID  == 3)
  index_keep <- index_keep & (life_table$start_year  <= 2040)

  life_table <- life_table[index_keep,]

  # life_table <- life_table[life_table$MidPeriod=="2020",]

  # life_table2 <- dplyr::select(life_table[1:21,],age=AgeGrpStart,qx,lx,dx,Lx,Tx,ex)
  # life_table2$age <- 0:(nrow(life_table2)-1)
  # calculate_ex(life_table2 )


  # plot ab
  # data <- life_table[life_table$LocID==4 & life_table$AgeGrpStart==0,]
  # data <- dplyr::select(data,x=MidPeriod , y=qx)
  # data %>% echarts4r::e_chart(x) %>% echarts4r::e_scatter(y) %>% echarts4r::e_x_axis(min= 1950)

  # calculate ex ----

  # life_table$ex_c <- life_table$dx / life_table$lx


  life_table <- dplyr::select(life_table
                       , loc_id = LocID
                       , start_year
                       , age_grp_start = AgeGrpStart
                       , qx
                       , lx
  )%>%  left_join(dplyr::select(country, world_bank_name, loc_id), by='loc_id')

  # life_table <- life_table[life_table$loc_id==598,]
  # make value to midele year
  # life_table$age_grp_start <- ifelse(life_table$age_grp_start!=0,life_table$age_grp_start + 2.5,0)

  write_rds(life_table,paste0(data_un_path,'/life_table.Rds'))


  # linearly extrapolate to single age and then calender year.
  data_wide_age <- dplyr::select (life_table, loc_id, start_year,age_grp_start,value= lx)
  data_wide_age           <- setDF(spread(data_wide_age, age_grp_start, value))
  data_wide_age_prefix    <-data_wide_age[,c(1,2,3)]
  data_wide_age           <-data_wide_age[,c(-1,-2,-3)]
  # data_wide_age$'99' <- setDF(data_wide_age)[,ncol(data_wide_age)]
  # data_wide_age <- cbind(data_wide_age , '99' = data_wide_age$`95`)

  new_cname_expand <- seq(1, 100, 1)
  data_wide_age_new <- data.frame(exp(t(apply(log(data_wide_age+1e-20), 1, function(x) spline(1:ncol(data_wide_age), x, length(new_cname_expand), method="natural")$y))))

  colnames(data_wide_age_new) <- new_cname_expand
  data_wide_age_new <- cbind(data_wide_age_prefix,data_wide_age_new)

  data_long    <-  gather(data_wide_age_new, key = "age_grp_start", value = "value", -loc_id,-start_year)
  data_long$age_grp_start <- as.integer(data_long$age_grp_start)
  data_long    <-  data_long[ order( data_long[,1], data_long[,2] , data_long[,3] ),]

  data_long$nominator <- c(data_long$value[2:nrow(data_long)],0)
  data_long$qB <- sapply(round(1- data_long$nominator/data_long$value,5),max,0)

  data_long <- data_long[data_long$age_grp_start!=100,]

  data_long <- dplyr::select(data_long, -value, -nominator)


  data_wide_year <-  spread(data_long, start_year, qB)

  data_wide_year_prefix    <- data_wide_year[,c(1,2)]
  data_wide_year           <- data_wide_year[,c(-1,-2)]

  # data_wide_year <- cbind('1900' = data_wide_year$`1950`, data_wide_year)

  new_cname_expand <- seq(1950, 2040, 1)

  data_wide_year_new <- data.frame(exp(t(apply(log(data_wide_year+1e-20), 1, function(x) spline(1:ncol(data_wide_year), x, length(new_cname_expand))$y))))

  colnames(data_wide_year_new) <- new_cname_expand

  # keep constant before 1950
  matrix <- data.frame(matrix(NA, ncol = length(1900:1949), nrow = nrow(data_wide_year_new)))
  colnames(matrix) <- as.character(1900:1949)
  for(i in 1:ncol(matrix)) {matrix[,i] <- data_wide_year_new[,1]}

  data_wide_year_new <- cbind(matrix,data_wide_year_new)
  data_wide_year_new <- cbind(data_wide_year_prefix,data_wide_year_new)

  life_table_single_year <-  gather(data_wide_year_new, key = "start_year", value = "qB", -loc_id,-age_grp_start)

  life_table_single_year$age_grp_start <- as.integer(life_table_single_year$age_grp_start)
  life_table_single_year$start_year    <- as.numeric(life_table_single_year$start_year)



  # write.csv(life_table,'data_outputs/life_table.csv',row.names = F)


  # plot aB
  # data <- life_table_single_year[life_table_single_year$loc_id==4 & life_table_single_year$age_grp_start==0,]
  # data <- dplyr::select(data,x=start_year, y=qB)
  # data %>% echarts4r::e_chart(x) %>% echarts4r::e_scatter(y,name='qB') %>% echarts4r::e_x_axis(min= 1900)

  return(list(life_table_single_year=life_table_single_year,
         population_single_year=population_single_year ))

}

