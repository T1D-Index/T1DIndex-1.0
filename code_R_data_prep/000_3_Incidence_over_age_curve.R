# incidence rate, Ronald Ma ---------------------------------------------------------------------------------------
run_adult_onset_surve <- function( min_study_start_year = 1980, min_overall_score=3, sub_saharan=FALSE,use_svd=FALSE ,score_equal=FALSE, run_CI=FALSE)
{ #
  # min_study_start_year = 1980 ;min_overall_score=3 ;use_svd=FALSE  ; sub_saharan=FALSE; run_CI <- TRUE
  # min_study_start_year = 1980 ;min_overall_score=3 ;use_svd=FALSE  ; sub_saharan=TRUE
  # min_study_start_year = 1978 ;min_overall_score=1 ;use_svd=FALSE  ; sub_saharan=TRUE ; score_equal=TRUE
  library(readxl)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(writexl)
  library(echarts4r)
  library(softImpute)
  library(reactable)
  library(arrow)
  # cell level rendering https://glin.github.io/reactable/articles/examples.html#cell-rendering-1
  source('code_tools/utils_plot.R')
  source('code_R/utils.R')

  incidence_Ronald <- data.frame(read_excel('data_incidence/incidence_adult_curve_raw.xlsx', 'incidence_Ronald_raw',skip = 0))



  incidence_Ronald$import[is.na(incidence_Ronald$import)] <- ''

  incidence_Ronald_list_of_studies <- incidence_Ronald[incidence_Ronald$import %in% c('yes','no'),]
  incidence_Ronald_list_of_studies <- unique(dplyr::select(incidence_Ronald_list_of_studies,"First.author",      "Country..region.",  "Study.year","Overall.Score","Inclusion"=import,"note"))
  incidence_Ronald_list_of_studies <- incidence_Ronald_list_of_studies[order(incidence_Ronald_list_of_studies$First.author),]
  incidence_Ronald_list_of_studies$"Sub Saharan" <- 'no'

  incidence_Ronald_list_of_studies$"Sub Saharan" [incidence_Ronald_list_of_studies$First.author %in%  c("Alemu64","Marshall","Mebrahtu","Sandy12")] <- 'yes'
  # write.csv(incidence_Ronald_list_of_studies,'temp/list_of_studies.csv',row.names=FALSE)

  incidence_Ronald <- incidence_Ronald[incidence_Ronald$import=='yes',]

  # fill NA CIs with perdentage lower and upper ---
  incidence_Ronald$lower <- as.numeric(incidence_Ronald$lower)
  incidence_Ronald$upper <- as.numeric(incidence_Ronald$upper)
  index_have_ci <- !is.na(incidence_Ronald$lower)
  ratio_lower <- median(incidence_Ronald$lower[index_have_ci]/incidence_Ronald$Incidence..95.CI.[index_have_ci])
  ratio_upper <- median(incidence_Ronald$upper[index_have_ci]/incidence_Ronald$Incidence..95.CI.[index_have_ci])

  incidence_Ronald$lower[!index_have_ci] <- incidence_Ronald$Incidence..95.CI.[!index_have_ci] * ratio_lower
  incidence_Ronald$upper[!index_have_ci] <- incidence_Ronald$Incidence..95.CI.[!index_have_ci] * ratio_upper

  if(run_CI)
  {
    incidence_Ronald$Incidence..95.CI. <- random_gen(mean=incidence_Ronald$Incidence..95.CI.
                                                     ,smr_ci_lower=incidence_Ronald$lower
                                                     ,smr_ci_upper=incidence_Ronald$upper
    )
  }

  # get middle year.------------------------------
  year <- incidence_Ronald$Study.year
  year <- gsub('T','',year )
  year <- gsub('\\*','',year )
  year_start <- as.numeric(substring(year,1,4))
  year_end   <- as.numeric(substring(year,nchar(year)-3,nchar(year)))
  year <- round((year_start+year_end)/2,0)

  incidence_Ronald$year <- year
  # ----------------------------
  exclude_list <- c("")

  exclude_list <- c("Alemu64","Marshall","Mebrahtu","Sandy12")
  # exclude_list <- c("Alemu64","Marshall","Mebrahtu","Sandy12")
  # exclude_list <- c("Alemu64")
  incidence_Ronald_all <- incidence_Ronald

  incidence_Ronald <- incidence_Ronald[!incidence_Ronald$First.author %in% exclude_list,]

  if(score_equal)
  {
    incidence_Ronald <- incidence_Ronald[incidence_Ronald$Overall.Score == min_overall_score ,]
  }else
  {
    incidence_Ronald <- incidence_Ronald[incidence_Ronald$Overall.Score >= min_overall_score ,]
  }


  incidence_Ronald <- incidence_Ronald[incidence_Ronald$year >= min_study_start_year ,]

  if(sub_saharan)
  {
    incidence_Ronald <- incidence_Ronald_all[incidence_Ronald_all$First.author %in% exclude_list,]
  }

  ## use sub african countries, but only for 0-24
  # incidence_Ronald_sub_africa <- incidence_Ronald_sub_africa[-(11:17),]
  # incidence_Ronald <- rbind(incidence_Ronald,incidence_Ronald_sub_africa )

  # if(min_study_start_year==2000)
  # {
  #   incidence_Ronald <- incidence_Ronald[incidence_Ronald$year < min_study_start_year+100 ,]
  #
  # }else
  # {
  #   incidence_Ronald <- incidence_Ronald[incidence_Ronald$year < min_study_start_year+10 ,]
  #
  # }
  incidence_Ronald_used     <- unique(dplyr::select(incidence_Ronald,"First.author",      "Country..region.",  "Study.year",year,"Overall.Score"))

  # write.csv(incidence_Ronald_used, "temp/studies_adult_curve.csv")

  incidence_Ronald_excluded <- unique(dplyr::select(incidence_Ronald[!incidence_Ronald$First.author %in%incidence_Ronald_used$First.author ,],"First.author",      "Country..region.",  "Study.year"))
  incidence_Ronald_excluded <- incidence_Ronald_excluded[!is.na(incidence_Ronald_excluded$First.author),]

  incidence_Ronald <- dplyr::select(incidence_Ronald,-year)

  # writexl::write_xlsx(list(
  #   'incidence_studies_used'                = incidence_Ronald_used
  #   ,'incidence_studies_excluded'           = incidence_Ronald_excluded
  # ), "data_incidence/list_of_studies.xlsx",col_names=TRUE)

  incidence_Ronald <- dplyr::select(incidence_Ronald,First.author,Country..region., Study.year,Sex , Age.at.Onset....,Incidence..95.CI. )
  incidence_Ronald <- incidence_Ronald[! is.na(incidence_Ronald$First.author),]


  rownames(incidence_Ronald) <- 1:nrow(incidence_Ronald)

  for(i in 1:nrow(incidence_Ronald))
  {
    age_start <- NA
    age_end <- NA
    age_bin_str <- incidence_Ronald$Age.at.Onset....[i]
    if(grepl('-',age_bin_str ))
    {
      age_start <- as.numeric( strsplit(age_bin_str,'-')[[1]][1])
      age_end   <- as.numeric( strsplit(age_bin_str,'-')[[1]][2])
    }
    incidence_Ronald$age_start[i] <- age_start
    incidence_Ronald$age_end[i]   <- age_end
  }


  incidence_Ronald <- incidence_Ronald[incidence_Ronald$Sex=='M+F',]
  incidence_expand <- data.frame()
  for(i in 1:nrow(incidence_Ronald))
  {
    incidence_temp <- incidence_Ronald[i,]
    incidence_expand_t <- do.call("rbind", replicate(incidence_temp$age_end-incidence_temp$age_start+1, incidence_temp, simplify = FALSE))
    incidence_expand_t$Age.at.Onset.... <- incidence_temp$age_start:(incidence_temp$age_end)
    incidence_expand <- rbind(incidence_expand, incidence_expand_t)
  }

  rownames(incidence_expand) <- 1:nrow(incidence_expand)
  incidence_expand$age_start <- NULL
  incidence_expand$age_end   <- NULL

  data_wide_MF <-  spread(incidence_expand,  Age.at.Onset...., Incidence..95.CI.)

  incidence_expand <- gather(data_wide_MF,key=Age.at.Onset...., value="Incidence..95.CI.",-"First.author",-"Country..region.",-"Study.year",-"Sex")

  incidence_expand <- incidence_expand[order(incidence_expand$First.author,decreasing = TRUE),]

  matrix_raw <-( dplyr::select(incidence_expand, x=Age.at.Onset...., y=First.author, z=  Incidence..95.CI. ))
  matrix_raw$x <- as.character(matrix_raw$x)
  matrix_raw$z <- as.numeric(matrix_raw$z)

  heat_map_raw <- plot_Heatmap(matrix_raw,"Incidence Raw")

  ## use ethiopia for sub african
  # matrix_raw_alemu <- matrix_raw[matrix_raw$y=="Alemu64",]
  # e_0 <- matrix_raw_alemu  %>%
  #   e_charts(x)  %>%
  #   e_step(z)  %>%
  #   e_title('Ethiopia')  %>%
  #   e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))
  #
  # saveRDS(e_0, file = "article_markdown/data/plot_incidence_alemu64.Rds")

  # try naive way of imputing the values --------------------------------
  data_wide_MF2 <-data_wide_MF
  matrix <- data_wide_MF2[,5:ncol(data_wide_MF)]


  if(FALSE)
  {
    for(i in 26:100)
    {
      matrix[,i] <- NA
    }
    colnames(matrix) <- 0:99
    # apoly global curve to sub-africa countries.

    for(i in 1:nrow(matrix))
    {
      for(j in 26:100)
      {
        # i <- 1; j <- 21
        matrix[i,j]   <- mean(unlist(matrix[i,1:20])) * matrix_plot_global_curve$z[j]/ mean(matrix_plot_global_curve$z[1:20])

      }
    }
    data_wide_MF2[,5:104] <- matrix
  }


  impute_By_Age_Ratio_Naive <- function(matrix)
  {
    matrix_impute <- matrix
    for(row_i in 1:nrow(matrix))
    {
      # row_i <- 2
      for(col_j in 1:ncol(matrix))
      {
        # col_j <- 19
        if(is.na(matrix[row_i, col_j]))
        {
          rows_with_data <- which(!is.na(matrix[,col_j]))
          col_vector     <- matrix[rows_with_data,col_j]

          cols_with_data <- which(!is.na(matrix[row_i,]))
          matrix_temp <-  matrix[rows_with_data, cols_with_data]

          row_vector     <- matrix[row_i,cols_with_data]

          index_not_null <- colSums(is.na(matrix_temp),2)==0
          matrix_temp    <- matrix_temp[,  index_not_null]
          row_vector     <- row_vector[ index_not_null]

          # matrix_impute[row_i,col_j] <- sum(row_vector) * (sum(col_vector)/sum(matrix_temp))

          matrix_impute[row_i,col_j] <- sum(row_vector) * mean(col_vector/rowSums(matrix_temp))

        }
      }
    }

    return(matrix_impute)
  }

  matrix_impute <- impute_By_Age_Ratio_Naive(matrix)

  data_wide_MF2[,5:ncol(data_wide_MF2)] <- matrix_impute


  # matrix2  %>%
  #   group_by(y)  %>%
  #   e_charts(x)  %>%
  #   e_line(z)  %>%
  #   e_title("Grouped data")

  #   SOftimpute -----------------------------------------------------------------
  if(use_svd)
  {

    data_wide_MF2 <-data_wide_MF
    # data_wide_MF2 <-data_wide_MF2[randomize_index,]
    Y <- data_wide_MF2[,5:ncol(data_wide_MF2)]
    Y <- as.matrix(Y)
    Y_scale <- Y
    lambda  <- 0
    tryCatch({
      Y_scale <- softImpute::biScale(Y, maxit = 100)
      lambda =0.44
    },error=function(e){Y_scale <- Y})

    fit1 <- softImpute::softImpute(Y_scale, rank.max = min(dim(Y)) - 1,  lambda =lambda )

    Y_predict <- complete(Y,fit1)

    Y_predict[Y_predict<0]<- 0
    data_wide_MF2[,5:ncol(data_wide_MF2)] <- Y_predict

  }



  incidence_expand2 <- gather(data_wide_MF2,key=Age.at.Onset...., value="Incidence..95.CI.",-"First.author",-"Country..region.",-"Study.year",-"Sex")

  incidence_expand2 <- incidence_expand2[order(incidence_expand2$First.author,decreasing = TRUE),]

  matrix_imputed <-( dplyr::select(incidence_expand2, x=Age.at.Onset...., y=First.author, z=  Incidence..95.CI. ))
  heat_map_imputed <- plot_Heatmap(matrix_imputed,"Incidence imputated")
  # #
  # matrix_imputed  %>%
  #   group_by(y)  %>%
  #   e_charts(x)  %>%
  #   e_line(z)  %>%
  #   e_title("Grouped data")
  #
  e_list <- ""

  for(i in 1:nrow(data_wide_MF))
  {
    # i <-2
    matrix_plot_raw     <- matrix_raw[matrix_raw$y==data_wide_MF$First.author[i],]

    matrix_plot_imputed <- matrix_imputed[matrix_imputed$y==data_wide_MF$First.author[i],]
    matrix_plot_imputed$y <- paste0(matrix_plot_imputed$y,' Imputed')

    # matrix_plot_imputed$z[!is.na(matrix_plot_raw$z) ] <- NA

    matrix_plot <- rbind(matrix_plot_imputed ,matrix_plot_raw)

    eval(parse(text = paste0(" e_",i,"  <-
      matrix_plot  %>%
      group_by(y)  %>%
      e_charts(x)  %>%
      e_step(z)  %>%
      e_title(data_wide_MF$First.author[i]) %>%
      e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))
      # e_y_axis(max = 100)
                             ") ))
    e_list <- paste0(e_list, " e_",i,", " )
    # plot <- data.frame(x=0:99,z=matrix_plot_imputed$z)  %>%
    #   e_charts(x)  %>%
    #   e_step(z)  %>%
    #   e_title("Rwanda") %>%
    #   e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))
    # # e_y_axis(max = 100)

    # saveRDS(plot, file = "article_markdown/data/plot_incidence_impute_rwanda.Rds")

  }




  # add global line ------------------------------
  matrix_imputed_scaled <- setDT(matrix_imputed)[,list(x=x,z=z,z_max = max(z)),by=c('y')]
  matrix_imputed_scaled$z_percentage <- round(matrix_imputed_scaled$z/matrix_imputed_scaled$z_max,2)*100

  matrix_plot <- setDT(matrix_imputed_scaled)[,list(y='Global',z = mean(z)),by=c('x')]
  matrix_plot <- setDT(matrix_plot)[,list(x=x,z=z,z_max = max(z)),by=c('y')]
  matrix_plot$z_percentage <- round(matrix_plot$z/matrix_plot$z_max,2)*100
  matrix_plot <- dplyr::select(matrix_plot,x,y,z=z_percentage)

  # saveRDS(matrix_plot, file = "article_markdown/data/DF_incidence_global_curve.Rds")
  # try plot US, India, Brazil and Rwanda using the global curve
  if(FALSE)
  {
    # incidence_demo <- incidence_calcs_new[incidence_calcs_new$`World Bank name` %in% c("United States","India","Brazil","Rwanda"),]
    incidence_demo <- incidence_calcs_new[incidence_calcs_new$`World Bank name` %in% c("Brazil"),]
    # incidence_demo <- select(incidence_demo, curve_base)
    x <- rep(0,100)
    x[1:5] <-incidence_demo$`0-4`
    x[6:10] <-incidence_demo$`5-9`
    x[11:15] <-incidence_demo$`10-14`
    for(i in 16:100)
    {
      # i <- 16
      x[i] <-  incidence_demo$curve_base *  matrix_plot$z[i]/ mean(matrix_plot$z[1:14])
    }
    e_0 <- data.frame(x=0:99, z= x)  %>%
      e_charts(x)  %>%
      e_step(z)  %>%
      e_title(incidence_demo$`World Bank name`)  %>%
      e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))

    # saveRDS(e_0, file = "article_markdown/data/plot_incidence_curve_brazil.Rds")


  }



  e_0 <- matrix_plot  %>%
    e_charts(x)  %>%
    e_step(z)  %>%
    e_title('African 0-24,with  Global 25+')  %>%
    e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))
  #   saveRDS(e_0, file = "article_markdown/plot_incidence_global_curve_naive.Rds")
  #   saveRDS(e_0, file = "article_markdown/data/plot_incidence_Africa_specific_curve_naive.Rds")
  # e_0
  # saveRDS(e_0, file = "article_markdown/plot_incidence_sensitivity_test_svd.Rds")

  # e_y_axis(max = 100)
  e_list <- paste0('e_0,',e_list )

  eval(parse(text =
               paste0("e_all <- e_arrange(",e_list,"   cols = 2)")
  ))

  writexl::write_xlsx(list(
    'incidence_spread_raw'           = data_wide_MF
    # ,'incidence_spread_imputed'           = data_wide_MF2
  ), "data_incidence/incidence_adult.xlsx",col_names=TRUE)

  #
  #
  # writexl::write_xlsx(list(
  #   'incidence_spread_raw'           = data_wide_MF
  #   ,'incidence_spread_imputed'           = data_wide_MF2
  # ), "data_incidence/incidence__imputed_matrix_completion.xlsx",col_names=TRUE)

  return(list(global_z=matrix_plot$z , number_of_studies = nrow(incidence_Ronald_used)))
}






if(FALSE)
{



  min_years_list <- c(1980,1990,2000)
  min_overall_score_list <- c(2,3,4)

  # min_years_list <- c(1980)
  # min_overall_score_list <- c(2)

  data_frame_plots <- data.frame()

  for(i in 1:length(min_years_list))
  {
    for(j in 1:length(min_overall_score_list))
    {
      print(i)
      #    i <- 1; j <- 1
      min_study_start_year <-  min_years_list[i]
      min_overall_score  <-  min_overall_score_list[j]

      results <- run_adult_onset_surve(min_study_start_year,min_overall_score,FALSE)
      global_z <-results$global_z
      number_of_studies <-results$number_of_studies
      data_frame_plots <- rbind(data_frame_plots,data.frame(min_year= min_study_start_year ,min_score=min_overall_score,plot= global_z, number_of_studies=number_of_studies ) )

    }
  }


  data <- setDT(data_frame_plots)[,list(

    "Minimum Grade >= 2" = list(c(unique(number_of_studies[min_score==2]),plot[min_score==2]) ),
    "Minimum Grade >= 3" = list(c(unique(number_of_studies[min_score==3]),plot[min_score==3])),
    "Minimum Grade >= 4" = list(c(unique(number_of_studies[min_score==4]),plot[min_score==4]))

  ),by=c('min_year') ]

  colnames(data)[1] <- "Study Year"


  plot_reactable <- reactable(data, resizable = TRUE,columns = list(

    "Minimum Grade >= 2" = colDef(cell = function(value, index) {

      data.frame(x=0:99,z=value[-1])  %>%
        e_charts(x,width = 400,height=400)  %>%
        e_step(z)  %>%
        e_title(paste0('Global #Studies:',value[1]) )  %>%
        e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))

    }),
    "Minimum Grade >= 3" = colDef(cell = function(value, index) {

      data.frame(x=0:99,z=value[-1])  %>%
        e_charts(x,width = 400,height=400)  %>%
        e_step(z)  %>%
        e_title(paste0('Global #Studies:',value[1]) )  %>%
        e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))

    }),
    "Minimum Grade >= 4" = colDef(cell = function(value, index) {

      data.frame(x=0:99,z=value[-1])  %>%
        e_charts(x,width = 400,height=400)  %>%
        e_step(z)  %>%
        e_title(paste0('Global #Studies:',value[1]) )  %>%
        e_x_axis(axisLabel = list(interval = 9 ,  rotate = 0))  })
  ))


  #   saveRDS(plot_reactable, file = "article_markdown/plot_incidence_sensitivity_test_naive.Rds")
  #   saveRDS(plot_reactable, file = "article_markdown/plot_incidence_sensitivity_test_svd.Rds")




  # global curve :

  results_34 <- run_adult_onset_surve(1980,3,FALSE)
  results_2  <- run_adult_onset_surve(1980,2,FALSE,score_equal=TRUE)

  comparison <- data.frame(`Scores 3 or 4` = results_34$global_z)
  comparison$`Scores 2` <- results_2$global_z



  results <- run_adult_onset_surve(1980,1,FALSE)


  global_curve <-run_adult_onset_surve(2000,2,FALSE , FALSE)
  # write.csv(data.frame(x= 0 :79 , y= global_curve$global_z[1:80] ), "temp/global_curve_.csv",row.names = FALSE)




}
