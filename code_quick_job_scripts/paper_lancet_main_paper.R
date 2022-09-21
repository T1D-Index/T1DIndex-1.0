source("code_R/data.R")
source("code_R_data_prep/DATA_CONSTANTS.R")



query_compose_execute <-function(year,age_min,age_max, country_filter=c(),schema = "main_ci_partition_scenario_2")
{  #  year <- 2021 ; age_min  <- 0; age_max <- 99 ; country_filter="" ; schema = "main_ci_partition"

  country_filter <- gsub("'","''",country_filter)
  if(length(country_filter) )
  {
    country_filter <- paste0(country_filter, collapse = "','")
    country_filter <- paste0('AND "Country" in  (\'', country_filter,'\')')

  }

  host <- "localhost"
  connec <- dbConnect(RPostgres::Postgres(),  dbname = "t1d", host = host,
                      port = "5432",  user = "postgres",   password = "postgrest1d")
  query <-
    paste0('SELECT "Country",
          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Prevalence"))::numeric,0) as Prevalence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Prevalence"))::numeric,0) as Prevalence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Prevalence"))::numeric,0) as Prevalence_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ghosts"))::numeric,0) as Missing_Prevalence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ghosts"))::numeric,0) as Missing_Prevalence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ghosts"))::numeric,0) as Missing_Prevalence_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,0) as missing_diagnosed_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,0) as missing_diagnosed_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,0) as missing_diagnosed_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,0) as missing_non_diagnosed_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,0) as missing_non_diagnosed_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,0) as missing_non_diagnosed_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY Incidence))::numeric,0) as Incidence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY Incidence))::numeric,0) as Incidence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY Incidence))::numeric,0) as Incidence_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY Incidence_rate))::numeric,1) as Incidence_rate_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY Incidence_rate))::numeric,1) as Incidence_rate_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY Incidence_rate))::numeric,1) as Incidence_rate_upper,




          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,0) as life_expectancy_non_t1ds_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,0) as life_expectancy_non_t1ds_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,0) as life_expectancy_non_t1ds_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY life_expectancy))::numeric,0) as life_expectancy_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY life_expectancy))::numeric,0) as life_expectancy_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY life_expectancy))::numeric,0) as life_expectancy_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,0) as early_death_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,0) as early_death_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,0) as early_death_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,0) as onset_death_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,0) as onset_death_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,0) as onset_death_upper

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_upper



          FROM
          (
              SELECT     "Country" , "run", "Prevalence","Ghosts",missing_diagnosed,missing_non_diagnosed, "Ann. early deaths", "Ann. onset deaths",Incidence
              , life_expectancy * diagnosis_rate + (1-diagnosis_rate)*0.5 AS life_expectancy
              --, life_expectancy  AS life_expectancy
              , life_expectancy_non_t1ds  AS life_expectancy_non_t1ds
              ,"Ann. background population"
              , Incidence /  "Ann. background population" * 100000   Incidence_rate

              FROM
              (
                  SELECT  "Country" , "run"
                  ,SUM("Prevalence") as "Prevalence"
                  ,SUM("Ann. background population") as "Ann. background population"
                  ,SUM("Ghosts (early death)") as "missing_diagnosed"
                  ,SUM("Ghosts (onset death)") as "missing_non_diagnosed"
                  ,SUM("Ghosts") as "Ghosts"
                  ,SUM("Ann. early deaths") as "Ann. early deaths"
                  ,SUM("Ann. onset deaths") as "Ann. onset deaths"
                  ,SUM("Incidence (1 base)") as  Incidence
                  ,SUM(CASE WHEN "Age"=10 THEN "Life expectency (1 background)" ELSE 0 END) as life_expectancy_non_t1ds
                  ,SUM(CASE WHEN "Age"=10 THEN "Life expectency (2 t1d base)" ELSE 0 END) as life_expectancy
                  ,SUM(CASE WHEN "Age"=10 THEN ("Incidence (1 base)"+0.001) / ("Ann. onset deaths"  + "Incidence (1 base)"+0.001) ELSE 0 END) as diagnosis_rate

                  ,sum(case when "Prevalence_Cumulative"<= "Prevalence_Total"/2 then 1 else 0 end)-1 median_age_prevalence
                  FROM

                      (
                        SELECT  *
                        ,  sum("Prevalence") over (partition by run, "Country" order by run,"Country","Age")          "Prevalence_Cumulative"
                        ,  sum("Prevalence") over (partition by run, "Country" )                                      "Prevalence_Total"
                        ,  sum("Incidence (1 base)") over (partition by run, "Country" order by run,"Country","Age")  "Incicdence_Cumulative"
                        FROM ',schema,'.main_ci_',year,'
                          WHERE 1=1
                     AND "Age" >= ',age_min,'
                     AND "Age" <= ',age_max,'
                     ',country_filter,'
                      )AS apple
                  group by "Country","run"
              ) AS apple
          ) AS apple
          GROUP BY "Country" ')

  data_put <- RPostgreSQL::dbGetQuery(connec, query )

  dbDisconnect(connec)

  return(data_put)
}


query_compose_execute_aggregate <-function(year,age_min,age_max,schema = "main_ci_partition_scenario_2",countries)
{  # year <- 2021 ; age_min  <- 0; age_max <- 99 ; country_filter="" ; schema = "main_ci_partition_scenario_2"


  host <- "localhost"
  connec <- dbConnect(RPostgres::Postgres(),  dbname = "t1d", host = host,
                      port = "5432",  user = "postgres",   password = "postgrest1d")
  round_digit <- 2
  query <-
    paste0('SELECT "Country",
          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Prevalence"))::numeric,',round_digit,') as Prevalence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Prevalence"))::numeric,',round_digit,') as Prevalence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Prevalence"))::numeric,',round_digit,') as Prevalence_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ghosts"))::numeric,',round_digit,') as Missing_Prevalence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ghosts"))::numeric,',round_digit,') as Missing_Prevalence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ghosts"))::numeric,',round_digit,') as Missing_Prevalence_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,',round_digit,') as missing_diagnosed_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,',round_digit,') as missing_diagnosed_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY missing_diagnosed))::numeric,',round_digit,') as missing_diagnosed_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,',round_digit,') as missing_non_diagnosed_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,',round_digit,') as missing_non_diagnosed_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY missing_non_diagnosed))::numeric,',round_digit,') as missing_non_diagnosed_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY Incidence))::numeric,',round_digit,') as Incidence_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY Incidence))::numeric,',round_digit,') as Incidence_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY Incidence))::numeric,',round_digit,') as Incidence_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY Incidence_rate))::numeric,',round_digit,') as Incidence_rate_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY Incidence_rate))::numeric,',round_digit,') as Incidence_rate_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY Incidence_rate))::numeric,',round_digit,') as Incidence_rate_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,',round_digit,') as life_expectancy_non_t1ds_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,',round_digit,') as life_expectancy_non_t1ds_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY life_expectancy_non_t1ds))::numeric,',round_digit,') as life_expectancy_non_t1ds_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY life_expectancy))::numeric,',round_digit,') as life_expectancy_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY life_expectancy))::numeric,',round_digit,') as life_expectancy_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY life_expectancy))::numeric,',round_digit,') as life_expectancy_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,',round_digit,') as early_death_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,',round_digit,') as early_death_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. early deaths"))::numeric,',round_digit,') as early_death_upper,

          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,',round_digit,') as onset_death_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,',round_digit,') as onset_death_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. onset deaths"))::numeric,',round_digit,') as onset_death_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. total deaths"))::numeric,',round_digit,') as total_death_lower ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. total deaths"))::numeric,',round_digit,') as total_death_median ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. total deaths"))::numeric,',round_digit,') as total_death_upper,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. early deaths/1000s"))::numeric,',round_digit,') as early_death_lower_1000 ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. early deaths/1000s"))::numeric,',round_digit,') as early_death_median_1000 ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. early deaths/1000s"))::numeric,',round_digit,') as early_death_upper_1000,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. onset deaths/1000s"))::numeric,',round_digit,') as onset_death_lower_1000 ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. onset deaths/1000s"))::numeric,',round_digit,') as onset_death_median_1000 ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. onset deaths/1000s"))::numeric,',round_digit,') as onset_death_upper_1000,


          ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY "Ann. total deaths/1000s"))::numeric,',round_digit,') as total_death_lower_1000 ,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. total deaths/1000s"))::numeric,',round_digit,') as total_death_median_1000 ,
          ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY "Ann. total deaths/1000s"))::numeric,',round_digit,') as total_death_upper_1000,

          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY "Ann. background population"))::numeric,',round_digit,') as background_population,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY prevalence_missing_10))::numeric,',round_digit,') as prevalence_missing_10,
          ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY background_population_10))::numeric,',round_digit,') as background_population_10

         --  ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_lower ,
         --  ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_median ,
         --  ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY median_age_prevalence))::numeric,0) as median_age_prevalence_upper,

          -- ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY median_age_prevalence_iqr))::numeric,0) as median_age_prevalence_iqr_lower ,
         --  ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY median_age_prevalence_iqr))::numeric,0) as median_age_prevalence_iqr_median ,
         --  ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY median_age_prevalence_iqr))::numeric,0) as median_age_prevalence_iqr_upper,

          -- ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY median_age_incidence))::numeric,0) as median_age_incidence_lower ,
          -- ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY median_age_incidence))::numeric,0) as median_age_incidence_median ,
          -- ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY median_age_incidence))::numeric,0) as median_age_incidence_upper,


          -- ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY median_age_incidence_iqr))::numeric,0) as median_age_incidence_iqr_lower ,
          -- ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY median_age_incidence_iqr))::numeric,0) as median_age_incidence_iqr_median ,
          -- ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY median_age_incidence_iqr))::numeric,0) as median_age_incidence_iqr_upper,


         -- ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY mean_age_prevalence))::numeric,0) as mean_age_prevalence_lower ,
          -- ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY mean_age_prevalence))::numeric,0) as mean_age_prevalence_median ,
          -- ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY mean_age_prevalence))::numeric,0) as mean_age_prevalence_upper,

         -- ROUND( (percentile_cont(0.025) WITHIN GROUP (ORDER BY mean_age_incidence))::numeric,0) as mean_age_incidence_lower ,
          -- ROUND( (percentile_cont(0.5)   WITHIN GROUP (ORDER BY mean_age_incidence))::numeric,0) as mean_age_incidence_median ,
          -- ROUND( (percentile_cont(0.975) WITHIN GROUP (ORDER BY mean_age_incidence))::numeric,0) as mean_age_incidence_upper

          FROM
          (
              SELECT     "Country" , "run", "Prevalence","Ghosts",missing_diagnosed,missing_non_diagnosed, "Ann. early deaths", "Ann. onset deaths","Ann. total deaths",Incidence
              , life_expectancy * diagnosis_rate + (1-diagnosis_rate)*0.5 AS life_expectancy
              --, life_expectancy  AS life_expectancy
              , life_expectancy_non_t1ds  AS life_expectancy_non_t1ds
              ,"Ann. background population"
              , Incidence /  "Ann. background population" * 100000   Incidence_rate
              ,prevalence_missing_10
              ,background_population_10
              -- ,median_age_prevalence
              -- ,median_age_incidence
              -- ,median_age_prevalence_iqr
              -- ,median_age_incidence_iqr
              -- ,mean_age_prevalence
              -- ,mean_age_incidence
,"Ann. total deaths/1000s"
,"Ann. early deaths/1000s"
,"Ann. onset deaths/1000s"
              FROM
              (
                  SELECT  "Country" , "run"
                  ,SUM("Prevalence") as "Prevalence"
                  ,SUM("Ann. background population") as "Ann. background population"
                  ,SUM("Ghosts (early death)") as "missing_diagnosed"
                  ,SUM("Ghosts (onset death)") as "missing_non_diagnosed"
                  ,SUM("Ghosts") as "Ghosts"

                  ,SUM(CASE WHEN "Age"=10 THEN "Prevalence"+ "Ghosts" ELSE 0 END) as prevalence_missing_10
                  ,SUM(CASE WHEN "Age"=10 THEN "Ann. background population" ELSE 0 END) as background_population_10

                  ,SUM("Ann. early deaths") as "Ann. early deaths"
                  ,SUM("Ann. onset deaths") as "Ann. onset deaths"
                  ,SUM("Ann. onset deaths") + SUM("Ann. early deaths") as "Ann. total deaths"
                  ,round((sum("Ann. early deaths")::numeric + sum("Ann. onset deaths")::numeric) *1000/sum("Ann. background population")::numeric,2)  "Ann. total deaths/1000s"
                  ,round((sum("Ann. early deaths")::numeric) *1000/sum("Ann. background population")::numeric,2)  "Ann. early deaths/1000s"
                  ,round((sum("Ann. onset deaths")::numeric) *1000/sum("Ann. background population")::numeric,2)  "Ann. onset deaths/1000s"

                  ,SUM("Incidence (1 base)") as  Incidence

                  ,SUM(CASE WHEN "Age"=10 THEN "Life expectency (1 background)" ELSE 0 END) as life_expectancy_non_t1ds
                  ,SUM(CASE WHEN "Age"=10 THEN "Life expectency (2 t1d base)"   ELSE 0 END) as life_expectancy
                  ,SUM(CASE WHEN "Age"=10 THEN ("Incidence (1 base)"+0.001) / ("Ann. onset deaths"  + "Incidence (1 base)"+0.001) ELSE 0 END) as diagnosis_rate

                  -- ,sum(case when "Prevalence_Cumulative"<= "Prevalence_Total"/2 then 1 else 0 end)-1 median_age_prevalence
                  -- ,sum(case when "Incidence_Cumulative"<= "Incidence_Total"/2 then 1 else 0 end)-1 median_age_incidence

                  -- ,sum(case when "Prevalence_Cumulative"<= "Prevalence_Total"/4*3 then 1 else 0 end)-1 - (sum(case when "Prevalence_Cumulative"<= "Prevalence_Total"/4*1 then 1 else 0 end)-1) median_age_prevalence_iqr
                  -- ,sum(case when "Incidence_Cumulative"<= "Incidence_Total"/4*3 then 1 else 0 end)-1 - (sum(case when "Incidence_Cumulative"<= "Incidence_Total"/4*1 then 1 else 0 end)-1) median_age_incidence_iqr

                  -- ,sum("Prevalence"  * ("Age" +1) )/ (sum("Prevalence")+0.01) - 1                 mean_age_prevalence
                  -- ,sum("Incidence (1 base)"  * ("Age" +1) )/ (sum("Incidence (1 base)"+0.01) ) - 1 mean_age_incidence

                     FROM

                      (
                        SELECT  *
                       -- ,  sum("Prevalence") over (partition by run, "Country" order by run,"Country","Age")          "Prevalence_Cumulative"
                        --,  sum("Prevalence") over (partition by run, "Country" )                                      "Prevalence_Total"
                       -- ,  sum("Incidence (1 base)") over (partition by run, "Country" order by run,"Country","Age")  "Incidence_Cumulative"
                       -- ,  sum("Incidence (1 base)") over (partition by run, "Country" )                              "Incidence_Total"
                        FROM ',schema,'.main_ci_',year,'
                          WHERE 1=1
                     AND "Age" >= ',age_min,'
                     AND "Age" <= ',age_max,'
                      )AS apple
                  group by "Country","run"
              ) AS apple
          ) AS apple
          GROUP BY "Country" ')

  data_put <- RPostgreSQL::dbGetQuery(connec, query )
  colnames_initial <- colnames(data_put)

  aggregate_country <- function(data_put)
  {
    data_put_global <- setDT(data_put)[,list(
      prevalence_lower=sum(prevalence_lower)                , prevalence_median=sum(prevalence_median)                , prevalence_upper=sum(prevalence_upper),
      missing_prevalence_lower=sum(missing_prevalence_lower), missing_prevalence_median=sum(missing_prevalence_median), missing_prevalence_upper=sum(missing_prevalence_upper),
      missing_diagnosed_lower=sum(missing_diagnosed_lower)  , missing_diagnosed_median=sum(missing_diagnosed_median), missing_diagnosed_upper=sum(missing_diagnosed_upper),
      missing_non_diagnosed_lower=sum(missing_non_diagnosed_lower)  , missing_non_diagnosed_median=sum(missing_non_diagnosed_median), missing_non_diagnosed_upper=sum(missing_non_diagnosed_upper),
      incidence_lower=sum(incidence_lower)  , incidence_median=sum(incidence_median), incidence_upper=sum(incidence_upper),

      incidence_rate_lower= round(sum(incidence_lower)/sum(background_population)*100000 ,round_digit)
      , incidence_rate_median=round(sum(incidence_median)/sum(background_population)*100000,round_digit)
      , incidence_rate_upper=round(sum(incidence_upper)/sum(background_population)*100000,round_digit),

      life_expectancy_non_t1ds_lower=round(sum(life_expectancy_non_t1ds_lower*background_population_10)/sum(background_population_10),round_digit)
      , life_expectancy_non_t1ds_median=round(sum(life_expectancy_non_t1ds_median*background_population_10)/sum(background_population_10),round_digit)
      , life_expectancy_non_t1ds_upper=round(sum(life_expectancy_non_t1ds_upper*background_population_10)/sum(background_population_10),round_digit),

      life_expectancy_lower=round(sum(life_expectancy_lower * (prevalence_missing_10 ) )/ sum(prevalence_missing_10 ),round_digit)
      , life_expectancy_median=round(sum(life_expectancy_median* (prevalence_missing_10 )  )/ sum(prevalence_missing_10),round_digit)
      , life_expectancy_upper=round(sum(life_expectancy_upper* (prevalence_missing_10 )  )/ sum(prevalence_missing_10),round_digit),


      # median_age_prevalence_lower=round(sum(median_age_prevalence_lower * (prevalence_median ) )/ sum(prevalence_median ),round_digit)
      # , median_age_prevalence_median=round(sum(median_age_prevalence_median* (prevalence_median )  )/ sum(prevalence_median),round_digit)
      # , median_age_prevalence_upper=round(sum(median_age_prevalence_upper* (prevalence_median )  )/ sum(prevalence_median),round_digit),
      #
      #
      # median_age_prevalence_iqr_lower=round(sum(median_age_prevalence_iqr_lower * (prevalence_median ) )/ sum(prevalence_median ),round_digit)
      # , median_age_prevalence_iqr_median=round(sum(median_age_prevalence_iqr_median* (prevalence_median )  )/ sum(prevalence_median),round_digit)
      # , median_age_prevalence_iqr_upper=round(sum(median_age_prevalence_iqr_upper* (prevalence_median )  )/ sum(prevalence_median),round_digit),
      #
      #
      # median_age_incidence_lower=round(sum(median_age_incidence_lower * (incidence_median ) )/ sum(incidence_median ),round_digit)
      # , median_age_incidence_median=round(sum(median_age_incidence_median* (incidence_median )  )/ sum(incidence_median),round_digit)
      # , median_age_incidence_upper=round(sum(median_age_incidence_upper* (incidence_median )  )/ sum(incidence_median),round_digit),
      #
      # median_age_incidence_iqr_lower=round(sum(median_age_incidence_iqr_lower * (incidence_median ) )/ sum(incidence_median ),round_digit)
      # , median_age_incidence_iqr_median=round(sum(median_age_incidence_iqr_median* (incidence_median )  )/ sum(incidence_median),round_digit)
      # , median_age_incidence_iqr_upper=round(sum(median_age_incidence_iqr_upper* (incidence_median )  )/ sum(incidence_median),round_digit),
      #
      #
      #   mean_age_prevalence_lower=round(sum(mean_age_prevalence_lower * (prevalence_median ) )/ sum(prevalence_median ),round_digit)
      # , mean_age_prevalence_median=round(sum(mean_age_prevalence_median* (prevalence_median )  )/ sum(prevalence_median),round_digit)
      # , mean_age_prevalence_upper=round(sum(mean_age_prevalence_upper* (prevalence_median )  )/ sum(prevalence_median),round_digit),
      #
      #   mean_age_incidence_lower=round(sum(mean_age_incidence_lower * (incidence_median ) )/ sum(incidence_median ),round_digit)
      # , mean_age_incidence_median=round(sum(mean_age_incidence_median* (incidence_median )  )/ sum(incidence_median),round_digit)
      # , mean_age_incidence_upper=round(sum(mean_age_incidence_upper* (incidence_median )  )/ sum(incidence_median),round_digit),



      early_death_lower=sum(early_death_lower)  , early_death_median=sum(early_death_median), early_death_upper=sum(early_death_upper),
      onset_death_lower=sum(onset_death_lower)  , onset_death_median=sum(onset_death_median), onset_death_upper=sum(onset_death_upper),
      total_death_lower=sum(total_death_lower)  , total_death_median=sum(total_death_median), total_death_upper=sum(total_death_upper),


      early_death_lower_1000=sum(early_death_lower_1000)  , early_death_median_1000=sum(early_death_median_1000), early_death_upper_1000=sum(early_death_upper_1000),
      onset_death_lower_1000=sum(onset_death_lower_1000)  , onset_death_median_1000=sum(onset_death_median_1000), onset_death_upper_1000=sum(onset_death_upper_1000),
      total_death_lower_1000=sum(total_death_lower_1000)  , total_death_median_1000=sum(total_death_median_1000), total_death_upper_1000=sum(total_death_upper_1000),


      background_population = sum(background_population)
    ),by=c("Country")]

  }

  data_put_global <- data_put[data_put$Country %in% countries$world_bank_name,]

  data_put_global$Country <- "GLOBAL"

  data_put_global <- aggregate_country(data_put_global)


  data_put_incom_level <- data_put %>% dplyr::inner_join(select(countries, Country= world_bank_name, group_by= wd_income_category ))
  data_put_incom_level$Country <- data_put_incom_level$group_by

  data_put_incom_level <- aggregate_country(data_put_incom_level)

  data_put_region <- data_put %>% dplyr::inner_join(select(countries, Country= world_bank_name, group_by= wd_region ))
  data_put_region$Country <- data_put_region$group_by

  data_put_region <- aggregate_country(data_put_region)

  data_put <- data_put[data_put$Country %in% countries$world_bank_name,]

  data_put_all <- rbind(setDF(data_put_global)[,colnames(data_put_global)]
                        ,setDF(data_put_incom_level)[,colnames(data_put_global)]
                        ,setDF(data_put_region)[,colnames(data_put_global)]
                        ,setDF(data_put)[,colnames(data_put_global)])

  dbDisconnect(connec)

  # write.csv(data_put_all, "temp/data_all_wider_CIs_2021.csv")
  return(data_put_all)
}


library(RPostgreSQL)
library(dplyr)
library(data.table)
library(stringi)
countries <- readRDS("data_internal/country.Rds")
countries$wd_income_category[countries$wd_income_category==''] <- countries$world_bank_classification[countries$wd_income_category=='' ]

format_comma <- function(value)
{ # value <- total_00_99$prevalence_median
  value_return <-  format(round(as.numeric(value), 0), nsmall=0, big.mark=",")

  index_1_significant <- lapply(trimws(value_return), nchar)==1
  value_return[index_1_significant] <-  format(round(as.numeric(value[index_1_significant]), 1), nsmall=1, big.mark=",")

  value_return
}


if(FALSE)
{

  # compose  main_table1 --------------------------------------
  total_00_99 <- query_compose_execute_aggregate (2021,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_00_19 <- query_compose_execute_aggregate (2021,0,19,schema = "main_ci_partition_scenario_1",countries)
  total_20_99 <- query_compose_execute_aggregate (2021,20,99,schema = "main_ci_partition_scenario_1",countries)

  # total_00_99  <- query_compose_execute(2021,0,99)
  # total_00_19  <- query_compose_execute(2021,0,19)
  # total_20_99  <- query_compose_execute(2021,20,99)

  round_digit <- 1
  total_00_99$prevalence_median <-  trimws(format_comma(total_00_99$prevalence_median))
  total_00_99$prevalence_lower  <-  trimws(format_comma(total_00_99$prevalence_lower))
  total_00_99$prevalence_upper  <-  trimws(format_comma(total_00_99$prevalence_upper))

  total_00_99$'Total' <- paste0( total_00_99$prevalence_median,'\n('
                                 ,total_00_99$prevalence_lower,'-',total_00_99$prevalence_upper,')')



  total_00_19$prevalence_median <-  trimws(format_comma(total_00_19$prevalence_median))
  total_00_19$prevalence_lower <-  trimws(format_comma(total_00_19$prevalence_lower))
  total_00_19$prevalence_upper <-  trimws(format_comma(total_00_19$prevalence_upper))

  total_00_19$'<20' <- paste0( total_00_19$prevalence_median,'\n('
                               ,total_00_19$prevalence_lower,'-',total_00_19$prevalence_upper,')')


  total_20_99$prevalence_median <-  trimws(format_comma(total_20_99$prevalence_median))
  total_20_99$prevalence_lower <-  trimws(format_comma(total_20_99$prevalence_lower))
  total_20_99$prevalence_upper <-  trimws(format_comma(total_20_99$prevalence_upper))

  total_20_99$'20+' <- paste0( total_20_99$prevalence_median,'\n('
                               ,total_20_99$prevalence_lower,'-',total_20_99$prevalence_upper,')')




  total_00_99$life_expectancy_median <-  round(total_00_99$life_expectancy_median,0)
  total_00_99$life_expectancy_lower <-  round(total_00_99$life_expectancy_lower,0)
  total_00_99$life_expectancy_upper <-  round(total_00_99$life_expectancy_upper,0)

  total_00_99$life_expectancy_non_t1ds_median <-  round(total_00_99$life_expectancy_non_t1ds_median,0)

  total_00_99$'T1D-onset' <- paste0( total_00_99$life_expectancy_median,'\n('
                                     ,total_00_99$life_expectancy_lower,'-',total_00_99$life_expectancy_upper,')')





  total_merge <- cbind(  dplyr::select(total_00_99,Country, Total = Total )
                         ,dplyr::select(total_00_19        , "<20" = '<20' )
                         ,dplyr::select(total_20_99        , "20+" = '20+')
                         ,dplyr::select(total_00_99        , "T1D-onset" = 'T1D-onset')
                         ,dplyr::select(total_00_99        , "Non-T1D" = life_expectancy_non_t1ds_median)
  )

  # country_db <- run_query_df ('SELECT * FROM country;')
  country_db <- readRDS("data_internal/country.Rds")
  country_db <- dplyr::select(country_db, Country= world_bank_name, Region=wd_region)

  total_merge <- total_merge %>% dplyr::left_join(country_db, by="Country")
  total_merge$Region[is.na(total_merge$Region)] <- "AA"




  spacing <- total_merge[1,]; spacing[] <- ""

  table1 <- rbind(
    total_merge[total_merge$Country=="GLOBAL",]
    ,total_merge[total_merge$Country=="HIC",]
    ,total_merge[total_merge$Country=="UMIC",]
    ,total_merge[total_merge$Country=="LMIC",]
    ,total_merge[total_merge$Country=="LIC",]
    ,spacing
    ,total_merge[total_merge$Country=="North America",]
    ,total_merge[total_merge$Region=="North America",]
    ,spacing
    ,total_merge[total_merge$Country=="Latin America & Caribbean",]
    ,total_merge[total_merge$Region=="Latin America & Caribbean",]
    ,spacing
    ,total_merge[total_merge$Country=="Europe & Central Asia",]
    ,total_merge[total_merge$Region=="Europe & Central Asia",]
    ,spacing
    ,total_merge[total_merge$Country=="Middle East & North Africa",]
    ,total_merge[total_merge$Region=="Middle East & North Africa",]
    ,spacing
    ,total_merge[total_merge$Country=="Sub-Saharan Africa" ,]
    ,total_merge[total_merge$Region=="Sub-Saharan Africa" ,]
    ,spacing
    ,total_merge[total_merge$Country=="South Asia" ,]
    ,total_merge[total_merge$Region=="South Asia" ,]
    ,spacing
    ,total_merge[total_merge$Country=="East Asia & Pacific",]
    ,total_merge[total_merge$Region=="East Asia & Pacific",]
  )


  table1$Region <- NULL

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  rownames(code_paper_country_name) <- code_paper_country_name$code
  index_replace <- table1$Country  %in% code_paper_country_name$code
  table1$Country[index_replace] <- code_paper_country_name[table1$Country[index_replace],]$paper


  table1$Country <- stri_trans_general(table1$Country, "Latin-ASCII")

  # split row for font control
  table2 <- table1[0,]

  for(i in 1:nrow(table1))
  {  # i <- 1; j <- 2
    for(j in 2:5)
    {
      texts <-  strsplit(  table1[i,j],"\n" )[[1]]
      table2[i*2-1,j] <- texts[1]
      table2[i*2,j]   <- texts[2]
    }

    table2[i*2-1,c(1,6)] <- table1[i,c(1,6)]
    # table2[i*2,]   <- table1[i,]

  }
  table2[is.na(table2)] <- ""


  # write.csv(table2,"temp/Main_paper_Table1_ci.csv",row.names = FALSE)


  # Table 2 ----------------------------------------------

  total_00_99  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_00_24  <- query_compose_execute_aggregate(2021,0,24,schema = "main_ci_partition_scenario_2",countries)
  total_25_99  <- query_compose_execute_aggregate(2021,25,99,schema = "main_ci_partition_scenario_2",countries)


  total_00_99$total_death <- total_00_99$early_death_median + total_00_99$onset_death_median
  total_merge <- cbind(  dplyr::select(total_00_24,Country, "<25 non-diagnosis"=onset_death_median , "<25 early mortality"=early_death_median  )
                         ,dplyr::select(total_25_99        , "25+ early mortality" = early_death_median )
                         ,dplyr::select(total_00_99        , "Total" = total_death ))
  table2 <- rbind(
    total_merge[total_merge$Country=="GLOBAL",],
    total_merge[total_merge$Country=="North America",],
    total_merge[total_merge$Country=="Latin America & Caribbean",],
    total_merge[total_merge$Country=="Europe & Central Asia",],
    total_merge[total_merge$Country=="Middle East & North Africa",],
    total_merge[total_merge$Country=="Sub-Saharan Africa",],
    total_merge[total_merge$Country=="South Asia",],
    total_merge[total_merge$Country=="East Asia & Pacific",]
  )


  # lower
  total_00_99$total_death <- total_00_99$early_death_lower + total_00_99$onset_death_lower
  total_merge <- cbind(  dplyr::select(total_00_24,Country, "<25 non-diagnosis"=onset_death_lower , "<25 early mortality"=early_death_lower  )
                         ,dplyr::select(total_25_99        , "25+ early mortality" = early_death_lower )
                         ,dplyr::select(total_00_99        , "Total" = total_death ))
  table2_lower <- rbind(
    total_merge[total_merge$Country=="GLOBAL",],
    total_merge[total_merge$Country=="North America",],
    total_merge[total_merge$Country=="Latin America & Caribbean",],
    total_merge[total_merge$Country=="Europe & Central Asia",],
    total_merge[total_merge$Country=="Middle East & North Africa",],
    total_merge[total_merge$Country=="Sub-Saharan Africa",],
    total_merge[total_merge$Country=="South Asia",],
    total_merge[total_merge$Country=="East Asia & Pacific",]
  )

  # upper
  total_00_99$total_death <- total_00_99$early_death_upper + total_00_99$onset_death_upper
  total_merge <- cbind(  dplyr::select(total_00_24,Country, "<25 non-diagnosis"=onset_death_upper , "<25 early mortality"=early_death_upper  )
                         ,dplyr::select(total_25_99        , "25+ early mortality" = early_death_upper )
                         ,dplyr::select(total_00_99        , "Total" = total_death ))
  table2_upper <- rbind(
    total_merge[total_merge$Country=="GLOBAL",],
    total_merge[total_merge$Country=="North America",],
    total_merge[total_merge$Country=="Latin America & Caribbean",],
    total_merge[total_merge$Country=="Europe & Central Asia",],
    total_merge[total_merge$Country=="Middle East & North Africa",],
    total_merge[total_merge$Country=="Sub-Saharan Africa",],
    total_merge[total_merge$Country=="South Asia",],
    total_merge[total_merge$Country=="East Asia & Pacific",]
  )

  table2$Total <- format(round(table2$Total/1000,1), nsmall=0, big.mark=",")
  table2$`25+ early mortality` <- format(round(table2$`25+ early mortality`/1000,1), nsmall=0, big.mark=",")
  table2$`<25 non-diagnosis`  <- format(round(table2$`<25 non-diagnosis` /1000,1), nsmall=0, big.mark=",")
  table2$`<25 early mortality` <- format(round(table2$`<25 early mortality`/1000,1), nsmall=0, big.mark=",")

  table2_lower$Total <- format(round(table2_lower$Total/1000,1), nsmall=0, big.mark=",")
  table2_lower$`25+ early mortality` <- format(round(table2_lower$`25+ early mortality`/1000,1), nsmall=0, big.mark=",")
  table2_lower$`<25 non-diagnosis`  <- format(round(table2_lower$`<25 non-diagnosis` /1000,1), nsmall=0, big.mark=",")
  table2_lower$`<25 early mortality` <- format(round(table2_lower$`<25 early mortality`/1000,1), nsmall=0, big.mark=",")

  table2_upper$Total <- format(round(table2_upper$Total/1000,1), nsmall=0, big.mark=",")
  table2_upper$`25+ early mortality` <- format(round(table2_upper$`25+ early mortality`/1000,1), nsmall=0, big.mark=",")
  table2_upper$`<25 non-diagnosis`  <- format(round(table2_upper$`<25 non-diagnosis` /1000,1), nsmall=0, big.mark=",")
  table2_upper$`<25 early mortality` <- format(round(table2_upper$`<25 early mortality`/1000,1), nsmall=0, big.mark=",")


  table2$`25+ early mortality` <- paste0( table2$`25+ early mortality`,"(", table2_lower$`25+ early mortality`, "-",table2_upper$`25+ early mortality`, ")")
  table2$Total <- paste0( table2$Total,"(", table2_lower$Total, "-",table2_upper$Total, ")")
  table2$`<25 non-diagnosis` <- paste0( table2$`<25 non-diagnosis`,"(", table2_lower$`<25 non-diagnosis`, "-",table2_upper$`<25 non-diagnosis`, ")")
  table2$`<25 early mortality` <- paste0( table2$`<25 early mortality`,"(", table2_lower$`<25 early mortality`, "-",table2_upper$`<25 early mortality`, ")")



  write.csv(table2,"temp/main_paper_table2.csv",row.names = FALSE)


  # figure 7 -------------------
  # total_00_99  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)

  region_list <- c( "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )
  total_2000_00_19  <- query_compose_execute_aggregate(2000,0,19,schema = "main_ci_partition_scenario_1",countries)
  total_2020_00_19  <- query_compose_execute_aggregate(2020,0,19,schema = "main_ci_partition_scenario_1",countries)
  total_2040_00_19  <- query_compose_execute_aggregate(2040,0,19,schema = "main_ci_partition_scenario_1",countries)

  total_2000_20_69  <- query_compose_execute_aggregate(2000,20,69,schema = "main_ci_partition_scenario_1",countries)
  total_2020_20_69  <- query_compose_execute_aggregate(2020,20,69,schema = "main_ci_partition_scenario_1",countries)
  total_2040_20_69  <- query_compose_execute_aggregate(2040,20,69,schema = "main_ci_partition_scenario_1",countries)

  total_2000_70_99  <- query_compose_execute_aggregate(2000,70,99,schema = "main_ci_partition_scenario_1",countries)
  total_2020_70_99  <- query_compose_execute_aggregate(2020,70,99,schema = "main_ci_partition_scenario_1",countries)
  total_2040_70_99  <- query_compose_execute_aggregate(2040,70,99,schema = "main_ci_partition_scenario_1",countries)

  figure7 <- c()
  i <- 1
  for(i in 1:length(region_list))
  {
    figure7 <- rbind(figure7, data.frame( "2000"= total_2000_00_19$prevalence_median[total_2000_00_19$Country==region_list[i]]
                                          ,"2020"= total_2020_00_19$prevalence_median[total_2020_00_19$Country==region_list[i]]
                                          ,"2040"= total_2040_00_19$prevalence_median[total_2040_00_19$Country==region_list[i]]
    ))
    figure7 <- rbind(figure7, data.frame( "2000"= total_2000_20_69$prevalence_median[total_2000_20_69$Country==region_list[i]]
                                          ,"2020"= total_2020_20_69$prevalence_median[total_2020_20_69$Country==region_list[i]]
                                          ,"2040"= total_2040_20_69$prevalence_median[total_2040_20_69$Country==region_list[i]]
    ))
    figure7 <- rbind(figure7, data.frame( "2000"= total_2000_70_99$prevalence_median[total_2000_70_99$Country==region_list[i]]
                                          ,"2020"= total_2020_70_99$prevalence_median[total_2020_70_99$Country==region_list[i]]
                                          ,"2040"= total_2040_70_99$prevalence_median[total_2040_70_99$Country==region_list[i]]
    ))
  }
  write.csv(figure7,"temp/main_paper_figure7_conservative.csv",row.names = FALSE)

  # figure 6 -------------------
  country_list <- c( "United States","Brazil","India","Democratic Republic of the Congo")
  for(i in 1:4)
  {
    # i <- 1
    total_00_09  <- query_compose_execute(2021,0,9,country_list[i])
    total_10_19  <- query_compose_execute(2021,10,19,country_list[i])
    total_20_29  <- query_compose_execute(2021,20,29,country_list[i])
    total_30_39  <- query_compose_execute(2021,30,39,country_list[i])
    total_40_49  <- query_compose_execute(2021,40,49,country_list[i])
    total_50_59  <- query_compose_execute(2021,50,59,country_list[i])
    total_60_69  <- query_compose_execute(2021,60,69,country_list[i])
    total_70_79  <- query_compose_execute(2021,70,79,country_list[i])
    total_80_89  <- query_compose_execute(2021,80,89,country_list[i])
    total_90_99  <- query_compose_execute(2021,90,99,country_list[i])

    figure_6_1 <- data.frame("00 to 09"=c(total_00_09$missing_non_diagnosed_median,total_00_09$missing_diagnosed_median, total_00_09$prevalence_median ))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("10 to 19"=c(total_10_19$missing_non_diagnosed_median,total_10_19$missing_diagnosed_median, total_10_19$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("20 to 29"=c(total_20_29$missing_non_diagnosed_median,total_20_29$missing_diagnosed_median, total_20_29$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("30 to 39"=c(total_30_39$missing_non_diagnosed_median,total_30_39$missing_diagnosed_median, total_30_39$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("40 to 49"=c(total_40_49$missing_non_diagnosed_median,total_40_49$missing_diagnosed_median, total_40_49$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("50 to 59"=c(total_50_59$missing_non_diagnosed_median,total_50_59$missing_diagnosed_median, total_50_59$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("60 to 69"=c(total_60_69$missing_non_diagnosed_median,total_60_69$missing_diagnosed_median, total_60_69$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("70 to 79"=c(total_70_79$missing_non_diagnosed_median,total_70_79$missing_diagnosed_median, total_70_79$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("80 to 89"=c(total_80_89$missing_non_diagnosed_median,total_80_89$missing_diagnosed_median, total_80_89$prevalence_median )))
    figure_6_1 <- cbind(figure_6_1,figure_6_1 <- data.frame("90 to 99"=c(total_90_99$missing_non_diagnosed_median,total_90_99$missing_diagnosed_median, total_90_99$prevalence_median )))

    write.csv(figure_6_1,paste0("temp/main_paper_figure6_",i,".csv"),row.names = FALSE)
  }

  # figure 4 -------------------
  country_list <- c( "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )

  total_00_09  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)

  total_00_09 <- dplyr::select(total_00_09, Country
                               ,  missing_non_diagnosed_median
                               , missing_diagnosed_median
                               ,living=prevalence_median)



  row.names(total_00_09) <- total_00_09$Country
  total_00_09 <- t(total_00_09[country_list,])

  write.csv(total_00_09,paste0("temp/main_paper_figure4.csv"),row.names = FALSE)

  # figure 5A -------------------

  list <-  0:15 *5
  figure_5A <- data.frame(age_min= list)
  figure_5A$incidence_rate <- NA
  figure_5A$incidence_case <- NA
  for(i in 1: length(list))
  { # i <- 1
    total_00_09  <- query_compose_execute_aggregate(2021,list[i],list[i]+4,schema = "main_ci_partition_scenario_2",countries)
    figure_5A$incidence_rate[i] <-total_00_09$incidence_rate_median
    figure_5A$incidence_case[i] <-total_00_09$incidence_median
  }

  figure_5A <- t(figure_5A)
  write.csv(figure_5A,paste0("temp/main_paper_figure5A.csv"),row.names = FALSE)

  # figure 5B -------------------
  country_list <- c( "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )

  list <-  0:15 *5
  figure_5B <- data.frame(Country= country_list)
  for(i in 1: length(list))
  { # i <- 1
    total_temp  <- dplyr::select(query_compose_execute_aggregate(2021,list[i],list[i]+4,schema = "main_ci_partition_scenario_2",countries),Country, incidence_rate_median )
    row.names(total_temp) <- total_temp$Country
    total_temp <- total_temp[country_list,]

    eval(parse(text = paste0("figure_5B <- cbind(figure_5B, age_",list[i],"=total_temp$incidence_rate_median )")))
  }

  write.csv(figure_5B,paste0("temp/main_paper_figure5B.csv"),row.names = FALSE)




  # Appendix --------------------------------------------------------------------------------------------------------------------------------------------
  # total_2040_00_19  <- query_compose_execute_aggregate(2040,0,19,schema = "main_ci_partition_scenario_1",countries)

  # Appendix 6 , Table 2 -----------------------------------------
  Appendix_Table2 <- read.csv("temp/Appendix Table 2 Paediatric estimates (2021, 0-19) - Sheet1.csv")
  colnames_initial <- colnames(Appendix_Table2)
  Appendix_Table2$IDF.Atlas <- as.numeric(gsub(",","",Appendix_Table2$IDF.Atlas))
  Appendix_Table2$Our.study.CI <- NA
  total_2021_00_19  <- query_compose_execute_aggregate(2021,0,19,schema = "main_ci_partition_scenario_1",countries)

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  code_paper_country_name$Country <- code_paper_country_name$code
  total_2021_00_19 <- total_2021_00_19 %>% dplyr::left_join(code_paper_country_name,by="Country")
  total_2021_00_19$paper[is.na(total_2021_00_19$paper)] <-  total_2021_00_19$Country[is.na(total_2021_00_19$paper)]


  for(i in 1:nrow(Appendix_Table2))
  { # i <- 1
    match <- total_2021_00_19$paper == Appendix_Table2$Country[i]
    if(sum(match))
    {
      Appendix_Table2$Our.study.CI[i] <- total_2021_00_19$prevalence_median[match]
    }

  }
  index_no_na <- !is.na(Appendix_Table2$Our.study.CI)
  Appendix_Table2$X..difference.CI[index_no_na] <- ( Appendix_Table2$IDF.Atlas[index_no_na]-Appendix_Table2$Our.study.CI[index_no_na])/Appendix_Table2$Our.study.CI[index_no_na]*100
  Appendix_Table2$X..difference.CI[index_no_na] <- paste0(round(Appendix_Table2$X..difference.CI[index_no_na],0),"%")
  Appendix_Table2$X..difference.CI[(Appendix_Table2$X..difference.CI=="NA%")] <- "-"
  # write.csv(Appendix_Table2[is.na(Appendix_Table2$Our.study.CI),],paste0("temp/Appendix_Table2_not_matched.csv"),row.names = FALSE)
  Appendix_Table2[is.na(Appendix_Table2)] <- ""

  Appendix_Table2$Our.study <- Appendix_Table2$Our.study.CI
  Appendix_Table2$X..difference  <- Appendix_Table2$X..difference.CI
  Appendix_Table2 <- Appendix_Table2[,colnames_initial]
  write.csv(Appendix_Table2,paste0("temp/Appendix6_Table2.csv"),row.names = FALSE)

  # Appendix 6 ,Table 3 -----------------------------------------
  Appendix_Table2 <- read.csv("temp/Appendix Table 2 Paediatric estimates (2021, 0-19) - Sheet1.csv")
  colnames_initial <- colnames(Appendix_Table2)
  Appendix_Table2$IDF.Atlas <- as.numeric(gsub(",","",Appendix_Table2$IDF.Atlas))
  Appendix_Table2$Our.study.CI <- NA
  total_2021_00_19  <- query_compose_execute_aggregate(2017,0,14,schema = "main_ci_partition_scenario_1",countries)

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  code_paper_country_name$Country <- code_paper_country_name$code
  total_2021_00_19 <- total_2021_00_19 %>% dplyr::left_join(code_paper_country_name,by="Country")
  total_2021_00_19$paper[is.na(total_2021_00_19$paper)] <-  total_2021_00_19$Country[is.na(total_2021_00_19$paper)]


  for(i in 1:nrow(Appendix_Table2))
  { # i <- 1
    match <- total_2021_00_19$paper == Appendix_Table2$Country[i]
    if(sum(match))
    {
      Appendix_Table2$Our.study.CI[i] <- total_2021_00_19$prevalence_median[match]
    }

  }
  Appendix_Table2$Our.study <- Appendix_Table2$Our.study.CI
  Appendix_Table3 <- select(Appendix_Table2,Country,Our.study)
  write.csv(Appendix_Table3,paste0("temp/Appendix6_Table3.csv"),row.names = FALSE)


  # Appendix 6 ,Table 4 -----------------------------------------
  Appendix_Table2 <- read.csv("temp/Appendix Table 2 Paediatric estimates (2021, 0-19) - Sheet1.csv")
  colnames_initial <- colnames(Appendix_Table2)
  Appendix_Table2$IDF.Atlas <- as.numeric(gsub(",","",Appendix_Table2$IDF.Atlas))
  Appendix_Table2$Our.study.CI <- NA
  total_2021_00_19  <- query_compose_execute_aggregate(2017,0,99,schema = "main_ci_partition_scenario_1",countries)

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  code_paper_country_name$Country <- code_paper_country_name$code
  total_2021_00_19 <- total_2021_00_19 %>% dplyr::left_join(code_paper_country_name,by="Country")
  total_2021_00_19$paper[is.na(total_2021_00_19$paper)] <-  total_2021_00_19$Country[is.na(total_2021_00_19$paper)]


  for(i in 1:nrow(Appendix_Table2))
  { # i <- 1
    match <- total_2021_00_19$paper == Appendix_Table2$Country[i]
    if(sum(match))
    {
      Appendix_Table2$Our.study.CI[i] <- total_2021_00_19$prevalence_median[match]
    }

  }
  Appendix_Table2$Our.study <- Appendix_Table2$Our.study.CI
  Appendix_Table4 <- select(Appendix_Table2,Country,Our.study)
  write.csv(Appendix_Table4,paste0("temp/Appendix6_Table4.csv"),row.names = FALSE)


  # Appendix 8 ,Table 1 -----------------------------------------
  # total_2040_00_19  <- query_compose_execute_aggregate(2040,0,19,schema = "main_ci_partition_scenario_1",countries)

  appendix8_table1 <- read.csv("data_paper_submission/appendix8_table1.csv")

  total_2021_00_99  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2021_00_19  <- query_compose_execute_aggregate(2021,0,19,schema = "main_ci_partition_scenario_1",countries)
  total_2021_20_99  <- query_compose_execute_aggregate(2021,20,99,schema = "main_ci_partition_scenario_1",countries)

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  rownames(code_paper_country_name) <- code_paper_country_name$code
  index_replace <- total_2021_00_99$Country  %in% code_paper_country_name$code
  total_2021_00_99$Country[index_replace] <- code_paper_country_name[total_2021_00_99$Country[index_replace],]$paper
  index_replace <- total_2021_00_19$Country  %in% code_paper_country_name$code
  total_2021_00_19$Country[index_replace] <- code_paper_country_name[total_2021_00_19$Country[index_replace],]$paper
  index_replace <- total_2021_20_99$Country  %in% code_paper_country_name$code
  total_2021_20_99$Country[index_replace] <- code_paper_country_name[total_2021_20_99$Country[index_replace],]$paper
  index_replace <- appendix8_table1$Region.or.country  %in% code_paper_country_name$code
  appendix8_table1$Region.or.country[index_replace] <- code_paper_country_name[appendix8_table1$Region.or.country[index_replace],]$paper


  total_2021_00_99$Country[total_2021_00_99$Country=="HIC"] <- "High-income"
  total_2021_00_99$Country[total_2021_00_99$Country=="LIC"] <- "Low-income"
  total_2021_00_99$Country[total_2021_00_99$Country=="LMIC"] <- "Lower-middle income"
  total_2021_00_99$Country[total_2021_00_99$Country=="UMIC"] <- "Upper-middle income"
  total_2021_00_99$Country[total_2021_00_99$Country=="Russian Federation"] <- "Russia"
  total_2021_00_99$Country[total_2021_00_99$Country=="State of Palestine"] <- "Palestine"
  total_2021_00_99$Country[total_2021_00_99$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_00_19$Country[total_2021_00_19$Country=="HIC"] <- "High-income"
  total_2021_00_19$Country[total_2021_00_19$Country=="LIC"] <- "Low-income"
  total_2021_00_19$Country[total_2021_00_19$Country=="LMIC"] <- "Lower-middle income"
  total_2021_00_19$Country[total_2021_00_19$Country=="UMIC"] <- "Upper-middle income"
  total_2021_00_19$Country[total_2021_00_19$Country=="Russian Federation"] <- "Russia"
  total_2021_00_19$Country[total_2021_00_19$Country=="State of Palestine"] <- "Palestine"
  total_2021_00_19$Country[total_2021_00_19$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_20_99$Country[total_2021_20_99$Country=="HIC"] <- "High-income"
  total_2021_20_99$Country[total_2021_20_99$Country=="LIC"] <- "Low-income"
  total_2021_20_99$Country[total_2021_20_99$Country=="LMIC"] <- "Lower-middle income"
  total_2021_20_99$Country[total_2021_20_99$Country=="UMIC"] <- "Upper-middle income"
  total_2021_20_99$Country[total_2021_20_99$Country=="Russian Federation"] <- "Russia"
  total_2021_20_99$Country[total_2021_20_99$Country=="State of Palestine"] <- "Palestine"
  total_2021_20_99$Country[total_2021_20_99$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  appendix8_table1$incidence_total <- NA
  appendix8_table1$'incidence_<20' <- NA
  appendix8_table1$'incidence_20+' <- NA
  appendix8_table1$Diagnosis_rate <- NA

  appendix8_table1$prevalence_total <- NA
  appendix8_table1$'prevalence_<20' <- NA
  appendix8_table1$'prevalence_20+' <- NA

  appendix8_table1$missing_prevalence_total <- NA
  appendix8_table1$missing_prevalence_diagnosed <- NA
  appendix8_table1$missing_prevalence_undiagnosed <- NA

  appendix8_table1$remain_life_t1d <- NA
  appendix8_table1$remain_life_non_t1d <- NA
  appendix8_table1$difference  <- NA

  # for(i in 2:19)
  # {
  #   total_2021_00_99[,i] <- format_comma(total_2021_00_99[,i])
  #   total_2021_00_19[,i] <- format_comma(total_2021_00_19[,i])
  #   total_2021_20_99[,i] <- format_comma(total_2021_20_99[,i])
  # }


  for(i in 1:nrow(appendix8_table1))
  { # i <- 1
    match <- total_2021_00_99$Country == appendix8_table1$Region.or.country[i]
    if(sum(match))
    {
      appendix8_table1$incidence_total[i] <-  paste0(format_comma(total_2021_00_99$incidence_median[match]),'\n(',format_comma(total_2021_00_99$incidence_lower[match]),'-',format_comma(total_2021_00_99$incidence_upper[match]),')')
      appendix8_table1$'incidence_<20'[i]  <- paste0(format_comma(total_2021_00_19$incidence_median[match]),'\n(',format_comma(total_2021_00_19$incidence_lower[match]),'-',format_comma(total_2021_00_19$incidence_upper[match]),')')
      appendix8_table1$'incidence_20+'[i]  <- paste0(format_comma(total_2021_20_99$incidence_median[match]),'\n(',format_comma(total_2021_20_99$incidence_lower[match]),'-',format_comma(total_2021_20_99$incidence_upper[match]),')')

      appendix8_table1$Diagnosis_rate[i] <-   paste0(format_comma(round(total_2021_00_99$incidence_median[match]/  (total_2021_00_99$incidence_median[match] + total_2021_00_99$onset_death_median[match])*100,0)),
                                                     '\n(',format_comma(round(total_2021_00_99$incidence_lower[match]/  (total_2021_00_99$incidence_lower[match] + total_2021_00_99$onset_death_median[match])*100,0))
                                                     ,'-',format_comma(round(total_2021_00_99$incidence_upper[match]/  (total_2021_00_99$incidence_upper[match] + total_2021_00_99$onset_death_median[match])*100,0)),')')

      appendix8_table1$prevalence_total[i] <-  paste0(format_comma(total_2021_00_99$prevalence_median[match]),'\n(',format_comma(total_2021_00_99$prevalence_lower[match]),'-',format_comma(total_2021_00_99$prevalence_upper[match]),')')
      appendix8_table1$'prevalence_<20'[i]  <- paste0(format_comma(total_2021_00_19$prevalence_median[match]),'\n(',format_comma(total_2021_00_19$prevalence_lower[match]),'-',format_comma(total_2021_00_19$prevalence_upper[match]),')')
      appendix8_table1$'prevalence_20+'[i]  <- paste0(format_comma(total_2021_20_99$prevalence_median[match]),'\n(',format_comma(total_2021_20_99$prevalence_lower[match]),'-',format_comma(total_2021_20_99$prevalence_upper[match]),')')



      appendix8_table1$missing_prevalence_total[i]        <- paste0(format_comma(total_2021_00_99$missing_prevalence_median[match]),'\n(',format_comma(total_2021_00_99$missing_prevalence_lower[match]),'-',format_comma(total_2021_00_99$missing_prevalence_upper[match]),')')
      appendix8_table1$missing_prevalence_diagnosed[i]    <- paste0(format_comma(total_2021_00_99$missing_diagnosed_median[match]),'\n(',format_comma(total_2021_00_99$missing_diagnosed_lower[match]),'-',format_comma(total_2021_00_99$missing_diagnosed_upper[match]),')')
      appendix8_table1$missing_prevalence_undiagnosed[i]  <- paste0(format_comma(total_2021_00_99$missing_non_diagnosed_median[match]),'\n(',format_comma(total_2021_00_99$missing_non_diagnosed_lower[match]),'-',format_comma(total_2021_00_99$missing_non_diagnosed_upper[match]),')')

      appendix8_table1$remain_life_t1d[i]        <- paste0(round(total_2021_00_99$life_expectancy_median[match],0)
                                                           ,'\n(',round(total_2021_00_99$life_expectancy_lower[match],0)
                                                           ,'-',round(total_2021_00_99$life_expectancy_upper[match],0),')')
      appendix8_table1$remain_life_non_t1d[i]    <- round(total_2021_00_99$life_expectancy_non_t1ds_median[match],0)

      appendix8_table1$difference[i] <-  paste0( round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_median[match],0)
                                                 ,'\n(',  round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_upper[match],0)
                                                 ,'-',  round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_lower[match],0),')')


    }

  }


  appendix8_table1$Diagnosis_rate[is.nan(appendix8_table1$Diagnosis_rate)] <- 100

  appendix8_table1$Region.or.country <- stri_trans_general(appendix8_table1$Region.or.country, "Latin-ASCII")


  table2 <- appendix8_table1[0,]

  for(i in 1:nrow(appendix8_table1))
  {  # i <- 1; j <- 2
    for(j in c(2:12,14) )
    {
      texts <-  strsplit(  appendix8_table1[i,j],"\n" )[[1]]
      table2[i*2-1,j] <- texts[1]
      table2[i*2,j]   <- texts[2]
    }

    table2[i*2-1,c(1,13)] <- appendix8_table1[i,c(1,13)]
    # table2[i*2,]   <- table1[i,]

  }
  table2[is.na(table2)] <- ""



  # write.csv(table2,paste0("temp/appendix8_table1_ci.csv"),row.names = FALSE)

  #--- only for regions,  but, with age breakdown 20-59 and 60 +


  appendix8_table1 <- read.csv("data_paper_submission/appendix8_table1.csv")

  total_2021_00_99  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2021_00_19  <- query_compose_execute_aggregate(2021,0,19,schema = "main_ci_partition_scenario_1",countries)
  total_2021_20_99  <- query_compose_execute_aggregate(2021,20,99,schema = "main_ci_partition_scenario_1",countries)
  total_2021_20_59  <- query_compose_execute_aggregate(2021,20,59,schema = "main_ci_partition_scenario_1",countries)
  total_2021_60_99  <- query_compose_execute_aggregate(2021,60,99,schema = "main_ci_partition_scenario_1",countries)

  code_paper_country_name <- read.csv("data_internal/code_paper_country_name.csv")
  rownames(code_paper_country_name) <- code_paper_country_name$code
  index_replace <- total_2021_00_99$Country  %in% code_paper_country_name$code
  total_2021_00_99$Country[index_replace] <- code_paper_country_name[total_2021_00_99$Country[index_replace],]$paper

  index_replace <- total_2021_00_19$Country  %in% code_paper_country_name$code
  total_2021_00_19$Country[index_replace] <- code_paper_country_name[total_2021_00_19$Country[index_replace],]$paper

  index_replace <- total_2021_20_99$Country  %in% code_paper_country_name$code
  total_2021_20_99$Country[index_replace] <- code_paper_country_name[total_2021_20_99$Country[index_replace],]$paper

  index_replace <- total_2021_20_59$Country  %in% code_paper_country_name$code
  total_2021_20_59$Country[index_replace] <- code_paper_country_name[total_2021_20_59$Country[index_replace],]$paper

  index_replace <- total_2021_60_99$Country  %in% code_paper_country_name$code
  total_2021_60_99$Country[index_replace] <- code_paper_country_name[total_2021_60_99$Country[index_replace],]$paper

  index_replace <- appendix8_table1$Region.or.country  %in% code_paper_country_name$code
  appendix8_table1$Region.or.country[index_replace] <- code_paper_country_name[appendix8_table1$Region.or.country[index_replace],]$paper




  total_2021_00_99$Country[total_2021_00_99$Country=="HIC"] <- "High-income"
  total_2021_00_99$Country[total_2021_00_99$Country=="LIC"] <- "Low-income"
  total_2021_00_99$Country[total_2021_00_99$Country=="LMIC"] <- "Lower-middle income"
  total_2021_00_99$Country[total_2021_00_99$Country=="UMIC"] <- "Upper-middle income"
  total_2021_00_99$Country[total_2021_00_99$Country=="Russian Federation"] <- "Russia"
  total_2021_00_99$Country[total_2021_00_99$Country=="State of Palestine"] <- "Palestine"
  total_2021_00_99$Country[total_2021_00_99$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_00_19$Country[total_2021_00_19$Country=="HIC"] <- "High-income"
  total_2021_00_19$Country[total_2021_00_19$Country=="LIC"] <- "Low-income"
  total_2021_00_19$Country[total_2021_00_19$Country=="LMIC"] <- "Lower-middle income"
  total_2021_00_19$Country[total_2021_00_19$Country=="UMIC"] <- "Upper-middle income"
  total_2021_00_19$Country[total_2021_00_19$Country=="Russian Federation"] <- "Russia"
  total_2021_00_19$Country[total_2021_00_19$Country=="State of Palestine"] <- "Palestine"
  total_2021_00_19$Country[total_2021_00_19$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_20_99$Country[total_2021_20_99$Country=="HIC"] <- "High-income"
  total_2021_20_99$Country[total_2021_20_99$Country=="LIC"] <- "Low-income"
  total_2021_20_99$Country[total_2021_20_99$Country=="LMIC"] <- "Lower-middle income"
  total_2021_20_99$Country[total_2021_20_99$Country=="UMIC"] <- "Upper-middle income"
  total_2021_20_99$Country[total_2021_20_99$Country=="Russian Federation"] <- "Russia"
  total_2021_20_99$Country[total_2021_20_99$Country=="State of Palestine"] <- "Palestine"
  total_2021_20_99$Country[total_2021_20_99$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_20_59$Country[total_2021_20_59$Country=="HIC"] <- "High-income"
  total_2021_20_59$Country[total_2021_20_59$Country=="LIC"] <- "Low-income"
  total_2021_20_59$Country[total_2021_20_59$Country=="LMIC"] <- "Lower-middle income"
  total_2021_20_59$Country[total_2021_20_59$Country=="UMIC"] <- "Upper-middle income"
  total_2021_20_59$Country[total_2021_20_59$Country=="Russian Federation"] <- "Russia"
  total_2021_20_59$Country[total_2021_20_59$Country=="State of Palestine"] <- "Palestine"
  total_2021_20_59$Country[total_2021_20_59$Country=="Democratic People's Republic of Korea"] <- "North Korea"

  total_2021_60_99$Country[total_2021_60_99$Country=="HIC"] <- "High-income"
  total_2021_60_99$Country[total_2021_60_99$Country=="LIC"] <- "Low-income"
  total_2021_60_99$Country[total_2021_60_99$Country=="LMIC"] <- "Lower-middle income"
  total_2021_60_99$Country[total_2021_60_99$Country=="UMIC"] <- "Upper-middle income"
  total_2021_60_99$Country[total_2021_60_99$Country=="Russian Federation"] <- "Russia"
  total_2021_60_99$Country[total_2021_60_99$Country=="State of Palestine"] <- "Palestine"
  total_2021_60_99$Country[total_2021_60_99$Country=="Democratic People's Republic of Korea"] <- "North Korea"




  appendix8_table1$incidence_total <- NA
  appendix8_table1$'incidence_<20' <- NA
  # appendix8_table1$'incidence_20+' <- NA
  appendix8_table1$'incidence_20_59' <- NA
  appendix8_table1$'incidence_60+' <- NA
  appendix8_table1$Diagnosis_rate <- NA

  appendix8_table1$prevalence_total <- NA
  appendix8_table1$'prevalence_<20' <- NA
  # appendix8_table1$'prevalence_20+' <- NA
  appendix8_table1$'prevalence_20_59' <- NA
  appendix8_table1$'prevalence_60+' <- NA

  appendix8_table1$missing_prevalence_total <- NA
  appendix8_table1$missing_prevalence_diagnosed <- NA
  appendix8_table1$missing_prevalence_undiagnosed <- NA

  appendix8_table1$remain_life_t1d <- NA
  appendix8_table1$remain_life_non_t1d <- NA
  appendix8_table1$difference  <- NA

  # for(i in 2:19)
  # {
  #   total_2021_00_99[,i] <- format_comma(total_2021_00_99[,i])
  #   total_2021_00_19[,i] <- format_comma(total_2021_00_19[,i])
  #   total_2021_20_99[,i] <- format_comma(total_2021_20_99[,i])
  # }


  for(i in 1:nrow(appendix8_table1))
  { # i <- 1
    match <- total_2021_00_99$Country == appendix8_table1$Region.or.country[i]
    if(sum(match))
    {
      appendix8_table1$incidence_total[i] <-  paste0(format_comma(total_2021_00_99$incidence_median[match]),'\n(',format_comma(total_2021_00_99$incidence_lower[match]),'-',format_comma(total_2021_00_99$incidence_upper[match]),')')
      appendix8_table1$'incidence_<20'[i]  <- paste0(format_comma(total_2021_00_19$incidence_median[match]),'\n(',format_comma(total_2021_00_19$incidence_lower[match]),'-',format_comma(total_2021_00_19$incidence_upper[match]),')')
      # appendix8_table1$'incidence_20+'[i]  <- paste0(format_comma(total_2021_20_99$incidence_median[match]),'\n(',format_comma(total_2021_20_99$incidence_lower[match]),'-',format_comma(total_2021_20_99$incidence_upper[match]),')')
      appendix8_table1$'incidence_20_59'[i]  <- paste0(format_comma(total_2021_20_59$incidence_median[match]),'\n(',format_comma(total_2021_20_59$incidence_lower[match]),'-',format_comma(total_2021_20_59$incidence_upper[match]),')')
      appendix8_table1$'incidence_60+'[i]  <- paste0(format_comma(total_2021_60_99$incidence_median[match]),'\n(',format_comma(total_2021_60_99$incidence_lower[match]),'-',format_comma(total_2021_60_99$incidence_upper[match]),')')

      appendix8_table1$Diagnosis_rate[i] <-   paste0(format_comma(round(total_2021_00_99$incidence_median[match]/  (total_2021_00_99$incidence_median[match] + total_2021_00_99$onset_death_median[match])*100,0)),
                                                     '\n(',format_comma(round(total_2021_00_99$incidence_lower[match]/  (total_2021_00_99$incidence_lower[match] + total_2021_00_99$onset_death_median[match])*100,0))
                                                     ,'-',format_comma(round(total_2021_00_99$incidence_upper[match]/  (total_2021_00_99$incidence_upper[match] + total_2021_00_99$onset_death_median[match])*100,0)),')')

      appendix8_table1$prevalence_total[i] <-  paste0(format_comma(total_2021_00_99$prevalence_median[match]),'\n(',format_comma(total_2021_00_99$prevalence_lower[match]),'-',format_comma(total_2021_00_99$prevalence_upper[match]),')')
      appendix8_table1$'prevalence_<20'[i]  <- paste0(format_comma(total_2021_00_19$prevalence_median[match]),'\n(',format_comma(total_2021_00_19$prevalence_lower[match]),'-',format_comma(total_2021_00_19$prevalence_upper[match]),')')
      # appendix8_table1$'prevalence_20+'[i]  <- paste0(format_comma(total_2021_20_99$prevalence_median[match]),'\n(',format_comma(total_2021_20_99$prevalence_lower[match]),'-',format_comma(total_2021_20_99$prevalence_upper[match]),')')
      appendix8_table1$'prevalence_20_59'[i]  <- paste0(format_comma(total_2021_20_59$prevalence_median[match]),'\n(',format_comma(total_2021_20_59$prevalence_lower[match]),'-',format_comma(total_2021_20_59$prevalence_upper[match]),')')
      appendix8_table1$'prevalence_60+'[i]  <- paste0(format_comma(total_2021_60_99$prevalence_median[match]),'\n(',format_comma(total_2021_60_99$prevalence_lower[match]),'-',format_comma(total_2021_60_99$prevalence_upper[match]),')')



      appendix8_table1$missing_prevalence_total[i]        <- paste0(format_comma(total_2021_00_99$missing_prevalence_median[match]),'\n(',format_comma(total_2021_00_99$missing_prevalence_lower[match]),'-',format_comma(total_2021_00_99$missing_prevalence_upper[match]),')')
      appendix8_table1$missing_prevalence_diagnosed[i]    <- paste0(format_comma(total_2021_00_99$missing_diagnosed_median[match]),'\n(',format_comma(total_2021_00_99$missing_diagnosed_lower[match]),'-',format_comma(total_2021_00_99$missing_diagnosed_upper[match]),')')
      appendix8_table1$missing_prevalence_undiagnosed[i]  <- paste0(format_comma(total_2021_00_99$missing_non_diagnosed_median[match]),'\n(',format_comma(total_2021_00_99$missing_non_diagnosed_lower[match]),'-',format_comma(total_2021_00_99$missing_non_diagnosed_upper[match]),')')

      appendix8_table1$remain_life_t1d[i]        <- paste0(round(total_2021_00_99$life_expectancy_median[match],0)
                                                           ,'\n(',round(total_2021_00_99$life_expectancy_lower[match],0)
                                                           ,'-',round(total_2021_00_99$life_expectancy_upper[match],0),')')
      appendix8_table1$remain_life_non_t1d[i]    <- round(total_2021_00_99$life_expectancy_non_t1ds_median[match],0)

      appendix8_table1$difference[i] <-  paste0( round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_median[match],0)
                                                 ,'\n(',  round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_upper[match],0)
                                                 ,'-',  round(total_2021_00_99$life_expectancy_non_t1ds_median[match] - total_2021_00_99$life_expectancy_lower[match],0),')')


    }

  }


  appendix8_table1$Diagnosis_rate[is.nan(appendix8_table1$Diagnosis_rate)] <- 100

  appendix8_table1$Region.or.country <- stri_trans_general(appendix8_table1$Region.or.country, "Latin-ASCII")

  region_list <- c( "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )
  income_list <- c( "High-income" , "Low-income", "Lower-middle income", "Upper-middle income")
  agg_list <- c("GLOBAL", region_list,income_list)

  # appendix8_table1  <- appendix8_table1[appendix8_table1$Region.or.country %in% agg_list ,]
  table2 <- appendix8_table1[0,]

  for(i in 1:nrow(appendix8_table1))
  {  # i <- 1; j <- 2
    for(j in c(2:14,16) )
    {
      texts <-  strsplit(  appendix8_table1[i,j],"\n" )[[1]]
      table2[i*2-1,j] <- texts[1]
      table2[i*2,j]   <- texts[2]
    }

    table2[i*2-1,c(1,15)] <- appendix8_table1[i,c(1,15)]
    # table2[i*2,]   <- table1[i,]

  }
  table2[is.na(table2)] <- ""


  # write.csv(table2,paste0("temp/appendix8_table1_24_Aug.csv"),row.names = FALSE)




  # Projections -------


  # figure 7 -------------------
  # total_00_99  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)

  region_list <- c("GLOBAL", "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )
  total_2000_00_99_1  <- query_compose_execute_aggregate(2000,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2005_00_99_1  <- query_compose_execute_aggregate(2005,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2010_00_99_1  <- query_compose_execute_aggregate(2010,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2015_00_99_1  <- query_compose_execute_aggregate(2015,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2020_00_99_1  <- query_compose_execute_aggregate(2020,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2025_00_99_1  <- query_compose_execute_aggregate(2025,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2030_00_99_1  <- query_compose_execute_aggregate(2030,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2035_00_99_1  <- query_compose_execute_aggregate(2035,0,99,schema = "main_ci_partition_scenario_1",countries)
  total_2040_00_99_1  <- query_compose_execute_aggregate(2040,0,99,schema = "main_ci_partition_scenario_1",countries)

  total_2000_00_99_2  <- query_compose_execute_aggregate(2000,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2005_00_99_2  <- query_compose_execute_aggregate(2005,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2010_00_99_2  <- query_compose_execute_aggregate(2010,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2015_00_99_2  <- query_compose_execute_aggregate(2015,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2020_00_99_2  <- query_compose_execute_aggregate(2020,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2025_00_99_2  <- query_compose_execute_aggregate(2025,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2030_00_99_2  <- query_compose_execute_aggregate(2030,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2035_00_99_2  <- query_compose_execute_aggregate(2035,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_2040_00_99_2  <- query_compose_execute_aggregate(2040,0,99,schema = "main_ci_partition_scenario_2",countries)

  #------------------------------
  table_projection <- data.frame()
  # for( column_name in c("prevalence_median", "missing_prevalence_median","incidence_median","life_expectancy_median"))
    for( column_name in c("prevalence_lower","prevalence_median","prevalence_upper"))
    {
  # convert to comma seperated
  total_2000_00_99_1[,column_name] <- format_comma(total_2000_00_99_1[,column_name])
  total_2005_00_99_1[,column_name] <- format_comma(total_2005_00_99_1[,column_name])
  total_2010_00_99_1[,column_name] <- format_comma(total_2010_00_99_1[,column_name])
  total_2015_00_99_1[,column_name] <- format_comma(total_2015_00_99_1[,column_name])
  total_2020_00_99_1[,column_name] <- format_comma(total_2020_00_99_1[,column_name])
  total_2025_00_99_1[,column_name] <- format_comma(total_2025_00_99_1[,column_name])
  total_2030_00_99_1[,column_name] <- format_comma(total_2030_00_99_1[,column_name])
  total_2035_00_99_1[,column_name] <- format_comma(total_2035_00_99_1[,column_name])
  total_2040_00_99_1[,column_name] <- format_comma(total_2040_00_99_1[,column_name])

  total_2000_00_99_2[,column_name] <- format_comma(total_2000_00_99_2[,column_name])
  total_2005_00_99_2[,column_name] <- format_comma(total_2005_00_99_2[,column_name])
  total_2010_00_99_2[,column_name] <- format_comma(total_2010_00_99_2[,column_name])
  total_2015_00_99_2[,column_name] <- format_comma(total_2015_00_99_2[,column_name])
  total_2020_00_99_2[,column_name] <- format_comma(total_2020_00_99_2[,column_name])
  total_2025_00_99_2[,column_name] <- format_comma(total_2025_00_99_2[,column_name])
  total_2030_00_99_2[,column_name] <- format_comma(total_2030_00_99_2[,column_name])
  total_2035_00_99_2[,column_name] <- format_comma(total_2035_00_99_2[,column_name])
  total_2040_00_99_2[,column_name] <- format_comma(total_2040_00_99_2[,column_name])




    prevalence_1 <-  as.data.frame( cbind(
      country  =dplyr::select(total_2000_00_99_1,Country)[total_2000_00_99_1$Country %in% region_list,],
      year_2005=dplyr::select(total_2000_00_99_1,column_name)[total_2000_00_99_1$Country %in% region_list,],
      year_2010=dplyr::select(total_2010_00_99_1,column_name)[total_2010_00_99_1$Country %in% region_list,],
      year_2015=dplyr::select(total_2015_00_99_1,column_name)[total_2015_00_99_1$Country %in% region_list,],
      year_2020=dplyr::select(total_2020_00_99_1,column_name)[total_2020_00_99_1$Country %in% region_list,],
      year_2025=dplyr::select(total_2025_00_99_1,column_name)[total_2025_00_99_1$Country %in% region_list,],
      year_2030=dplyr::select(total_2030_00_99_1,column_name)[total_2030_00_99_1$Country %in% region_list,],
      year_2035=dplyr::select(total_2035_00_99_1,column_name)[total_2035_00_99_1$Country %in% region_list,],
      year_2040=dplyr::select(total_2040_00_99_1,column_name)[total_2040_00_99_1$Country %in% region_list,])
    )
    prevalence_1$scenario <- "conservative"
    prevalence_2 <-  as.data.frame( cbind(
      country  =dplyr::select(total_2000_00_99_2,Country)[total_2000_00_99_2$Country %in% region_list,],
      year_2005=dplyr::select(total_2000_00_99_2,column_name)[total_2000_00_99_2$Country %in% region_list,],
      year_2010=dplyr::select(total_2010_00_99_2,column_name)[total_2010_00_99_2$Country %in% region_list,],
      year_2015=dplyr::select(total_2015_00_99_2,column_name)[total_2015_00_99_2$Country %in% region_list,],
      year_2020=dplyr::select(total_2020_00_99_2,column_name)[total_2020_00_99_2$Country %in% region_list,],
      year_2025=dplyr::select(total_2025_00_99_2,column_name)[total_2025_00_99_2$Country %in% region_list,],
      year_2030=dplyr::select(total_2030_00_99_2,column_name)[total_2030_00_99_2$Country %in% region_list,],
      year_2035=dplyr::select(total_2035_00_99_2,column_name)[total_2035_00_99_2$Country %in% region_list,],
      year_2040=dplyr::select(total_2040_00_99_2,column_name)[total_2040_00_99_2$Country %in% region_list,])
    )
    prevalence_2$scenario <- "momentum"
    prevalence <- rbind(prevalence_1,prevalence_2)
    prevalence$value <- column_name
    prevalence <- prevalence[c(1,9,2,10,3,11,4,12,5,13,6,14,7,15,8,16),]
    table_projection <- rbind(table_projection, prevalence)
  }




  write.csv(table_projection,"temp/table_projection_prevalence_lower_upper_median.csv",row.names = FALSE)

  # incidence mean , median age of diagnosis

  year <- 2021


  total_2021_00_99_2  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)
  total_temp <- total_2021_00_99_2[total_2021_00_99_2$Country %in%region_list,]

  total_temp <- select(total_temp,Country, median_age_prevalence_median,median_age_prevalence_iqr_median,mean_age_prevalence_median, median_age_incidence_median,median_age_incidence_iqr_median,mean_age_incidence_median )

  colnames(total_temp) <- gsub("_median","",colnames(total_temp) )
  # write.csv(total_temp,"temp/age_of_prevalence_incidence.csv",row.names = FALSE)


  country_list  <- countries$world_bank_name

  data_  <- Data_Run_query_return_df(paste0('  SELECT  "Country" , "run", "Age", "Prevalence", "Incidence (1 base)"   FROM main_ci_partition_scenario_2.main_ci_',year,' ') )
  data_$Prevalence <- round(data_$Prevalence,0)
  data_$`Incidence (1 base)` <- round(data_$`Incidence (1 base)`,0)

  country_list_df <- data.frame(country_list=country_list)

  country_list_df$Prevalence <- NA
  country_list_df$Incidence  <- NA
  country_list_df$median_age_prevalence <- NA
  country_list_df$median_age_prevalence_iqr <- NA
  country_list_df$median_age_incidence <- NA
  country_list_df$median_age_incidence_iqr <- NA


  country_list_df$mean_age_prevalence <- NA
  country_list_df$mean_age_prevalence_sd <- NA
  country_list_df$mean_age_incidence <- NA
  country_list_df$mean_age_incidence_sd <- NA

  for(i  in 1: length(country_list) )
    # for(i  in 1: 4 )
    {    # i <- 5

    print(i)
   data_temp <-  data_[data_$Country %in% country_list[i],]


   data_temp_expand_prevalence  <- setNames(tidyr::uncount(as.data.frame(dplyr::select(data_temp, "Country", "run","Age","Prevalence")), Prevalence ), c("Country", "run","Age"))
   data_temp_expand_incidence   <- as.data.frame(dplyr::select(data_temp, "Country", "run","Age",`Incidence (1 base)`))

   data_temp_expand_incidence   <- data_temp_expand_incidence[data_temp_expand_incidence$`Incidence (1 base)`>0,]
   data_temp_expand_incidence   <- setNames(tidyr::uncount(data_temp_expand_incidence, `Incidence (1 base)` ), c("Country", "run","Age"))

   country_list_df$median_age_prevalence[i] <- quantile(data_temp_expand_prevalence$Age,probs=c(.5))
   country_list_df$median_age_prevalence_iqr[i] <- quantile(data_temp_expand_prevalence$Age,probs=c(.75)) - quantile(data_temp_expand_prevalence$Age,probs=c(.25))

   country_list_df$median_age_incidence[i] <- quantile(data_temp_expand_incidence$Age,probs=c(.5))
   country_list_df$median_age_incidence_iqr[i] <- quantile(data_temp_expand_incidence$Age,probs=c(.75)) - quantile(data_temp_expand_incidence$Age,probs=c(.25))

   country_list_df$mean_age_prevalence[i]    <- mean(data_temp_expand_prevalence$Age)
   country_list_df$mean_age_prevalence_sd[i] <- sd(data_temp_expand_prevalence$Age)

   country_list_df$mean_age_incidence[i]     <- mean(data_temp_expand_incidence$Age)
   country_list_df$mean_age_incidence_sd[i] <- sd(data_temp_expand_incidence$Age)

   prevalence_incidence <- setDT(data_temp)[,list(Prevalence=sum(Prevalence), Incidence=sum(`Incidence (1 base)`)  ),by="run"]

   country_list_df$Prevalence[i] <- median(prevalence_incidence$Prevalence)
   country_list_df$Incidence[i] <- median(prevalence_incidence$Incidence)


  }

  head(country_list_df)


  data_put_region <- country_list_df %>% dplyr::inner_join(dplyr::select(countries, country_list= world_bank_name, group_by= wd_region ))


  result_df_region <- setDT(data_put_region)[,list(
    median_age_prevalence=round(sum(median_age_prevalence* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,median_age_prevalence_iqr=round(sum(median_age_prevalence_iqr* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,mean_age_prevalence=round(sum(mean_age_prevalence* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,mean_age_prevalence_sd=round(sum(mean_age_prevalence_sd* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)

    ,median_age_incidence=round(sum(median_age_incidence* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,median_age_incidence_iqr=round(sum(median_age_incidence_iqr* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,mean_age_incidence=round(sum(mean_age_incidence* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,mean_age_incidence_sd=round(sum(mean_age_incidence_sd* (Incidence) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
  ),by=c("group_by")]


  data_put_region$group_by <- "GLOBAL"
  result_df_global <- setDT(data_put_region)[,list(
    median_age_prevalence=round(sum(median_age_prevalence* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,median_age_prevalence_iqr=round(sum(median_age_prevalence_iqr* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,mean_age_prevalence=round(sum(mean_age_prevalence* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)
    ,mean_age_prevalence_sd=round(sum(mean_age_prevalence_sd* (Prevalence ) , na.rm = TRUE  )/ sum(Prevalence, na.rm = TRUE),1)

    ,median_age_incidence=round(sum(median_age_incidence* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,median_age_incidence_iqr=round(sum(median_age_incidence_iqr* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,mean_age_incidence=round(sum(mean_age_incidence* (Incidence ) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
    ,mean_age_incidence_sd=round(sum(mean_age_incidence_sd* (Incidence) , na.rm = TRUE  )/ sum(Incidence, na.rm = TRUE),1)
  ),by=c("group_by")]

  total_temp <- rbind(result_df_global, result_df_region)


write.csv(total_temp,"temp/age_of_prevalence_incidence2.csv",row.names = FALSE)

  data_agg <- setDT(data_)[,list(prevalence_mean=mean()),by=c("Country","run")]

  #   new calculation median age of diagnosis , incidence -------  .  this one makes CI too small.

  # agg_life_expectancy_median <- function(year,frequency)
  # {   df <- data.frame(year=year,frequency= frequency)
  #     df <- setDT(df)[,list(frequency=sum(frequency)),by="year"]
  #     df <- df[order(df$year)]
  #     df$frequency <- df$frequency + 0.01  #  for extrem cases
  #     df$frequency_cumsum <- cumsum(df$frequency)
  #     index_same_value_index <- df$frequency_cumsum== df$frequency_cumsum[nrow(df)]/2
  #     if(sum(index_same_value_index)>0)
  #     {
  #       median <- (df$year[which(index_same_value_index) ] + df$year[which(index_same_value_index)+1]) /2
  #       median
  #     }else{
  #       index <- min(which(df$frequency_cumsum >  df$frequency_cumsum[nrow(df)]/2) )
  #       median <- df$year[index]
  #       median
  #     }
  #
  #     # median <- sum(year * frequency )/ sum(frequency)
  #     median
  # }
  #
  # data_  <- Data_Run_query_return_df(paste0('  SELECT  "Country" , "run", "Age", "Prevalence", "Incidence (1 base)"   FROM main_ci_partition_scenario_2.main_ci_',year,' ') )
  # data_$Prevalence <- round(data_$Prevalence,0)
  # data_$`Incidence (1 base)` <- round(data_$`Incidence (1 base)`,0)
  #
  #
  # data_ <- data_ %>% dplyr::inner_join(dplyr::select(countries, Country= world_bank_name, group_by= wd_region ))
  #
  #
  # data_agg <- setDT(data_)[,list(
  #   median_age_prevalence = agg_life_expectancy_median(Age,Prevalence)
  # ),by= c("group_by","run") ]
  #
  #
  # data_agg_agg <- setDT(data_agg)[,list(
  #   median_age_prevalence = median(median_age_prevalence)
  #   ,median_age_prevalence_iqr = quantile(median_age_prevalence,probs=c(.75)) - quantile(median_age_prevalence,probs=c(.25))
  # ),by= c("group_by") ]




  # Appendix 4 Table 3 ,  Estimated mortality due to type 1 diabetes by region (2021) -----

  region_list <- c("GLOBAL", "North America", "Latin America & Caribbean","Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa","South Asia","East Asia & Pacific"   )

  total_2021_00_24_1  <- query_compose_execute_aggregate(2021,0,24 ,schema = "main_ci_partition_scenario_2",countries)
  total_2021_25_99_1  <- query_compose_execute_aggregate(2021,25,99,schema = "main_ci_partition_scenario_2",countries)
  total_2021_00_99_1  <- query_compose_execute_aggregate(2021,0,99,schema = "main_ci_partition_scenario_2",countries)



  colnames_ <- c("Country","<25 non-diagnosis","<25 early mortality", "<25 total mortality", "25+ early mortality" , "Total")
  median_ <- as.data.frame(cbind(total_2021_00_24_1$Country,total_2021_00_24_1$onset_death_median,total_2021_00_24_1$early_death_median ,total_2021_00_24_1$total_death_median ,total_2021_25_99_1$early_death_median, total_2021_00_99_1$total_death_median  ))
  colnames(median_) <- colnames_
  median_ <-median_[median_$Country %in% region_list,]

  colnames_ <- c("Country","<25 non-diagnosis","<25 early mortality", "<25 total mortality", "25+ early mortality" , "Total")
  lower_ <- as.data.frame(cbind(total_2021_00_24_1$Country,total_2021_00_24_1$onset_death_lower,total_2021_00_24_1$early_death_lower ,total_2021_00_24_1$total_death_lower ,total_2021_25_99_1$early_death_lower, total_2021_00_99_1$total_death_lower ))
  colnames(lower_) <- colnames_
  lower_ <-lower_[lower_$Country %in% region_list,]

  colnames_ <- c("Country","<25 non-diagnosis","<25 early mortality", "<25 total mortality", "25+ early mortality" , "Total")
  upper_ <- as.data.frame(cbind(total_2021_00_24_1$Country,total_2021_00_24_1$onset_death_upper,total_2021_00_24_1$early_death_upper ,total_2021_00_24_1$total_death_upper ,total_2021_25_99_1$early_death_upper, total_2021_00_99_1$total_death_upper ))
  colnames(upper_) <- colnames_
  upper_ <-upper_[upper_$Country %in% region_list,]


  table_final <- data.frame()
  for(i in 1:nrow(median_))
  { # i <- 1
    row_temp <-  median_[i,]
    row_temp2 <-  median_[i,]
    for( j in 2:6){
      # j <- 2
      row_temp[j] <- format_comma ( as.numeric(row_temp[j]))
      row_temp2[j] <- paste0("(",format_comma ( as.numeric(lower_[i,j]) ),"-",format_comma ( as.numeric(upper_[i,j])),")")
    }
    row_temp <- rbind(row_temp,row_temp2)

    table_final <- rbind(table_final, row_temp)
  }


  write.csv(table_final,"temp/Appendix4 Table3 24 Aug.csv",row.names = FALSE)


}

