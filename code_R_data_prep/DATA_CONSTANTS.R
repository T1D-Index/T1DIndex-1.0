host_name <<- "localhost"
#

Data_Run_query_return_df <- function(query)
{
  con  <- dbConnect(RPostgres::Postgres(),  dbname = "t1d"
                                  , host = host_name
                                  , port = "5432",  user = "postgres",   password = "postgrest1d")
  df  <- dbGetQuery(con,  query)
  dbDisconnect(con)
  return(df)
}




Data_dump_data_frame <- function(df,schema_name,table_name)
{

   # schema_name= "index_input"; table_name <- "country"  ;df <- country

  # dbExecute(con,"VACUUM;")


  con  <- dbConnect(RPostgres::Postgres(),  dbname = "t1d"
                                  , host = host_name
                                  , port = "5432",  user = "postgres",   password = "postgrest1d")
  dbWriteTable(con, DBI::SQL(paste0(schema_name,".",table_name))    , df, overwrite=TRUE,row.names=FALSE)
  dbDisconnect(con)




}


