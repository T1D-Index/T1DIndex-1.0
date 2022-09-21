
refresh_one_country_file <- function(
  country_wb_name,data_dir,cache_dir,countries, start_year=1960, end_year=2040
) {  # start_year=1960; end_year=2040
  source('code_R/prevalence_utils.R')

  country_code <- countries$world_bank_code[countries$world_bank_name==country_wb_name]
  cache_file   <- file.path(cache_dir, paste0(country_code, ".binary"))
  log_timing <- grepl("mcmc_1_0\\/AFG\\.binary",cache_file )


  # country_name <- country_name_list[1]
  library(RSQLite)
  library(dplyr)
  library(tidyr)
  library(arrow)
  Log=function(fmt, ...) { cat(sprintf(paste0(fmt, '\n'), ...)) }

  Log('Starting model for %s...', country_wb_name)
  tryCatch({

    prev <- prevalence_and_ghost_pop(
      country_wb_name=country_wb_name,
      # hba1c=hba1c_function(country_name),
      start_year=start_year,
      end_year=end_year,
      data_dir=data_dir,
      log_timing=log_timing
    )

    assert(nrow(prev$prev_merge_wide)==8100 ,message = "make sure run result row numbers are as expected " )

    Log('Saving results for %s to %s.', country_wb_name, cache_file)
        # write.fst(prev_merge_wide, cache_file,compress = 90)

    if(log_timing){ sink("log.txt",append=TRUE);cat(paste0(Sys.time()," write_parquet \n") );sink()}
    write_parquet(prev$prev_merge_wide, cache_file)
    if(log_timing){ sink("log.txt",append=TRUE);cat(paste0(Sys.time()," write_parquet finish \n") );sink()}


  },
  error = function(cond) {
    Log('Error running model for %s:', country_wb_name)
    sink("log.txt",append=TRUE);cat(paste0( country_wb_name , "  ",  cond, " \n") );sink()
    NA
  },
  warning = function(cond) {
    Log('Warning running model for %s:', country_wb_name)
    NA
  },
  finally = {
  })

}
