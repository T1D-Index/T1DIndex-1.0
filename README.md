# T1DIndex   
Shiny dashboard 
https://t1dindex.shinyapps.io/dashboard/

# Platform and notes
The codebase is currently configured to run on Windows 10 Pro with R version 4.2.0,  Assuming you are using Rstudio, you will be required to download a number of packages that is required for this codebase to run properly.  
ALso check to make sure these packages are also installed :
`library(softImpute)`
`library(ggraph)`
`library(igraph)`

# Step 1:  Set working directory
Once downloaded or cloned from this codebase, you need to first set working directory to the following:

```setwd("~/GitHub/t1d-index") ```

as all paths are referenced based on working directory.

# Step 2: Set up database,
Most of the stages of the code will run against data stored in a Postgre SQL database. 
you will need to download postgreSQL to your local and create a empty dabaase called "t1d"  
Inside "t1d" database,  create a schema called "index_parameters"
configure the user_name and password accordingly based on the R code , or change the R code instead:
`\code_R_data_prep\DATA_CONSTANTS.R'`

# Step 3:  Push coutry profile data and constants to database 

Source R file:   `source(~/GitHub/t1d-index/code_R_data_prep/000_0_0_build_database.R)`
run code in the function or run function `build_database()` will populate database with relevavent information.

# Step 4:  Run Mortality Model
simply run R script 
`\code_R_data_prep\000_2_Mortality_Diagnosed_046_single_run.R`

Variable model_data_global_age contains SMR (standard morality rate ) for all 201 countries of all age brackets.

To save the result , simply run code that is by default commented out :
 ```saveRDS(model_data_global_age,"data_mortality/machine_learning_model_smr.Rds")```

# Step 5:  Prepare input for model  (Currently unable to run due to data sensitivity issue, can run Step 6 directly)
`source("~/GitHub/t1d-index/code_R_data_prep/000_0_build_input_matrices.R")`

and run: 

`build_input_matrices(version_no= "0.4.15", random_data_ci = FALSE,re_run_data_points=TRUE)`


# Step 6:  Run model for each Country 
`source("~/GitHub/t1d-index/code_R/001_main.R")`

`main_(version_no="0.4.15",run_per_country=TRUE,run_merge_country=TRUE ,scenario=2)`

Results are saved under folder : `~/GitHub/t1d-index/reruns_scenario_2_launch`

to view data of any specific country , for example , afganistan , do:

`read_parquet(“~/GitHub/t1d-index/reruns_scenario_2_launch/rerun_0.4.15_0/AFG.binary”)`

note that, the results are not exactly the same as in the Lancet paper or Shiny Dashboard,  this is because, we used Monte carlo methodology to rerun 1000 times and took the median values to present.
