options(timeout=60*20)
# links are now obsolete ,  csv data can be found at : https://population.un.org/wpp/Download/Files/5_Archive/WPP2019-CSV-data.zip
# WPP location
download.file(    url = "https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX"
                  , destfile = 'data_un/WPP2019_F01_LOCATIONS.XLSX', mode="wb")


# WPP population 5 age
download.file(    url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationByAgeSex_Medium.csv"
                  , destfile = 'data_un/WPP2019_PopulationByAgeSex_Medium.csv', mode="wb")
# WPP population single age
download.file(    url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_1950-2019.csv"
                  , destfile = 'data_un/WPP2019_PopulationBySingleAgeSex_1950-2019.csv', mode="wb")

download.file(    url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_PopulationBySingleAgeSex_2020-2100.csv"
                  , destfile = 'data_un/WPP2019_PopulationBySingleAgeSex_2020-2100.csv', mode="wb")


# WPP life table
download.file(    url = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_Life_Table_Medium.csv"
                  , destfile = 'data_un/WPP2019_Life_Table_Medium.csv', mode="wb")


# convert to Rds format and delete csvs
data_temp <- data.table::fread('data_un/WPP2019_PopulationByAgeSex_Medium.csv')
saveRDS(data_temp     , paste0("data_un/WPP2019_PopulationByAgeSex_Medium.Rds"))
file.remove(                   "data_un/WPP2019_PopulationByAgeSex_Medium.csv")

data_temp <- data.table::fread('data_un/WPP2019_PopulationBySingleAgeSex_1950-2019.csv')
saveRDS(data_temp     , paste0("data_un/WPP2019_PopulationBySingleAgeSex_1950-2019.Rds"))
file.remove(                   "data_un/WPP2019_PopulationBySingleAgeSex_1950-2019.csv")

data_temp <- data.table::fread('data_un/WPP2019_PopulationBySingleAgeSex_2020-2100.csv')
saveRDS(data_temp     , paste0("data_un/WPP2019_PopulationBySingleAgeSex_2020-2100.Rds"))
file.remove(                   "data_un/WPP2019_PopulationBySingleAgeSex_2020-2100.csv")

data_temp <- data.table::fread('data_un/WPP2019_Life_Table_Medium.csv')
saveRDS(data_temp     , paste0("data_un/WPP2019_Life_Table_Medium.Rds"))
file.remove(                   "data_un/WPP2019_Life_Table_Medium.csv")


# write_parquet(population, paste0("data_un/population.parquet"), compression = "gzip", compression_level = 5)
