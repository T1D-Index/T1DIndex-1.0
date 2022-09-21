read_country_region_mapping <- function()
{
  country_region_mapping <- read_excel('data_sub_national/country-region-mapping.xlsx', 'merge',skip = 0)  # only using format , overrtie content.
  country_region_mapping$name <- trimws(stri_trans_general(country_region_mapping$name, "Latin-ASCII"))
  country_region_mapping      <- country_region_mapping[!is.na(country_region_mapping$Country),]

  setDT(country_region_mapping)[,population_ratio_percentage:=list(prop.table(Population)),by=c("Country")]

  country_region_mapping$Mortality <-  as.numeric( gsub(",",".",country_region_mapping$Mortality) )
  setDT(country_region_mapping)[,background_mortality_ratio:=list( (Mortality )/( sum(Mortality*Population) /sum(Population)  )  ),by=c("Country")]

  country_region_mapping$`gdp per capita` <-  as.numeric( gsub(",","",country_region_mapping$`gdp per capita`) )
  setDT(country_region_mapping)[,gdp_ratio:=list( (`gdp per capita` )/( sum(`gdp per capita`*Population) /sum(Population)  )  ),by=c("Country")]


  # country_region_mapping$imr <- as.numeric( gsub(",","", country_region_mapping$imr))/100
  # country_region_mapping$imr_ratio <- country_region_mapping$imr/(sum(country_region_mapping$imr * country_region_mapping$Population_2019)/sum(country_region_mapping$Population_2019))

  country_region_mapping
}

read_country <- function()
{
  countries_codes_and_coordinates    <- read.csv('data_internal/countries_codes_and_coordinates.csv',stringsAsFactors = F,encoding='UTF-8')
  countries_codes_and_coordinates$world_bank_code <- trimws(countries_codes_and_coordinates$Alpha.3.code)
  countries_codes_and_coordinates$Alpha.2.code    <- trimws(countries_codes_and_coordinates$Alpha.2.code)
  country    <- read.csv('data_internal/country.csv',stringsAsFactors = F,encoding='UTF-8')
  countries_codes_and_coordinates <- dplyr::select(countries_codes_and_coordinates,-Country ,-Numeric.code)
  country   <- country %>% left_join(unique(countries_codes_and_coordinates), by="world_bank_code")

  # Load IDF data and use region and income category from Graham.
  estimates <- data.frame(read_excel('data_incidence/IDF Atlas childhood T1DM 2021 estimates 6th July for Fei.xlsx', '2021 estimates',skip = 0))
  estimates_analysis_group <- data.frame(read_excel('data_incidence/IDF Atlas childhood T1DM 2021 estimates 6th July for Fei.xlsx', 'analysis_group',skip = 0))
  colnames(estimates) [!is.na(estimates[1,])] <-  estimates[1,][!is.na(estimates[1,])]
  estimates <- estimates[2:nrow(estimates),]
  estimates <- estimates[,c(2,5,6,30:44,60,66) ]
  estimates <- estimates[!is.na(estimates$Country),]


  country <-  merge(country, dplyr::select(estimates_analysis_group, world_bank_name, Country=Country), by='world_bank_name',all.x=TRUE)
  country <-  merge(country, dplyr::select(estimates
                                           , Country,wd_region     =`World Bank Region`
                                           , wd_income_category    = World.Bank.Income.category.June.2021
                                           , idf_reference_country = Country.used.to.make.extrapolations..0.14. ), by="Country",all.x=TRUE)

  country[is.na(country)] <- ''
  colnames(country)[1] <- "idf_country_name"

  country$wd_income_category[country$wd_income_category==''] <- country$world_bank_classification[country$wd_income_category=='' ]

  country$drop_country <- as.character(country$drop_country)

  country
}
