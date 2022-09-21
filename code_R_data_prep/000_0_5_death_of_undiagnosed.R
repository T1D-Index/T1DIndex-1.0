
onset_death_rates <- function()
{


  source("code_R/utils.R")
  library(readxl)
  country <- readRDS("data_internal/country.Rds")

  # unzip('data_onset/offline.zip',exdir='data_onset',overwrite = T)


  # load("data_onset/model_data.Rda")
  model_datas_from_survey <- function()
  {
    code_ddx <- function(ddx) {
      ordered(ddx, levels=c("0%", "0-10%", "10-20%", "30-40%", "70-80%", "20-30%",
                            "40-50%", "80%+", "50-60%", "No data"))
    }
    # process template for identifying fields in offline data
    template <- read_excel('data_onset/offline_template.xlsx', col_names = FALSE)
    coords <- NULL
    for (col in seq_len(ncol(template))) {
      mask <- !is.na(template[[col]]) & str_detect(template[[col]], '\\[[^\\]]+\\]')
      fields <- str_replace_all(template[[col]], '\\[|\\]', '')
      for (r in which(mask)) {
        coords <- bind_rows(coords, tibble(col=col, row=r, field=fields[r]))
      }
    }
    coords <- coords %>%
      arrange(1000*row + col)

    # load offline data
    files <- dir(path='data_onset/offline/', pattern = '*.xlsx', full.names = TRUE)
    # drop excel temps
    files <- files[!str_detect(files, '\\~\\$')]
    clean_data <- tibble(id=seq_along(files), filename=files)
    for (field in coords$field) {
      clean_data[[field]] <- as.character(NA)
    }
    for (i in seq_along(files)) {
      data <- read_excel(files[i], col_names = FALSE)
      for (j in seq_len(nrow(coords))) {
        clean_data[i, coords$field[j]] <- data[coords$row[j], coords$col[j]]
      }
    }

    messy_argentina <- c("ARGENTINA", "Argentine", "Republica Argentina", "ARGENTINA",
                         "Republica Argentina")
    clean_data$country_wk[clean_data$country_wk %in% messy_argentina] <- 'Argentina'

    # cast fields to type
    field_types <- read_excel('data_onset/offline_template.xlsx', 'FieldTypes')
    nums <- with(field_types, ColumnName[Type == 'numeric'])
    for (col in nums) {
      clean_data[[col]] <- as.numeric(clean_data[[col]])
    }
    # TODO: simplify some fields in offline data like Q21
    offline_data <- clean_data %>%
      mutate(
        # deem emailing in a response as positive election to participate
        elect_part="Yes! I'd like to complete the survey now (est. 10 minutes)",
        source='offline')

    field_names <- read_excel('data_onset/field_names.xlsx')
    exclusions  <- c(11785763698,
                     11785768853,
                     11785794102,
                     11786337191,
                     11786760002,
                     11789066667,
                     11789174009,
                     11789242296,
                     11789765108
    )

    survey_data_unfiltered <- read_excel('data_onset/Survey the global state of T1D.xlsx', col_names=field_names$Field, skip=2) %>%
      mutate(source='online') %>%
      bind_rows(offline_data) %>%
      select_at(c('source',field_names$Field[field_names$Include])) %>%
      mutate(
        od_pre_2000_rate = code_ddx(od_pre_2000_rate),
        od_2000_2010_rate = code_ddx(od_2000_2010_rate),
        od_2010_rate = code_ddx(od_2010_rate),
      ) %>%
      dplyr::select(-followup_email, -reminder_email3, -reminder_email_addr2, -reminder_email_addr)


    countries <- read_csv('data_onset/country.csv',
                          col_types = cols(
                            world_bank_name = col_character(),
                            united_nations_name = col_character(),
                            country_wk = col_character(),
                            drop_country = col_skip(),
                            iso3c = col_character(),
                            world_bank_classification = col_character(),
                            jdrf_region_name = col_character(),
                            jdrf_broad_region_name = col_character(),
                            jdrf_broad_region_code = col_skip(),
                            legacy_region_code = col_skip(),
                            legacy_region_name = col_skip(),
                            loc_id = col_double()
                          ))





    survey_data <- survey_data_unfiltered %>%
      filter(
        elect_part == "Yes! I'd like to complete the survey now (est. 10 minutes)"
        & !resp_id %in% exclusions
        & !is.na(country_wk)
      ) %>%
      left_join(countries, by='country_wk') %>%
      mutate(
        income_group = recode(world_bank_classification,
                              LIC='Lower income',
                              LMIC='Lower income',
                              UMIC='Middle income',
                              HIC='High income'),
        income_group = ordered(income_group,
                               levels=c("High income", "Middle income", "Lower income", NA)),
        world_bank_classification = ordered(world_bank_classification,
                                            levels=c("HIC", "UMIC", "LMIC", "LIC", NA))
      ) %>%
      group_by(country_wk) %>%
      ungroup


    print("responses: ")
    print(paste0("number of respondents : ",nrow(survey_data)) )
    print(paste0("number of countries : ",n_distinct(survey_data$country_wk)))
    print(table(survey_data$world_bank_classification))


    od_rate_refdata <- tribble(
      ~rate, ~lower, ~upper, ~midpoint,
      "0%",      0,      0,         0,
      "0-10%",      0,    0.1,      0.05,
      "10-20%",    0.1,    0.2,      0.15,
      "20-30%",    0.2,    0.3,      0.25,
      "30-40%",    0.3,    0.4,      0.35,
      "40-50%",    0.4,    0.5,      0.45,
      "50-60%",    0.5,    0.6,      0.55,
      "60-70%",    0.6,    0.7,      0.65,
      "70-80%",    0.7,    0.8,      0.75,
      "80%+",    0.8,      1,       0.9
    )

    midpoints <- function(rate) {
      case_when(
        rate == "0%" ~ 0.0,
        rate == "0-10%" ~ 0.05,
        rate == "10-20%" ~ 0.15,
        rate == "20-30%" ~ 0.25,
        rate == "30-40%" ~ 0.35,
        rate == "40-50%" ~ 0.45,
        rate == "50-60%" ~ 0.55,
        rate == "60-70%" ~ 0.65,
        rate == "70-80%" ~ 0.75,
        rate == "80%+" ~ 0.90
      )
    }
    survey_data <- dplyr::select(survey_data, jdrf_broad_region_name, jdrf_region_name, country_wk, iso3c, od_pre_2000_rate,world_bank_classification,world_bank_name,od_pre_2000_rate,od_2000_2010_rate,od_2010_rate)

    survey_data <- survey_data[(!is.na(survey_data$od_pre_2000_rate))|(!is.na(survey_data$od_2000_2010_rate))|(!is.na(survey_data$od_2010_rate)),]



    print("responses: ")
    print(paste0("number of respondents : ",nrow(survey_data[survey_data$world_bank_classification!="HIC",])) )
    print(paste0("number of countries : ",n_distinct(survey_data$country_wk[survey_data$world_bank_classification!="HIC"])))
    print(table(survey_data[survey_data$world_bank_classification!="HIC",]$world_bank_classification))

    survey_od_data <- bind_rows(
      survey_data %>%
        filter(!is.na(od_pre_2000_rate)) %>%
        transmute(jdrf_broad_region_name, jdrf_region_name, country=country_wk, iso3c, period=1, rate=od_pre_2000_rate,world_bank_classification,world_bank_name),
      survey_data %>%
        filter(!is.na(od_2000_2010_rate)) %>%
        transmute(jdrf_broad_region_name, jdrf_region_name, country=country_wk, iso3c, period=2, rate=od_2000_2010_rate,world_bank_classification,world_bank_name),
      survey_data %>%
        filter(!is.na(od_2010_rate)) %>%
        transmute(jdrf_broad_region_name, jdrf_region_name, country=country_wk, iso3c, period=3, rate=od_2010_rate     ,world_bank_classification,world_bank_name)
    ) %>%
      inner_join(od_rate_refdata, by='rate') %>%
      filter(!is.na(midpoint)) %>%
      mutate(period = as.factor(period))

    sum( (!is.na(survey_data$od_pre_2000_rate)) |  (!is.na(survey_data$od_2000_2010_rate)) |  (!is.na(survey_data$od_2010_rate)))

    survey_od_data <- survey_od_data[survey_od_data$world_bank_classification!='HIC',]

    model_data <- dplyr::select(survey_od_data, country=world_bank_name,year= period,income_category=world_bank_classification ,midpoint  )

    model_data$year <- as.character(model_data$year)
    model_data$death_undiagnosed <- model_data$midpoint

    # model_data      <- data.frame(setDT(model_data)[,list(death_undiagnosed=mean(death_undiagnosed)),by=c("country","year","income_category")])
    model_data
  }

  model_data <- model_datas_from_survey()
  #----------------------------------------------------------------------------------------------------------------------------------

  model_data$year[model_data$year==1] <- 1995
  model_data$year[model_data$year==2] <- 2005
  model_data$year[model_data$year==3] <- 2015
  model_data$year <- as.numeric(model_data$year)
  model_data <- model_data %>% left_join(dplyr::select(country,country=world_bank_name,wd_region), by="country")
  # model_data$wd_income_category[model_data$wd_income_category=="HIC"] <- "UMIC"

  model_data_method2_region <- setDT(model_data)[,list(rate = mean(death_undiagnosed)),by=c("year","wd_region")]
  model_data_method2_income <- setDT(model_data)[,list( number_of_response = .N,
                                                       left  =  mean(death_undiagnosed)-qnorm(0.975)*sd(death_undiagnosed)/sqrt(length(death_undiagnosed))
                                                       ,rate = mean(death_undiagnosed)
                                                       ,right =  mean(death_undiagnosed)+qnorm(0.975)*sd(death_undiagnosed)/sqrt(length(death_undiagnosed))
  )
                                                 ,by=c("year","income_category")]


  model_data_method2_region_paper <- model_data_method2_region
  model_data_method2_region_paper$rate <- 1- model_data_method2_region_paper$rate


  model_data_method2_income_paper <- model_data_method2_income
  model_data_method2_income_paper$rate <- 1- model_data_method2_income_paper$rate


  data_plot <- model_data_method2_income

  data_plot$rate <- 1- data_plot$rate
  data_plot$left <- 1- data_plot$left
  data_plot$right <- 1- data_plot$right

  e_umic <- data_plot[model_data_method2_income$income_category=="UMIC",] %>%
    e_charts(year) %>%
    e_line(rate,name="UMIC") %>%
    e_band(right,left)%>%e_x_axis(name="Year")%>%e_y_axis(name="Diagnosis rate")

  e_lmic <- data_plot[model_data_method2_income$income_category=="LMIC",] %>%
    e_charts(year) %>%
    e_line(rate,name="LMIC") %>%
    e_band(right,left)%>%e_x_axis(name="Year")%>%e_y_axis(name="Diagnosis rate")


  e_lic <- data_plot[model_data_method2_income$income_category=="LIC",] %>%
    e_charts(year) %>%
    e_line(rate,name="LIC") %>%
    e_band(right,left)%>%e_x_axis(name="Year")%>%e_y_axis(name="Diagnosis rate")



  e_arrange(e_umic, e_lmic, e_lic, cols =3  )


  #  middle ----------------------------------------------------------------------------------------------------------------
  model_data_method2 <- model_data_method2_region %>% full_join(model_data_method2_income, by="year")
  model_data_method2$value <- (model_data_method2$rate.x + model_data_method2$rate.y) /2
  model_data_method2_paper <- model_data_method2
  model_data_method2_paper$value <-1- model_data_method2$value

  if(FALSE)
  {# for paper
    model_data_method2_region$rate <- round(1-model_data_method2_region$rate,2)*100
    model_data_method2_income$rate <- round(1-model_data_method2_income$rate,2)*100
    model_data_method2$value <- round(1-model_data_method2$value,2)*100
    model_data_method2_region_spread    <- spread(model_data_method2_region,   year       ,    rate)
    model_data_method2_income_spread    <- spread(model_data_method2_income,   year       ,    rate)
    model_data_method2_spread    <- spread(model_data_method2[model_data_method2$year==2015,c("wd_region","income_category","value")],income_category ,value)
  }
  get_od_rates <- function(country,model_data_method2 )
  {
    country_list <- dplyr::select(country, world_bank_name, loc_id,income_category= world_bank_classification, wd_region)
    country_list <- country_list %>% full_join(data.frame(year=c(1995,2005,2015)),by=character())
    country_list <- country_list %>% left_join(dplyr::select(model_data_method2,-rate.x,-rate.y) ,by= c( "year","wd_region","income_category") )
    country_list$value[country_list$income_category=="HIC"] <- 0
    country_list$age <- 1
    country_list <- country_list[,c("loc_id", "year",   "value",  "age" )]
    od_rates <- imputation_year_age(country_list)
    od_rates$value[od_rates$age>=25] <- 0
    od_rates
  }

  od_rates <- get_od_rates(country,model_data_method2 )

  model_data_method <- model_data_method2

  #  left ----------------------------------------------------------------------------------------------------------------
  model_data_method2 <- model_data_method2_region %>% full_join(dplyr::select(model_data_method2_income, year,income_category,rate=left), by="year")
  model_data_method2$value <- (model_data_method2$rate.x + model_data_method2$rate.y) /2

  od_rates_left <- get_od_rates(country,model_data_method2 )

  #  right ----------------------------------------------------------------------------------------------------------------
  model_data_method2 <- model_data_method2_region %>% full_join(dplyr::select(model_data_method2_income, year,income_category,rate=right), by="year")
  model_data_method2$value <- (model_data_method2$rate.x + model_data_method2$rate.y) /2

  od_rates_right <- get_od_rates(country,model_data_method2 )


  od_rates$value_left  <- od_rates_left $value
  od_rates$value_right <- od_rates_right$value


  saveRDS(od_rates, paste0('data_internal/onset_death_rates_method2.binary'))
  # write_csv(od_rates, 'data_internal/onset_death_rates_method2.csv')
  return(list(od_rates=od_rates, model_data_method=model_data_method, get_od_rates=get_od_rates))
  #---------------------------------------------------------------------------------------------------------------------------------------------------------
}
