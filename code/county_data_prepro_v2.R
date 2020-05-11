library(tidyverse)
library(readxl)


## custom functions ----
# CDC WONDER Multiple Cause of death help functions; accessed 2 Nov 2019
cdc_data_loader <- function(file_path) {
  # stopping point 
  cdc_lines <- lines <- read_lines(file_path)
  stop_pt <- 1
  for (i in seq_along(cdc_lines)) {
    if (str_detect(cdc_lines[i], pattern = 'Total')) {
      stop_pt <- i
      break
    }
    stop_pt <- i
  }
  
  # load data
  cdc <- read_tsv(file_path, na = c('Suppressed', 'Missing', 'Unreliable'), 
                  col_types = cols(`County Code` = col_character(), Deaths = col_double(),
                                   Population = col_double(), `Crude Rate` = col_double()),
                  n_max = (stop_pt - 2))
  cdc <- select(cdc, -Notes)
  
  return(cdc)
}


cdc_num_na <- function(type) {
  root_folder <- str_c('./data_county/death_', type)
  file_paths <- dir(root_folder, full.names = TRUE)
  
  years <- seq(1999, 2017) %>% as.character()
  num_na <- vector(mode = 'numeric', length = length(years))
  na_ratio <- vector(mode = 'numeric', length = length(years))
  
  for (i in seq_along(file_paths)) {
    cdc <- cdc_data_loader(file_paths[i])
    num_na[i] <- sum(is.na(cdc$Deaths))
    na_ratio[i] <- num_na[i] / nrow(cdc)
  }
  na_stat <- tibble(year = years, suppressed = num_na, ratio = na_ratio)
  
  return(na_stat)
}


cdc_aggregator <- function(type) {
  root_folder <- str_c('./data_county/death_', type)
  file_paths <- dir(root_folder, full.names = TRUE)
  
  years <- seq(1999, 2017) %>% as.character()
  data_container <- vector(mode = 'list', length = length(file_paths))
  
  for (i in seq_along(data_container)) {
    cdc <- cdc_data_loader(file_paths[i])
    cdc <- cdc %>% 
      mutate(Year = years[i]) %>% 
      rename(year = Year, county_code = `County Code`, county = County, population = Population, deaths = Deaths) %>% 
      mutate(state_code = substr(county_code, 1, 2)) %>% 
      select(year, state_code, county_code, county, population, deaths)
    
    data_container[[i]] <- cdc
  }
  
  aggregated <- bind_rows(data_container)
  aggregated <- aggregated %>% 
    arrange(year, state_code, county_code)
  aggregated$year <- aggregated$year %>% as.character()
  
  return(aggregated)
}


acs_processor <- function(est_year) {
  # est_year <- '5year'
  years <- seq(2008, 2017) %>% as.character()
  acs_datacode <- read_excel('./data_county/acs_datacode.xlsx')
  dataset_codes <- unique(acs_datacode$dataset_code)
  folder_names <- str_c('./data_county/aff_', est_year, '/aff_', years)
  
  # create a data container
  data_container <- vector('list', length(folder_names))
  for (i in seq_along(folder_names)) {
    data_container[[i]] <- vector('list', length(dataset_codes))
  }
  
  # extract target datasets
  # i <- 1
  for (i in seq_along(folder_names)) {
    folder_name <- folder_names[i]
    paths <- dir(folder_name, full.names = TRUE)
    paths <- paths[str_detect(paths, pattern = 'with_ann')]
    
    # j <- 1
    for (j in seq_along(dataset_codes)) {
      path <- paths[str_detect(paths, pattern = dataset_codes[j])]
      data_names <- acs_datacode %>% 
        filter(dataset_code == dataset_codes[j]) %>% 
        pull(name)
      
      acs <- read_csv(path, skip = 1, na = c('N', '-'))
      acs_colnames <- colnames(acs)
      
      # k <- 1
      data_idx <- vector('integer', length(data_names))
      for (k in seq_along(data_names)) {
        data_idx[k] <- which(str_detect(acs_colnames, pattern = data_names[k]) == TRUE)[1]
      }
      data_idx <- na.omit(data_idx)
      
      acs_rcd <- acs %>% 
        mutate(year = years[i]) %>% 
        select(year, Id2, Geography, data_idx) %>% 
        rename(county_code = Id2, county = Geography) %>% 
        mutate(state_code = str_sub(county_code, 1, 2)) %>% 
        select(year, state_code, county_code, county, everything())
      
      data_container[[i]][[j]] <- acs_rcd
    }
  }
  
  data_container2 <- vector('list', length(folder_names))
  for (i in seq_along(folder_names)) {
    data_container2[[i]] <- data_container[[i]] %>% 
      reduce(full_join, by = c('year', 'state_code', 'county_code', 'county')) %>%
      arrange(year, county_code)
  }
  
  acs_agg <- bind_rows(data_container2) %>% 
    arrange(year, state_code, county_code)
  
  return(acs_agg)
}


## suppressed counties ----
# supp_all <- cdc_num_na('all')
# supp_fentanyl <- cdc_num_na('fentanyl')
# supp_heroin <- cdc_num_na('heroin')


# write_csv(supp_all, path = './result_county/supp_all.csv')
# write_csv(supp_fentanyl, path = './result_county/supp_fentanyl.csv')
# write_csv(supp_heroin, path = './result_county/supp_heroin.csv')


## overdose deaths ----
deaths_all <- cdc_aggregator('all') %>% 
  rename(death_all = deaths)
deaths_fentanyl <- cdc_aggregator('fentanyl') %>% 
  rename(death_fen = deaths)
deaths_heroin <- cdc_aggregator('heroin') %>% 
  rename(death_he = deaths)
# deaths_illicit <- inner_join(deaths_fentanyl, deaths_heroin, key = c('year', 'county_code'))
# deaths_illicit <- deaths_illicit %>% 
#   mutate(death_illicit = death_fen + death_he, death_ratio_illicit = death_ratio_fen + death_ratio_he) %>% 
#   select(year, county_code, county, death_illicit, death_ratio_illicit)


## labor ----
file_paths <- dir('./data_county/labor', full.names = TRUE)
file_paths <- file_paths[1:19]  # 2000 ~ 2018

data_container <- vector(mode = 'list', length = length(file_paths))
for (i in seq_along(file_paths)) {
  labor <- read_excel(file_paths[i], skip = 6, col_names = FALSE)
  colnames(labor) <- c("laus", "state_code", "county_fips","county", "year", "empty", 
                       "labor_force", "employed", "unemployed", "unemployed_rate")
  
  labor <- labor %>% 
    mutate(county_code = str_c(state_code, county_fips)) %>% 
    select(year, state_code, county_code, county, labor_force, unemployed_rate) 
  # drop_na()
  
  data_container[[i]] <- labor
}
labor <- bind_rows(data_container)
labor <- labor %>% 
  arrange(year, state_code, county_code)
labor$year <- labor$year %>% as.character()


## income and poverty ----
income_poverty <- read_csv('./data_county/income_poverty/income_poverty_09-17.csv')
income_poverty_county <- income_poverty %>% 
  filter(str_sub(`County ID`, -3) != '000') %>% 
  # select(-State) %>% 
  rename(year = Year, state_code = State, county_code = `County ID`, county = `State / County Name`) %>% 
  arrange(year, state_code, county_code)

# select columns only for interest
income_poverty_county <- income_poverty_county %>% 
  select(year, state_code, county_code, county, `All Ages in Poverty Percent`, 
         `Under Age 18 in Poverty Percent`, `Median Household Income in Dollars`) %>% 
  rename(all_poverty_rate = `All Ages in Poverty Percent`,
         under18_poverty_rate = `Under Age 18 in Poverty Percent`,
         income = `Median Household Income in Dollars`)

income_poverty_county$income <- income_poverty_county$income %>%
  str_remove_all(pattern = "[$,]") %>% 
  as.numeric()
income_poverty_county$year <- income_poverty_county$year %>% as.character()

income_poverty_2008 <- read_excel('./data_county/income_poverty/est08all.xls', range = cell_rows(3:3197))
income_poverty_2008 <- income_poverty_2008 %>% 
  select(`State FIPS`, `County FIPS`, `Name`, 
         `Poverty Percent All Ages`, `Poverty Percent Under Age 18`, `Median Household Income`) %>% 
  rename(state_code = `State FIPS`, county_fips = `County FIPS`, county = `Name`, 
         all_poverty_rate = `Poverty Percent All Ages`, 
         under18_poverty_rate = `Poverty Percent Under Age 18`, 
         income = `Median Household Income`) %>% 
  mutate(new_county_fips = str_pad(county_fips, width = 3, 'left', pad = '0')) %>% 
  mutate(year = '2008', county_code = str_c(state_code, new_county_fips)) %>% 
  select(year, state_code, county_code, county, all_poverty_rate, under18_poverty_rate, income) %>% 
  filter(str_sub(county_code, -3) != '000') 

income_poverty_2008$year <- income_poverty_2008$year %>% as.character()

income_poverty <- rbind(income_poverty_2008, income_poverty_county) %>% 
  arrange(year, state_code, county_code)


## insurance ----
insurance <- read_csv('./data_county/insurance.csv')
insurance <- insurance %>%
  select(Year, ID, Name, `Uninsured: %`) %>% 
  rename(year = Year, county_code = ID, county = Name, uninsured_rate = `Uninsured: %`) %>% 
  filter(str_sub(`county_code`, -3) != '000') %>% 
  mutate(state_code = str_sub(county_code, 1, 2)) %>% 
  select(year, state_code, county_code, county, uninsured_rate) %>% 
  arrange(year, state_code, county_code) 
insurance$year <- insurance$year %>% as.character()


## American Community Survey ----
acs_agg <- acs_processor('5year')


## aggregation ----
county_data <- full_join(income_poverty, select(insurance, -county), key = c('year', 'state_code', 'county_code')) %>% 
  full_join(., select(labor, -county), key = c('year', 'state_code', 'county_code')) %>% 
  full_join(., select(acs_agg, -county), key = c('year', 'state_code', 'county_code')) %>% 
  full_join(., select(deaths_all, -county), key = c('year', 'state_code', 'county_code')) %>% 
  full_join(., select(deaths_heroin, -county), key = c('year', 'state_code', 'county_code')) %>% 
  full_join(., select(deaths_fentanyl, -county), key = c('year', 'state_code', 'county_code'))

county_data2 <- county_data %>% 
  rename(population_acs = `Total; Estimate; Total population`, 
         male = `Male; Estimate; Total population`, 
         age_15to44_1 = `Total; Estimate; Total population - SELECTED AGE CATEGORIES - 15 to 44 years`, 
         age_over65_1 = `Total; Estimate; Total population - SELECTED AGE CATEGORIES - 65 years and over`, 
         old_dependency_rate_1 = `Total; Estimate; Total population - SUMMARY INDICATORS - Old-age dependency ratio`, 
         child_dependency_rate_1 = `Total; Estimate; Total population - SUMMARY INDICATORS - Child dependency ratio`, 
         white_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - White`, 
         black_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - Black or African American`, 
         asian_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - Asian`, 
         nativeAm_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - American Indian and Alaska Native`, 
         islander_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - Native Hawaiian and Other Pacific Islander`, 
         other_race_1 = `Total; Estimate; Total population - RACE AND HISPANIC OR LATINO ORIGIN - One race - Some other race`, 
         less_highschool = `Total; Estimate; EDUCATIONAL ATTAINMENT - Population 25 years and over - Less than high school graduate`, 
         above_bs = `Total; Estimate; EDUCATIONAL ATTAINMENT - Population 25 years and over - Bachelor's degree or higher`, 
         veteran_rate = `Total; Estimate; VETERAN STATUS - Civilian population 18 years and over - Civilian veteran`, 
         female_household = `Female householder, no husband present, family household; Estimate; Total households`, 
         household = `Total; Estimate; Total households`, 
         divorce_rate = `Divorced; Estimate; Population 15 years and over`, 
         age_15to44_2 = `Total; Estimate; SELECTED AGE CATEGORIES - 15 to 44 years`, 
         age_over65_2 = `Total; Estimate; SELECTED AGE CATEGORIES - 65 years and over`, 
         old_dependency_rate_2 = `Total; Estimate; SUMMARY INDICATORS - Age dependency ratio - Old-age dependency ratio`, 
         child_dependency_rate_2 = `Total; Estimate; SUMMARY INDICATORS - Age dependency ratio - Child dependency ratio`, 
         white_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - White`, 
         black_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Black or African American`, 
         asian_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Asian`, 
         nativeAm_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - American Indian and Alaska Native`, 
         islander_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Native Hawaiian and Other Pacific Islander`, 
         other_race_2 = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Some other race`, 
         old_dependency_rate_3 = `Total; Estimate; SUMMARY INDICATORS - Old-age dependency ratio`, 
         child_dependency_rate_3 = `Total; Estimate; SUMMARY INDICATORS - Child dependency ratio`, 
         age_under18 = `Total; Estimate; SELECTED AGE CATEGORIES - Under 18 years`)

county_data3 <- county_data2 %>% 
  mutate(age_15to44 = pmin(age_15to44_1, age_15to44_2, na.rm = TRUE), 
         age_over65 = pmin(age_over65_1, age_over65_2, na.rm = TRUE),
         old_dependecy_rate = pmin(old_dependency_rate_1, old_dependency_rate_2, old_dependency_rate_3, na.rm = TRUE),
         child_dependency_rate = pmin(child_dependency_rate_1, child_dependency_rate_2, child_dependency_rate_3, na.rm = TRUE),
         white = pmin(white_1, white_2, na.rm = TRUE),
         black = pmin(black_1, black_2, na.rm = TRUE),
         asian = pmin(asian_1, asian_2, na.rm = TRUE),
         nativeAm = pmin(nativeAm_1, nativeAm_2, na.rm = TRUE),
         islander = pmin(islander_1, islander_2, na.rm = TRUE),
         other_race = pmin(other_race_1, other_race_2, na.rm = TRUE))

county_data4 <- county_data3 %>% 
  select(year, state_code, county_code, county, all_poverty_rate, under18_poverty_rate, income, uninsured_rate, labor_force,
         unemployed_rate, population, population_acs, male, age_15to44, age_over65, age_under18, old_dependecy_rate,
         child_dependency_rate, white, black, asian, nativeAm, islander, other_race, less_highschool, above_bs,
         veteran_rate, female_household, household, divorce_rate, death_all, death_he, death_fen)

county_data5 <- county_data4 %>% 
  mutate(labor_force_rate = labor_force / population,
         male_rate = male / population_acs,
         female_household_rate = female_household / household)
county_data5 <- county_data5 %>% 
  rowwise() %>% 
  mutate(other_races = sum(nativeAm, islander, other_race, na.rm = TRUE)) %>% 
  select(year, state_code, county_code, county, population, population_acs, male_rate, 
         age_under18, age_15to44, age_over65, white, black, asian, other_races,
         income, all_poverty_rate, under18_poverty_rate, labor_force_rate, uninsured_rate,
         unemployed_rate, old_dependecy_rate, child_dependency_rate, less_highschool,
         above_bs, veteran_rate, female_household_rate, divorce_rate,
         death_all, death_he, death_fen)
county_data5$other_races <- na_if(county_data5$other_races, 0)
county_data5 <- county_data5[!is.na(county_data5$year), ]

write_csv(county_data5, path = './data_county/county_data.csv')


## Add medicare reimbursement data ----
reimb <- read_excel('./data_county/medicare_reimb/pa_reimb_county_2010.xls')
reimb$`County ID` <- str_pad(reimb$`County ID`, width = 5, 'left', pad = '0')


## retouch 
county_df <- read_csv('./data_county/county_data.csv', col_types = cols(age_under18 = col_number()))
county_df <- county_df[!is.na(county_df$year), ]

county_df_2017age <- county_df %>% 
  filter(year == 2017) %>% 
  mutate(age_under18_2017 = age_under18 / population_acs * 100,
         age_15to44_2017 = age_15to44 / population_acs * 100,
         age_over65_2017 = age_over65 / population_acs * 100) %>%
  select(age_under18_2017, age_15to44_2017, age_over65_2017)

# county_df_2017age %>% 
#   rowwise() %>% 
#   mutate(age_sum = sum(age_under18_2017, age_15to44_2017, age_under18_2017)) %>% 
#   View()

county_df[which(county_df$year == 2017), c('age_under18', 'age_15to44', 'age_over65')] <- county_df_2017age
county_df$labor_force_rate <- county_df$labor_force_rate * 100
county_df$male_rate <- county_df$male_rate * 100
county_df$female_household_rate <- county_df$female_household_rate * 100

write_csv(county_df, './data_county/county_data_new.csv')
