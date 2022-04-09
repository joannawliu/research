# remaining todo/issues:
### decennial: median home value might instead be H076001
### for decennial + acs county subdivisions: drop observations with county_subdiv_code = 00000



# libraries
library(tidyverse)
library(tidycensus)
library(reshape2)

# API key
# census_api_key("API KEY HERE", install = TRUE)



########## decennial data: place ##########

# store variable names
vars_dec <- c(population = "P001001", white_residents = "P006002", 
              black_residents = "P006003", asian_residents = "P006005", 
              native_residents = "P006004", pac_isl_residents = "P006006", 
              other_race_residents = "P006007", multi_race_residents = "P006008", 
              nonhisp_white = "P007003", hisp_white = "P007011",
              median_hh_income = "P053001", poverty_pop = "P087002",
              median_home_value = "H085001")

# decennial dataframe for 2000
dec2000_place <- get_decennial(geography = "place", year = 2000, variables = vars_dec) %>%
  spread(variable, value) %>%    # move variables to cols
  mutate(year = 2000,    # add year col
         sum_other_race_residents = native_residents + pac_isl_residents +
           other_race_residents + multi_race_residents,    # sum other race
         p_poverty = poverty_pop / population,    # proportion below poverty level
         place_code = str_sub(GEOID, -5)) %>%    # separate place code
    relocate(GEOID, place_code, NAME, year, population,
             white_residents, black_residents, asian_residents, sum_other_race_residents,
             native_residents, pac_isl_residents, other_race_residents, multi_race_residents,
             nonhisp_white, hisp_white,
             median_hh_income, p_poverty, median_home_value)    # re-order cols

# export final decennial place df
# write.csv(dec2000_place, "dec_place_final.csv")



########## decennial data: county subdivision ##########

# store county subdivision states
subdiv_states <- c("CT", "HI", "KY", "MA", "NJ", "NY", "OH", "PA", "RI")

# decennial dataframe for 2000
dec2000_subdiv <- get_decennial(geography = "county subdivision", year = 2000, 
                                variables = vars_dec, state = subdiv_states) %>%
  spread(variable, value) %>%    # move variables to cols
  mutate(year = 2000,    # add year col
         sum_other_race_residents = native_residents + pac_isl_residents +
           other_race_residents + multi_race_residents,    # sum other race
         p_poverty = poverty_pop / population,    # proportion below poverty level
         state_code = substr(GEOID, 1, 2),    # separate state code
         county_code = substr(GEOID, 3, 5),    # separate county code
         county_subdiv_code = str_sub(GEOID, -5)) %>%    # separate county subdivision code
    relocate(GEOID, state_code, county_code, county_subdiv_code,
             NAME, year, population,
             white_residents, black_residents, asian_residents, sum_other_race_residents,
             native_residents, pac_isl_residents, other_race_residents, multi_race_residents,
             nonhisp_white, hisp_white,
             median_hh_income, p_poverty, median_home_value)    # re-order cols

# export final decennial county subdivision df
# write.csv(dec2000_subdiv, "dec_subdiv_final.csv")



########## ACS data: place ##########

# store variable names
vars_acs <- c(population = "B01003_001", 
              white_residents = "B02001_002", black_residents = "B02001_003", 
              asian_residents = "B02001_005", native_residents = "B02001_004", 
              pac_isl_residents = "B02001_006", other_race_residents = "B02001_007", 
              multi_race_residents = "B02001_008", 
              nonhisp_white = "B03002_003", hisp_white = "B03002_013", 
              median_hh_income = "B19013_001", poverty_pop = "B17021_002", 
              median_home_value = "B25077_001")

# function: create ACS dataframes
get_acs_place <- function(y) {
  df <- get_acs(geography = "place", year = y, variables = vars_acs) %>%
    select(-moe) %>%    # remove moe variable
    spread(variable, estimate) %>%    # move variables to cols
    mutate(year = y,    # create year variable
           sum_other_race_residents = native_residents + pac_isl_residents +
             other_race_residents + multi_race_residents,    # sum other race
           p_poverty = poverty_pop / population,    # proportion below poverty level
           place_code = str_sub(GEOID, -5)) %>%    # separate place code
    relocate(GEOID, place_code, NAME, year, population,
             white_residents, black_residents, asian_residents, sum_other_race_residents,
             native_residents, pac_isl_residents, other_race_residents, multi_race_residents,
             nonhisp_white, hisp_white,
             median_hh_income, p_poverty, median_home_value)    # re-order cols
  return(df)
}

# create dataframes by year
acs2009_place <- get_acs_place(2009)
acs2010_place <- get_acs_place(2010)
acs2011_place <- get_acs_place(2011)
acs2012_place <- get_acs_place(2012)
acs2013_place <- get_acs_place(2013)
acs2014_place <- get_acs_place(2014)
acs2015_place <- get_acs_place(2015)
acs2016_place <- get_acs_place(2016)
acs2017_place <- get_acs_place(2017)
acs2018_place <- get_acs_place(2018)
acs2019_place <- get_acs_place(2019)

# stack all years
acs_place <- rbind(acs2009_place, acs2010_place, acs2011_place,
                   acs2012_place, acs2013_place, acs2014_place,
                   acs2015_place, acs2016_place, acs2017_place, 
                   acs2018_place, acs2019_place)

# export final ACS place df
# write.csv(acs_place, "acs_place_final.csv")



########## ACS data: county subdivision ##########

# function: create ACS dataframes by year
get_acs_subdiv <- function(y) {
  df <- get_acs(geography = "county subdivision", year = y, variables = vars_acs,
                state = subdiv_states) %>%
    select(-moe) %>%    # remove moe variable
    spread(variable, estimate) %>%    # move variables to cols
    mutate(year = y,    # create year variable
           sum_other_race_residents = native_residents + pac_isl_residents +
             other_race_residents + multi_race_residents,    # sum other race
           p_poverty = poverty_pop / population,    # proportion below poverty level
           state_code = substr(GEOID, 1, 2),    # separate state code
           county_code = substr(GEOID, 3, 5),    # separate county code
           county_subdiv_code = str_sub(GEOID, -5)) %>%    # separate county subdivision code
    relocate(GEOID, state_code, county_code, county_subdiv_code, 
             NAME, year, population,
             white_residents, black_residents, asian_residents, sum_other_race_residents,
             native_residents, pac_isl_residents, other_race_residents, multi_race_residents,
             nonhisp_white, hisp_white,
             median_hh_income, p_poverty, median_home_value)    # re-order cols
  return(df)
}

# create dataframes by year
acs2009_subdiv <- get_acs_subdiv(2009)
acs2010_subdiv <- get_acs_subdiv(2010)
acs2011_subdiv <- get_acs_subdiv(2011)
acs2012_subdiv <- get_acs_subdiv(2012)
acs2013_subdiv <- get_acs_subdiv(2013)
acs2014_subdiv <- get_acs_subdiv(2014)
acs2015_subdiv <- get_acs_subdiv(2015)
acs2016_subdiv <- get_acs_subdiv(2016)
acs2017_subdiv <- get_acs_subdiv(2017)
acs2018_subdiv <- get_acs_subdiv(2018)
acs2019_subdiv <- get_acs_subdiv(2019)

# stack all years
acs_subdiv <- rbind(acs2009_subdiv, acs2010_subdiv, acs2011_subdiv,
                   acs2012_subdiv, acs2013_subdiv, acs2014_subdiv,
                   acs2015_subdiv, acs2016_subdiv, acs2017_subdiv, 
                   acs2018_subdiv, acs2019_subdiv)

# export final ACS county subdivision df
# write.csv(acs_subdiv, "acs_subdiv_final.csv")

