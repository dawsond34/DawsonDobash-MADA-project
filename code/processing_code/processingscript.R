###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.

library(readr) #for loading CSV files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse) #for cleaning and 'tidying' the data
library(countrycode) #changes country codes to full names

#path to data
#note the use of the here() package and not absolute paths

data_location1 <- here::here("data","raw_data","Covid Data - Africa.csv")
data_location2 <- here::here("data","raw_data","Healthcare_Funds.csv")
data_location3 <- here::here("data","raw_data","Worldwide Vaccine Data.csv")
data_location4 <- here::here("data","raw_data","Covid Data - Asia.csv")
data_location5 <- here::here("data","raw_data","Covid Data - Europe.csv")
data_location6 <- here::here("data","raw_data","Covid Data - North America.csv")
data_location7 <- here::here("data","raw_data","Covid Data - Oceania.csv")
data_location8 <- here::here("data","raw_data","Covid Data - South America.csv")
data_location9 <- here::here("data","raw_data","GDP per capita.csv")
data_location10 <- here::here("data","raw_data","Country_Govt.csv")

#load data from the data locations specified for each data set 

africa <- readr::read_csv(data_location1)
funds <- readr::read_csv(data_location2)
vaccine <- readr::read_csv(data_location3)
asia <- readr::read_csv(data_location4)
europe <- readr::read_csv(data_location5)
n_america <- readr::read_csv(data_location6)
oceania <- readr::read_csv(data_location7)
s_america <- readr::read_csv(data_location8)
gdp <- readr::read_csv(data_location9)
govt <- readr::read_csv(data_location10)


#take a look at the data.I only included looking at the africa data set because the other continent data sets have the
#exact layout just different countries

dplyr::glimpse(africa)
dplyr::glimpse(funds)
dplyr::glimpse(vaccine)

#Adding indicator variables for each continent to determine which continent each country is from. This will be used 
#for further analysis based on continents

africa <- africa %>% mutate(location = "Africa")
asia <- asia %>% mutate(location = "Asia")
europe <- europe %>% mutate(location = "Europe")
n_america <- n_america %>% mutate(location = "N. America")
s_america <- s_america %>% mutate(location = "S. America")
oceania <- oceania %>% mutate(location = "Oceania")

#For the south america data set, two of the variables that should be numeric were character variables. I found this 
#when trying to merge data sets and for south america countries, the values became all na.

s_america$`Total Recovered` = as.numeric((gsub(",", "", s_america$`Total Recovered`)))
s_america$`Active Cases` = as.numeric((gsub(",", "", s_america$`Active Cases`)))

#combining continent data sets. I used rbind because although it is probably not the best way of combining data sets
#but since all of these data sets have the exact layout. 

world = rbind(africa,europe,asia,n_america,s_america,oceania)

#Making variables into numeric variables (This is just to make sure all variable are the correct class)

world$`Total Recovered` = as.numeric(world$`Total Recovered`)
world$`Active Cases` = as.numeric(world$`Active Cases`)

#Look at glimpse of the world data set

glimpse(world)

#Cleaning the data set by keeping only certain variables and removing all variables that have na's 
#(only removing 24 obs)
#I renamed a variable for the country so it matches the other data sets to make it easy to combine

clean_world <- world %>% select(`Country, Other`, `Total Cases`, `Total Deaths`, `Total Recovered`, `Active Cases`, 
                                `Total Tests`, Population, location)
clean_world = drop_na(clean_world)
clean_world = rename(clean_world, 'Country' = 'Country, Other')

#Filtering funds data set to only the latest year and keeping only needed variables. I also changed the countries 
#3 letter abbreviation to the full country name

clean_funds <- funds %>% filter(SUBJECT == "TOT") %>% select(LOCATION, TIME, Value) %>% 
  arrange(desc(TIME)) %>% group_by(LOCATION)
clean_funds2 = distinct(clean_funds, LOCATION, .keep_all = TRUE)
clean_funds2$Country = countrycode(clean_funds2$LOCATION, "iso3c", "country.name")
clean_funds2$Country = ifelse(clean_funds2$Country == "Czechia", "Czech Republic", 
                              ifelse(clean_funds2$Country == "United Kingdom", "UK", clean_funds2$Country))

gdp <- gdp %>% mutate(ifelse(Country == "United Kingdom", "UK", 
                             ifelse(Country == "Bahamas, The", "Bahamas", 
                                    ifelse(Country == "Brunei Darussalam", "Brunei", 
                                           ifelse(Country == "Cabo Verde", "Cape Verde", 
                                                  ifelse(Country == "Congo, Dem. Rep.", "Congo", 
                                                         ifelse(Country == "Egypt, Arab Rep.", "Egypt", 
                                                                ifelse(Country == "Gambia, The", "Gambia", 
                                                                       ifelse(Country == "Hong Kong SAR, China", "Hong Kong", 
                                                                              ifelse(Country == "Iran, Islamic Rep.", "Iran", 
                                                                                     ifelse(Country == "Cote d'Ivoire", "Ivory Coast", 
                                                                                            ifelse(Country == "Congo, Rep.", "Republic of the Congo", 
                                                                                                   ifelse(Country == "Russian Federation", "Russia", 
                                                                                                          ifelse(Country == "Slovak Republic", "Slovakia", 
                                                                                                                 ifelse(Country == "Korea, Rep.", "South Korea", Country))))))))))))))) %>%
  mutate(Country = `ifelse(...)`) %>% select(-`ifelse(...)`)

#Cleaning the vaccine data set

clean_vaccine = drop_na(vaccine)

#BIG EDIT/CLEANING
#Need to change names of observations for: South Korea, USA, UK, Sao Tome and Principe, UAE, 
#Cape Verde (Capo Verde), CAR (Central African Republic), Czechia (Czech Republic), Dominican Republic, DRC (Republic of Congo)
#Since these are just different names for countries not a code, I have to go into each csv file to rename the countries

#Merging the clean world and clean vaccine data sets first by country as I only want observations that are complete.
#Using the merge function will remove all na observations even if it is only within one variable.

merged_data = merge(clean_world, clean_vaccine, by="Country")

#After merging the two data sets, next I want to merge the final data set, clean funds2. 
#Since this data set is only 48 countries, it want to merge it into the combined data set but also including all 
#observations from the combined data set. I am also removing the variable LOCATION as it is a redundant variable now

merged_data2 = merge(merged_data, clean_funds2, by="Country", all.x = T)
merged_data3 = merge(merged_data2, gdp, by="Country", all.x = T)
merged_data4 = merge(merged_data3, govt, by="Country", all.x = T)
merged_data4 <- merged_data4 %>%  select(-LOCATION, -"2019")

#Need to create some more variables such as proportions
processeddata <- merged_data4 %>% mutate(prop_death = `Total Deaths`/`Total Cases`,
                                         prop_recov = `Total Recovered`/`Total Cases`,
                                         pct_cases = (`Total Cases`/Population)*100,
                                         test_per_person = `Total Tests`/Population,
                                         prop_diff_recov_vs_death = prop_recov - prop_death,
                                         location = as.factor(location),
                                         prop_vacc = `% of population vaccinated`/100,
                                         logit_vacc = logit(prop_vacc),
                                         gdp_per_capita = as.numeric(gsub(",", "",`2020`)),
                                         government = ifelse(government == "Absolute Monarchy" | government == "Constitutional Monarchy", "Monarchy", government)) %>% 
  filter(`Total Cases` >= 1000) %>% select(-`2020`)


#Looking at the complete data set

glimpse(processeddata)

# location to save file

save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


