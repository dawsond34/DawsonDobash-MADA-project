###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readr) #for loading CSV files
library(dplyr) #for data processing
library(here) #to set paths
library(tidyverse)

#path to data
#note the use of the here() package and not absolute paths
data_location1 <- here::here("data","raw_data","Covid Data - Africa.csv")
data_location2 <- here::here("data","raw_data","Healthcare_Funds.csv")
data_location3 <- here::here("data","raw_data","Worldwide Vaccine Data.csv")


#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata1 <- readr::read_csv(data_location1)
rawdata2 <- readr::read_csv(data_location2)
rawdata3 <- readr::read_csv(data_location3)

#take a look at the data
dplyr::glimpse(rawdata1)
dplyr::glimpse(rawdata2)
dplyr::glimpse(rawdata3)

#dataset is so small, we can print it to the screen.
#that is often not possible.
print(rawdata1)
print(rawdata2)
print(rawdata3)

# looks like we have measurements for height (in centimeters) and weight (in kilogram)

# this is one way of doing it. Note that if the data gets updated, 
# we need to decide if the thresholds are ok (newborns could be <50)

# save data as RDS
# I suggest you save your processed and cleaned data as RDS or RDA/Rdata files. 
# This preserves coding like factors, characters, numeric, etc. 
# If you save as CSV, that information would get lost.
# See here for some suggestions on how to store your processed data:
# http://www.sthda.com/english/wiki/saving-data-into-r-data-format-rds-and-rdata

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(processeddata, file = save_data_location)


