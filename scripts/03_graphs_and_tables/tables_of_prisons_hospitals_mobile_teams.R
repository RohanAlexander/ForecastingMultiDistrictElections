#### Purpose ####
# We need to know how many hospitals, prisons, and special mobility units we are
# losing by the geocoding.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 17 October 2010


#### Workspace setup ####
# Call the necessary packages
library(tidyverse)
# library(sp)
# library(rgdal)


#### Read in the data ####
# Read in the booths data for each year
files <- c("inputs/data/election_2016/GeneralPollingPlacesDownload-20499.csv", 
           "inputs/data/election_2013/GeneralPollingPlacesDownload-17496.csv",
           "inputs/data/election_2010/GeneralPollingPlacesDownload-15508.csv",
           "inputs/data/election_2007/GeneralPollingPlacesDownload-13745.csv")
names(files) <- c("2016", "2013", "2010", "2007")
booths <- purrr::map_df(files, readr::read_csv, skip = 1, guess_max = 10000, .id = "Year")
rm(files)

removing_these <- booths %>%
  mutate(keep = str_detect(PollingPlaceNm, "Special Hospital Team"),
         keep = if_else(str_detect(PollingPlaceNm, "Other Mobile Team"), TRUE, keep),
         keep = if_else(str_detect(PollingPlaceNm, "Remote Mobile Team"), TRUE, keep),
         keep = if_else(str_detect(PollingPlaceNm, "Prison Mobile Team"), TRUE, keep)) %>% 
  filter(keep == TRUE)


all_files <- list.files(
  "inputs/data",
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE)
all_files <- tibble(file = all_files)
all_files <- all_files %>% 
  mutate(first_pref_file = str_detect(file, "HouseStateFirstPrefsByPollingPlaceDownload")) %>% 
  filter(first_pref_file == TRUE) %>% 
  separate(file, c("first", "second", "state"), sep = "-", remove=FALSE) %>% 
  mutate(state = str_remove(state, ".csv")) %>% 
  select(-first_pref_file, -second, -first)
all_files <- all_files %>% 
  separate(file, c("inputs", "data", "year", "rest"), sep = "/", remove=FALSE) %>% 
  select(-inputs, -data, -rest)
files <- all_files$file
names(files) <- all_files$year

first_prefs_by_booth <- purrr::map_df(files, readr::read_csv, skip = 1, .id = "Year")
first_prefs_by_booth$Year <- str_remove(first_prefs_by_booth$Year, "election_")
first_prefs_by_booth$Year <- as.integer(first_prefs_by_booth$Year)


first_prefs_by_booth <- first_prefs_by_booth %>% 
  select(Year, PollingPlaceID, OrdinaryVotes)

removing_these$Year <- removing_these$Year %>% as.integer()

removing_these_counts <- removing_these %>% 
  left_join(first_prefs_by_booth, by = c("Year", "PollingPlaceID")) %>% 
  group_by(Year, PollingPlaceNm) %>% 
  summarise(numer = sum(OrdinaryVotes, na.rm = TRUE))

removing_these_counts <- 
  removing_these_counts %>% 
  mutate(Type = if_else(str_detect(PollingPlaceNm, "Special Hospital Team") == TRUE, "Special Hospital", "Unsure"),
         Type = if_else(str_detect(PollingPlaceNm, "Other Mobile Team") == TRUE, "Other Mobile", Type),
         Type = if_else(str_detect(PollingPlaceNm, "Remote Mobile Team") == TRUE, "Remote Mobile", Type),
         Type = if_else(str_detect(PollingPlaceNm, "Prison Mobile Team") == TRUE, "Prison Mobile", Type))

removing_these_counts$Type %>% unique()


removing_these_counts_table <-  
  removing_these_counts %>% 
  group_by(Year, Type) %>% 
  summarise(Number = sum(numer, na.rm = TRUE)) %>% 
  spread(Type, Number) %>% 
  mutate(`Other/Prison` = sum(`Other Mobile`, `Prison Mobile`, na.rm = TRUE)) %>% 
  rename(Hospital = `Special Hospital`,
         Remote = `Remote Mobile`
         ) %>% 
  select(Year, Hospital, Remote, `Other/Prison`)
  
write_csv(removing_these_counts_table, "outputs/data/tables/hospitalsPrisonsOther.csv")  
  


