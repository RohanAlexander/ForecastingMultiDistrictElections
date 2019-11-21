#### Purpose ####
# We need to push the past election results all into 2019 geographies. We have 
# the maps (shapefiles) for 2019, so we just need to use the geocodes for the 
# booths for the past elections and then we can assign each booth to its 2019 
# division and re-jig.


#### Contact ####
# Author: Rohan Alexander
# Contact: rohan.alexander@utoronto.ca
# Last updated: 5 October 2010


#### Workspace setup ####
# Call the necessary packages
library(tidyverse)
library(sp)
library(rgdal)


#### Read in the data ####
# Read in shapefile of the maps from the AEC for the 2019 divisions
australia <- readOGR("inputs/data/maps/national-esri-fe2019", 
                     layer = "COM_ELB_region")
# Read in the booths data for each year
files <- c("inputs/data/election_2016/GeneralPollingPlacesDownload-20499.csv", 
       "inputs/data/election_2013/GeneralPollingPlacesDownload-17496.csv",
       "inputs/data/election_2010/GeneralPollingPlacesDownload-15508.csv",
       "inputs/data/election_2007/GeneralPollingPlacesDownload-13745.csv")
names(files) <- c("2016", "2013", "2010", "2007")
booths <- purrr::map_df(files, readr::read_csv, skip = 1, guess_max = 10000, .id = "Year")
rm(files)


#### Amend the booths data so that it is geographic ####
# Reduce the booths data to only rows with that have latitude and longitude
booths_reduced <- booths %>%
  select(Year, PollingPlaceID, Latitude, Longitude) %>% 
  filter(!is.na(Longitude))
# Grab latitude and longitude for those booth locations and turn them into 
# coordinates
nicely_formated_coordinates <-
  SpatialPoints(booths_reduced[, c("Longitude", "Latitude")])
# Merge these coordinates back into the reduced booths dataset
booths_reduced <-
  SpatialPointsDataFrame(nicely_formated_coordinates, booths_reduced)
# Impose a projection on that merged dataset so that it is the same as the 
# polygons shapefile digitized from the maps
proj4string(booths_reduced) <-
  CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs ")
# Use this to check what the projection is if you need to: 
# booth_data_no_geo_nas_coords@proj4string, 
# australia@proj4string


#### Merge the shapefile with the reduced geocoded dataset ####
# Create a dataframe where each row lines up with the booths data, but the 
# columns are from the polygons.
merged_data <-
  over(booths_reduced, australia)

booths_reduced_with_2019_division <- dplyr::bind_cols(as_tibble(merged_data), 
                                                      as_tibble(booths_reduced))

booths_reduced_with_2019_division <- booths_reduced_with_2019_division %>% 
  select(Year, Elect_div, State, PollingPlaceID, Latitude, Longitude)

write_csv(booths_reduced_with_2019_division, 
          "outputs/data/elections/booths_reduced_with_2019_division.csv")

