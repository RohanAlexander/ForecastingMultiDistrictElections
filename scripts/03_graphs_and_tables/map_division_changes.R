# install.packages("caret")
library(caret)
library(dplyr)
# install.packages("ggmap")
library(ggmap)
# install.packages("rgeos")
library(rgeos)

citation("ggmap")


bbox <- c(left = 148.95, bottom = -35.5, right = 149.3, top = -35.1)

map <- get_stamenmap(bbox, zoom = 11, maptype = "toner-lite")

canberra <- ggmap(map)

australia <- rgdal::readOGR("inputs/data/maps/national-esri-fe2019", 
                     layer = "COM_ELB_region")

just_act <- australia[australia@data$State == "ACT",]


# Read in the booths data for each year
booths <- readr::read_csv("inputs/data/election_2016/GeneralPollingPlacesDownload-20499.csv", 
                          skip = 1, 
                          guess_max = 10000
                          )
#### Amend the booths data so that it is geographic ####
# Reduce the booths data to only rows with that have latitude and longitude
booths_reduced <- booths %>%
  filter(State == "ACT") %>% 
  select(PollingPlaceID, DivisionNm, Latitude, Longitude) %>% 
  filter(!is.na(Longitude))
# Grab latitude and longitude for those booth locations and turn them into 
# coordinates

# install.packages("mapproj")
library(mapproj)

data_2019 <- fortify(just_act)
data_2019$id[data_2019$id == 0] <- "Bean"
data_2019$id[data_2019$id == 1] <- "Canberra"
data_2019$id[data_2019$id == 2] <- "Fenner"

# https://gis.stackexchange.com/questions/209314/ggmap-plot-polygon-from-shapefile
# https://rstudio-pubs-static.s3.amazonaws.com/520529_7c4aa01eeee04cf6aafdbfe4a5ee4ae0.html
ggmap(map, extent = "normal", maprange = FALSE) +
  geom_point(data = booths_reduced,
             aes(x = Longitude, y = Latitude, colour = DivisionNm),
             ) +
  geom_polygon(data = data_2019,
               aes(long, lat, group = group, fill = id),
               alpha = 0.2,
               color = "black") +
  scale_fill_brewer(name = "2019 Division:", palette = "Set1") +
  scale_color_brewer(name = "2016 Division:", palette = "Set1") +
  coord_map(projection="mercator",
            xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
            ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# +
  # theme(legend.position = "bottom")

ggsave("outputs/figures/map.pdf", width = 20, height = 10, units = "cm")


