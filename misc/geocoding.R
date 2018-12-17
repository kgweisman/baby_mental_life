library(readxl)
# library(ggmap)
library(sf)
library(mapview)

temp_s1 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Baby mental life/baby_mental_life/study 1/data/raw/Baby mental life: Study 1_August 9, 2018_07.29.csv") %>%
  select(ResponseId, starts_with("Location")) %>%
  filter(!grepl("loc", tolower(LocationLongitude))) %>%
  rename(lat = LocationLatitude,
         lon = LocationLongitude) %>%
  select(ResponseId, lon, lat) %>%
  mutate_at(vars(lat, lon), funs(as.numeric(as.character(.)))) %>%
  filter(!is.na(lat), !is.na(lon)) #%>%
  # group_by(ResponseId) %>%
  # mutate(loc = revgeocode(c(lon, lat))) %>%
  # ungroup()

locations_s1 <- st_as_sf(temp_s1, coords = c("lon", "lat"), crs = 4326)
mapview(locations_s1)

temp_s2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Baby mental life/baby_mental_life/study 2/data/raw/Baby mental life: Study 2 - MEN_September 5, 2018_16.01.csv") %>%
  select(ResponseId, starts_with("Location")) %>%
  full_join(read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/Baby mental life/baby_mental_life/study 2/data/raw/Baby mental life: Study 2 - WOMEN_September 5, 2018_15.59.csv") %>%
              select(ResponseId, starts_with("Location"))) %>%
  filter(!grepl("loc", tolower(LocationLongitude))) %>%
  rename(lat = LocationLatitude,
         lon = LocationLongitude) %>%
  select(ResponseId, lon, lat) %>%
  mutate_at(vars(lat, lon), funs(as.numeric(as.character(.)))) %>%
  filter(!is.na(lat), !is.na(lon)) #%>%
# group_by(ResponseId) %>%
# mutate(loc = revgeocode(c(lon, lat))) %>%
# ungroup()

locations_s2 <- st_as_sf(temp_s2, coords = c("lon", "lat"), crs = 4326)
mapview(locations_s2)
