library(tidyverse)

# d0 <- read.csv("../data/raw/Baby mental life: Study 1_August 1, 2018_09.03.csv")

d0 <- read.csv("./data/raw/Baby mental life: Study 1_August 9, 2018_07.29.csv")

# get rid of repeating GPS coordinates
repeat_gps <- d0 %>% 
  select(starts_with("Location"), ResponseId) %>%
  mutate_at(vars(starts_with("Location")), 
            funs(as.numeric(as.character(.)))) %>%
  filter(!is.na(LocationLatitude), !is.na(LocationLongitude)) %>%
  count(LocationLatitude, LocationLongitude) %>%
  filter(n > 1) %>%
  mutate(latlong = paste(LocationLatitude, LocationLongitude, sep = ", "))

d0_norep <- d0 %>%
  mutate_at(vars(starts_with("Location")), 
            funs(as.numeric(as.character(.)))) %>%
  mutate(latlong = paste(LocationLatitude, LocationLongitude, sep = ", "),
         duplicateGPS = ifelse(latlong %in% repeat_gps$latlong, 
                               TRUE, FALSE)) %>%
  select(-latlong)

# remove identifying variables
d1 <- d0_norep %>%
  select(-c(IPAddress, starts_with("Recipient"), ExternalReference,
            starts_with("Location"), DistributionChannel, MTurkCode)) %>%
  data.frame() %>%
  mutate(ResponseId = 999:(998+nrow(d0_norep)))

write.csv(d1, "./data/deidentified/baby_mental_life_s1_data.csv")
