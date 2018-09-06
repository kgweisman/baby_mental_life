library(tidyverse)

d0_f <- read.csv("./data/raw/Baby mental life: Study 2 - WOMEN_September 5, 2018_15.59.csv")
d0_m <- read.csv("./data/raw/Baby mental life: Study 2 - MEN_September 5, 2018_16.01.csv")
d0 <- full_join(d0_f, d0_m)

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

write.csv(d1, "./data/deidentified/baby_mental_life_s2_data.csv")
