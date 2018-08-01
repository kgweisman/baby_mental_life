library(tidyverse)

d0 <- read.csv("../data/raw/Baby mental life: Study 1_August 1, 2018_09.03.csv")

# remove identifying variables
d1 <- d0 %>%
  select(-c(IPAddress, starts_with("Recipient"), ExternalReference,
            starts_with("Location"), DistributionChannel, MTurkCode)) %>%
  data.frame() %>%
  mutate(ResponseId = 10001:(10000+nrow(d0)))

write.csv(d1, "../data/deidentified/baby_mental_life_s1_data.csv")