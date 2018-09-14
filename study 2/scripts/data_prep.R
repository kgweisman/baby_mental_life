# data preparation for baby mental life study 2
library(tidyverse)

# load in de-identified raw data
d0 <- read.csv("./data/deidentified/baby_mental_life_s2_data.csv") %>% select(-X)

# make question key
question_key <- d0[1,] %>%
  t() %>%
  data.frame() %>%
  rownames_to_column("question_qualtrics") %>%
  rename("question_text" = X1) %>%
  mutate(question = recode(question_qualtrics,
                           "Duration..in.seconds." = "Duration",
                           "Q2" = "Age",
                           "Q3" = "GenderSex",
                           "Q3_3_TEXT" = "GenderSex_fillIn",
                           "Q4" = "EnglishProf",
                           "Q5" = "FirstLang",
                           "Q5_2_TEXT" = "FirstLang_fillIn",
                           "Q40" = "RaceEthnicity",
                           "Q40_10_TEXT" = "RaceEthnicity_fillIn",
                           "Q41" = "Education",
                           "Q42" = "Income",
                           "Q43" = "MaritalStatus",
                           "Q43_6_TEXT" = "MaritalStatus_fillIn",
                           "Q44" = "HouseholdSize",
                           "Q45" = "Parent",
                           "Q47" = "ChildrenNumber",
                           "Q48" = "ChildrenYoungestAge",
                           "Q48_1_TEXT" = "ChildrenYoungestAge_fillIn1",
                           "Q48_2_TEXT" = "ChildrenYoungestAge_fillIn2",
                           "Q49" = "ChildrenOldestAge",
                           "Q49_1_TEXT" = "ChildrenOldestAge_fillIn1",
                           "Q49_2_TEXT" = "ChildrenOldestAge_fillIn2",
                           "Q50" = "Attention",
                           "Q51" = "Comments",
                           .default = question_qualtrics),
         question = case_when(grepl("the following questions", question_text) ~
                                gsub("^.*extent is a ", "", question_text),
                              TRUE ~ question),
         question = case_when(grepl("capable of...", question_text) ~
                                gsub("capable of... ", "", tolower(question)),
                              TRUE ~ question),
         question = gsub(" ", "_", question),
         question = gsub("'", "", question),
         question = gsub("5-year-old_-_", "target60mo_", question),
         question = gsub("4-year-old_-_", "target48mo_", question),
         question = gsub("3-year-old_-_", "target36mo_", question),
         question = gsub("2-year-old_-_", "target24mo_", question),
         question = gsub("18-month-old_-_", "target18mo_", question),
         question = gsub("12-month-old_-_", "target12mo_", question),
         question = gsub("9-month-old_-_", "target09mo_", question),
         question = gsub("6-month-old_-_", "target06mo_", question),
         question = gsub("4-month-old_-_", "target04mo_", question),
         question = gsub("3-month-old_-_", "target03mo_", question),
         question = gsub("2-month-old_-_", "target02mo_", question),
         question = gsub("1-month-old_-_", "target01mo_", question),
         question = gsub("4-day-old_-_", "target0Xmo_", question),
         question = gsub("newborn_-_", "target00mo_", question)) %>%
  mutate(question = gsub("-", "_", question),
         question = gsub(" \\(for_example,_smooth,_rough\\)", "", question))

# rename questions
d1 <- d0 %>%
  # get rid of extra info in first two rows  
  filter(!is.na(as.numeric(as.character(Q2)))) %>% 
  gather(question_qualtrics, response, -c(ResponseId, duplicateGPS)) %>%
  left_join(question_key %>% select(question_qualtrics, question)) %>%
  select(-question_qualtrics) %>%
  spread(question, response)

# implement inclusion/exclusion criteria
d2 <- d1 %>%
  filter(Age >= 18, Age <= 45,
         EnglishProf %in% c("Advanced", "Superior"),
         `target04mo_choose_seventy_one` == 71,
         `target0Xmo_please_select_ninety_two` == 92,
         `target12mo_set_this_answer_to_zero` == 0,
         `target36mo_move_the_slider_to_fifty` == 50,
         Attention == "Yes")

# remove people with another identical set of GPS coordinates among people who passed attention checks
d3 <- d2 %>%
  filter(duplicateGPS == F) %>%
  select(-duplicateGPS)

# recode variables & drop extraneous variables
d4 <- d3 %>%
  select(-c(EndDate, Finished, 
            payment, Progress, 
            RecordedDate, StartDate, Status, 
            timeEstimate, UserLanguage)) %>%
  mutate_at(vars(c(starts_with("target"), Age, ChildrenNumber, 
                   ChildrenOldestAge_fillIn1, ChildrenOldestAge_fillIn2, 
                   ChildrenYoungestAge_fillIn1, ChildrenYoungestAge_fillIn2,
                   Duration, HouseholdSize)),
            funs(as.numeric(.))) %>%
  mutate(Education = factor(Education,
                            levels = c("No schooling completed", 
                                       "Nursery school to 8th grade", 
                                       "Some high school, no diploma", 
                                       "High school graduate, diploma or equivalent (including GED)", 
                                       "Some college credit, no degree", 
                                       "Trade school, technical school, or vocational school",
                                       "Associate's degree (for example, AA, AS)",
                                       "Bachelor's degree (for example, BA, BS)",
                                       "Master's degree (for example, MA, MS)",
                                       "Doctor or professional degree (for example, PhD, JD, MD, MBA)")),
         Income = factor(Income,
                         levels = c("$5,001 - 15,000", 
                                    "$15,001 - 30,000", 
                                    "$30,001 - 60,000",
                                    "$60,001 - 90,000",
                                    "$90,001 - 150,000",
                                    "Greater than $150,000",
                                    "Prefer not to say")),
         Parent = factor(Parent,
                         levels = c("No", "Yes")))

# make useful datasets
# final dataset with all measured variables
d <- d4 %>% distinct()

# demographic information
d_demo <- d %>% 
  select(ResponseId, Duration,
         Age, starts_with("GenderSex"), starts_with("RaceEthnicity"),
         starts_with("FirstLang"),
         Education, Income, HouseholdSize,
         starts_with("MaritalStatus"),
         Parent, starts_with("Children"), 
         Comments) %>%
  mutate(RaceEthnicity_collapse = ifelse(grepl(",([A-Za-z])", RaceEthnicity),
                                         "Multiple", RaceEthnicity)) %>%
  mutate(ChildrenOldestAge_collapse = case_when(
    ChildrenOldestAge %in% c("My oldest child has not yet been born (I am/my partner is pregnant)", "My oldest child is deceased", "Prefer not to say") ~ ChildrenOldestAge,
    ChildrenOldestAge == "In months:" ~ 
      ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 1,
             "< 1 year",
             ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 3, 
                    "1 - 3 years",
                    ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 5, 
                           "3 - 5 years",
                           ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 10, 
                                  "5 - 10 years",
                                  ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 18, 
                                         "10 - 18 years",
                                         "> 18 years"))))),
    ChildrenOldestAge == "In years:" ~
      ifelse(as.numeric(ChildrenOldestAge_fillIn2) < 1,
             "< 1 year",
             ifelse(as.numeric(ChildrenOldestAge_fillIn2) < 3, 
                    "1 - 3 years",
                    ifelse(as.numeric(ChildrenOldestAge_fillIn2) < 5, 
                           "3 - 5 years",
                           ifelse(as.numeric(ChildrenOldestAge_fillIn2) < 10, 
                                  "5 - 10 years",
                                  ifelse(as.numeric(ChildrenOldestAge_fillIn2) < 18, 
                                         "10 - 18 years",
                                         "> 18 years"))))),
    TRUE ~ "NA")) %>%
  mutate(ChildrenOldestAge_collapse = 
           factor(ChildrenOldestAge_collapse,
                  levels = c("My oldest child has not yet been born (I am/my partner is pregnant)",
                             "< 1 year",
                             "1 - 3 years",
                             "3 - 5 years",
                             "5 - 10 years",
                             "10 - 18 years",
                             "> 18 years",
                             "My oldest child is deceased",
                             "Prefer not to say"))) %>%
  mutate(ChildrenYoungestAge_collapse = case_when(
    ChildrenYoungestAge %in% c("My youngest child has not yet been born (I am/my partner is pregnant)", "My youngest child is deceased", "Prefer not to say") ~ ChildrenYoungestAge,
    ChildrenYoungestAge == "In months:" ~ 
      ifelse(as.numeric(ChildrenYoungestAge_fillIn1)/12 < 1,
             "< 1 year",
             ifelse(as.numeric(ChildrenYoungestAge_fillIn1)/12 < 3, 
                    "1 - 3 years",
                    ifelse(as.numeric(ChildrenYoungestAge_fillIn1)/12 < 5, 
                           "3 - 5 years",
                           ifelse(as.numeric(ChildrenYoungestAge_fillIn1)/12 < 10, 
                                  "5 - 10 years",
                                  ifelse(as.numeric(ChildrenYoungestAge_fillIn1)/12 < 18, 
                                         "10 - 18 years",
                                         "> 18 years"))))),
    ChildrenYoungestAge == "In years:" ~
      ifelse(as.numeric(ChildrenYoungestAge_fillIn2) < 1,
             "< 1 year",
             ifelse(as.numeric(ChildrenYoungestAge_fillIn2) < 3, 
                    "1 - 3 years",
                    ifelse(as.numeric(ChildrenYoungestAge_fillIn2) < 5, 
                           "3 - 5 years",
                           ifelse(as.numeric(ChildrenYoungestAge_fillIn2) < 10, 
                                  "5 - 10 years",
                                  ifelse(as.numeric(ChildrenYoungestAge_fillIn2) < 18, 
                                         "10 - 18 years",
                                         "> 18 years"))))),
    TRUE ~ "NA")) %>%
  mutate(ChildrenYoungestAge_collapse = 
           factor(ChildrenYoungestAge_collapse,
                  levels = c("My Youngest child has not yet been born (I am/my partner is pregnant)",
                             "< 1 year",
                             "1 - 3 years",
                             "3 - 5 years",
                             "5 - 10 years",
                             "10 - 18 years",
                             "> 18 years",
                             "My Youngest child is deceased",
                             "Prefer not to say")))  
# all assessments of ALL TARGETS, RepsonseId as rownames
d_all <- d %>% 
  select(ResponseId, starts_with("target"), 
         -c(contains("seventy"), contains("fifty"),
            contains("zero"), contains("ninety"), contains("please"))) %>%
  gather(question, response, -ResponseId) %>%
  mutate(target = gsub("_.*$", "", question),
         capacity = gsub("target..mo_", "", question),
         subid = paste(ResponseId, target, sep = "_")) %>%
  select(-ResponseId, -question, -target) %>%
  spread(capacity, response) %>%
  column_to_rownames("subid")
