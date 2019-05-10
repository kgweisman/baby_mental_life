# data preparation for baby mental life study 3
library(tidyverse)

# load in de-identified raw data
d0 <- read.csv("./data/deidentified/baby_mental_life_s3_data.csv") %>% 
  select(-X) %>%
  mutate(ResponseId = as.character(ResponseId))

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
                           "Q26" = "RaceEthnicity",
                           "Q26_10_TEXT" = "RaceEthnicity_fillIn",
                           "Q27" = "Education",
                           "Q28" = "Income",
                           "Q29" = "MaritalStatus",
                           "Q29_6_TEXT" = "MaritalStatus_fillIn",
                           "Q30" = "HouseholdSize",
                           "Q31" = "Parent",
                           "Q33" = "ChildrenNumber",
                           "Q34" = "ChildrenYoungestAge",
                           "Q34_1_TEXT" = "ChildrenYoungestAge_fillIn1",
                           "Q34_2_TEXT" = "ChildrenYoungestAge_fillIn2",
                           "Q35" = "ChildrenOldestAge",
                           "Q35_1_TEXT" = "ChildrenOldestAge_fillIn1",
                           "Q35_2_TEXT" = "ChildrenOldestAge_fillIn2",
                           "Q36" = "Attention",
                           "Q37" = "Comments",
                           .default = NA_character_),
         aspect = case_when(
           grepl("To what extent", question_text) ~ "cap_rating",
           grepl("In your opinion", question_text) &
             grepl("_TEXT", question_qualtrics) ~ "dev_factor_rating_free",
           grepl("In your opinion", question_text) ~ "dev_factor_rating",
           grepl("If you had to", question_text) & 
             grepl("_TEXT", question_qualtrics) ~ "dev_factor_choice_free",
           grepl("If you had to", question_text) ~ "dev_factor_choice",
           grepl("Are there any other", question_text) ~ "dev_factor_free",
           TRUE ~ NA_character_),
         capacity = case_when(
           grepl("hungry", question_text) ~ "getting_hungry",
           grepl("controlling", question_text) ~ "controlling_their_emotions",
           grepl("distressed", question_text) ~ "feeling_distressed",
           grepl("happy", question_text) ~ "feeling_happy",
           grepl("helpless", question_text) ~ "feeling_helpless",
           grepl("pain", question_text) ~ "feeling_pain",
           grepl("learning", question_text) ~ "learning_from_other_people",
           grepl("reasoning", question_text) ~ "reasoning_about_things",
           TRUE ~ NA_character_),
         target = case_when(
           grepl("5-year-old", question_text) ~ "target60mo",
           grepl("4-year-old", question_text) ~ "target48mo",
           grepl("3-year-old", question_text) ~ "target36mo",
           grepl("2-year-old", question_text) ~ "target24mo",
           grepl("18-month-old", question_text) ~ "target18mo",
           grepl("12-month-old", question_text) ~ "target12mo",
           grepl("9-month-old", question_text) ~ "target09mo",
           grepl("6-month-old", question_text) ~ "target06mo",
           grepl("4-month-old", question_text) ~ "target04mo",
           grepl("3-month-old", question_text) ~ "target03mo",
           grepl("2-month-old", question_text) ~ "target02mo",
           grepl("1-month-old", question_text) ~ "target01mo",
           grepl("4-day-old", question_text) ~ "target0Xmo",
           grepl("newborn", question_text) ~ "target00mo",
           TRUE ~ NA_character_),
         dev_factor = case_when(
           grepl("To what extent", question_text) ~ NA_character_,
           grepl("other factors", question_text) ~ "other",
           grepl("_1$", question_qualtrics) ~ "preprogrammed",
           grepl("_2$", question_qualtrics) ~ "womb_experiences",
           grepl("_3$", question_qualtrics) ~ "body_grows",
           grepl("_4$", question_qualtrics) ~ "brain_changes",
           grepl("_5$", question_qualtrics) ~ "senses_improve",
           grepl("_6$", question_qualtrics) ~ "observes_objects",
           grepl("_7$", question_qualtrics) ~ "experiments",
           grepl("_8$", question_qualtrics) ~ "observes_people",
           grepl("_9$", question_qualtrics) ~ "interacts_people",
           grepl("_10$", question_qualtrics) ~ "people_teach",
           grepl("_11$", question_qualtrics) ~ "CHECK",
           grepl("had to choose", question_text) ~ "most_important",
           # grepl("how much of a role", question_text) ~ "XX",
           TRUE ~ NA_character_)) %>%
  mutate_at(vars(question, aspect, target, dev_factor),
            funs(gsub(" ", "_", .))) %>%
  mutate_at(vars(question, aspect, target, dev_factor),
            funs(gsub("^_", "", .))) %>%
  mutate_at(vars(question, aspect, target, dev_factor),
            funs(gsub("_$", "", .))) %>%
  unite(col = NEW,
        aspect, target, dev_factor, capacity,
        sep = "__", remove = F) %>%
  mutate(NEW = gsub("__NA", "", NEW),
         NEW = gsub("NA__", "", NEW),
         NEW = gsub("NA", "", NEW),
         NEW = ifelse(NEW == "", NA_character_, NEW),
         question = ifelse(!is.na(question), question,
                           ifelse(!is.na(NEW), NEW,
                                  question_qualtrics))) %>%
  select(-NEW)

# rename questions
d1 <- d0  %>%
  # get rid of extra info in first two rows  
  filter(!is.na(as.numeric(as.character(Q2)))) %>% 
  gather(question_qualtrics, response, -c(ResponseId, duplicateGPS)) %>%
  left_join(question_key %>% select(question_qualtrics, question)) %>%
  select(-question_qualtrics) %>%
  spread(question, response)

# determine correct response to embedded attention check questions
attn_embed_key <- d1 %>% 
  select(ResponseId, contains("CHECK")) %>% 
  gather(question, response, -ResponseId) %>%
  count(question, response) %>%
  group_by(question) %>%
  top_n(1, n) %>%
  ungroup() %>%
  select(-n) %>%
  rename(correct = response)

# count how many embedded attention check questions passed
d_attn_embed <- d1 %>%
  select(ResponseId, contains("CHECK")) %>%
  gather(question, response, -ResponseId) %>%
  left_join(attn_embed_key) %>%
  mutate(pass = (response == correct)) %>%
  count(ResponseId, pass) %>%
  filter(pass == T) %>%
  select(-pass) %>%
  # must pass >= 7 of 8 embedded attention checks (i.e., fail < 2)
  mutate(pass_embed = ifelse(n >= 7, T, F)) %>%
  filter(pass_embed == T)

# retrieve coding of free response attention/comprehension check
d_attn_free <- readxl::read_excel("./data/attention check only/attention_check_coding_all.xlsx") %>%
  filter(MATCH_simple == "3 matches") %>%
  select(ResponseId, Q36_coded_KW_simple) %>%
  rename(attn_free_coded = Q36_coded_KW_simple) %>%
  filter(attn_free_coded %in% c("pass", "fail")) %>%
  full_join(read.csv("./data/attention check only/attention_check_coding_consensus.csv") %>%
    select(ResponseId, MATCH_FINAL_CONSENSUS) %>%
      rename(attn_free_coded = MATCH_FINAL_CONSENSUS)) %>%
  mutate(ResponseId = as.character(ResponseId))

# check for discrepencies
# d_attn_free %>% count(ResponseId) %>% filter(n > 1)
  
# implement inclusion/exclusion criteria
d2 <- d1 %>%
  mutate(ResponseId = as.character(ResponseId)) %>%
  left_join(d_attn_embed %>% select(ResponseId, pass_embed)) %>%
  left_join(d_attn_free) %>%
  filter(Age >= 18, Age <= 45,
         EnglishProf %in% c("Advanced", "Superior"),
         pass_embed == T,
         attn_free_coded == "pass")

# recode variables & drop extraneous variables
d3 <- d2 %>%
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
d <- d3 %>% distinct()

# remove people with another identical set of GPS coordinates among people who passed attention checks
d_nodup <- d %>%
  filter(duplicateGPS == F) %>%
  select(-duplicateGPS)
# NOTE: this is NOT treated as our primary dataset here

# demographic information
d_demo <- d %>% 
  select(ResponseId, Duration,
         Age, starts_with("GenderSex"), starts_with("RaceEthnicity"),
         starts_with("FirstLang"),
         Education, Income, HouseholdSize,
         starts_with("MaritalStatus"),
         Parent, starts_with("Children"), 
         Comments) %>%
  mutate_at(vars(contains("_fillIn")), funs(as.character)) %>%
  mutate_at(vars(contains("_fillIn1"), contains("_filIn2")), 
            funs(as.numeric)) %>%
  mutate(RaceEthnicity_collapse = ifelse(grepl(",([A-Za-z])", RaceEthnicity),
                                         "Multiple", RaceEthnicity)) %>%
  mutate(ChildrenYoungestAge_collapse = case_when(
    ChildrenYoungestAge %in% c(
      "My youngest child has not yet been born (I am/my partner is pregnant)", 
      "My youngest child is deceased", 
      "Prefer not to say") ~ ChildrenYoungestAge,
    is.na(ChildrenYoungestAge) | ChildrenYoungestAge == "" ~ NA_character_,
    grepl("In months", ChildrenYoungestAge) ~
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
    grepl("In years", ChildrenYoungestAge) ~
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
    TRUE ~ NA_character_)) %>%
  mutate(ChildrenOldestAge_collapse = case_when(
    ChildrenOldestAge %in% c(
      "My oldest child has not yet been born (I am/my partner is pregnant)", 
      "My oldest child is deceased", 
      "Prefer not to say") ~ ChildrenOldestAge,
    grepl("I only have one child", ChildrenOldestAge) ~ ChildrenYoungestAge_collapse,
    is.na(ChildrenOldestAge) | ChildrenOldestAge == "" ~ NA_character_,
    # grepl("In months", ChildrenOldestAge) ~
    #   ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 1,
    #          "< 1 year",
    #          ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 3,
    #                 "1 - 3 years",
    #                 ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 5,
    #                        "3 - 5 years",
    #                        ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 10,
    #                               "5 - 10 years",
    #                               ifelse(as.numeric(ChildrenOldestAge_fillIn1)/12 < 18,
    #                                      "10 - 18 years",
    #                                      "> 18 years"))))),
    grepl("In years", ChildrenOldestAge) ~
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
    TRUE ~ NA_character_)) %>%
  mutate_at(vars(ChildrenYoungestAge_collapse, ChildrenOldestAge_collapse),
            funs(factor(gsub(".*deceased", "Deceased", .),
                        levels = c("< 1 year", 
                                   "1 - 3 years", 
                                   "3 - 5 years",
                                   "5 - 10 years", 
                                   "10 - 18 years", 
                                   "> 18 years", 
                                   "Deceased",
                                   "Prefer not to say"))))

# capacity key
capacities_s3 <- data.frame(capacity = c("controlling_their_emotions",
                                         "reasoning_about_things",
                                         "getting_hungry", 
                                         "feeling_pain",
                                         "feeling_happy", 
                                         "learning_from_other_people",
                                         "feeling_distressed", 
                                         "feeling_helpless"),
                            domain = c(rep("COG", 2),
                                       rep("BOD", 2),
                                       rep("POS", 2),
                                       rep("NEG", 2))) %>%
  mutate(domain = factor(domain))

# capacity ratings for ALL TARGETS, ALL CAPACITIES
d_cap_rating <- d %>% 
  select(ResponseId, starts_with("cap_rating"), -contains("CHECK")) %>%
  gather(question, response, -ResponseId) %>%
  mutate(question = gsub("cap_rating__", "", question)) %>%
  mutate(target = gsub("__.*$", "", question),
         capacity = gsub("target..mo__", "", question)) %>%
  select(ResponseId, target, capacity, response) %>%
  mutate(response = as.numeric(response),
         target_num = recode(target,
                             "target00mo" = 0,
                             "target0Xmo" = round(4/30, 3),
                             "target01mo" = 1,
                             "target02mo" = 2,
                             "target04mo" = 4,
                             "target06mo" = 6,
                             "target09mo" = 9,
                             "target12mo" = 12,
                             "target18mo" = 18,
                             "target24mo" = 24,
                             "target36mo" = 36,
                             "target48mo" = 48,
                             "target60mo" = 60),
         target_ord = recode_factor(target,
                                    "target00mo" = "newborns",
                                    "target0Xmo" = "4-day-olds",
                                    "target01mo" = "1-month-olds",
                                    "target02mo" = "2-month-olds",
                                    "target04mo" = "4-month-olds",
                                    "target06mo" = "6-month-olds",
                                    "target09mo" = "9-month-olds",
                                    "target12mo" = "12-month-olds",
                                    "target18mo" = "18-month-olds",
                                    "target24mo" = "2-year-olds",
                                    "target36mo" = "3-year-olds",
                                    "target48mo" = "4-year-olds",
                                    "target60mo" = "5-year-olds")) %>%
  left_join(capacities_s3) %>%
  select(ResponseId, target, target_num, target_ord, 
         domain, capacity, response)

# dev_factor ratings for ALL TARGETS, ALL CAPACITIES
d_dev_factor_rating <- d %>% 
  select(ResponseId, starts_with("dev_factor_rating"), -contains("CHECK")) %>%
  gather(question, response, -ResponseId) %>%
  mutate(question = gsub("dev_factor_rating__", "", question)) %>%
  mutate(dev_factor = gsub("__.*$", "", question),
         capacity = gsub("^.*__", "", question)) %>%
  left_join(capacities_s3) %>%
  select(ResponseId, dev_factor, domain, capacity, response) %>%
  mutate(response = as.numeric(response),
         dev_factor = factor(dev_factor,
                             levels = c("preprogrammed", 
                                        "womb_experiences",
                                        "body_grows",
                                        "brain_changes",
                                        "senses_improve", 
                                        "observes_objects",
                                        "experiments",
                                        "observes_people",
                                        "interacts_people",
                                        "people_teach")))

# dev_factor "most important" for ALL TARGETS, ALL CAPACITIES
d_dev_factor_most_important <- d %>% 
  select(ResponseId, starts_with("dev_factor_choice"), -contains("CHECK")) %>%
  gather(question, response, -ResponseId) %>%
  # filter(!is.na(response), response != "") %>%
  mutate(question = gsub("dev_factor_choice__", "", question)) %>%
  mutate(dev_factor = gsub("__.*$", "", question),
         capacity = gsub("^.*__", "", question)) %>%
  left_join(capacities_s3) %>%
  select(ResponseId, dev_factor, domain, capacity, question, response) %>%
  mutate(dev_factor = factor(dev_factor,
                             levels = c("preprogrammed", 
                                        "womb_experiences",
                                        "body_grows",
                                        "brain_changes",
                                        "senses_improve", 
                                        "observes_objects",
                                        "observes_people",
                                        "experiments",
                                        "interacts_people",
                                        "people_teach")))

d_dev_factor_most_important_choice <- d_dev_factor_most_important %>%
  filter(!is.na(response), response != "", !grepl("free", question)) %>%
  select(-dev_factor, -question)

# # check for duplicates/redundancies
# d_dev_factor_most_important_choice %>%
#   count(ResponseId) %>%
#   filter(n != 8)

d_dev_factor_most_important_free <- d_dev_factor_most_important %>%
  filter(!is.na(response), response != "", grepl("free", question)) %>%
  select(-dev_factor, -question)

rm(d_dev_factor_most_important)

# dev factor "other" (optional) responses for ALL TARGETS, ALL CAPACITIES
d_dev_factor_other <- d %>% 
  select(ResponseId, starts_with("dev_factor_free"), -contains("CHECK")) %>%
  gather(question, response, -ResponseId) %>%
  mutate(question = gsub("dev_factor_free__", "", question)) %>%
  mutate(target = gsub("__.*$", "", question),
         capacity = gsub("other__", "", question)) %>%
  left_join(capacities_s3) %>%
  select(ResponseId, target, domain, capacity, response) %>%
  mutate(response = gsub(" $", "", response),
         response = gsub("\\.$", "", response),
         response = gsub("\\!$", "", response)) %>%
  filter(!is.na(response), response != "", response != " ",
         tolower(response) != "n/a", tolower(response) != "na",
         tolower(response) != "no", 
         tolower(response) != "none",
         tolower(response) != "nope", 
         tolower(response) != "no idea",
         tolower(response) != "cant think of any", 
         tolower(response) != "no nothing else",
         tolower(response) != "not sure", 
         tolower(response) != "note sure",
         tolower(response) != "don't know",
         tolower(response) != "i don't know",
         tolower(response) != "not that i can think of",
         tolower(response) != "none i can think of",
         tolower(response) != "none that i can think of",
         tolower(response) != "none i can think of at this time",
         tolower(response) != "none that i know of",
         tolower(response) != "nothing i can think of",
         tolower(response) != "not that were not listed",
         tolower(response) != "nothing comes to mind",
         !grepl("nothing to add", tolower(response)),
         !grepl("can't think of any", tolower(response)),
         !grepl("don't think so", tolower(response)),
         !grepl("don\\;t think so", tolower(response)))

# remove extra dataframes
rm(attn_embed_key, d_attn_embed, d_attn_free, d0, d1, d2, d3)
