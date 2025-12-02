# import libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmerTest)
library(emmeans)
library(gtsummary)
library(gt)
library(patchwork)
library(glmnet)
library(corrplot)
library(ggcorrplot)
library(readr)

# import data 
df <- read_csv("Shuen_20250909.csv")

# data cleaning
cleaned_df <- df

# remove rows with NA values 
cleaned_df <- cleaned_df[!is.na(cleaned_df$pasc90_180_status_participant), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$pasc181_365_status_participant), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$pasc366_548_status_participant), ]

cleaned_df <- cleaned_df[!is.na(cleaned_df$a_sex), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$age_bracket), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$a_race), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$a_ethnicity), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$a_income), ]
cleaned_df <- cleaned_df[!is.na(cleaned_df$a_education), ]

cleaned_df <-cleaned_df %>%
  filter(a_sex != 99 & a_race != 99 & a_education != 99 & a_income != 99 & a_income!= 88 & a_ethnicity !=99)

# cleaned data to use
new_df <- cleaned_df  %>%
  mutate(pascever = case_when(
    pasc90_180_status_participant == 1 ~ 1,
    pasc90_180_status_participant == 0 ~ 0,
    TRUE ~ NA)) %>%
  filter(!is.na(pascever))

table(new_df$pascever)

anyDuplicated(new_df$record_id)

# data pre-processing
# WEMWBS baseline (a_men)
wemwbs_baseline <- new_df %>%
  select(starts_with("a_men_")) %>%
  names()

new_df <- new_df %>%
  rowwise() %>%
  mutate(
    wemwbs_baseline_answered = sum(!is.na(c_across(all_of(wemwbs_baseline)))),  
    wemwbs_baseline_score = if (wemwbs_baseline_answered == 14) {
      sum(c_across(all_of(wemwbs_baseline)), na.rm = TRUE) 
    } else {
      NA_real_  
    }
  ) %>%
  ungroup()

# data to use
new_df <- new_df %>%
  rowwise() %>%
  filter(!is.na(wemwbs_baseline_score)) %>%
  ungroup()

# WEMWBS 6 months followup i.e. 181 days (af_men)
wemwbs_followup <- new_df %>%
  select(starts_with("af_men_")) %>%
  names()

new_df <- new_df %>%
  rowwise() %>%
  mutate(
    wemwbs_followup_answered = sum(!is.na(c_across(all_of(wemwbs_followup)))),  
    wemwbs_followup_score = if (wemwbs_followup_answered == 14) {
      sum(c_across(all_of(wemwbs_followup)), na.rm = TRUE)
    } else {
      NA_real_  
    }
  ) %>%
  ungroup()

# WEMWBS 12 months followup i.e. 366 days (af12_men)
wemwbs_12 <- new_df %>%
  select(starts_with("af12_men_")) %>%
  names()

new_df <- new_df %>%
  rowwise() %>%
  mutate(
    wemwbs_12_answered = sum(!is.na(c_across(all_of(wemwbs_12)))),  
    wemwbs_12_score = if (wemwbs_12_answered == 14) {
      sum(c_across(all_of(wemwbs_12)), na.rm = TRUE)
    } else {
      NA_real_  
    }
  ) %>%
  ungroup()

# WEMWBS 18 months followup i.e. 548 days (af18_men)
wemwbs_18 <- new_df %>%
  select(starts_with("af18_men_")) %>%
  names()

new_df <- new_df %>%
  rowwise() %>%
  mutate(
    wemwbs_18_answered = sum(!is.na(c_across(all_of(wemwbs_18)))),  
    wemwbs_18_score = if (wemwbs_18_answered == 14) {
      sum(c_across(all_of(wemwbs_18)), na.rm = TRUE)
    } else {
      NA_real_  
    }
  ) %>%
  ungroup()

# WEMWBS standardised score (sums up wemwbs_baseline + wemwbs_followup + wemwbs_12 + wemwbs_18)
wemwbs_standardised <- new_df %>%
  select(starts_with("a_men_")) %>%
  select(starts_with("af_men_")) %>%
  select(starts_with("af12_men_")) %>%
  select(starts_with("af18_men_")) %>%
  names()

new_df <- new_df %>%
  rowwise() %>%
  mutate(
    wemwbs_standardised = sum(c_across(c(wemwbs_baseline_score, wemwbs_followup_score,
                                         wemwbs_12_score, wemwbs_18_score)),
                              na.rm = TRUE)
  ) %>%
  ungroup()

# select columns we want to use - demographics, PASC status, WEMWBS scores
new_df <- new_df %>%
  select(record_id,
         age_bracket,
         a_sex,
         a_race,
         a_ethnicity,
         a_income,
         a_education,
         pasc90_180_status_participant,
         pasc181_365_status_participant,
         pasc366_548_status_participant,
         wemwbs_baseline_score, 
         wemwbs_followup_score, 
         wemwbs_12_score, 
         wemwbs_18_score,
         wemwbs_standardised)

# re-coded accordingly
recoded_new_df <- new_df %>%
  filter(a_sex %in% c(1, 2)) %>%
  mutate(a_sex = recode(a_sex,
                        `1` = "Male",
                        `2` = "Female")) %>%
  mutate(age_bracket = recode(age_bracket,
                              `1` = "0 - 17 years",
                              `2` = "18 - 24 years",
                              `3` = "25 - 34 years",
                              `4` = "35 - 44 years",
                              `5` = "45 - 54 years",
                              `6` = "55 - 64 years",
                              `7` = "65 - 74 years",
                              `8` = "75 - 84 years",
                              `9` = "85+ years")) %>%
  mutate(a_race = recode(a_race,
                         `1` = "Asian / Asian American",
                         `2` = "Black / African American",
                         `3` = "Native Hawaiian / Pacific Islander",
                         `4` = "American Indian / Alaska Native",
                         `5` = "White / Caucasian", 
                         `6` = "Other race",
                         `99` = "Prefer not to answer")) %>%
  mutate(a_ethnicity = recode(a_ethnicity,
                              `1` = "Hispanic",
                              `2` = "Non-Hispanic", 
                              `99` = "Prefer not to answer")) %>%
  mutate(a_income = recode(a_income,
                           `1` = "$0 to $24,999",
                           `2` = "$25,000 to $49,999",
                           `3` = "$50,000 to $74,999",
                           `4` = "$75,000 to $99,999",
                           `5` = "$100,000 to $149,999",
                           `6` = "$150,000 or more",
                           `88` = "Don't know / Not sure",
                           `99` = "Prefer not to answer")) %>%
  mutate(a_education = recode(a_education,
                              `0` = "Never attended school / only kindergarten",
                              `1` = "Grades 1 - 8",
                              `2` = "Grades 9 - 11",
                              `3` = "High school graduate",
                              `4` = "College, 1-3 years",
                              `5` = "College, 4 years or more", 
                              `6` = "Post-grad / professional degree",
                              `99` = "Prefer not to answer"))

# create 90-180 days dataframe where PASC Status = 0 / 1
pasc_90_180_days <- recoded_new_df %>%
  filter(pasc90_180_status_participant %in% c(1, 0)) %>% # filter for 1 & 0 
  mutate(pasc90_180_status_participant = recode(pasc90_180_status_participant,
                                                `1` = "PASC90+",
                                                `0` = "PASC90-"))

# create 181-365 days dataframe where PASC Status = 0 / 1
pasc_181_365_days <- recoded_new_df %>%
  filter(pasc181_365_status_participant %in% c(1, 0)) %>% # filter for 1 & 0 
  mutate(pasc181_365_status_participant = recode(pasc181_365_status_participant,
                                                 `1` = "PASC181+",
                                                 `0` = "PASC181-"))

# create 366-548 days dataframe where PASC Status = 0 / 1
pasc_366_548_days <- recoded_new_df %>%
  filter(pasc366_548_status_participant %in% c(1, 0)) %>% # filter for 1 & 0 
  mutate(pasc366_548_status_participant = recode(pasc366_548_status_participant,
                                                 `1` = "PASC366+",
                                                 `0` = "PASC366-"))

# PASC+ Participants
pasc_positive <- recoded_new_df %>%
  filter(pasc90_180_status_participant == '1') %>%
  filter(pasc181_365_status_participant == '1') %>%
  filter(pasc366_548_status_participant == '1')

# PASC- Participants
pasc_negative <- recoded_new_df %>%
  filter(pasc90_180_status_participant == '0') %>%
  filter(pasc181_365_status_participant == '0') %>%
  filter(pasc366_548_status_participant == '0')

# check for duplicates (return 0 if no duplicates)
anyDuplicated(new_df$record_id)
anyDuplicated(recoded_new_df$record_id)
anyDuplicated(pasc_90_180_days$record_id)
anyDuplicated(pasc_181_365_days$record_id)
anyDuplicated(pasc_366_548_days$record_id)
anyDuplicated(pasc_positive$record_id)
anyDuplicated(pasc_negative$record_id)

# export cleaned data
write.csv(new_df, "new_Shuen_20250909.csv", row.names = FALSE)

# WEMWBS scores over time from 90, 181, 366, 548 days (for all participants)
wemwbs_scores_over_time <- new_df %>%
  select(record_id,
         wemwbs_baseline_score, 
         wemwbs_followup_score, 
         wemwbs_12_score, 
         wemwbs_18_score) %>%
  pivot_longer(
    cols = starts_with("wemwbs"),
    names_to = "timepoint", 
    values_to = "score"
  ) %>%
  mutate(day = case_when(
    timepoint == "wemwbs_baseline_score" ~ 90,     
    timepoint == "wemwbs_followup_score" ~ 181,
    timepoint == "wemwbs_12_score" ~ 366,
    timepoint == "wemwbs_18_score" ~ 548,
    TRUE ~ NA_real_))

# WEMWBS scores over time from 90, 181, 366, 548 days (for PASC+ participants)
pasc_positive_wemwbs_scores_over_time <- pasc_positive %>%
  select(record_id,
         wemwbs_baseline_score, 
         wemwbs_followup_score, 
         wemwbs_12_score, 
         wemwbs_18_score) %>%
  pivot_longer(
    cols = starts_with("wemwbs"),
    names_to = "timepoint", 
    values_to = "score"
  ) %>%
  mutate(day = case_when(
    timepoint == "wemwbs_baseline_score" ~ 90,     
    timepoint == "wemwbs_followup_score" ~ 181,
    timepoint == "wemwbs_12_score" ~ 366,
    timepoint == "wemwbs_18_score" ~ 548,
    TRUE ~ NA_real_))

# WEMWBS scores over time from 90, 181, 366, 548 days (for PASC- participants)
pasc_negative_wemwbs_scores_over_time <- pasc_negative %>%
  select(record_id,
         wemwbs_baseline_score, 
         wemwbs_followup_score, 
         wemwbs_12_score, 
         wemwbs_18_score) %>%
  pivot_longer(
    cols = starts_with("wemwbs"),
    names_to = "timepoint", 
    values_to = "score"
  ) %>%
  mutate(day = case_when(
    timepoint == "wemwbs_baseline_score" ~ 90,
    timepoint == "wemwbs_followup_score" ~ 181,
    timepoint == "wemwbs_12_score" ~ 366,
    timepoint == "wemwbs_18_score" ~ 548,
    TRUE ~ NA_real_))


# exploratory data analysis
# summary statistics (demographics stratified by PASC status for 90-180 days) 
table(pasc_90_180_days$age_bracket, pasc_90_180_days$pasc90_180_status_participant)
table(pasc_90_180_days$a_sex, pasc_90_180_days$pasc90_180_status_participant)
table(pasc_90_180_days$a_race, pasc_90_180_days$pasc90_180_status_participant)
table(pasc_90_180_days$a_ethnicity, pasc_90_180_days$pasc90_180_status_participant)
table(pasc_90_180_days$a_income, pasc_90_180_days$pasc90_180_status_participant)
table(pasc_90_180_days$a_education, pasc_90_180_days$pasc90_180_status_participant)

# summary statistics (demographics stratified by PASC status for 181-365 days)
table(pasc_181_365_days$age_bracket, pasc_181_365_days$pasc181_365_status_participant)
table(pasc_181_365_days$a_sex, pasc_181_365_days$pasc181_365_status_participant)
table(pasc_181_365_days$a_race, pasc_181_365_days$pasc181_365_status_participant)
table(pasc_181_365_days$a_ethnicity, pasc_181_365_days$pasc181_365_status_participant)
table(pasc_181_365_days$a_income, pasc_181_365_days$pasc181_365_status_participant)
table(pasc_181_365_days$a_education, pasc_181_365_days$pasc181_365_status_participant)

# summary statistics (demographics stratified by PASC status for 366-548 days)
table(pasc_366_548_days$age_bracket, pasc_366_548_days$pasc366_548_status_participant)
table(pasc_366_548_days$a_sex, pasc_366_548_days$pasc366_548_status_participant)
table(pasc_366_548_days$a_race, pasc_366_548_days$pasc366_548_status_participant)
table(pasc_366_548_days$a_ethnicity, pasc_366_548_days$pasc366_548_status_participant)
table(pasc_366_548_days$a_income, pasc_366_548_days$pasc366_548_status_participant)
table(pasc_366_548_days$a_education, pasc_366_548_days$pasc366_548_status_participant)

# summary statistics (WEMWBS Baseline stratified by PASC status for 90-180 days)
pasc_90_180_days %>%
  group_by(pasc90_180_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_baseline_score, na.rm = TRUE),
    sd   = sd(wemwbs_baseline_score, na.rm = TRUE),
    median = median(wemwbs_baseline_score, na.rm = TRUE),
    min = min(wemwbs_baseline_score, na.rm = TRUE),
    max = max(wemwbs_baseline_score, na.rm = TRUE)
  )

# summary statistics (WEMWBS Followup stratified by PASC status for 90-180 days) 
pasc_90_180_days %>%
  group_by(pasc90_180_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_followup_score, na.rm = TRUE),
    sd   = sd(wemwbs_followup_score, na.rm = TRUE),
    median = median(wemwbs_followup_score, na.rm = TRUE),
    min = min(wemwbs_followup_score, na.rm = TRUE),
    max = max(wemwbs_followup_score, na.rm = TRUE)
  )

# summary statistics (WEMWBS Baseline stratified by PASC status for 181-365 days)
pasc_181_365_days %>%
  group_by(pasc181_365_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_baseline_score, na.rm = TRUE),
    sd   = sd(wemwbs_baseline_score, na.rm = TRUE),
    median = median(wemwbs_baseline_score, na.rm = TRUE),
    min = min(wemwbs_baseline_score, na.rm = TRUE),
    max = max(wemwbs_baseline_score, na.rm = TRUE)
  )

# summary statistics (WEMWBS12 stratified by PASC status for 181-365 days)
pasc_181_365_days %>%
  group_by(pasc181_365_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_12_score, na.rm = TRUE),
    sd   = sd(wemwbs_12_score, na.rm = TRUE),
    median = median(wemwbs_12_score, na.rm = TRUE),
    min = min(wemwbs_12_score, na.rm = TRUE),
    max = max(wemwbs_12_score, na.rm = TRUE)
  )

# summary statistics (WEMWBS Baseline stratified by PASC status for 366-548 days) 
pasc_366_548_days %>%
  group_by(pasc366_548_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_baseline_score, na.rm = TRUE),
    sd   = sd(wemwbs_baseline_score, na.rm = TRUE),
    median = median(wemwbs_baseline_score, na.rm = TRUE),
    min = min(wemwbs_baseline_score, na.rm = TRUE),
    max = max(wemwbs_baseline_score, na.rm = TRUE)
  )

# summary statistics (WEMWBS18 stratified by PASC status for 366-548 days) 
pasc_366_548_days %>%
  group_by(pasc366_548_status_participant) %>%
  summarise(
    n = n(),
    mean = mean(wemwbs_18_score, na.rm = TRUE),
    sd   = sd(wemwbs_18_score, na.rm = TRUE),
    median = median(wemwbs_18_score, na.rm = TRUE),
    min = min(wemwbs_18_score, na.rm = TRUE),
    max = max(wemwbs_18_score, na.rm = TRUE)
  )

# data visualizations
# distribution of PASC status across demographics for 90-180 days (sex)
ggplot(pasc_90_180_days, aes(x = a_sex, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Gender") +
  theme_minimal()

# distribution of PASC status across demographics for 90-180 days (age)
ggplot(pasc_90_180_days, aes(x = age_bracket, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Age Groups") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 90-180 days (race)
ggplot(pasc_90_180_days, aes(x = a_race, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Race", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Race") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 90-180 days (ethnicity)
ggplot(pasc_90_180_days, aes(x = a_ethnicity, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Ethnicity", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Ethnicity") +
  theme_minimal()

# distribution of PASC status across demographics for 90-180 days (income)
ggplot(pasc_90_180_days, aes(x = a_income, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Income", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Income") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 90-180 days (education)
ggplot(pasc_90_180_days, aes(x = a_education, fill = pasc90_180_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Education", y = "Count", fill = "PASC90 Status",
       title = "Distribution of PASC90 Status across Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 90-180 days (WEMWBS baseline)
ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_baseline_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC90 Status", y = "WEMWBS Baseline Score", title = "Boxplot of WEMWBS Baseline Score by PASC90 Status")

ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_baseline_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC90 Status", y = "WEMWBS Baseline Score", title = "Violin + Boxplot of WEMWBS Baseline Score by PASC90 Status")

# distribution of PASC status across demographics for 90-180 days (WEMWBS followup)
ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_followup_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC90 Status", y = "WEMWBS Followup Score", title = "Boxplot of WEMWBS Followup Score by PASC90 Status")

ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_followup_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC90 Status", y = "WEMWBS Followup Score", title = "Violin + Boxplot of WEMWBS Followup Score by PASC90 Status")


# distribution of PASC status across demographics for 181-365 days (sex)
ggplot(pasc_181_365_days, aes(x = a_sex, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Gender") +
  theme_minimal()

# distribution of PASC status across demographics for 181-365 days (age)
ggplot(pasc_181_365_days, aes(x = age_bracket, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Age Groups") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 181-365 days (race)
ggplot(pasc_181_365_days, aes(x = a_race, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Race", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Race") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 181-365 days (ethnicity)
ggplot(pasc_181_365_days, aes(x = a_ethnicity, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Ethnicity", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Ethnicity") +
  theme_minimal()

# distribution of PASC status across demographics for 181-365 days (income)
ggplot(pasc_181_365_days, aes(x = a_income, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Income", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Income") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 181-365 days (education)
ggplot(pasc_181_365_days, aes(x = a_education, fill = pasc181_365_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Education", y = "Count", fill = "PASC181 Status",
       title = "Distribution of PASC181 Status across Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 181-365 days (WEMWBS baseline)
ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_baseline_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC181 Status", y = "WEMWBS Baseline Score", title = "Boxplot of WEMWBS Baseline Score by PASC181 Status")

ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_baseline_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC181 Status", y = "WEMWBS Baseline Score", title = "Violin + Boxplot of WEMWBS Baseline Score by PASC181 Status")

# distribution of PASC status across demographics for 181-365 days (WEMWBS12)
ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_12_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC181 Status", y = "WEMWBS12 Score", title = "Boxplot of WEMWBS12 Score by PASC181 Status")

ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_12_score, fill = pasc181_365_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC181 Status", y = "WEMWBS12 Score", title = "Violin + Boxplot of WEMWBS12 Score by PASC181 Status")


# distribution of PASC status across demographics for 366-548 days (sex)
ggplot(pasc_366_548_days, aes(x = a_sex, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Gender") +
  theme_minimal()

# distribution of PASC status across demographics for 366-548 days (age)
ggplot(pasc_366_548_days, aes(x = age_bracket, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Age Groups") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 366-548 days (race)
ggplot(pasc_366_548_days, aes(x = a_race, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Race", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Race") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 366-548 days (ethnicity)
ggplot(pasc_366_548_days, aes(x = a_ethnicity, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Ethnicity", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Ethnicity") +
  theme_minimal()

# distribution of PASC status across demographics for 366-548 days (income)
ggplot(pasc_366_548_days, aes(x = a_income, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Income", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Income") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 366-548 days (education)
ggplot(pasc_366_548_days, aes(x = a_education, fill = pasc366_548_status_participant)) +
  geom_bar(position = "dodge") +
  labs(x = "Education", y = "Count", fill = "PASC366 Status",
       title = "Distribution of PASC366 Status across Education") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) 

# distribution of PASC status across demographics for 366-548 days (WEMWBS baseline)
ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_baseline_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC366 Status", y = "WEMWBS Baseline Score", title = "Boxplot of WEMWBS Baseline Score by PASC366 Status")

ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_baseline_score, fill = pasc366_548_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC366 Status", y = "WEMWBS Baseline Score", title = "Violin + Boxplot of WEMWBS Baseline Score by PASC366 Status")


# distribution of PASC status across demographics for 366-548 days (WEMWBS18)
ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_18_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "PASC366 Status", y = "WEMWBS18 Score", title = "Boxplot of WEMWBS18 Score by PASC366 Status")

ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_18_score, fill = pasc366_548_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC366 Status", y = "WEMWBS18 Score", title = "Violin + Boxplot of WEMWBS18 Score by PASC366 Status")

# WEMWBS scores over time for all participants
ggplot(wemwbs_scores_over_time, aes(x = day, y = score, group = record_id)) +
  geom_line(alpha = 0.3, color = "blue") +
  geom_point(alpha = 0.5, color = "blue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +  
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black") +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(title = "wemwbs_scores of all participants over time",
       x = "Days", y = "wemwbs_scores") +
  theme_minimal()

ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = day, y = score)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +
  stat_summary(fun = mean, geom = "point", color = "lightblue", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 20) +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(x = "Days", y = "wemwbs_scores",
       title = "wemwbs_scores of all participants") +
  theme_minimal()

ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = factor(day), y = score)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Days", y = "wemwbs_scores",
       title = "Distribution of wemwbs_scores of all participants over time") +
  theme_minimal()

# WEMWBS scores over time for PASC+ participants
ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = day, y = score, group = record_id)) +
  geom_line(alpha = 0.3, color = "blue") +
  geom_point(alpha = 0.5, color = "blue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +  
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black") +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(title = "wemwbs_scores of PASC+ Participants over time",
       x = "Days", y = "wemwbs_scores") +
  theme_minimal()

ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = day, y = score)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +
  stat_summary(fun = mean, geom = "point", color = "lightblue", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 20) +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(x = "Days", y = "wemwbs_scores",
       title = "wemwbs_scores of PASC+ Participants") +
  theme_minimal()

ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = factor(day), y = score)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Days", y = "wemwbs_scores",
       title = "Distribution of wemwbs_scores of PASC+ Participants over time") +
  theme_minimal()

# WEMWBS scores over time for PASC- participants
ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = day, y = score, group = record_id)) +
  geom_line(alpha = 0.3, color = "darkgrey") +
  geom_point(alpha = 0.5, color = "darkgrey") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1.2) +  
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black") +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(title = "Average wemwbs_scores of PASC- Participants over time",
       x = "Days", y = "wemwbs_scores") +
  theme_minimal()

ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = day, y = score)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black") +
  stat_summary(fun = mean, geom = "point", color = "darkgrey", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 20) +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(x = "Days", y = "wemwbs_scores",
       title = "wemwbs_scores of PASC- Participants") +
  theme_minimal()

ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = factor(day), y = score)) +
  geom_boxplot(fill = "darkgrey") +
  labs(x = "Days", y = "wemwbs_scores",
       title = "Distribution of wemwbs_scores of PASC- Participants over time") +
  theme_minimal()


# side-by-side plots
# PASC+ vs PASC- (gender 90)
p1 <- ggplot(pasc_90_180_days, aes(x = a_sex, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Gender", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (gender 181)
p2 <-ggplot(pasc_181_365_days, aes(x = a_sex, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Gender", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (gender 366)
p3 <- ggplot(pasc_366_548_days, aes(x = a_sex, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Gender", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p1 + p2 + p3) +
  plot_annotation(
    title = "Distribution of PASC Status across Gender"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (age 90)
p4 <- ggplot(pasc_90_180_days, aes(x = age_bracket, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Age", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (age 181)
p5 <- ggplot(pasc_181_365_days, aes(x = age_bracket, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Age", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (age 366)
p6 <- ggplot(pasc_366_548_days, aes(x = age_bracket, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Age", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p4 + p5 + p6) +
  plot_annotation(
    title = "Distribution of PASC Status across Age Groups"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (race 90)
p7 <- ggplot(pasc_90_180_days, aes(x = a_race, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Race", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (race 181)
p8 <- ggplot(pasc_181_365_days, aes(x = a_race, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Race", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (race 366)
p9 <- ggplot(pasc_366_548_days, aes(x = a_race, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Race", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal()  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p7 + p8 + p9) +
  plot_annotation(
    title = "Distribution of PASC Status across Race"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (ethnicity 90)
p10 <- ggplot(pasc_90_180_days, aes(x = a_ethnicity, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Ethnicity", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (ethnicity 181)
p11 <- ggplot(pasc_181_365_days, aes(x = a_ethnicity, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Ethnicity", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (ethnicity 366)
p12 <- ggplot(pasc_366_548_days, aes(x = a_ethnicity, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Ethnicity", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p10 + p11 + p12) +
  plot_annotation(
    title = "Distribution of PASC Status across Ethnicity"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (income 90)
p13 <- ggplot(pasc_90_180_days, aes(x = a_income, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Income", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (income 181)
p14 <- ggplot(pasc_181_365_days, aes(x = a_income, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Income", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (income 366)
p15 <- ggplot(pasc_366_548_days, aes(x = a_income, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Income", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p13 + p14 + p15) +
  plot_annotation(
    title = "Distribution of PASC Status across Income"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (education 90)
p16 <- ggplot(pasc_90_180_days, aes(x = a_education, fill = pasc90_180_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Education", y = "Count", fill = "PASC90 Status",
       title = "90-180 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (education 181)
p17 <- ggplot(pasc_181_365_days, aes(x = a_education, fill = pasc181_365_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Education", y = "Count", fill = "PASC181 Status",
       title = "181-365 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (education 366)
p18 <- ggplot(pasc_366_548_days, aes(x = a_education, fill = pasc366_548_status_participant)) +
  geom_bar(position = position_dodge(width = 0.7), width = 0.6) +
  labs(x = "Education", y = "Count", fill = "PASC366 Status",
       title = "366-548 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10, r = 10, b = 2, l = 60, unit = "pt")) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p16 + p17 + p18) +
  plot_annotation(
    title = "Distribution of PASC Status across Education"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC- (wemwbs_baseline 90)
p19 <- ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_baseline_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC90 Status", y = "WEMWBS Baseline Score", title = "90-180 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (wemwbs_baseline 181)
p20 <- ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_baseline_score, fill = pasc181_365_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC181 Status", y = "WEMWBS Baseline Score", title = "181-365 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

# PASC+ vs PASC- (wemwbs_baseline 366)
p21 <- ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_baseline_score, fill = pasc366_548_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC366 Status", y = "WEMWBS Baseline Score", title = "366-548 days") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p19 + p20 + p21)  +
  plot_annotation(
    title = "Distribution of WEMWBS Baseline across PASC Status"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# wemwbs_followup vs PASC status
p22 <- ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_followup_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC90 Status", y = "WEMWBS Followup Score", title = "af_men followup score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

p23 <- ggplot(pasc_181_365_days, aes(x = pasc181_365_status_participant, y = wemwbs_12_score, fill = pasc181_365_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC181 Status", y = "WEMWBS12 Followup Score", title = "af12_men followup score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

p24 <- ggplot(pasc_366_548_days, aes(x = pasc366_548_status_participant, y = wemwbs_18_score, fill = pasc366_548_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC366 Status", y = "WEMWBS18 Followup Score", title = "af18_men followup score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "lightblue"))

(p22 + p23 + p24) +
  plot_annotation(
    title = "Distribution of WEMWBS Followup Scores across corresponding followup time period"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# PASC+ vs PASC-
p25 <- ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = day, y = score, group = ID)) +
  geom_line(alpha = 0.3, color = "darkgrey") +
  geom_point(alpha = 0.5, color = "darkgrey") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black") +  
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black") +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(title = "PASC-",
       x = "Days", y = "wemwbs_scores") +
  theme_minimal()

p26 <- ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = day, y = score, group = ID)) +
  geom_line(alpha = 0.3, color = "lightblue") +
  geom_point(alpha = 0.5, color = "lightblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black") +  
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black") +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(title = "PASC+",
       x = "Days", y = "wemwbs_scores") +
  theme_minimal()

(p25 + p26) +
  plot_annotation(
    title = "wemwbs_scores of PASC- vs PASC+ individuals"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# range of wemwbs scores
p27 <- ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = day, y = score)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "darkgrey") +
  stat_summary(fun = mean, geom = "point", color = "darkgrey", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 20) +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(x = "Days", y = "wemwbs_scores",
       title = "PASC-") +
  theme_minimal()

p28 <- ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = day, y = score)) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "lightblue") +
  stat_summary(fun = mean, geom = "point", color = "lightblue", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 20) +
  scale_x_continuous(breaks = c(90, 181, 366, 548)) +
  labs(x = "Days", y = "wemwbs_scores",
       title = "PASC+") +
  theme_minimal()

(p27 + p28) +
  plot_annotation(
    title = "Range of wemwbs_scores for PASC- vs PASC+ individuals"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# violin plots of wemwbs scores
p29 <- ggplot(pasc_negative_wemwbs_scores_over_time, aes(x = factor(day), y = score)) +
  geom_boxplot(fill = "lightgrey") +
  labs(x = "Days", y = "wemwbs_scores",
       title = "PASC-") +
  theme_minimal()

p30 <- ggplot(pasc_positive_wemwbs_scores_over_time, aes(x = factor(day), y = score)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Days", y = "wemwbs_scores",
       title = "PASC+") +
  theme_minimal()

(p29 + p30) +
  plot_annotation(
    title = "Boxplots of wemwbs_scores for PASC- vs PASC+ individuals"
  ) & theme(
    plot.title = element_text(hjust = 0.5)  
  )

# finalized visualization for use in Paper (wemwbs_baseline 90)
ggplot(pasc_90_180_days, aes(x = pasc90_180_status_participant, y = wemwbs_baseline_score, fill = pasc90_180_status_participant)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(x = "PASC status", y = "WEMWBS baseline score", title = "Distribution of WEMWBS baseline score across 90-180 days") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("orange", "lightblue")) 


# correlation plot
num_vars <- new_df %>%
  select(where(is.numeric)) %>%   
  select(
    a_sex,
    age_bracket,
    a_race,
    a_ethnicity,
    a_income,
    a_education,
    pasc90_180_status_participant,
    wemwbs_baseline_score
  ) 

cor_mat <- cor(num_vars, use = "pairwise.complete.obs")

ggcorrplot(cor_mat,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("orange", "white", "lightblue"),
           title = "Correlation Plot",
           ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))


cor_df <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq)

cor_df <- as.data.frame(as.table(cor_mat)) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
  filter(as.numeric(factor(var1)) < as.numeric(factor(var2)))

top_corr <- cor_df %>%
  arrange(desc(abs(correlation))) %>%
  head(20)

# return top correlations
top_corr


# statistical models (exposures vs outcome)
# logistic regression
lg_df <- new_df %>%
  filter(pasc90_180_status_participant %in% c(1, 0)) #%>% # filter for 1 & 0 

anyDuplicated(lg_df$record_id)

# 90 - 180 days
pasc_90_180_glm <- glm(
  pasc90_180_status_participant ~ a_sex + age_bracket + a_race + a_ethnicity + a_income + a_education + wemwbs_baseline_score,
  data = lg_df,
  family = binomial
)
summary(pasc_90_180_glm)


# LASSO regression
# LASSO without 10-fold cross-validation (1st run)
# specify X
X_90 <- model.matrix(pasc90_180_status_participant ~ a_sex + age_bracket + a_race + a_ethnicity + a_income + a_education + 
                       wemwbs_baseline_score, data = new_df)
# specify y
y_90 <- new_df$pasc90_180_status_participant
y_90 <- as.numeric(y_90)

# run lasso without 10-fold CV
cv.lasso <- cv.glmnet(X_90, y_90, alpha = 1) 

# lambda
best_lambda_90 <- cv.lasso$lambda.min
best_lambda_90

# coefficients
coef_90 <- coef(cv.lasso, s = "lambda.min")
coef_90


# LASSO with 10-fold cross-validation (2nd run)
# rerun lasso with 10-fold CV
cv.lasso <- cv.glmnet(X_90, y_90, alpha = 1, nfolds = 10, standardize = FALSE) 

# new lambda
best_lambda_90 <- cv.lasso$lambda.min
best_lambda_90

# new coefficients
coef_90 <- coef(cv.lasso, s = "lambda.min")
coef_90


# Table 1 - Participant Characteristics and WEMWBS Scores
table_90 <- new_df %>%
  select(a_sex, age_bracket, a_race, a_ethnicity, a_income, a_education, wemwbs_baseline_score) %>%
  mutate(
    Age = factor(age_bracket, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")),
    Sex  = factor(a_sex, labels = c("Male", "Female")),
    Race = factor(a_race, labels = c("Asian / Asian American", "Black / African American", "American Indian / Alaska Native", "White / Caucasian", "Native Hawaiian / Pacific Islander")),
    Ethnicity = factor(a_ethnicity, labels = c("Hispanic", "Non-Hispanic")),
    Income = factor (a_income, labels = c("$0 - $24,999", "$25,000 - $49,999", "$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999", "$150,000 or more")),
    Education = factor(a_education, labels = c("Never attended school / only kindergarten", "High school graduate or GED", "College, 1-3 years, or some college, associate's degree, or technical school", "College, 4 years or more, or college graduate", "Post-graduate education or professional degree (e.g., MA, PhD, MD, DDS)")),
    length.out = n()
  )

table_all <- table_90 %>%
  pivot_longer(cols = c(Age, Sex, Race, Ethnicity, Income, Education),
               names_to = "Variable", values_to = "Level") %>%
  group_by(Variable, Level) %>%
  summarise(
    count = n(),
    percent = count / sum(count) * 100,
    percentage_display = ifelse(percent < 5,
                                "<5%",
                                sprintf("%.1f%%", percent)),
    mean_wemwbs = mean(wemwbs_baseline_score, na.rm = TRUE)
  ) %>%
  mutate(
    percent = count / sum(count) * 100
    ) %>%
  mutate(
    # format percentage display
    percentage_display = ifelse(percent < 5,
                                "<5%",
                                sprintf("%.1f%%", percent))
  ) %>%
  select(-count) %>% 
  select(-percent) %>% 
  arrange(Variable, Level)


table_all %>%
  gt() %>%
  fmt_number(
    columns = c(mean_wemwbs),
    decimals = 2
  ) %>%
  tab_header(
    title = "Table 1. Participant Characteristics and WEMWBS Scores"
  ) %>%
  cols_label(
    Variable = "Demographic Variable",
    Level = "Category",
    percentage_display = "N (%)",
    mean_wemwbs = "Mean WEMWBS Baseline Score"
  ) %>%
  cols_align(
    align = "left",
    columns = c(Variable, Level)  
  ) %>%
  cols_align(
    align = "center",
    columns = c(percentage_display, mean_wemwbs)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),  
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    table.border.top.width = 2,
    table.border.bottom.width = 2,
    data_row.padding = px(3)
  ) %>%
  opt_row_striping() 


# extra analysis (exploratory)

# ANOVA statistical test 
# wemwbs_baseline_score as response, as outcome for ANOVA must be continuous
pasc_wemwbs_aov <- aov(wemwbs_baseline_score ~ a_sex + age_bracket + a_race + a_ethnicity + a_income + a_education + 
                         pasc90_180_status_participant, 
                       data = new_df)

# generate anova output table
summary(pasc_wemwbs_aov)


# chi-square tests
# to check if variables are related or independent
# outcome - 90-180 days PASC status
chisq.test(table(new_df$age_bracket, new_df$pasc90_180_status_participant))
chisq.test(table(new_df$a_sex, new_df$pasc90_180_status_participant))
chisq.test(table(new_df$a_race, new_df$pasc90_180_status_participant))
chisq.test(table(new_df$a_ethnicity, new_df$pasc90_180_status_participant))
chisq.test(table(new_df$a_income, new_df$pasc90_180_status_participant))
chisq.test(table(new_df$a_education, new_df$pasc90_180_status_participant))


# diagnostic plots
# purpose: Check if model assumptions are satisfied
# outcome: Model assumptions seem reasonably satisfied

# Normal plot and Residuals vs. Fitted 
resid_values <- resid(pasc_wemwbs_aov)
fitted_values <- fitted(pasc_wemwbs_aov)

par(mfrow = c(1, 2))

# Q-Q Plot for Normality
# checks the normality assumption of residuals
qqnorm(resid_values, main = "Q-Q Plot of Residuals")
qqline(resid_values, col = "red", lwd = 2)

# Residuals vs. Fitted 
# checks the homoscedasticity assumption (constant variance of residuals)
plot(fitted_values, resid_values,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "steelblue")
abline(h = 0, col = "red", lwd = 2)

