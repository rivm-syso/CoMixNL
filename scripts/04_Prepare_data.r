################################################################################
#
# Copyright 2022 Rijksinstituut voor Volksgezondheid en Milieu (RIVM).
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU Affero General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Prepare data for analysis
#
# - Recoding part_id to unique participants
# - Adding variables that are needed for analysis to participant_data
# - Determining start and end dates of waves
# - Determining start and end dates of holiday periods
# - Using identical age groups for all data sources
# 
################################################################################

age_group_breaks <- c(0, 12, 18, 25, seq(35, 65, 10), Inf)
age_group_labels <- c("0-11", "12-17",  "18-24", "25-34", "35-44", "45-54", "55-64", "65+")


participant_data <- participant_data %>%
  mutate(
    # part_id in raw data is concatenation of participant id and survey round, 
    # separate to be able to identify unique participants
    part_id = floor(part_id / 100),
    # define 8 age groups (identical to age groups used to invite participants)
    age_group = cut(part_age, 
                    breaks = age_group_breaks, 
                    right = FALSE, 
                    include_lowest = TRUE, 
                    labels = age_group_labels),
    # define 3 large age groups for separate analyses
    part_age_group = cut(part_age, 
                         breaks = c(0, 18, 65, Inf), 
                         right = FALSE, 
                         include_lowest = TRUE, 
                         labels = c("0-17", "18-64", "65+")),
    series = ifelse(survey_round > 8, "2021", "2020"),
    weekend = weekdays(date) %in% c("Saturday", "Sunday"),
    holiday = date %in% schoolholidays | date %in% holidays
  ) %>% 
  group_by(part_id) %>% 
  arrange(survey_round) %>% 
  # impute missing part_elevated_risk from answers in other waves, and add index of participation round 
  mutate(risk = case_when(sum(part_elevated_risk == "no", na.rm = TRUE) > sum(part_elevated_risk == "yes", na.rm = TRUE) ~ "no",
                          sum(part_elevated_risk == "no", na.rm = TRUE) < sum(part_elevated_risk == "yes", na.rm = TRUE) ~ "yes",
                          TRUE ~ NA_character_),
         part_elevated_risk = if_else(is.na(part_elevated_risk) | part_elevated_risk == "no answer", risk, part_elevated_risk),
         part_round = 1:n()) %>% 
  dplyr::select(-risk) %>% 
  ungroup


contact_data <- contact_data %>%
  mutate(
    # part_id in raw data is concatenation of participant id and survey round, 
    # separate to be able to identify unique participants
    part_id = floor(part_id / 100))


# dates of survey rounds
dates_of_waves <- participant_data %>%
  group_by(survey_round) %>%
  summarise(date_round = mean.Date(survey_date-1),
            date_round_min = min(survey_date-1),
            date_round_max = max(survey_date-1),
            .groups = "drop")


# dates of holiday periods

sorted_holidays <- sort(c(holidays, schoolholidays))

dates_of_holidays <- tibble(holiday_start = sorted_holidays[c(10, diff(sorted_holidays)) > 1],
                            holiday_end = sorted_holidays[c(diff(sorted_holidays), 10) > 1],
                            series = if_else(holiday_start >= as.Date("2020-12-15"), "2021", "2020")) %>% 
  filter(holiday_start < as.Date("2020-10-01") | holiday_start > as.Date("2020-12-01")) %>% 
  mutate(holiday_end = if_else(holiday_end == as.Date("2020-08-30"), as.Date("2020-08-10"), holiday_end)) %>% 
  bind_rows(tibble(holiday_start = as.Date("2021-09-28"),
                   holiday_end = as.Date("2021-09-27"),
                   series = "2021"))




vaccination_start_dates <- vaccination_start_dates %>% 
  mutate(age = 2021-birthyear,
         age_group = cut(age, 
                         breaks = age_group_breaks, 
                         right = FALSE, 
                         include_lowest = TRUE, 
                         labels = age_group_labels))

high_risk <- high_risk %>% 
  mutate(age_group = cut(age, 
                         breaks = age_group_breaks, 
                         right = FALSE, 
                         include_lowest = TRUE, 
                         labels = age_group_labels))

population_data_2020 <- population_data_2020 %>% 
  mutate(age_group = cut(age, 
                         breaks = age_group_breaks, 
                         right = FALSE, 
                         include_lowest = TRUE, 
                         labels = age_group_labels))

population_data_2021 <- population_data_2021 %>% 
  mutate(age_group = cut(age, 
                         breaks = age_group_breaks, 
                         right = FALSE, 
                         include_lowest = TRUE, 
                         labels = age_group_labels))


rm(age_group_breaks)
rm(age_group_labels)
rm(sorted_holidays)
