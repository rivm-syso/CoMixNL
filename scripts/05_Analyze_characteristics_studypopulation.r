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
# Analyze characteristics study population
#
# - Number of respondents per wave and drop out rates
# - Triangle of participant rounds versus survey rounds
# - Vaccination coverage over time per age group + high risk status
# Produces figures 1 and S1 and tables 1 and S1 in article 
#
################################################################################

# characteristics per unique participant (with most common age group and gender)
participant_characteristics <- participant_data %>% 
  group_by(part_id, series) %>% 
  summarise(age_group = names(sort(table(age_group), decreasing = TRUE)[1]),
            part_gender = names(sort(table(part_gender), decreasing = TRUE)[1]),
            part_risk = case_when(all(is.na(part_elevated_risk)) ~ NA_character_,
                                  sum(part_elevated_risk == "yes") == 0 ~ "no",
                                  sum(part_elevated_risk == "no") == 0 ~ "yes",
                                  TRUE ~ "mixed"),
            hh_size = round(mean(hh_size))) %>% 
  ungroup


######## Figure 1A: Number of participants included per survey round in the 2020 and 2021 series, by age group ######## 

number_of_respondents_per_wave_per_age <- participant_data %>%
  group_by(series, survey_round, age_group) %>%
  count() %>%
  left_join(dates_of_waves)

p_resp <- ggplot(data = number_of_respondents_per_wave_per_age,
       mapping = aes(
         x = date_round,
         y = n,
         col = age_group)
       ) +
  geom_point() +
  geom_line() +
  scale_x_date(
    breaks = "month",
    date_labels = "%b"
  ) +
  scale_y_continuous(
    limits = c(0, 400),
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = "number of participants",
    col = "age group"
  ) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(colour = 1),
    strip.placement = "outside"
  ) +
  facet_grid(
    cols = vars(series),
    scales = "free_x",
    space = "free",
    switch = "x"
  )

# ggsave(filename = paste0("./figures/number_participants.png"), 
#        width = 7, height = 4, dpi = 300)

# drop-out rates per age group

number_of_respondents_per_wave_per_age %>% 
  group_by(age_group) %>% 
  arrange(survey_round) %>% 
  # calculate drop out rate wrt previous wave
  mutate(n_prev = lag(n, n = 1, default = 0),
         rate = 1-n/n_prev) %>% 
  # filter out starting survey rounds
  filter(!(survey_round %in% c(1, 9, 19))) %>%
  group_by(series, age_group) %>% 
  summarise(mean_rate = mean(rate)) %>% 
  View


######## Figure 1B: Risk status of study and general population ########### 

high_risk_population <- participant_characteristics %>% 
  group_by(series, age_group, part_risk) %>%
  count %>% 
  # only include participants with unambiguous risk status
  filter(!is.na(part_risk) & part_risk != "mixed") %>% 
  group_by(series, age_group) %>% 
  mutate(n_tot = sum(n)) %>% 
  filter(part_risk == "yes") %>% 
  mutate(high_risk_mean = n/n_tot,
         high_risk_upper = calculate_binomialprob_upper(n_tot = n_tot, n_obs = n),
         high_risk_lower = calculate_binomialprob_lower(n_tot = n_tot, n_obs = n)
  ) %>% 
  full_join(high_risk %>% 
              full_join(population_data_2021) %>% 
              group_by(age_group) %>% 
              summarise(high_risk_general = sum(high_risk*population)/sum(population)))


p_risk <- ggplot(data = high_risk_population,
                 aes(x = age_group, y = high_risk_general, fill = age_group, group = 1)) +
  geom_bar(stat = "identity", 
           aes(col = "general population")) +
  geom_pointrange(aes(y = high_risk_mean, ymin = high_risk_lower, ymax = high_risk_upper, group = 1, shape = "study population"),
                  col = 1) +
  scale_shape_manual('', values = 21) +
  scale_color_manual('', values = NA) +
  theme_light() +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c(1, 1),
        legend.text = element_text(size=8),
        axis.text.x = element_text(angle = 90),
        legend.margin = margin(t = -10, b = -10, unit = "pt"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1)) +
  guides(fill = "none") +
  labs(x = "age group",
       y = "high risk fraction") +
  facet_wrap(facets = vars(series),
             nrow = 1,
             scale = "free_x")


######## Figure 1C: Vaccination status of study and general population ########### 

vaccination_proportions_of_participants_by_wave_and_age <- participant_data %>%
  filter(series == "2021") %>%
  group_by(survey_round, age_group) %>%
  summarise(
    n = n(),
    vacc = sum(part_vacc),
    frac_vacc_mean = mean(part_vacc),
    frac_vacc_upper = calculate_binomialprob_upper(n_tot = n, n_obs = vacc),
    frac_vacc_lower = calculate_binomialprob_lower(n_tot = n, n_obs = vacc)
  ) %>%
  left_join(dates_of_waves, by = "survey_round")


p_vacc <- ggplot(
  data = vaccination_coverage,
  aes(x = date, 
      y = vacc_coverage,
      col = age_group)) +
  geom_line() +
  geom_pointrange(data = vaccination_proportions_of_participants_by_wave_and_age,
                  aes(x = date_round, y = frac_vacc_mean, ymin = frac_vacc_lower, ymax = frac_vacc_upper, color = age_group),
                  fatten = 3) +
  geom_vline(data = vaccination_start_dates %>% 
               group_by(age_group) %>% 
               summarise(mean_start_date = mean(date)),
             aes(xintercept = mean_start_date,
                 col = age_group),
             lty = 2) +
  scale_x_date(limits = c(ymd(NA), as.Date("2021-10-01")-1),
               date_breaks = "month",
               minor_breaks = "month",
               date_labels = "%b") +
  labs(x = "2021",
       y = "vaccination coverage") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1)) +
  guides(col = "none") +
  facet_wrap(facets = vars(age_group))



inset_vacc <- ggplot() +
  geom_segment(data = tibble(x = c(1, 0.5),
                             xend = c(1, 1.5),
                             y = c(1.5, 4),
                             yend = c(2.5, 4),
                             type = factor(c(2, 1))),
               aes(x = x, y = y, xend = xend, yend = yend, lty = type),
               show.legend = FALSE) +
  geom_pointrange(data = tibble(x = 1, y = 6, ymin = 5.5, ymax = 6.5),
                  aes(x = x, y = y, ymin = ymin, ymax = ymax)) +
  geom_text(data = tibble(x = rep(3, 3), y = c(2, 4, 6), 
                          label = c("mean vaccination start date\nfor non-risk groups",
                                    "mean vaccination coverage\nfor general population",
                                    "mean vaccination coverage (and 95% CI)\nfor study population")),
            aes(x = x, y = y, label = label), 
            hjust = 0, 
            size = 3) +
  coord_cartesian(xlim = c(-2, 20),
                  ylim = c(0, 7)) +
  theme_void()


######## Figure 1: Description of study population, compared to general population ########### 

pAB <- plot_grid(p_resp,
          p_risk,
          rel_widths = c(3,2),
          nrow = 1,
          labels = c("A","B"),
          label_size = 18,
          label_fontfamily = "sans")

pC <- plot_grid(ggdraw(p_vacc) + draw_plot(inset_vacc, 2/3, 0, 1/3, 1/3),
                labels = c("C"),
                label_size = 18,
                label_fontfamily = "sans")

plot_grid(pAB,
          pC,
          rel_heights = c(2,3),
          ncol = 1)


ggsave(filename = paste0("./figures/characteristics_studypopulation.png"), 
       width = 12, height = 9, dpi = 300)



######## Figure S1: Number of participants by survey round and participant round ######## 

participant_data %>% 
  mutate(survey_round = if_else(series == "2021", as.integer(survey_round - 8), survey_round)) %>% 
  group_by(part_round, survey_round, series) %>% 
  count %>% 
  ggplot(aes(x = survey_round, y = part_round, fill = n)) +
  geom_tile() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c(
    trans = "log",
    breaks = c(1, 10, 100, 1000)) +
  labs(x = "survey round",
       y = "participant round",
       fill = "number of\nparticipants") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1)) +
  facet_grid(cols = vars(series),
             scales = "free_x",
             space = "free")

ggsave(filename = paste0("./figures/participant_rounds.png"), width = 7, height = 4, dpi = 300)


######## Table S1: Number of participants per survey round of study series in 2020 and 2021 in eight age groups ############

participant_data %>%
  filter(!duplicated(part_id)) %>% 
  group_by(series) %>% 
  count

participants_rounds_table <- participant_data %>%
  mutate(survey_round = if_else(series == "2021", as.integer(survey_round - 8), survey_round)) %>% 
  group_by(age_group, series, survey_round) %>% 
  count %>% 
  # add target numbers of participants per age group
  bind_rows(tibble(age_group = c("0-11", "12-17",  "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                   series = "2020",
                   survey_round = 0L,
                   n = as.integer(c(0, 0, 162, 235, 225, 277, 249, 352)))) %>%
  bind_rows(tibble(age_group = c("0-11", "12-17",  "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                   series = "2021",
                   survey_round = 0L,
                   n = as.integer(c(150, 150, 248, 206, 163, 184, 161, 238)))) %>%
  group_by(series, survey_round) %>% 
  mutate(total = sum(n)) %>% 
  pivot_wider(names_from = age_group, values_from = n) %>% 
  arrange(series, survey_round) %>% 
  replace_na(replace = list(`0-11` = 0, `12-17` = 0)) %>% 
  relocate(total, .after = last_col()) 


xtable(participants_rounds_table, type = "latex", 
       caption= "Number of participants per survey round of study series in 2020 and 2021 in eight age groups", 
       align=c("l", rep("r", 11)),
       table.placement = "h",
       hline.after = c(-1, nrow(tab))) %>% 
  print(include.rownames = FALSE)

########  Table 1: Number of participants in the 2020 and 2021 series by participant characteristics ############


participant_characteristics_agegroup <- participant_characteristics %>% 
  group_by(series, age_group) %>% 
  count %>%
  group_by(series) %>% 
  mutate(perc_study = 100*n/sum(n)) %>% 
  full_join(bind_rows("2020" = population_data_2020 %>% filter(age >= 18),
                      "2021" = population_data_2021,
                      .id = "series") %>% 
              group_by(series, age_group) %>% 
              summarise(n = sum(population)) %>% 
              group_by(series) %>% 
              mutate(perc_general = 100*n/sum(n)) %>% 
              select(-n))


participant_characteristics_gender <- participant_characteristics %>% 
  mutate(gender = if_else(part_gender == "F", "Female", "Male", NA_character_)) %>% 
  group_by(series, gender) %>% 
  count %>%
  group_by(series) %>% 
  mutate(perc_study = 100*n/sum(n)) %>% 
  full_join(bind_rows("2020" = population_data_2020 %>% filter(age >= 18),
                      "2021" = population_data_2021,
                      .id = "series") %>% 
              group_by(series, gender) %>% 
              summarise(n = sum(population)) %>% 
              group_by(series) %>% 
              mutate(perc_general = 100*n/sum(n)) %>% 
              select(-n))


participant_characteristics_risk <- participant_characteristics %>% 
  rename(high_risk = part_risk) %>% 
  group_by(series, high_risk) %>% 
  count %>%
  # only include risk status of participants with unambiguous risk status
  mutate(include = if_else(high_risk %in% c("yes", "no"), 1, 0)) %>% 
  group_by(series) %>% 
  mutate(n_tot = sum(include*n),
         perc_study = 100*include*n/n_tot) %>% 
  select(-include, -n_tot) %>% 
  full_join(bind_rows("2020" = high_risk %>% full_join(population_data_2020) %>% filter(age >= 18),
                      "2021" = high_risk %>% full_join(population_data_2021),
                      .id = "series") %>% 
              rename(yes = high_risk) %>% 
              mutate(no = 1-yes) %>% 
              pivot_longer(cols = c("no", "yes"), names_to = "high_risk", values_to = "fraction") %>% 
              group_by(series, high_risk) %>% 
              summarise(n = sum(fraction*population)) %>% 
              group_by(series) %>% 
              mutate(perc_general = 100*n/sum(n)) %>% 
              select(-n))


participant_characteristics_hhsize <- participant_characteristics %>% 
  mutate(hh_size = if_else(hh_size >= 5, "5+", as.character(hh_size), NA_character_)) %>% 
  group_by(series, hh_size) %>% 
  count %>%
  group_by(series) %>% 
  mutate(perc_study = 100*n/sum(n)) %>% 
  full_join(tibble(hh_size = c("1", "2", "3", "4", "5+"),
                   perc_general = c(17, 31, 17, 23, 12)))


bind_rows("age group" = participant_characteristics_agegroup %>% rename(value = age_group),
          "gender" = participant_characteristics_gender %>% rename(value = gender),
          "high risk" = participant_characteristics_risk %>% rename(value = high_risk),
          "household size" = participant_characteristics_hhsize %>% rename(value = hh_size),
          .id = "var") %>% 
  ungroup %>% 
  arrange(series, var) %>% 
  select(-series) %>% 
  xtable(type = "latex", 
         caption= "Number of participants in the 2020 and 2021 series by participant characteristics", 
         #align=c("l", rep("r", 11)),
         digits = 0,
         table.placement = "h",
         hline.after = c(-1, nrow(tab))) %>% 
  print(include.rownames = FALSE)


