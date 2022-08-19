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
# Analyze contacts study population
#
# - Exclude participants with >= 1000 contacts per wave
# - Exclude participants who participated >= 4 times but did not report any contact
# - Examine powerlaw-like distribution of number of contacts
# - Analyse number of contacts (excluding household members) categorized in 
#   activity levels
# Produces figures 3 and S2 and table S2 in paper
# 
################################################################################


# 744 participants switch risk level
participant_data %>% 
  group_by(part_id) %>% 
  summarise(risk_transition = (sum(part_elevated_risk == "yes", na.rm = TRUE) > 0) & 
              (sum(part_elevated_risk == "no", na.rm = TRUE) > 0)) %>% 
  group_by(risk_transition) %>% 
  count

# 1080 transitions from unvaccinated to vaccinated
participant_data %>% 
  group_by(part_id) %>% 
  arrange(survey_round) %>% 
  mutate(diff_vacc = c(0, diff(part_vacc))) %>% 
  group_by(diff_vacc) %>% 
  count

# 28115 participant-wave combinations (excluding 12 with more than 1000 contacts)
number_of_contacts_by_wave_and_participant <- contact_data %>% 
  group_by(
    survey_round,
    part_id
  ) %>%
  summarise(
    number_of_contacts_tot = n(),
    number_of_contacts_hh = sum(cnt_household),
    number_of_contacts = n() - sum(cnt_household),
    number_of_contacts_school = sum(cnt_school),
    number_of_contacts_work = sum(cnt_work),
    number_of_contacts_other = number_of_contacts - number_of_contacts_school - number_of_contacts_work,
    .groups = "drop"
  ) %>%
  full_join(
    participant_data,
    by = c("survey_round", "part_id")
  ) %>%
  replace_na(replace = list(number_of_contacts = 0, 
                            number_of_contacts_tot = 0, 
                            number_of_contacts_hh = 0, 
                            number_of_contacts_school = 0, 
                            number_of_contacts_work = 0, 
                            number_of_contacts_other = 0)) %>% 
  filter(number_of_contacts < 1000)


# 58 participants that participated 4 times or more but did not report any contacts
# (many but not all in single person households)
number_of_contacts_by_wave_and_participant %>% 
  group_by(part_id) %>% 
  filter((max(part_round) >= 4 & sum(number_of_contacts_tot) == 0)) %>% 
  filter(part_round == max(part_round)) %>% 
  select(part_id, part_round, hh_size) %>%
  distinct() %>% 
  View
  
# omit them (resulting in 27430 participant-wave combinations)
number_of_contacts_by_wave_and_participant <- number_of_contacts_by_wave_and_participant %>% 
  group_by(part_id) %>% 
  filter(!(max(part_round) >= 4 & sum(number_of_contacts_tot) == 0)) %>% 
  ungroup


########### powerlaw-like distribution of number of contacts ###################

number_of_contacts_by_wave_and_participant %>% 
  group_by(part_age_group, series) %>% 
  arrange(number_of_contacts) %>% 
  mutate(cumsum = (n() - (1:n()-1))/n()) %>% 
  ungroup %>% 
  ggplot(aes(x = number_of_contacts, 
             y = cumsum, 
             col = part_age_group, 
             lty = series %>% fct_rev)) +
  geom_line() +
  scale_x_continuous(trans = "log",
                     breaks = c(1, 10, 100, 1000)) +
  scale_y_continuous(trans = "log", 
                     limits = c(1/10000,1),
                     breaks = c(0.0001, 0.001, 0.01, 0.1, 1)) +
  theme_light() +
  labs(x = "number of contacts (excluding household)",
       y = " complementary CDF",
       lty = " series",
       col = "age group")

ggsave(filename = paste0("./figures/powerlaw_contacts.png"), 
       width = 7, height = 4, dpi = 300)

########### analysis of number of contacts #####################################

# analysis of number of contacts (i.e. excluding household contacts)
# categorized in activity levels
fit_data <- number_of_contacts_by_wave_and_participant %>% 
  filter(!is.na(part_elevated_risk)) %>% 
  select(part_age_group, series, survey_round, date, weekend, holiday, 
         part_id, part_age, part_round, part_vacc, part_elevated_risk, number_of_contacts) %>% 
  mutate(date_trans = as.integer(date - min(date)),
         part_id = factor(part_id),
         part_elevated_risk = if_else(part_elevated_risk == "yes", TRUE, FALSE),
         part_vacc = as.logical(part_vacc),
         activity = cut(number_of_contacts, c(0,1,2,3,5,10,Inf), 
                        include_lowest = TRUE, right = FALSE) %>% as.integer) 

# check number of observations per category and series
fit_data %>% select(series, activity) %>% table

# fit each series and age group separately
for(ser in unique(fit_data$series)) {
  for(age_group in levels(fit_data$part_age_group)){
    print(paste("start", ser, age_group, now()))
    
    data <- fit_data %>% filter(series == ser, part_age_group == age_group)
    
    if(nrow(data) > 0) {
      fit_bam_ocat <- bam(
        formula = activity ~
          s(date_trans, bs = "ps", k = 10) +
          s(part_round, bs = "ps", k = 6) +
          s(part_age, bs = "ps", k = 6) +
          s(part_id, bs = "re") +
          part_vacc +
          part_elevated_risk +
          weekend +
          holiday
        ,
        family = ocat(R = 6),
        data = data)
      
      fit_bam_ocat$data <- data
      
      saveRDS(fit_bam_ocat, paste0("./results/fit6cat_", ser, "_", age_group, ".rds"))
      
    }
    print(paste("end", ser, age_group, now()))
  }
}


############### read in all fit objects ########################################

fit <- list()
for(ser in unique(participant_data$series)) {
  for(age_group in levels(participant_data$part_age_group)){
    print(paste("reading", ser, age_group))
    
    file <- paste0("./results/fit6cat_", ser, "_", age_group, ".rds")
    if(file.exists(file)) {
      fit[[paste0(ser, "_", age_group)]] <- readRDS(file)
    }
  }
}

########### Table S2 Stationary fixed effects ##################################


# extract stationary fixed effects from summary of fit object
results_fixedeffects <- lapply(1:length(fit), function(i) {
  tmp <- summary(fit[[i]]);
  tmp$p.table %>% 
    as_tibble %>% 
    mutate(Variable = rownames(tmp$p.table)) %>% 
    add_row(Variable = "Deviance explained", 
            Estimate = tmp$dev.expl) %>% 
    mutate(series = as.character(c(2020, 2020, 2021, 2021, 2021))[i],
           age_group = c("18-64", "65+", "0-17", "18-64", "65+")[i])}) %>% 
  bind_rows() %>% 
  select(series, age_group, Variable, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`)


xtable(results_fixedeffects, 
       type = "latex", 
       caption= "Results for fixed effects of generalised additive model", 
       align=c("l", rep("r", 7)),
       table.placement = "h",
       hline.after = c(-1, nrow(tab))) %>% 
  print(include.rownames = FALSE)

# https://environmentalcomputing.net/statistics/gams/


########### Figure S2 Non-stationary fixed effects #############################

# set graphical output to 2x2 window to avoid pushing Enter to get through loop
par(mfrow = c(2, 2))

spline_data <- tibble()
for(set in names(fit)) {
  tmp <- plot(fit[[set]])
  spline_data <- bind_rows(spline_data, 
                           lapply(1:3, function(i) tibble(set = set,
                                                          what = tmp[[i]]$xlab,
                                                          x = tmp[[i]]$x,
                                                          fit = tmp[[i]]$fit[,1],
                                                          se = tmp[[i]]$se)) %>% 
                             bind_rows())
}
# restore graphical output window
par(mfrow = c(1, 1))


ggplot(data = spline_data %>% 
         mutate(what = case_when(what == "date_trans" ~ "days since 16 Apr 2020",
                                 what == "part_age" ~ "participant age",
                                 TRUE ~ "participant round")),
       aes(x = x, y = fit)) +
  geom_line() +
  geom_line(aes(y = fit + se), lty = 2) +
  geom_line(aes(y = fit - se), lty = 2) +
  theme_light() +
  labs(x = NULL,
       y = "effect") +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1),
        strip.placement = "outside") +
  facet_grid(cols = vars(what),
             rows = vars(set),
             scales = "free_x",
             switch = "x")

ggsave(filename = paste0("./figures/results_splines.png"), 
       width = 10, height = 6, dpi = 300)
