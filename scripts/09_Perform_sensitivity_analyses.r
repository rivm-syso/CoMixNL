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
# Sensitivity analysis
#
# - Analyse contacts with parametric distribution with and without capping
# - Analyse contacts with dependency between age groups
# 
################################################################################

########### analysis of number of contacts with negative binomial ##############

# uses fit_data from 06_Analyze_contacts_studypopulation.r

# fit each series and age group separately, with negative binomial
# no max to number of contacts
for(ser in unique(fit_data$series)) {
  for(age_group in levels(fit_data$part_age_group)){
    print(paste("start", ser, age_group, now()))
    
    data <- fit_data %>% filter(series == ser, part_age_group == age_group)
    
    if(nrow(data) > 0) {
      fit_gam_nb <- gam(
        formula = number_of_contacts ~
          s(date_trans, bs = "ps", k = 10) +
          s(part_round, bs = "ps", k = 6) +
          s(part_age, bs = "ps", k = 6) +
          s(part_id, bs = "re") +
          part_vacc +
          part_elevated_risk +
          weekend +
          holiday
        ,
        family = nb(),
        data = data)
      
      fit_gam_nb$data <- data
      
      saveRDS(fit_gam_nb, paste0("./results/fit_nb_", ser, "_", age_group, ".rds"))
      
    }
    print(paste("end", ser, age_group, now()))
  }
}


# fit each series and age group separately, with negative binomial,
# max number of contacts is 50
for(ser in unique(fit_data$series)) {
  for(age_group in levels(fit_data$part_age_group)){
    print(paste("start", ser, age_group, now()))
    
    data <- fit_data %>% filter(series == ser, part_age_group == age_group) %>% 
      mutate(number_of_contacts = if_else(number_of_contacts >= 50, as.integer(50), number_of_contacts))
    
    if(nrow(data) > 0) {
      fit_gam_nb <- gam(
        formula = number_of_contacts ~
          s(date_trans, bs = "ps", k = 10) +
          s(part_round, bs = "ps", k = 6) +
          s(part_age, bs = "ps", k = 6) +
          s(part_id, bs = "re") +
          part_vacc +
          part_elevated_risk +
          weekend +
          holiday
        ,
        family = nb(),
        data = data)
      
      fit_gam_nb$data <- data
      
      saveRDS(fit_gam_nb, paste0("./results/fit_nbcap_", ser, "_", age_group, ".rds"))
      
    }
    print(paste("end", ser, age_group, now()))
  }
}


# read in all fit objects
fit <- list()
for(ser in unique(participant_data$series)) {
  for(age_group in levels(participant_data$part_age_group)){
    print(paste("reading", ser, age_group))
    
    #file <- paste0("./results/fit_nb_", ser, "_", age_group, ".rds")
    file <- paste0("./results/fit_nbcap_", ser, "_", age_group, ".rds")
    if(file.exists(file)) {
      fit[[paste0(ser, "_", age_group)]] <- readRDS(file)
    }
  }
}

fitted_data <- lapply(names(fit), function(i) fit[[i]]$data) %>% bind_rows()


# Predict number of contacts of study population

sim_data <- tibble()

for(set in names(fit)) {
  print(paste(set, now()))
  ser <- str_split(set, pattern = "_") %>% unlist %>% pluck(1)
  age_group <- str_split(set, pattern = "_") %>% unlist %>% pluck(2)
  
  data <- fitted_data %>% filter(series == ser, part_age_group == age_group)
  
  if(nrow(data) > 0) {
    
    # draw 200 parameter sets from fit object
    beta <- rmvn(
      n = 200,
      mu = coef(fit[[set]]),
      V = vcov(fit[[set]]))
    
    tmp_data <- lapply(1:200, 
                       function(i) data %>% 
                         mutate(pred = predict(
                           object = list_modify(fit[[set]], coefficients = beta[i,]),
                           newdata = data,
                           type = "response")) %>% 
                         group_by(survey_round) %>% 
                         summarise(series = ser,
                                   age_group = age_group,
                                   sim = i,
                                   n_cont = mean(pred))) %>% 
      bind_rows
    
    sim_data <- bind_rows(sim_data, tmp_data)
  }
}




#saveRDS(sim_data, "./results/pred_studypopulation_nb.rds")
#saveRDS(sim_data, "./results/pred_studypopulation_nbcap.rds")

sim_data <- bind_rows("nb" = readRDS("./results/pred_studypopulation_nb.rds"),
                      "nbcap" = readRDS("./results/pred_studypopulation_nbcap.rds"),
                      .id = "model")


plot_data <- sim_data %>% 
  group_by(model, series, age_group, survey_round) %>% 
  summarise(pred_lower = quantile(n_cont, 0.025),
            pred_upper = quantile(n_cont, 0.975),
            pred_median = quantile(n_cont, 0.5)) %>% 
  left_join(bind_rows("nb" = fit_data %>% 
                        rename(age_group = part_age_group) %>% 
                        group_by(series, age_group, survey_round) %>% 
                        summarise(obs_mean = mean(number_of_contacts),
                                  obs_lower = pmax(0, obs_mean - 1.96*sd(number_of_contacts)/sqrt(n())),
                                  obs_upper = obs_mean + 1.96*sd(number_of_contacts)/sqrt(n())),
                      "nbcap" = fit_data %>% 
                        mutate(number_of_contacts = if_else(number_of_contacts >= 50, as.integer(50), number_of_contacts)) %>% 
                        rename(age_group = part_age_group) %>% 
                        group_by(series, age_group, survey_round) %>% 
                        summarise(obs_mean = mean(number_of_contacts),
                                  obs_lower = pmax(0, obs_mean - 1.96*sd(number_of_contacts)/sqrt(n())),
                                  obs_upper = obs_mean + 1.96*sd(number_of_contacts)/sqrt(n())),
                      .id = "model")) %>% 
  left_join(dates_of_waves) 


(fit_data %>% filter(number_of_contacts > 50) %>% nrow)/(fit_data %>% nrow)

ggplot(data = plot_data %>% 
         mutate(date_round = if_else(model == "nb", date_round_min + 1, date_round_max - 1),
                model = factor(model, levels = c("nb", "nbcap"), labels = c("No maximum number of contacts", "Maximum number of contacts set at 50"))),
       aes(x = date_round_min, 
           xend = date_round_max, 
           y = pred_median, 
           yend = pred_median, 
           ymin = pred_lower, 
           ymax = pred_upper, 
           col = model, 
           fill = model)) +
  geom_rect(data = dates_of_holidays,
            aes(xmin = holiday_start - 0.5, xmax = holiday_end + 0.5, ymin = 0, ymax = 11.2),
            fill = "grey",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_rect(aes(xmin = date_round_min, xmax = date_round_max), alpha = 0.5, col = NA) +
  geom_segment(aes(linetype = "Fitted")) +
  scale_linetype_manual('', values = "solid")+
  geom_pointrange(aes(x = date_round, y = obs_mean, ymin = obs_lower, ymax = obs_upper, shape = "Observed"),
                  #size = 0.2,
                  lwd = 0.4,
                  stroke = 0.4,
                  col = rgb(0.3,0.3,0.3)) +
  scale_shape_manual('', values = 21) +
  scale_x_date(breaks = "month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 11.2),
                     expand = c(0, 0)) +
  labs(subtitle = "Number of contacts fitted to negative binomial distribution",
       x = NULL,
       y = "Number of contacts",
       colour = NULL,
       fill = NULL) +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         size = guide_legend(order = 1)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1),
        strip.placement = "outside",
        legend.position = "bottom") +
  facet_grid(rows = vars(age_group),
             cols = vars(series),
             scales = "free_x",
             space = "free",
             switch = "x")

ggsave(filename = paste0("./figures/fit_studypopulation_nb.pdf"), width = 7, height = 5)


########### analysis of number of contacts with age dependence #################

# fit each series and age group separately
for(ser in unique(fit_data$series)) {
  print(paste("start", ser, now()))
  
  data <- fit_data %>% filter(series == ser)
  
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
    
    saveRDS(fit_bam_ocat, paste0("./results/fit6cat_", ser, ".rds"))
    
  }
  print(paste("end", ser, now()))
}


# read in all fit objects
fit <- list()
for(ser in unique(participant_data$series)) {
  print(paste("reading", ser))
  
  file <- paste0("./results/fit6cat_", ser, ".rds")
  if(file.exists(file)) {
    fit[[paste0(ser)]] <- readRDS(file)
  }
}

fitted_data <- lapply(names(fit), function(i) fit[[i]]$data) %>% bind_rows()


# Predict number of contacts of study population

sim_data <- tibble()

for(set in names(fit)) {
  print(paste(set, now()))
  ser <- set
  
  data <- fit_data %>% filter(series == ser)
  
  if(nrow(data) > 0) {
    
    # draw 200 parameter sets from fit object
    beta <- rmvn(
      n = 200,
      mu = coef(fit[[set]]),
      V = vcov(fit[[set]]))
    
    tmp_data <- lapply(1:200, 
                       function(i) data %>% 
                         mutate(pred = predict(
                           object = list_modify(fit[[set]], coefficients = beta[i,]),
                           newdata = data,
                           type = "response")) %>% 
                         group_by(survey_round) %>% 
                         summarise(series = ser,
                                   #age_group = age_group,
                                   sim = i,
                                   activity = 1:6,
                                   frac_activity = colMeans(pred),
                                   cum_activity = cumsum(frac_activity))) %>% 
      bind_rows
    
    sim_data <- bind_rows(sim_data, tmp_data)
  }
}


#saveRDS(sim_data, "./results/pred_studypopulation_series.rds")

# Compare fitted and observed contacts of study population


sim_data <- readRDS("./results/pred_studypopulation_series.rds")

sim_data %>% 
  group_by(series, survey_round, activity) %>% 
  summarise(cum_pred_lower = quantile(cum_activity, 0.025),
            cum_pred_upper = quantile(cum_activity, 0.975),
            cum_pred = quantile(cum_activity, 0.5))



plot_data <- sim_data %>% 
  filter(activity != 6) %>% 
  mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))) %>% 
  group_by(series, survey_round, activity) %>% 
  summarise(cum_pred_lower = 1-quantile(cum_activity, 0.025),
            cum_pred_upper = 1-quantile(cum_activity, 0.975),
            cum_pred = 1-quantile(cum_activity, 0.5)) %>% 
  left_join(fit_data %>% 
              mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))) %>% 
              group_by(series, survey_round, activity) %>% 
              count %>% 
              group_by(series, survey_round) %>% 
              mutate(frac_obs = n/sum(n),
                     sum_obs = 1-cumsum(frac_obs)) %>% 
              dplyr::select(-n)) %>% 
  left_join(dates_of_waves) 



ggplot(data = plot_data,
       aes(x = date_round_min, 
           xend = date_round_max, 
           y = cum_pred, 
           yend = cum_pred, 
           ymin = cum_pred_lower, 
           ymax = cum_pred_upper, 
           col = factor(activity), 
           fill = factor(activity))) +
  geom_rect(data = dates_of_holidays,
            aes(xmin = holiday_start - 0.5, xmax = holiday_end + 0.5, ymin = 0, ymax = 0.99),
            fill = "grey",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_rect(aes(xmin = date_round_min, xmax = date_round_max), alpha = 0.3, col = NA) +
  geom_segment(aes(linetype = "Fitted")) +
  scale_linetype_manual('', values = "solid")+
  geom_point(aes(x = date_round, y = sum_obs, col = factor(activity), shape = "Observed")) +
  scale_shape_manual('', values = 1) +
  scale_x_date(breaks = "month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.99),
                     expand = c(0, 0)) +
  labs(subtitle = "Model without distinction between age groups",
       x = NULL,
       y = "Fraction of participants",
       colour = "Number of contacts",
       fill = "Number of contacts") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1),
        strip.placement = "outside",
        legend.position = "bottom") +
  facet_grid(cols = vars(series),
             scales = "free_x",
             space = "free",
             switch = "x")

ggsave(filename = paste0("./figures/fit_studypopulation_series.pdf"), width = 7, height = 4)

