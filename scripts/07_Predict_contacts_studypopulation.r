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
# Predict contacts study population
#
# - drawing parameter sets from fit objects
# - using predict function
#
# Produces figure 4 in paper
# 
################################################################################

# read in all fit objects
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

fit_data <- lapply(names(fit), function(i) fit[[i]]$data) %>% bind_rows()


############### Predict number of contacts of study population #################

sim_data <- tibble()

for(set in names(fit)) {
  print(paste(set, now()))
  ser <- str_split(set, pattern = "_") %>% unlist %>% pluck(1)
  age_group <- str_split(set, pattern = "_") %>% unlist %>% pluck(2)
  
  data <- fit_data %>% filter(series == ser, part_age_group == age_group)
  
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
                                   activity = 1:6,
                                   frac_activity = colMeans(pred),
                                   cum_activity = cumsum(frac_activity))) %>% 
      bind_rows
    
    sim_data <- bind_rows(sim_data, tmp_data)
  }
}


saveRDS(sim_data, "./results/pred_studypopulation_cat6.rds")

####### Compare predicted and observed contacts of study population ############


sim_data <- readRDS("./results/pred_studypopulation_cat6.rds")

sim_data %>% 
  group_by(series, age_group, survey_round, activity) %>% 
  summarise(cum_pred_lower = quantile(cum_activity, 0.025),
            cum_pred_upper = quantile(cum_activity, 0.975),
            cum_pred = quantile(cum_activity, 0.5))



plot_data <- sim_data %>% 
  filter(activity != 6) %>% 
  mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))) %>% 
  group_by(series, age_group, survey_round, activity) %>% 
  summarise(cum_pred_lower = 1-quantile(cum_activity, 0.025),
            cum_pred_upper = 1-quantile(cum_activity, 0.975),
            cum_pred = 1-quantile(cum_activity, 0.5)) %>% 
  left_join(fit_data %>% 
              mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))) %>% 
              rename(age_group = part_age_group) %>% 
              group_by(series, age_group, survey_round, activity) %>% 
              count %>% 
              group_by(series, age_group, survey_round) %>% 
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
  geom_segment(aes(linetype = "fitted")) +
  scale_linetype_manual('', values = "solid")+
  geom_point(aes(x = date_round, y = sum_obs, col = factor(activity), shape = "observed")) +
  scale_shape_manual('', values = 1) +
  scale_x_date(breaks = "month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.99),
                     expand = c(0, 0)) +
  labs(x = NULL,
       y = "fraction of participants",
       colour = "number of contacts",
       fill = "number of contacts") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1)) +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 1),
        legend.position = "bottom") +
  facet_grid(rows = vars(age_group),
             cols = vars(series),
             scales = "free_x",
             space = "free")

ggsave(filename = paste0("./figures/prediction_studypopulation_cat6.png"), width = 10, height = 6, dpi = 300)

