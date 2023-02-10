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
# Predict contacts general population
#
# - drawing parameter sets from fit objects
# - generating new data to resemble general population
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


# define list of functions to interpolate vaccination coverage over time per age group
vaccination_coverage_extended <- vaccination_coverage %>% 
  bind_rows(expand_grid(age_group = levels(vaccination_coverage$age_group),
                        tibble(date = c(as.Date("2021-01-06"), as.Date("2021-12-31")),
                               vacc_coverage = c(0, 1)))) %>% 
  arrange(date, age_group) %>% 
  mutate(age_group = factor(age_group, levels = levels(vaccination_coverage$age_group)))


vaccination_coverage_function <- lapply(levels(vaccination_coverage_extended$age_group),
                                        function(i) vaccination_coverage_extended %>% 
                                          filter(age_group == i) %>% 
                                          select(vacc_coverage, date) %>% 
                                          approxfun)

names(vaccination_coverage_function) <- levels(vaccination_coverage_extended$age_group)



pred_data <- tibble()

for(set in names(fit)) {
  print(paste(set, now()))
  ser <- str_split(set, pattern = "_") %>% unlist %>% pluck(1)
  age_group <- str_split(set, pattern = "_") %>% unlist %>% pluck(2)
  
  data <- fit[[set]]$data
  
  beta <- rmvn(
    n = 200,
    mu = coef(fit[[set]]),
    V = vcov(fit[[set]]))
  
  tmp_data <- 
    lapply(1:200,
           function(i) {
             new_data <- generate_new_data(data = data, 
                                           pop_data = population_data_2021,
                                           high_risk_population = high_risk, 
                                           vacc_cov_function = vaccination_coverage_function)
             
             new_data %>% 
               mutate(pred = predict(
                 object = list_modify(fit[[set]], coefficients = beta[i,]),
                 newdata = new_data,
                 type = "response")) %>% 
               group_by(date) %>% 
               summarise(series = ser,
                         age_group = age_group,
                         sim = i,
                         activity = 1:6,
                         frac_activity = colMeans(pred),
                         cum_activity = cumsum(frac_activity)) %>% 
               filter(activity != 6)
             }
    ) %>% 
    bind_rows
  
  pred_data <- bind_rows(pred_data, tmp_data %>% 
                           group_by(series, age_group, date, activity) %>% 
                           summarise(cum_pred_lower = quantile(cum_activity, 0.025),
                                     cum_pred_upper = quantile(cum_activity, 0.975),
                                     cum_pred = quantile(cum_activity, 0.5)))
}

saveRDS(pred_data, "./results/pred_generalpopulation_cat6.rds")

######### Compare predicted and pico contacts of general population ############


pred_data <- readRDS("./results/pred_generalpopulation_cat6.rds")

ggplot(data = pred_data %>% 
         mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))),
       aes(x = date, y = 1-cum_pred, col = factor(activity), fill = factor(activity))) +
  geom_rect(data = dates_of_holidays,
            aes(xmin = holiday_start - 0.5, xmax = holiday_end + 0.5, ymin = 0, ymax = 0.99),
            fill = "grey",
            alpha = 0.3,
            inherit.aes = FALSE) +
  geom_ribbon(aes(ymin = 1-cum_pred_lower, ymax = 1-cum_pred_upper),
              alpha = 0.3, col = NA) +
  geom_line() +
  geom_pointrange(data = pico_data %>% 
               filter(activity < 6) %>% 
               mutate(activity = factor(activity, levels = 1:5, labels = c("> 0", "> 1", "> 2", "> 4", "> 9"))),
             aes(y = 1-mean,
                 ymin = 1-lower,
                 ymax = 1-upper,
                 shape = "PiCo survey"),
             size = 0.4,
             col = rgb(0.3,0.3,0.3),
             stroke = 0.6) +
  scale_shape_manual('', values = 21) +
  scale_x_date(breaks = "month",
               date_labels = "%b",
               expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.99),
                     expand = c(0, 0)) +
  labs(x = NULL,
       y = "Fraction of population",
       colour = "Number of contacts",
       fill = "Number of contacts") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1)) +
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


ggsave(filename = paste0("./figures/prediction_generalpopulation_cat6.pdf"), width = 7, height = 5)



