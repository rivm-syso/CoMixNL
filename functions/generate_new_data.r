# Generate new data to resemble general population in terms of
# - age distribution
# - vaccination coverage over time
# - elevated risk fraction
# for each day during the study period
# to use in predict function

generate_new_data <- function(data, pop_data, high_risk_population, vacc_cov_function) {
  
  part_ids <- data %>% filter(!duplicated(part_id)) %>% pull(part_id)
  start_date <- data %>% pull(date) %>% min
  end_date <- data %>% pull(date) %>% max
  start_age <- c(0, 18, 65)[which.min(abs(c(0, 18, 65) - min(data$part_age)))]
  end_age <- c(17, 64, 99)[which.min(abs(c(17, 64, 99) - max(data$part_age)))]
  
  weights <- pop_data %>% 
    group_by(age) %>% 
    summarise(population = sum(population)) %>% 
    filter(age >= start_age,
           age <= end_age) %>% 
    pull(population)
  
  population <- tibble(age = sample(start_age:end_age, size = length(part_ids), prob = weights, replace = TRUE),
                       part_id = sample(part_ids), replace = TRUE) %>% 
    left_join(high_risk_population) %>% 
    mutate(part_elevated_risk = rbernoulli(n = n(), p = high_risk),
           age_group = cut(age, breaks = c(0, 12, 18, 25, seq(35, 65, 10), Inf), right = FALSE, include_lowest = TRUE)) %>% 
    rowwise %>% 
    mutate(vacc_date = vacc_cov_function[[age_group]](runif(n = 1)),
           vacc_date = as.Date(vacc_date, origin = as.Date("1970-01-01"))) %>% 
    rename(part_age = age) %>% 
    ungroup
  
  new_data <- expand_grid(tibble(date = seq.Date(start_date, end_date, by = "day")),
                          population) %>% 
    mutate(part_vacc = vacc_date < date,
           part_round = 1,
           date_trans = as.integer(date - as.Date("2020-04-16")),
           weekend = weekdays(date) %in% c("Saturday", "Sunday"),
           holiday = date %in% schoolholidays | date %in% holidays) %>% 
    select(part_id, part_age, date, date_trans, part_round, part_elevated_risk, part_vacc, weekend, holiday)
}
