
sce <- read_rds("data-raw/scenarios.rds")
net <- read_rds("data-raw/network.rds")
sen <- read_rds("data-raw/sensitivity.rds")
dis <- read_rds("data-raw/distancing.rds")
dis2 <- read_rds("data-raw/distancing2.rds")
out <- read_rds("data-raw/outside.rds")
tes <- read_rds("data-raw/testing.rds")

#Scenarios
sce %>% filter(week == 9) %>%
  group_by(intervention) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.75)/468,
            Lcases = quantile(cumcases, 0.25)/468)

sce %>% filter(week == 3) %>%
  group_by(intervention) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine, 0.75)/468,
            Lcases = quantile(weekly_quarantine, 0.25)/468)

sce %>% filter(week == 9) %>%
  group_by(intervention) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine, 0.75)/468,
            Lcases = quantile(weekly_quarantine, 0.25)/468)


#testing
tes %>% filter(week == 9) %>%
  group_by(intervention,testing) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.75)/468,
            Lcases = quantile(cumcases, 0.25)/468)

tes %>% filter(week == 3) %>%
  group_by(intervention,testing) %>%
  summarise(medcases = median(weekly_tests)/468,
            Ucases = quantile(weekly_tests,0.75)/468,
            Lcases = quantile(weekly_tests, 0.25)/468)


#distancing
dis %>% filter(week == 9) %>%
  group_by(intervention,distancing) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.75)/468,
            Lcases = quantile(cumcases, 0.25)/468)

dis %>% filter(week == 3) %>%
  group_by(intervention,distancing) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine,0.75)/468,
            Lcases = quantile(weekly_quarantine, 0.25)/468)
