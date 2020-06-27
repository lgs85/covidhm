###########################################
#SIMULATE TESTING
###########################################

library(covidhm)
library(dplyr)
library(purrr)


# Set number of replicates ------------------------------------------------

nreps <- 1000


# Load in network and convert to pairwise list ----------------------------


## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")


# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, net = haslemere, n.sim = nreps, num.initial.cases = 1,prop.asym=0.4,
                         prop.ascertain = 0.9, cap_max_days = 69, delay_shape = 1, delay_scale = 1.4,
                         R = 0.8,presymrate = 0.2, sensitivity = "high",
                         testing = "realistic", outside = 0.001)

# Simulate scenarios ------------------------------------------------------

res1 <- scenario_sim2(scenario = "primary_quarantine", cap_max_tests = 5)
res2 <- scenario_sim2(scenario = "primary_quarantine", cap_max_tests = 25)
res3 <- scenario_sim2(scenario = "primary_quarantine", cap_max_tests = 50)
res4 <- scenario_sim2(scenario = "secondary_quarantine", cap_max_tests = 5)
res5 <- scenario_sim2(scenario = "secondary_quarantine", cap_max_tests = 25)
res6 <- scenario_sim2(scenario = "secondary_quarantine", cap_max_tests = 50)


# Bind together results and save output -----------------------------------

res <- bind_rows(res1,res2,res3,res4,res5,res6) %>%
  mutate(intervention = rep(c("Primary tracing","Secondary tracing"),
                            each = nrow(res1)*3),
         testing = rep(rep(c("5 tests per day","25 tests per day","50 tests per day"),
                           each = nrow(res1)),2))


saveRDS(res, file = "data-raw/testing.rds")

