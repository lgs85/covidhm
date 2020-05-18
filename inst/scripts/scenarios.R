###########################################
#SIMULATE CONTACT TRACING SCENARIOS
###########################################

library(covidhm)
library(dplyr)
library(purrr)

# Set number of replicates ------------------------------------------------

nreps <- 1000


## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")


# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, net = haslemere, n.sim = nreps, num.initial.cases = 1,prop.asym=0.4,
                         prop.ascertain = 0.8, cap_max_days = 100,
                         delay_shape = 1, delay_scale = 1.4,R = 1,presymrate = 0.4,
                         outside = 0.001, sensitivity = "high", testing = "none",cap_max_tests = Inf)

# Simulate scenarios ------------------------------------------------------

res1 <- scenario_sim2(scenario = "nothing")
res2 <- scenario_sim2(scenario = "isolation")
res3 <- scenario_sim2(scenario = "primary_quarantine")
res4 <- scenario_sim2(scenario = "secondary_quarantine")


# Bind together results and save output -----------------------------------

res <- bind_rows(res1,res2,res3,res4) %>%
  mutate(intervention = rep(c("Nothing",
                                "Case isolation",
                                "Primary tracing",
                                "Secondary tracing"),
                                each = nrow(res1)))


saveRDS(res, file = "data-raw/scenarios.rds")
