#######################################################
#SIMULATE CONTACT TRACING SCENARIOS WITH DENSE MATRICES
#######################################################

library(covidhm)
library(dplyr)
library(purrr)

load("data-raw/am_7 and am_16.RData")

net7 <- format_network(am_7)
net16 <- format_network(am_16)

# Set number of replicates ------------------------------------------------

nreps <- 1000


## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")


# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, n.sim = nreps, num.initial.cases = 1,prop.asym=0.4,
                         prop.ascertain = 0.9, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 0.8,presymrate = 0.2,
                         outside = 0.001, sensitivity = "high", testing = "none",cap_max_tests = Inf)

# Simulate scenarios ------------------------------------------------------

res1 <- scenario_sim2(net = net7, scenario = "nothing")
res2 <- scenario_sim2(net = net7, scenario = "isolation")
res3 <- scenario_sim2(net = net7, scenario = "primary_quarantine")
res4 <- scenario_sim2(net = net7, scenario = "secondary_quarantine")
res5 <- scenario_sim2(net = net16, scenario = "nothing")
res6 <- scenario_sim2(net = net16, scenario = "isolation")
res7 <- scenario_sim2(net = net16, scenario = "primary_quarantine")
res8 <- scenario_sim2(net = net16, scenario = "secondary_quarantine")


# Bind together results and save output -----------------------------------

res <- bind_rows(res1,res2,res3,res4) %>%
  mutate(intervention = rep(c("Nothing",
                              "Case isolation",
                              "Primary tracing",
                              "Secondary tracing"),
                            each = nrow(res1)))


saveRDS(res, file = "data-raw/scenarios7.rds")

res <- bind_rows(res5,res6,res7,res8) %>%
  mutate(intervention = rep(c("Nothing",
                              "Case isolation",
                              "Primary tracing",
                              "Secondary tracing"),
                            each = nrow(res1)))


saveRDS(res, file = "data-raw/scenarios16.rds")

