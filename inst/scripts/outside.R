###########################################
#SIMULATE OUTSIDE INFECTION
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
                         prop.ascertain = 0.8, cap_max_days = 69, delay_shape = 1, delay_scale = 1.4,
                         R = 1,presymrate = 0.4, sensitivity = "high",
                         testing = "none",cap_max_tests = Inf)

# Simulate scenarios ------------------------------------------------------

res1 <- scenario_sim2(scenario = "nothing", outside = 0.0001)
res2 <- scenario_sim2(scenario = "nothing", outside = 0.001)
res3 <- scenario_sim2(scenario = "nothing", outside = 0.005)
res4 <- scenario_sim2(scenario = "nothing", outside = 0.01)
res5 <- scenario_sim2(scenario = "primary_quarantine", outside = 0.0001)
res6 <- scenario_sim2(scenario = "primary_quarantine", outside = 0.001)
res7 <- scenario_sim2(scenario = "primary_quarantine", outside = 0.005)
res8 <- scenario_sim2(scenario = "primary_quarantine", outside = 0.01)
res9 <- scenario_sim2(scenario = "secondary_quarantine", outside = 0.0001)
res10 <- scenario_sim2(scenario = "secondary_quarantine", outside = 0.001)
res11 <- scenario_sim2(scenario = "secondary_quarantine", outside = 0.005)
res12 <- scenario_sim2(scenario = "secondary_quarantine", outside = 0.01)


# Bind together results and save output -----------------------------------

res <- bind_rows(res1,res2,res3,res4,res5,res6,res7,res8,res9,res10,res11,res12) %>%
  mutate(intervention = rep(c("Nothing", "Primary tracing","Secondary tracing"),
                       each = nrow(res1)*4),
         outside = rep(rep(c("outside = 0.0001","outside = 0.001","outside = 0.005","outside = 0.01"),
                                each = nrow(res1)),3))


saveRDS(res, file = "data-raw/outside.rds")

