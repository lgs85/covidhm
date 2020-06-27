###########################################
#SIMULATE EPIDIMIC WITH SOCIAL DISTANCING - supplementary method
###########################################

library(covidhm)
library(dplyr)
library(purrr)
library(igraph)

# Set number of replicates ------------------------------------------------

nreps <- 1000

# Load Haselmere network --------------------------------------------------

load("data-raw/am_list.RData")
load("data-raw/amcontacts.RData")

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")



# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, num.initial.cases = 1, prop.asym=0.4,
                         prop.ascertain = 0.9, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 0.8,presymrate = 0.2,
                         outside = 0.001, sensitivity = "high", testing = "none")




#20% reduction in contacts
for(i in 1:nreps)
{

  m <- dist1_func(am_list[[1]],"matrix",0.2) #see aux_functions.R for this script
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, n.sim = 1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, n.sim = 1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, n.sim = 1, scenario = "secondary_quarantine")

  res1$sim <- i
  res2$sim <- i
  res3$sim <- i

  if(i == 1)
  {
    out1 <- res1
    out2 <- res2
    out3 <- res3
  } else
  {
    out1 <- bind_rows(out1,res1)
    out2 <- bind_rows(out2,res2)
    out3 <- bind_rows(out3,res3)
  }
}



#40% reduction in contacts
for(i in 1:nreps){

  m <- dist1_func(am_list[[1]],"matrix",0.4) #see aux_functions.R for this script
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, n.sim = 1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, n.sim = 1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, n.sim = 1, scenario = "secondary_quarantine")

  res1$sim <- i
  res2$sim <- i
  res3$sim <- i

  if(i == 1)
  {
    out4 <- res1
    out5 <- res2
    out6 <- res3
  } else
  {
    out4 <- bind_rows(out4,res1)
    out5 <- bind_rows(out5,res2)
    out6 <- bind_rows(out6,res3)
  }
}



#60% reduction in contacts
for(i in 1:nreps){

  m <- dist1_func(am_list[[1]],"matrix",0.6) #see aux_functions.R for this script
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, n.sim = 1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, n.sim = 1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, n.sim = 1, scenario = "secondary_quarantine")

  res1$sim <- i
  res2$sim <- i
  res3$sim <- i

  if(i == 1)
  {
    out7 <- res1
    out8 <- res2
    out9 <- res3
  } else
  {
    out7 <- bind_rows(out7,res1)
    out8 <- bind_rows(out8,res2)
    out9 <- bind_rows(out9,res3)
  }
}


# No distancing -----------------------------------------------------------

out10 <- scenario_sim2(net = haslemere, n.sim = nreps, scenario = "nothing")
out11 <- scenario_sim2(net = haslemere, n.sim = nreps, scenario = "primary_quarantine")
out12 <- scenario_sim2(net = haslemere, n.sim = nreps, scenario = "secondary_quarantine")


# bind and write output ---------------------------------------------------

res <- bind_rows(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12) %>%
  mutate(distancing = rep(c("30% reduction","60% reduction","90% reduction","0% reduction"),
                          each = nrow(out1)*3),
         intervention = rep(rep(c("Nothing", "Primary tracing","Secondary tracing"),
                                each = nrow(out1)),4))

saveRDS(res,"data-raw/distancing2.rds")


