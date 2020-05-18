###########################################
#SIMULATE INFECTION ON NULL NETWORKS
###########################################

library(covidhm)
library(dplyr)
library(purrr)
library(igraph)


# Set number of replicate simulations -------------------------------------

nreps <- 1000


# Load raw haslemere networks -------------------------------------------------

load("data-raw/am_list.RData")

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")




# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, n.sim = 1, num.initial.cases = 1, prop.asym=0.4,
                         prop.ascertain = 0.8, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 1,presymrate = 0.4,
                         outside = 0.001, sensitivity = "high", testing = "none",cap_max_tests = Inf)



# Simulate infection on simulated networks --------------------------------

#Rand
for(i in 1:nreps){

  m <- network_null(am_list[[1]],returns = "matrix",null = "edge")
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, scenario = "secondary_quarantine")

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


#Degcont
for(i in 1:nreps){

  m <- network_null(am_list[[1]],returns = "matrix",null = "deg")
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, scenario = "secondary_quarantine")

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



#Lattice
for(i in 1:nreps){

  m <- network_null(am_list[[1]],returns = "matrix",null = "latt")
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, scenario = "secondary_quarantine")

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



#Cluster
for(i in 1:nreps){

  m <- network_null(am_list[[1]],returns = "matrix",null = "clust")
  net1 <- format_network(m)

  res1 <- scenario_sim2(net = net1, scenario = "nothing")
  res2 <- scenario_sim2(net = net1, scenario = "primary_quarantine")
  res3 <- scenario_sim2(net = net1, scenario = "secondary_quarantine")

  res1$sim <- i
  res2$sim <- i
  res3$sim <- i

  if(i == 1)
  {
    out10 <- res1
    out11 <- res2
    out12 <- res3
  } else
  {
    out10 <- bind_rows(out10,res1)
    out11 <- bind_rows(out11,res2)
    out12 <- bind_rows(out12,res3)
  }
}




# bind and write output ---------------------------------------------------

res <- bind_rows(out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12) %>%
  mutate(network = rep(c("rand","degcont","lattice","cluster"),
                       each = nrow(out1)*3),
         intervention = rep(rep(c("Nothing", "Primary tracing","Secondary tracing"),
                            each = nrow(out1)),4))

saveRDS(res,"data-raw/network.rds")
