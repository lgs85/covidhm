library(tidyverse)
library(cowplot)
library(covidhm)

# Load in data ------------------------------------------------------------

sce <- read_rds("data-raw/scenarios.rds")
net <- read_rds("data-raw/network.rds")
sen <- read_rds("data-raw/sensitivity.rds")
dis <- read_rds("data-raw/distancing.rds")
dis2 <- read_rds("data-raw/distancing2.rds")
out <- read_rds("data-raw/outside.rds")
tes <- read_rds("data-raw/testing.rds")

load("data-raw/am_list.RData")





# Figure 1 - network examples ---------------------------------------------

m <- am_list[[1]]

plot_network2 <- purrr::partial(plot_network,
                                am = m,
                                num.initial.cases = 1,
                                prop.asym = 0.4,
                                delay_shape =  1,
                                delay_scale = 1.4,
                                prop.ascertain = 0.8,
                                presymrate = 0.4,
                                R = 1,
                                outside = 0.001,
                                sensitivity = "high",
                                testing = "none",
                                s = 332)

pdf("inst/plots/Figure_1.pdf",
    width = 8,
    height = 16)

layout(matrix(c(1,1,1,1,1,1,
                2,2,3,3,4,4,
                5,5,5,5,5,5,
                6,6,6,7,7,7,
                8,8,8,8,8,8,
                9,9,9,10,10,10,
                11,11,11,11,11,11,
                12,12,12,13,13,13),
             8,6,byrow = TRUE,
             ),widths = c(1,1,2,2,1,1),
       heights = c(0.1,1,0.1,1,0.1,1,0.1,1))

par(mar = c(1,0,0,0))

plot.new()
text(0.5,0.5,"Starting network",cex = 1.4)

plot.new()
legend("center",
       pch = c(19,19,19),
       col = c("darkgrey",
               "indianred1",
               "pink"),
       legend = c("Not infected",
                  "Infected",
                  "Recovered"),
       bty = "n",
       cex = 1.4)

par(mar = c(1,0,0,0))

plot_network2(day = 1,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"A",cex = 1.6)

plot.new()
legend("left",
       pch = c(NA,NA,0),
       lty = c(1,1,NA),
       col = c("deepskyblue1",
               "indianred1",
               "black"),
       legend = c("Contacts",
                  "Infections",
                  "Isolated/quarantined"),
       bty = "n",
       cex = 1.4)


plot.new()
text(0.5,0.5,"Day 10",cex = 1.4)

par(mar = c(1,0,0,0))

plot_network2(day = 10,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"B",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 10,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"E",cex = 1.6)


plot.new()
text(0.5,0.5,"Day 20",cex = 1.4)

par(mar = c(1,0,0,0))

plot_network2(day = 20,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"C",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 20,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"F",cex = 1.6)


plot.new()
text(0.5,0.5,"Day 70",cex = 1.4)

par(mar = c(1,0,0,0))

plot_network2(day = 70,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"D",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 70,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"G",cex = 1.6)

dev.off()




# Figure 2 - intervention scenarios ---------------------------------------

sce_figa <- sce  %>%
  mutate(intervention = recode(intervention, Nothing = "No control")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing"))) %>%
  case_plot(nrow = 1)+
  theme(legend.position = "top")


plot_network2 <- purrr::partial(plot_network,
                                am = am_list[[1]],
                                day = 20,
                                num.initial.cases = 1,
                                prop.asym = 0.4,
                                delay_shape =  1,
                                delay_scale = 1.4,
                                prop.ascertain = 0.8,
                                presymrate = 0.4,
                                R = 1,
                                outside = 0.001,
                                sensitivity = "high",
                                testing = "none",
                                s = 332)

sce_figb <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = FALSE,
                quarantine = FALSE,
                tracing = FALSE,
                secondary = FALSE)
  }

sce_figc <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = FALSE,
                tracing = FALSE,
                secondary = FALSE)
  }

sce_figd <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = TRUE,
                tracing = TRUE,
                secondary = FALSE)
  }

sce_fige <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = TRUE,
                tracing = TRUE,
                secondary = TRUE)
  }


sce_fig <- plot_grid(sce_figa,
                     plot_grid(NULL,sce_figb,sce_figc,sce_figd,sce_fige,nrow = 1,
                               rel_widths = c(0.25,1,1,1,1)),
                     nrow = 2,
                     rel_heights = c(1,0.7),
                     labels = "AUTO")


pdf("inst/plots/Figure_2.pdf",
    width = 12,
    height = 8)
sce_fig
dev.off()





# Figure 3 - null networks ------------------------------------------------

net_figa <- net  %>%
  mutate(network = recode(network, lattice = "Lattice null",
                          degcont = "Degree null",
                          rand = "Edge null",
                          cluster = "Cluster null")) %>%
  mutate(intervention = recode(intervention, Nothing = "No control")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing")),
         network = factor(network, levels = c("Edge null",
                                              "Degree null",
                                              "Lattice null",
                                              "Cluster null"))) %>%
  case_plot(facet = "grid",gridvar = "network") +
  theme(legend.position = "top")


plot_network2 <- purrr::partial(plot_network,
                                day = 20,
                                num.initial.cases = 1,
                                prop.asym = 0.4,
                                delay_shape =  1,
                                delay_scale = 1.4,
                                prop.ascertain = 0.8,
                                presymrate = 0.4,
                                R = 1,
                                outside = 0.001,
                                sensitivity = "high",
                                testing = "none",
                                isolation = TRUE,
                                quarantine = TRUE,
                                tracing = TRUE,
                                secondary = TRUE,
                                s = 333)

net_exa <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "edge")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exb <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "deg")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exc <- function(){
  par(mar = c(1,0,0,0))
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "latt")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exd <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "clust")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}


net_fig <- plot_grid(net_figa,
                     plot_grid(NULL,net_exa,net_exb,net_exc,net_exd,nrow = 1,
                               rel_widths = c(0.25,1,1,1,1)),
                     nrow = 2,
                     rel_heights = c(1,0.3),
                     labels = "AUTO")


pdf("inst/plots/Figure_3.pdf",
    width = 12,
    height = 12)
net_fig
dev.off()





# Figure 4 - test and release ---------------------------------------------

tes_fig <- tes %>%
  bind_rows(sce %>%
              filter(intervention %in% c("Primary tracing",
                                         "Secondary tracing")) %>%
              mutate(testing = "No testing")) %>%
  mutate(testing = factor(testing, levels = c("No testing",
                                              "5 tests per day",
                                              "25 tests per day",
                                              "50 tests per day"))) %>%
  case_plot(facet = "grid",gridvar = "testing",testing = TRUE)

pdf("inst/plots/Figure_4.pdf",
    width = 12,
    height = 6)
tes_fig
dev.off()





# Figure 5 - distancing and testing ---------------------------------------

dis_figa <- dis  %>%
  mutate(intervention = recode(intervention, Nothing = "No control")) %>%
  case_plot(facet = "grid", gridvar = "distancing", testing = TRUE)+
  theme(legend.position = "top")

plot_network2 <- purrr::partial(plot_network,
                                day = 20,
                                num.initial.cases = 1,
                                prop.asym = 0.4,
                                delay_shape =  1,
                                delay_scale = 1.4,
                                prop.ascertain = 0.8,
                                presymrate = 0.4,
                                R = 1,
                                outside = 0.001,
                                sensitivity = "high",
                                testing = "none",
                                isolation = TRUE,
                                quarantine = TRUE,
                                tracing = TRUE,
                                secondary = TRUE,
                                s = 333)

net_exa <- function(){
  par(mar = c(1,0,0,0))
  plot_network2(am = am_list[[1]])
}

net_exb <- function(){
  net1 <- dist1_func(am_list[[1]],returns = "matrix",dist.prop = 0.2)
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exc <- function(){
  par(mar = c(1,0,0,0))
  net1 <- dist1_func(am_list[[1]],returns = "matrix",dist.prop = 0.4)
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exd <- function(){
  net1 <- dist1_func(am_list[[1]],returns = "matrix",dist.prop = 0.6)
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}


dis_fig <- plot_grid(dis_figa,
                     plot_grid(NULL,net_exa,net_exb,net_exc,net_exd,nrow = 1,
                               rel_widths = c(0.25,1,1,1,1)),
                     nrow = 2,
                     rel_heights = c(1,0.3),
                     labels = "AUTO")



pdf("inst/plots/Figure_5.pdf",
    width = 12,
    height = 12)
dis_fig
dev.off()





# Figure S1 - null network examples ---------------------------------------

#Done by Josh Firth


# Figure S2 - distancing examples -----------------------------------------

#Done by Josh Firth


# Figure S3 - network distance thresholds ---------------------------------

#Done by Josh Firth


# Figure S4 - network edge weight options ---------------------------------

#Done by Josh Firth




# Figure S5 - proportion asymptomatic -------------------------------------


filtered <- sen %>%
  filter(delay == "Short",
         presymrate == 0.4,
         R == 6.5,
         num.initial.cases == 1,
         sensitivity == "high")

ll <- filtered %>%
  group_by(scenarioID) %>%
  pull(sims[[1]])

names(ll) <- filtered$scenarioID

dd <- bind_rows(ll, .id = "scenarioID") %>%
  mutate(scenarioID = as.numeric(scenarioID)) %>%
  left_join(select(filtered,scenarioID,control_effectiveness,prop.asym,scenario),by = "scenarioID")


asym_figa <- dd %>%
  rename(intervention = scenario) %>%
  filter(prop.asym == 0.2) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")

legend <- get_legend(asym_figa)
asym_figa <- asym_figa +
  theme(legend.position = "none")+
  ggtitle("Proportion asymptomatic = 0.2")

asym_figb <- dd %>%
  rename(intervention = scenario) %>%
  filter(prop.asym == 0.4) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")+
  theme(legend.position = "none")+
  ggtitle("Proportion asymptomatic = 0.4")


asym_fig <- plot_grid(asym_figa,asym_figb, legend,rel_widths = c(1,1,0.5),nrow = 1)


#Write to pdf
pdf("inst/plots/Figure_S5.pdf",
    width = 12,
    height = 5)

asym_fig
dev.off()



# Figure S6 - theta -------------------------------------------------------

filtered <- sen %>%
  filter(delay == "Short",
         prop.asym == 0.4,
         R == 6.5,
         num.initial.cases == 1,
         sensitivity == "high")

ll <- filtered %>%
  group_by(scenarioID) %>%
  pull(sims[[1]])

names(ll) <- filtered$scenarioID

dd <- bind_rows(ll, .id = "scenarioID") %>%
  mutate(scenarioID = as.numeric(scenarioID)) %>%
  left_join(select(filtered,scenarioID,control_effectiveness,presymrate,scenario),by = "scenarioID")

theta_figa <- dd %>%
  rename(intervention = scenario) %>%
  filter(presymrate == 0.2) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")

legend <- get_legend(theta_figa)
theta_figa <- theta_figa +
  theme(legend.position = "none")+
  ggtitle("Theta = 0.2")

theta_figb <- dd %>%
  rename(intervention = scenario) %>%
  filter(presymrate == 0.4) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")+
  theme(legend.position = "none")+
  ggtitle("Theta = 0.4")


theta_fig <- plot_grid(theta_figa,theta_figb,legend, rel_widths = c(1,1,0.5),nrow = 1)

#Write to pdf
pdf("inst/plots/Figure_S6.pdf",
    width = 12,
    height = 5)

theta_fig
dev.off()



# Figure S7 - delay -------------------------------------------------------


filtered <- sen %>%
  filter(presymrate == 0.4,
         prop.asym == 0.4,
         R == 6.5,
         num.initial.cases == 1,
         sensitivity == "high")

ll <- filtered %>%
  group_by(scenarioID) %>%
  pull(sims[[1]])

names(ll) <- filtered$scenarioID

dd <- bind_rows(ll, .id = "scenarioID") %>%
  mutate(scenarioID = as.numeric(scenarioID)) %>%
  left_join(select(filtered,scenarioID,control_effectiveness,delay,scenario),by = "scenarioID")


delay_figa <- dd %>%
  rename(intervention = scenario) %>%
  filter(delay == "Short") %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")

legend <- get_legend(delay_figa)
delay_figa <- delay_figa +
  theme(legend.position = "none")+
  ggtitle("Short delay")

delay_figb <- dd %>%
  rename(intervention = scenario) %>%
  filter(delay == "Medium") %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")+
  theme(legend.position = "none")+
  ggtitle("Medium delay")


delay_fig <- plot_grid(delay_figa,delay_figb,legend, rel_widths = c(1,1,0.5),nrow = 1)

#Write to pdf
pdf("inst/plots/Figure_S7.pdf",
    width = 12,
    height = 5)

delay_fig
dev.off()




# Figure S8 - initial cases ----------------------------------------------


filtered <- sen %>%
  filter(delay == "Short",
         presymrate == 0.4,
         prop.asym == 0.4,
         R == 6.5,
         sensitivity == "high")

ll <- filtered %>%
  group_by(scenarioID) %>%
  pull(sims[[1]])

names(ll) <- filtered$scenarioID

dd <- bind_rows(ll,.id = "scenarioID") %>%
  mutate(scenarioID = as.numeric(scenarioID)) %>%
  left_join(select(filtered,scenarioID,control_effectiveness,num.initial.cases,scenario),by = "scenarioID")

initial_case_figa <- dd %>%
  rename(intervention = scenario) %>%
  filter(num.initial.cases == 1) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")

legend <- get_legend(initial_case_figa)
initial_case_figa <- initial_case_figa +
  theme(legend.position = "none")+
  ggtitle("Initial cases = 1")

initial_case_figb <- dd %>%
  rename(intervention = scenario) %>%
  filter(num.initial.cases == 5) %>%
  case_plot(facet = "grid",gridvar = "control_effectiveness")+
  theme(legend.position = "none")+
  ggtitle("Initial cases = 5")


initial_case_fig <- plot_grid(initial_case_figa,initial_case_figb, legend,rel_widths = c(1,1,0.5),nrow = 1)

#Write to pdf
pdf("inst/plots/Figure_S8.pdf",
    width = 12,
    height = 5)

initial_case_fig

dev.off()




# Figure S9 - outside infection ------------------------------------------

out_fig <- out  %>%
  case_plot(facet = "grid", gridvar = "outside")


pdf("inst/plots/Figure_S9.pdf",
    width = 12,
    height = 8)
out_fig
dev.off()




# Figure S10 - distancing with advanced method ----------------------------


dis2_fig <- dis2  %>%
  case_plot(facet = "grid", gridvar = "distancing", testing = TRUE)


pdf("inst/plots/Figure_S10.pdf",
    width =12,
    height = 8)
dis2_fig
dev.off()






