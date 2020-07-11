###########################################
#SENSITIVITY TESTING OF EPIDEMIC MODEL
###########################################

library(covidhm)
library(tidyr)
library(tibble)
library(dplyr)
library(furrr)

# Make the log file
logs <- file.path("sensitivity_log.txt")
con <- file(logs, open = "wt")
# # Send Output to log
sink(con)
sink(con, type = "message")

load("data-raw/am_list.RData")

# Set number of replicates ------------------------------------------------

nreps = 1000


# Parameter sweep for sensitivity testing ---------------------------------

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Medium", "Short"),
    delay_shape = c(1.651524, 1),
    delay_scale = c(4.287786, 1.4)
  )),
  presymrate = c(0.2,0.4),
  prop.asym = c(0.2, 0.4),
  control_effectiveness = c(0.3, 0.6, 0.9),
  num.initial.cases = c(1, 5),
  scenario = c("primary_quarantine", "secondary_quarantine"),
  R = c(0.5,0.8,2)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenarioID = 1:dplyr::n())

## Parameterise fixed paramters
sim_with_params <- purrr::partial(scenario_sim,
                                  net = am_list[[1]],
                                  cap_max_days = 69,
                                  outside = 0.001,
                                  testing = FALSE,
                                  distancing = 0,
                                  cap_max_tests = Inf)

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")

## Run paramter sweep
sweep_results <- parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = nreps,
                                         show_progress = TRUE)

saveRDS(sweep_results, file = "data-raw/sensitivity.rds")

sink(type = "message")
sink()
