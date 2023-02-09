

library(tidyverse)

High_STEACS_NPV <- read_rds("Outputs/Data/High_STEACS_NPV_presentation") 

High_STEACS_PPV <- read_rds("Outputs/Data/High_STEACS_PPV_presentation")

Dataset_external <- read_rds("Outputs/Data/Dataset_external_cleaned")


source("Scripts/Rule out/0x - Functions/{function} Forest plot.R")

source("Scripts/Rule in/0x - Functions/{function} Forest plot.R")


# Internal ----------------------------------------------------------------

# Rule out

forest_plot_rule_out_internal(
  data = High_STEACS_NPV %>% rename(label = label_index),
  predictions = "first_troponin",
  thres = 5,
  title = "threshold_5_internal_all",
  folder = "Outputs/Plots/Manuscript"
)


forest_plot_rule_in_99th(
  data = High_STEACS_PPV %>% rename(label = label_index) %>% filter(above_99th_presentation == 1),
  predictions = "above_99th_presentation",
  thres = 0.5,
  title = "threshold_99th_internal",
  folder = "Outputs/Plots/Manuscript"
)


# External ----------------------------------------------------------------

forest_plot_rule_out_external(
  data = Dataset_external %>% rename(label = label_index) %>% filter(above_99th_presentation == 0),
  predictions = "first_troponin",
  thres = 5,
  title = "threshold_5_external_all",
  folder = "Outputs/Plots/Manuscript/"
)

forest_plot_rule_out_external(
  data = Dataset_external %>% rename(label = label_index) %>% 
    filter(ischemia == 0 & hours_since_symptoms > 3) %>% filter(above_99th_presentation == 0),
  predictions = "first_troponin",
  thres = 5,
  title = "threshold_5_external_subset",
  folder = "Outputs/Plots/Manuscript/"
)


# this is just to complete the supplement table 3
forest_plot_rule_in_external(
  data = Dataset_external %>% rename(label = label_index) %>% 
    filter(ischemia == 0 & hours_since_symptoms > 3) %>% filter(above_99th_presentation == 0),
  predictions = "first_troponin",
  thres = 5,
  title = "threshold_5_external_subset",
  folder = "Outputs/Plots/Manuscript/"
)


forest_plot_rule_in_external(
  data = Dataset_external %>% rename(label = label_index) %>% filter(above_99th_presentation == 1),
  predictions = "above_99th_presentation",
  thres = 0.5,
  title = "threshold_99th_external",
  folder = "Outputs/Plots/Manuscript"
)

# END

