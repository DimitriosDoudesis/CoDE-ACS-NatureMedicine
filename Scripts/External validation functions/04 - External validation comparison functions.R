

library(tidyverse) 
library(glue)
library(qwraps2)
library(forestplot)
library(berryFunctions)
library(Cairo)


# Function External validation -----------------------------------------------------

External_validation_metric_function_classic <-
  function(dataset,
           model_name,
           model_name_sex) {
    
    data_metrics <- data.frame()
    
    # rule out less than 2
    
    preds <- dataset[model_name]
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 2)
    preds <- factor(preds, levels = c("0", "1"))
    
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "<2 ng/L",
      npv = CM$stats["NPV", "Est"]*100,
      npv_lcl = CM$stats["NPV", "LCL"]*100,
      npv_ucl = CM$stats["NPV", "UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      ppv = NA,
      ppv_lcl = NA,
      ppv_ucl = NA,
      specificity = NA,
      specificity_lcl = NA,
      specificity_ucl = NA,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_in = NA
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    
    # rule out less than 5
    
    preds <- dataset[model_name]
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 5)
    preds <- factor(preds, levels = c("0", "1"))
    
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "<5 ng/L",
      npv = CM$stats["NPV", "Est"]*100,
      npv_lcl = CM$stats["NPV", "LCL"]*100,
      npv_ucl = CM$stats["NPV", "UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      ppv = NA,
      ppv_lcl = NA,
      ppv_ucl = NA,
      specificity = NA,
      specificity_lcl = NA,
      specificity_ucl = NA,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_in = NA
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    # rule in sex specific
    
    preds <- dataset[model_name_sex]
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 0.5)
    preds <- factor(preds, levels = c("0", "1"))
    
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "99th cen",
      npv = NA,
      npv_lcl = NA,
      npv_ucl = NA,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      ppv = CM$stats["PPV", "Est"]*100,
      ppv_lcl = CM$stats["PPV", "LCL"]*100,
      ppv_ucl = CM$stats["PPV", "UCL"]*100,
      specificity = CM$stats["Specificity", "Est"]*100,
      specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = NA,
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    
    
    # rule in 64
    
    preds <- dataset[model_name]
    label <- dataset$label_index
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= 64)
    preds <- factor(preds, levels = c("0", "1"))
    
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE,
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      threshold = "64 ng/L",
      npv = NA,
      npv_lcl = NA,
      npv_ucl = NA,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      ppv = CM$stats["PPV", "Est"]*100,
      ppv_lcl = CM$stats["PPV", "LCL"]*100,
      ppv_ucl = CM$stats["PPV", "UCL"]*100,
      specificity = CM$stats["Specificity", "Est"]*100,
      specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = NA,
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    # Round numbers
    data_metrics <- data_metrics %>% 
      mutate_if(is.numeric, round, 1)
    

    # function for creating CIs
    CI <- function(ce, ll, ul){
      paste0(
        formatC(ce, format = "f", digits = 1),
        " (",
        formatC(ll, format = "f", digits = 1),
        "-",
        formatC(ul, format = "f", digits = 1),
        ")")
    }
    
    
    data_metrics$npv_ci <- CI(data_metrics$npv, data_metrics$npv_lcl, data_metrics$npv_ucl)
    data_metrics$ppv_ci <- CI(data_metrics$ppv, data_metrics$ppv_lcl, data_metrics$ppv_ucl)
    data_metrics$sensititivy_ci <- CI(data_metrics$sensitivity, data_metrics$sensitivity_lcl, data_metrics$sensitivity_ucl)
    data_metrics$specificity_ci <- CI(data_metrics$specificity, data_metrics$specificity_lcl, data_metrics$specificity_ucl)
    
    
    data_metrics <- data.frame(
      Threshold = data_metrics$threshold,
      NPV = data_metrics$npv_ci,
      Sensitivity = data_metrics$sensititivy_ci,
      PPV = data_metrics$ppv_ci,
      Specificity = data_metrics$specificity_ci,
      TN = data_metrics$TN,
      FN = data_metrics$FN,
      TP =data_metrics$TP,
      FP = data_metrics$FP,
      `Rule out` = data_metrics$Rule_out,
      `Rule_in` = data_metrics$Rule_in
    )
    
    return(data_metrics)
    
    
  }


# Forest plots ------------------------------------------------------------

run = FALSE

if (run == TRUE){
  
source("Scripts/Rule in/0x - Functions/{function} Forest plot.R")

source("Scripts/Rule out/0x - Functions/{function} Forest plot.R")

  
dataset <-
  read_rds("Outputs/Data/Dataset_external_cleaned") %>% as_tibble()

  
forest_plot_rule_out_external(
  data = dataset %>% rename(label = label_index),
  predictions = "first_troponin",
  thres = 2,
  title = "thres_2_classic",
  folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
)

forest_plot_rule_out_external(
  data = dataset %>% rename(label = label_index),
  predictions = "first_troponin",
  thres = 5,
  title = "thres_5_classic",
  folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
)


forest_plot_rule_in_external(
  data = dataset %>% rename(label = label_index),
  predictions = "above_99th_presentation",
  thres = 0.5,
  title = "thres_99th_classic",
  folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
)

forest_plot_rule_in_external(
  data = dataset %>% rename(label = label_index),
  predictions = "first_troponin",
  thres = 64,
  title = "thres_64_classic",
  folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
)

}

# # END
