
 
library(tidyverse)
library(glue)
library(qwraps2)
library(forestplot)
library(berryFunctions)
library(Cairo)

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Function External validation -----------------------------------------------------

External_validation_metric_function_classic <-
  function(dataset,
           model_name,
           model_name_sex) {
    
    
    
    data_metrics <- data.frame()
    
    # # rule out less than 2
    # 
    # preds <- dataset[model_name]
    # label <- dataset$label_index
    # label <- factor(label, levels = c("0", "1"))
    # preds <- as.numeric(preds >= 2)
    # preds <- factor(preds, levels = c("0", "1"))
    # 
    # 
    # # New addition CHECK AGAIN
    # set.seed(1234)
    # CM <-
    #   qwraps2::confusion_matrix(preds,
    #                             label,
    #                             positive = "1",
    #                             boot = FALSE, # CHANGE HERE
    #                             boot_samples = 1000L)
    # 
    # CM_metrics <- data.frame(
    #   threshold = "<2 ng/L",
    #   # npv = CM$stats["NPV", "Boot Est"]*100,
    #   # npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
    #   # npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
    #   npv = CM$stats["NPV", "Est"]*100,
    #   npv_lcl = CM$stats["NPV", "LCL"]*100,
    #   npv_ucl = CM$stats["NPV", "UCL"]*100,
    #   # ppv = CM$stats["PPV", "Est"]*100,
    #   # ppv_lcl = CM$stats["PPV", "LCL"]*100,
    #   # ppv_ucl = CM$stats["PPV", "UCL"]*100,
    #   sensitivity = CM$stats["Sensitivity", "Est"]*100,
    #   sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
    #   sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
    #   ppv = NA,
    #   ppv_lcl = NA,
    #   ppv_ucl = NA,
    #   # sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
    #   # sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
    #   # sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
    #   specificity = NA,
    #   specificity_lcl = NA,
    #   specificity_ucl = NA,
    #   # specificity = CM$stats["Specificity", "Est"]*100,
    #   # specificity_lcl = CM$stats["Specificity", "LCL"]*100,
    #   # specificity_ucl = CM$stats["Specificity", "UCL"]*100,
    #   TN = CM$cells$true_negatives,
    #   FN = CM$cells$false_negatives,
    #   TP = CM$cells$true_positives,
    #   FP = CM$cells$false_positives,
    #   Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
    #   Rule_in = NA
    # )
    # 
    # data_metrics <- data_metrics %>%
    #   rbind(CM_metrics)
    
    
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
      npv = round_DD(CM$stats["NPV", "Est"]*100, 1),
      npv_lcl = round_DD(CM$stats["NPV", "LCL"]*100, 1),
      npv_ucl = round_DD(CM$stats["NPV", "UCL"]*100, 1),
      sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
      sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
      sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
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
      Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
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
      sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
      sensitivity_lcl = round_DD(CM$stats["Sensitivity", "LCL"]*100, 1),
      sensitivity_ucl = round_DD(CM$stats["Sensitivity", "UCL"]*100, 1),
      ppv = round_DD(CM$stats["PPV", "Est"]*100, 1),
      ppv_lcl = round_DD(CM$stats["PPV", "LCL"]*100, 1),
      ppv_ucl = round_DD(CM$stats["PPV", "UCL"]*100, 1),
      specificity = round_DD(CM$stats["Specificity", "Est"]*100, 1),
      specificity_lcl = round_DD(CM$stats["Specificity", "LCL"]*100, 1),
      specificity_ucl = round_DD(CM$stats["Specificity", "UCL"]*100, 1),
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = NA,
      Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
    
    
    # # rule in 64
    # 
    # preds <- dataset[model_name]
    # label <- dataset$label_index
    # label <- factor(label, levels = c("0", "1"))
    # preds <- as.numeric(preds >= 64) # checked and it is above or equal
    # preds <- factor(preds, levels = c("0", "1"))
    # 
    # 
    # set.seed(1234)
    # CM <-
    #   qwraps2::confusion_matrix(preds,
    #                             label,
    #                             positive = "1",
    #                             boot = FALSE, # CHANGE HERE
    #                             boot_samples = 1000L)
    # 
    # CM_metrics <- data.frame(
    #   threshold = "64 ng/L",
    #   npv = NA,
    #   npv_lcl = NA,
    #   npv_ucl = NA,
    #   # ppv = CM$stats["PPV", "Boot Est"]*100,
    #   # ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
    #   # ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
    #   # sensitivity = NA,
    #   # sensitivity_lcl = NA,
    #   # sensitivity_ucl = NA,
    #   sensitivity = CM$stats["Sensitivity", "Est"]*100,
    #   sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
    #   sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
    #   # specificity = CM$stats["Specificity", "Boot Est"]*100,
    #   # specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
    #   # specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
    #   # npv = CM$stats["NPV", "Est"]*100,
    #   # npv_lcl = CM$stats["NPV", "LCL"]*100,
    #   # npv_ucl = CM$stats["NPV", "UCL"]*100,
    #   ppv = CM$stats["PPV", "Est"]*100,
    #   ppv_lcl = CM$stats["PPV", "LCL"]*100,
    #   ppv_ucl = CM$stats["PPV", "UCL"]*100,
    #   specificity = CM$stats["Specificity", "Est"]*100,
    #   specificity_lcl = CM$stats["Specificity", "LCL"]*100,
    #   specificity_ucl = CM$stats["Specificity", "UCL"]*100,
    #   TN = CM$cells$true_negatives,
    #   FN = CM$cells$false_negatives,
    #   TP = CM$cells$true_positives,
    #   FP = CM$cells$false_positives,
    #   Rule_out = NA,
    #   Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
    # )
    # 
    # data_metrics <- data_metrics %>%
    #   rbind(CM_metrics)
    
    # Round numbers
    data_metrics <- data_metrics %>% 
      mutate_if(is.numeric, round_DD, 1)
    
    
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
  
  
  Dataset_external <- read_rds("Outputs/Data/Dataset_external_cleaned") %>% as_tibble()



  External_validation_metric_function_classic(dataset = Dataset_external,
                                      model_name = "first_troponin",
                                      model_name_sex = "above_99th_presentation")
  
  
  table(Dataset_external$ischemia)
  table(Dataset_external$hours_since_symptoms > 3)
  
  External_validation_metric_function_classic(dataset = Dataset_external %>% filter(ischemia == 0 & hours_since_symptoms > 3),
                                              model_name = "first_troponin",
                                              model_name_sex = "above_99th_presentation")
  
  
  
  source("Scripts/Rule in/0x - Functions/{function} Forest plot.R")
  
  source("Scripts/Rule out/0x - Functions/{function} Forest plot.R")
  
  
  dataset <-
    read_rds("Outputs/Data/Dataset_external_cleaned") %>% as_tibble()
  
  
  # forest_plot_rule_out_external(
  #   data = dataset %>% rename(label = label_index),
  #   predictions = "first_troponin",
  #   thres = 2,
  #   title = "thres_2_classic",
  #   folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
  # )
  
  
  # Forest plot in the correct population
  forest_plot_rule_out_external(
    # data = dataset %>% rename(label = label_index),
    data = dataset %>% rename(label = label_index) %>% filter(ischemia == 0 & hours_since_symptoms > 3),
    predictions = "first_troponin",
    thres = 5,
    title = "thres_5_noischaemia_3hourssymptoms",
    folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
  )
  
  
  forest_plot_rule_in_external(
    data = dataset %>% rename(label = label_index),
    predictions = "above_99th_presentation",
    thres = 0.5,
    title = "thres_99th_classic",
    folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
  )
  
  # forest_plot_rule_in_external(
  #   data = dataset %>% rename(label = label_index),
  #   predictions = "first_troponin",
  #   thres = 64,
  #   title = "thres_64_classic",
  #   folder = glue::glue("Outputs/Plots/Forest plots/Thresholds/External")
  # )
  
}

# # END
