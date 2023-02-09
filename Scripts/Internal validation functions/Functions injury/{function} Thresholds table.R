
# Thresholds table function

# libraries
library(tidyverse)
library(pROC)
library(glue)
library(qwraps2)

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

Threshold_table_rule_in <- function(data, model_preds, folder) {

  preds <- data[[model_preds]]
  label <- data$label
  label <- factor(label, levels = c("0", "1"))
  
  # ROC curve
  rocobj <-
    pROC::roc(
      response = label,
      predictor = preds,
      levels = levels(as.factor(label)),
      direction = "<",
      percent = TRUE,
      quiet = FALSE
    )
  
  df_metrics <- matrix(ncol = 11, nrow = 0) %>% data.frame()
  colnames(df_metrics) <-
    c(
      "threshold",
      "npv",
      "ppv",
      "sensitivity",
      "specificity",
      "TN",
      "FN",
      "TP",
      "FP",
      "Rule_out",
      "Rule_in"
    )
  
  # thresholds <- rocobj$thresholds[-c(1:10, length(rocobj$thresholds))]# Remove -INF, the first small values & INF

  # # For now
  # thresholds <- thresholds[thresholds <= 0.9] # to avoid errors in the confusion matrix and run faster
  # thresholds <- thresholds[thresholds >= 0.2] # to avoid errors in the confusion matrix and run faster
  # thresholds <- unique(round(thresholds, 3))
  
  # NEW
  thresholds <- seq(0.20,0.80,0.01)
  
  for (thres in thresholds) {
    
    preds_thres <- as.numeric(preds >= thres)
    preds_thres <- factor(preds_thres, levels = c("0", "1"))
    
    
    CM <-
      qwraps2::confusion_matrix(preds_thres,
                                label,
                                positive = "1",
                                # boot = TRUE,
                                boot = FALSE, # change to save time, EST doesn't change
                                boot_samples = 1000L)
    
    
    CM_metrics <- data.frame(
      threshold = thres*100,
      # npv = CM$stats["NPV", "Boot Est"]*100,
      # ppv = CM$stats["PPV", "Boot Est"]*100,
      # sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      # specificity = CM$stats["Specificity", "Boot Est"]*100,
      npv = round_DD(CM$stats["NPV", "Est"]*100, 1),
      ppv = round_DD(CM$stats["PPV", "Est"]*100, 1),
      sensitivity = round_DD(CM$stats["Sensitivity", "Est"]*100, 1),
      specificity = round_DD(CM$stats["Specificity", "Est"]*100, 1),
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
      Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    df_metrics <- df_metrics %>%
      rbind(CM_metrics)
  }
  
  write_rds(df_metrics,
          glue::glue("{folder}/{model_preds}_thresholds_table"))
  
} # end of function


# END ~~~