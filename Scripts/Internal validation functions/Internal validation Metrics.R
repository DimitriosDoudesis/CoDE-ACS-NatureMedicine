library(tidyverse)

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

Metrics_function_internal <-
  function(Data_out, metrics_out, title_out, 
           Data_in, metrics_in, title_in,
           npv_value, 
           sensitivity_value,
           ppv_value, 
           specificity_value){
    ### ~~~ AUC ~~~ ###
    
    # out
    Data_roc <-
      Data_out %>% dplyr::select(label_index, title_out) %>% dplyr::mutate(label_index = factor(label_index)) %>% dplyr::filter(complete.cases(.))
    
    
    # ROC curve
    roc_obj <-
      pROC::roc(
        response = Data_roc$label_index,
        predictor = Data_roc[[title_out]],
        levels = levels(as.factor(Data_out$label_index)),
        direction = "<",
        percent = TRUE,
        quiet = FALSE
      )
    
    auc_ci <- pROC::ci.auc(roc_obj) / 100
    
    AUC_model_out <- paste0(
      formatC(round_DD(auc_ci[2], 3), format = "f", digits = 3),
      " (",
      formatC(round_DD(auc_ci[1], 3), format = "f", digits = 3),
      "-",
      formatC(round_DD(auc_ci[3], 3), format = "f", digits = 3),
      ")"
    )
    
    #in
    Data_roc <-
      Data_in %>% dplyr::select(label_index, title_in) %>% dplyr::mutate(label_index = factor(label_index)) %>% dplyr::filter(complete.cases(.))
    
    
    # ROC curve
    roc_obj <-
      pROC::roc(
        response = Data_roc$label_index,
        predictor = Data_roc[[title_in]],
        levels = levels(as.factor(Data_in$label_index)),
        direction = "<",
        percent = TRUE,
        quiet = FALSE
      )
    
    auc_ci <- pROC::ci.auc(roc_obj) / 100
    
    AUC_model_in <- paste0(
      formatC(round_DD(auc_ci[2], 3), format = "f", digits = 3),
      " (",
      formatC(round_DD(auc_ci[1], 3), format = "f", digits = 3),
      "-",
      formatC(round_DD(auc_ci[3], 3), format = "f", digits = 3),
      ")"
    )
    
    ### ~~~ Brier score ~~~ ###
    #out
    BS_data <-
      Data_out %>% dplyr::select(label_index, title_out) %>% dplyr::mutate(label_index = label_index) %>% dplyr::filter(complete.cases(.))
    
    BS_data_out <- formatC(round_DD(DescTools::BrierScore(resp = BS_data$label_index,
                                             pred = BS_data[[title_out]]), 3), format = "f", digits = 3)
    
    #in
    BS_data <-
      Data_in %>% dplyr::select(label_index, title_in) %>% dplyr::mutate(label_index = label_index) %>% dplyr::filter(complete.cases(.))
    
    BS_data_in <- formatC(round_DD(DescTools::BrierScore(resp = BS_data$label_index,
                                                pred = BS_data[[title_in]]), 3), format = "f", digits = 3)
    
    ###~~~ Set threshold ~~~ ###
    
      low_thres <-
        metrics_out %>%
        dplyr::filter(npv >= npv_value & sensitivity >= sensitivity_value) %>% #  & sensitivity = NA
        data.table::last() %>%
        dplyr::select(threshold) %>%
        round_DD(0) %>%
        unlist()/100
      
      high_thres <-
        metrics_in %>%
        dplyr::filter(ppv >= ppv_value & specificity >= specificity_value) %>%
        data.table::first() %>%
        dplyr::select(threshold) %>%
        round_DD(0) %>%
        unlist()/100 
      
      
    ### ~~~ Low threshold ~~~ ###
    
    
    data_metrics <- data.frame()
    
    
      data_temp <- Data_out
      
      # preds
      preds <- data_temp[title_out]
      preds <- as.numeric(preds >= low_thres)
      preds <- factor(preds, levels = c("0", "1"))
      label <- data_temp$label_index
      label <- factor(label, levels = c("0", "1"))
      
      set.seed(123)
      CM <-
        qwraps2::confusion_matrix(preds,
                                  label,
                                  positive = "1",
                                  boot = TRUE,
                                  boot_samples = 1000L)
      
      CM_metrics <- data.frame(
        threshold = round_DD(low_thres*100, 0),
        npv = round_DD(CM$stats["NPV", "Boot Est"]*100, 1),
        npv_lcl = round_DD(CM$stats["NPV", "Boot LCL"]*100, 1),
        npv_ucl = round_DD(CM$stats["NPV", "Boot UCL"]*100, 1),
        ppv = round_DD(CM$stats["PPV", "Boot Est"]*100, 1),
        ppv_lcl = round_DD(CM$stats["PPV", "Boot LCL"]*100, 1),
        ppv_ucl = round_DD(CM$stats["PPV", "Boot UCL"]*100, 1),
        sensitivity = round_DD(CM$stats["Sensitivity", "Boot Est"]*100, 1),
        sensitivity_lcl = round_DD(CM$stats["Sensitivity", "Boot LCL"]*100, 1),
        sensitivity_ucl = round_DD(CM$stats["Sensitivity", "Boot UCL"]*100, 1),
        specificity = round_DD(CM$stats["Specificity", "Boot Est"]*100, 1),
        specificity_lcl = round_DD(CM$stats["Specificity", "Boot LCL"]*100, 1),
        specificity_ucl = round_DD(CM$stats["Specificity", "Boot UCL"]*100, 1),
        TN = CM$cells$true_negatives,
        FN = CM$cells$false_negatives,
        TP = CM$cells$true_positives,
        FP = CM$cells$false_positives,
        Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
        Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
        AUC = AUC_model_out,
        BS = BS_data_out
      )
      
      data_metrics <- data_metrics %>%
        rbind(CM_metrics)
      
  
    ### ~~~ High threshold ~~~ ###
    
    
      data_temp <- Data_in 
      
      # preds
      preds <- data_temp[title_in]
      preds <- as.numeric(preds >= high_thres)
      preds <- factor(preds, levels = c("0", "1"))
      label <- data_temp$label_index
      label <- factor(label, levels = c("0", "1"))
      
      set.seed(123)
      CM <-
        qwraps2::confusion_matrix(preds,
                                  label,
                                  positive = "1",
                                  boot = TRUE,
                                  boot_samples = 1000L)
      
      CM_metrics <- data.frame(
        threshold = round_DD(high_thres*100, 0),
        npv = round_DD(CM$stats["NPV", "Boot Est"]*100, 1),
        npv_lcl = round_DD(CM$stats["NPV", "Boot LCL"]*100, 1),
        npv_ucl = round_DD(CM$stats["NPV", "Boot UCL"]*100, 1),
        ppv = round_DD(CM$stats["PPV", "Boot Est"]*100, 1),
        ppv_lcl = round_DD(CM$stats["PPV", "Boot LCL"]*100, 1),
        ppv_ucl = round_DD(CM$stats["PPV", "Boot UCL"]*100, 1),
        sensitivity = round_DD(CM$stats["Sensitivity", "Boot Est"]*100, 1),
        sensitivity_lcl = round_DD(CM$stats["Sensitivity", "Boot LCL"]*100, 1),
        sensitivity_ucl = round_DD(CM$stats["Sensitivity", "Boot UCL"]*100, 1),
        specificity = round_DD(CM$stats["Specificity", "Boot Est"]*100, 1),
        specificity_lcl = round_DD(CM$stats["Specificity", "Boot LCL"]*100, 1),
        specificity_ucl = round_DD(CM$stats["Specificity", "Boot UCL"]*100, 1),
        TN = CM$cells$true_negatives,
        FN = CM$cells$false_negatives,
        TP = CM$cells$true_positives,
        FP = CM$cells$false_positives,
        Rule_out = round_DD(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
        Rule_in = round_DD(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
        AUC = AUC_model_in,
        BS = BS_data_in
      )
      
      data_metrics <- data_metrics %>%
        rbind(CM_metrics)
      
      


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
data_metrics$sensitivy_ci <- CI(data_metrics$sensitivity, data_metrics$sensitivity_lcl, data_metrics$sensitivity_ucl)
data_metrics$specificity_ci <- CI(data_metrics$specificity, data_metrics$specificity_lcl, data_metrics$specificity_ucl)

    metrics_table <- data.frame()
    
    low <- cbind.data.frame(
      data_metrics$threshold,
      data_metrics$AUC,
      data_metrics$BS,
      data_metrics$sensitivy_ci,
      data_metrics$npv_ci,
      " - ",
      " - ",
      data_metrics$TN,
      data_metrics$FN,
      data_metrics$TP,
      data_metrics$FP,
      data_metrics$Rule_out,
      " - "
      )
    
    colnames(low) <-
      c(
        "Threshold",
        "AUC",
        "Brier score",
        "Sensitivity (%)",
        "NPV (%)",
        "Specificity (%)",
        "PPV (%)",
        "TN",
        "FN",
        "TP",
        "FP",
        "Rule out (%)",
        "Rule in (%)"
      )
    
    low <- low[1,] %>% 
      mutate(across(everything(),as.character))
    
    high <- cbind.data.frame(
      data_metrics$threshold,
      data_metrics$AUC,
      data_metrics$BS,
      " - ",
      " - ",
      data_metrics$specificity_ci,
      data_metrics$ppv_ci,
      data_metrics$TN,
      data_metrics$FN,
      data_metrics$TP,
      data_metrics$FP,
      " - ",
      data_metrics$Rule_in
    )
    
    colnames(high) <-
      c(
        "Threshold",
        "AUC",
        "Brier score",
        "Sensitivity (%)",
        "NPV (%)",
        "Specificity (%)",
        "PPV (%)",
        "TN",
        "FN",
        "TP",
        "FP",
        "Rule out (%)",
        "Rule in (%)"
      )
    
    high <- high[2,] %>% 
      mutate(across(everything(),as.character))
    
    metrics_table <- rbind.data.frame(low, high)
    
    
    rownames(metrics_table) <- NULL
    
    # change the order to be consistent
    metrics_table <- metrics_table[,c(1,2,3,5,4, 7, 6, 8:13)]
    
    return(metrics_table)
    
  } #end of function 
