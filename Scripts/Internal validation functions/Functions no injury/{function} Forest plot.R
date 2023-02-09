

# Forest plot function

# libraries
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


forest_plot_rule_out_internal <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)

  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Cerebrovascular disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    Overall = Overall
  )
  
  
  # Part 2: Subgroup meta analysis ------------------------------------------
  
  
  data_metrics <- data.frame()
  
  for (one_subgroup in names(subgroups)) {
    one_subgroup_data = subgroups[[one_subgroup]]
    
    preds <- one_subgroup_data[predictions]
    label <- one_subgroup_data$label
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= thres)
    preds <- factor(preds, levels = c("0", "1"))
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = TRUE, # CHANGE HERE
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      subgroup = as.character(one_subgroup),
      npv = CM$stats["NPV", "Boot Est"]*100,
      npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
      npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
      ppv = CM$stats["PPV", "Boot Est"]*100,
      ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
      ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
      specificity = CM$stats["Specificity", "Boot Est"]*100,
      specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  

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
  
  
  
  rule_out <- data_metrics[nrow(data_metrics), "Rule_out"]
  
  
  
  # Part 3: Forest plot -----------------------------------------------------
  
  
  sub_text <- c(
    "Subgroups",
    NA,
    "Age",
    "  <65 years",
    "  \u226565 years",
    NA,
    "Sex",
    "  Male",
    "  Female",
    NA,
    "  Yes",
    "  No",
    NA,
    "Cerebrovascular disease",
    "  Yes",
    "  No",
    NA,
    "Diabetes",
    "  Yes",
    "  No",
    NA,
    "eGFR",
    "  <60 ml/min",
    "  \u226560 ml/min",
    NA,
    "Ischemic electrocardiogram",
    "  Yes",
    "  No",
    NA,
    "Time from onset of symptoms",
    "  \u22643 hours",
    "  >3 hours",
    NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  data_metrics$subgroup 
  
  ### NPV ###
  
  col_npv <- cbind(
    c("True negative",
      NA,
      NA,
      data_metrics$TN),
    c("False negative",
      NA,
      NA,
      data_metrics$FN),
    c(
      "Negative predictive value (95% CI)",
      NA,
      NA,
      data_metrics$npv_ci
    )
  )
  
  
  sub_text_npv <- cbind(sub_text, col_npv)
  
  Cairo(
    width = 20,
    height = 14,
    file = glue("{folder}/NPV plot {title}_{rule_out}.png"),
    type = "png",
    bg = "white",
    units = "in",
    dpi = 1200
  )
  
  p <- forestplot(
    sub_text_npv,
    mean = c(NA, NA, NA, data_metrics$npv),
    lower = c(NA, NA, NA, data_metrics$npv_lcl),
    upper = c(NA, NA, NA, data_metrics$npv_ucl),
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F),8), T),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.75),
      ticks = gpar(cex = 1.75),
      xlab = gpar(cex = 1.5),
      title = gpar(cex = 1.75)
    ),
    col = fpColors(
      box = "black",
      lines = "black",
      zero = "gray50"
    ),
    zero = data_metrics[nrow(data_metrics), "npv"],
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(96, 101),
    xticks = seq(96,100, 0.5)
  )
  
  
  plot(p)
  
  
  invisible(dev.off())
  
  
} # end of function



forest_plot_rule_out_internal_perc <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Cerebrovascular disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    Overall = Overall
  )
  
  
  # Part 2: Subgroup meta analysis ------------------------------------------
  
  
  data_metrics <- data.frame()
  
  for (one_subgroup in names(subgroups)) {
    one_subgroup_data = subgroups[[one_subgroup]]
    
    preds <- one_subgroup_data[predictions]
    label <- one_subgroup_data$label
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= thres)
    preds <- factor(preds, levels = c("0", "1"))
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = TRUE, # CHANGE HERE
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      subgroup = as.character(one_subgroup),
      npv = CM$stats["NPV", "Boot Est"]*100,
      npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
      npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
      ppv = CM$stats["PPV", "Boot Est"]*100,
      ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
      ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
      specificity = CM$stats["Specificity", "Boot Est"]*100,
      specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_out_N = CM$cells$negatives,
      Rule_in_N = CM$cells$positives,
      Total_N = sum(CM$cells$negatives + CM$cells$positives)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
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
  
  data_metrics$Rule_out_subgroup <- paste0(
    formatC(data_metrics$Rule_out, format = "f", digits = 0),
    "% [",
    data_metrics$Rule_out_N,
    "/",
    data_metrics$Total_N,
    "]")
  
  rule_out <- data_metrics[nrow(data_metrics), "Rule_out"]
  
  # Part 3: Forest plot -----------------------------------------------------
  
  sub_text <- c(
    "Subgroups",
    NA,
    "Age",
    "  <65 years",
    "  \u226565 years",
    NA,
    "Sex",
    "  Male",
    "  Female",
    NA,
    "Ischemic heart disease",
    "  Yes",
    "  No",
    NA,
    "Cerebrovascular disease",
    "  Yes",
    "  No",
    NA,
    "Diabetes",
    "  Yes",
    "  No",
    NA,
    "eGFR",
    "  <60 ml/min",
    "  \u226560 ml/min",
    NA,
    "Ischemic electrocardiogram",
    "  Yes",
    "  No",
    NA,
    "Time from onset of symptoms",
    "  \u22643 hours",
    "  >3 hours",
    NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  data_metrics$subgroup 
  
  ### NPV ###
  
  col_npv <- cbind(
    c("True negative",
      NA,
      NA,
      data_metrics$TN),
    c("False negative",
      NA,
      NA,
      data_metrics$FN),
    c(
      "Negative predictive value (95% CI)",
      NA,
      NA,
      data_metrics$npv_ci
    ),
    c(
      "Proportion ruled out",
      NA,
      NA,
      data_metrics$Rule_out_subgroup
    )
  )
  
  
  sub_text_npv <- cbind(sub_text, col_npv)
  
  Cairo(
    width = 23,
    height = 14,
    file = glue("{folder}/NPV plot {title}_{rule_out}.png"),
    type = "png",
    #pointsize=12,
    bg = "white",
    units = "in",
    dpi = 1200
  )
  
  p <- forestplot(
    sub_text_npv,
    mean = c(NA, NA, NA, data_metrics$npv),
    lower = c(NA, NA, NA, data_metrics$npv_lcl),
    upper = c(NA, NA, NA, data_metrics$npv_ucl),
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F),8), T),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.75),
      ticks = gpar(cex = 1.75),
      xlab = gpar(cex = 1.5),
      title = gpar(cex = 1.75)
    ),
    col = fpColors(
      box = "black",
      lines = "black",
      zero = "gray50"
    ),
    zero = data_metrics[nrow(data_metrics), "npv"], #99.5,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(96, 101),
    xticks = seq(96,100, 0.5)
  )
  
  print(p)
  
  invisible(dev.off())
  
} # end of function



forest_plot_rule_out_external <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  # Smoking
  Current_or_ex_smoker <-
    data %>% dplyr::filter(smoking == 1)
  Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # Hypertension
  Hypertension <- data %>% dplyr::filter(hypertension == 1)
  No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # Hyperlipidemia
  Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # Cerebrovascular disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    Current_or_ex_smoker = Current_or_ex_smoker,
    Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    Hypertension = Hypertension,
    No_hypertension = No_hypertension,
    Hyperlipidemia = Hyperlipidemia,
    No_hyperlipidemia = No_hyperlipidemia,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    Overall = Overall
  )
  
  
  # Part 2: Subgroup meta analysis ------------------------------------------
  
  
  data_metrics <- data.frame()
  
  for (one_subgroup in names(subgroups)) {
    one_subgroup_data = subgroups[[one_subgroup]]
    
    preds <- one_subgroup_data[predictions]
    label <- one_subgroup_data$label
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= thres)
    preds <- factor(preds, levels = c("0", "1"))
    
    # New addition CHECK AGAIN
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE, # CHANGE HERE
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      subgroup = as.character(one_subgroup),
      npv = CM$stats["NPV", "Est"]*100,
      npv_lcl = CM$stats["NPV", "LCL"]*100,
      npv_ucl = CM$stats["NPV", "UCL"]*100,
      ppv = CM$stats["PPV", "Est"]*100,
      ppv_lcl = CM$stats["PPV", "LCL"]*100,
      ppv_ucl = CM$stats["PPV", "UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      specificity = CM$stats["Specificity", "Est"]*100,
      specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 1),
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 1)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
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
  
  rule_out <- data_metrics[nrow(data_metrics), "Rule_out"]
  
  # Part 3: Forest plot -----------------------------------------------------
  
  
  sub_text <- c(
    "Subgroups",
    NA,
    "Age",
    "  <65 years",
    "  \u226565 years",
    NA,
    "Sex",
    "  Male",
    "  Female",
    NA,
    "Smoking",
    "  Current or ex-smoker",
    "  Non-smoker",
    NA,
    "Ischemic heart disease",
    "  Yes",
    "  No",
    NA,
    "Diabetes",
    "  Yes",
    "  No",
    NA,
    "Hypertension",
    "  Yes",
    "  No",
    NA,
    "Hyperlipidemia",
    "  Yes",
    "  No",
    NA,
    "Cerebrovascular disease",
    "  Yes",
    "  No",
    NA,
    "Ischemic electrocardiogram",
    "  Yes",
    "  No",
    NA,
    "eGFR",
    "  <60 ml/min",
    "  \u226560 ml/min",
    NA,
    "Time from onset of symptoms",
    "  \u22643 hours",
    "  >3 hours",
    NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31, 32, 35, 36, 39, 40, 43))
  

  ### NPV ###
  
  col_npv <- cbind(
    c("True negative",
      NA,
      NA,
      data_metrics$TN),
    c("False negative",
      NA,
      NA,
      data_metrics$FN),
    c(
      "Negative predictive value (95% CI)",
      NA,
      NA,
      data_metrics$npv_ci
    )
  )
  
  
  sub_text_npv <- cbind(sub_text, col_npv)
  
  Cairo(
    width = 20,
    height = 14,
    file = glue("{folder}/NPV plot {title}_{rule_out}.png"),
    type = "png",
    #pointsize=12,
    bg = "white",
    units = "in",
    dpi = 1200
  )
  
  p <- forestplot(
    sub_text_npv,
    mean = c(NA, NA, NA, data_metrics$npv),
    lower = c(NA, NA, NA, data_metrics$npv_lcl),
    upper = c(NA, NA, NA, data_metrics$npv_ucl),
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F), 11), TRUE),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.75),
      ticks = gpar(cex = 1.75),
      xlab = gpar(cex = 1.5),
      title = gpar(cex = 1.75)
    ),
    col = fpColors(
      box = "black",
      lines = "black",
      zero = "gray50"
    ),
    zero =  data_metrics[nrow(data_metrics), "npv"], #99.5,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(96, 101),
    xticks = seq(96,100, 0.5)
  )
  
  print(p)
  
  invisible(dev.off())
  
} # end of function



forest_plot_rule_out_external_perc <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  # Cerebrovascular disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  
  
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    Overall = Overall
  )
  
  
  # Part 2: Subgroup meta analysis ------------------------------------------
  
  
  data_metrics <- data.frame()
  
  for (one_subgroup in names(subgroups)) {
    one_subgroup_data = subgroups[[one_subgroup]]
    
    preds <- one_subgroup_data[predictions]
    label <- one_subgroup_data$label
    label <- factor(label, levels = c("0", "1"))
    preds <- as.numeric(preds >= thres)
    preds <- factor(preds, levels = c("0", "1"))
    
    set.seed(1234)
    CM <-
      qwraps2::confusion_matrix(preds,
                                label,
                                positive = "1",
                                boot = FALSE, # CHANGE HERE
                                boot_samples = 1000L)
    
    CM_metrics <- data.frame(
      subgroup = as.character(one_subgroup),
      npv = CM$stats["NPV", "Est"]*100,
      npv_lcl = CM$stats["NPV", "LCL"]*100,
      npv_ucl = CM$stats["NPV", "UCL"]*100,
      ppv = CM$stats["PPV", "Est"]*100,
      ppv_lcl = CM$stats["PPV", "LCL"]*100,
      ppv_ucl = CM$stats["PPV", "UCL"]*100,
      sensitivity = CM$stats["Sensitivity", "Est"]*100,
      sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      specificity = CM$stats["Specificity", "Est"]*100,
      specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      specificity_ucl = CM$stats["Specificity", "UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
      Rule_out_N = CM$cells$negatives,
      Rule_in_N = CM$cells$positives,
      Total_N = sum(CM$cells$negatives + CM$cells$positives)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
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
  
  
  data_metrics$Rule_out_subgroup <- paste0(
    formatC(data_metrics$Rule_out, format = "f", digits = 0),
    "% [",
    data_metrics$Rule_out_N,
    "/",
    data_metrics$Total_N,
    "]")

  rule_out <- data_metrics[nrow(data_metrics), "Rule_out"]
  
  # Part 3: Forest plot -----------------------------------------------------
  
  
  sub_text <- c(
    "Subgroups",
    NA,
    "Age",
    "  <65 years",
    "  \u226565 years",
    NA,
    "Sex",
    "  Male",
    "  Female",
    NA,
    "Ischemic heart disease",
    "  Yes",
    "  No",
    NA,
    "Cerebrovascular disease",
    "  Yes",
    "  No",
    NA,
    "Diabetes",
    "  Yes",
    "  No",
    NA,
    "eGFR",
    "  <60 ml/min",
    "  \u226560 ml/min",
    NA,
    "Ischemic electrocardiogram",
    "  Yes",
    "  No",
    NA,
    "Time from onset of symptoms",
    "  \u22643 hours",
    "  >3 hours",
    NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  ### NPV ###
  
  col_npv <- cbind(
    
    c("True negative",
      NA,
      NA,
      data_metrics$TN),
    c("False negative",
      NA,
      NA,
      data_metrics$FN),
    c(
      "Negative predictive value\n(95% CI)",
      NA,
      NA,
      data_metrics$npv_ci
    ),
    c(
      "Proportion ruled out",
      NA,
      NA,
      data_metrics$Rule_out_subgroup
    )
  )
  
  
  sub_text_npv <- cbind(sub_text, col_npv)
  
  Cairo(
    width = 23,
    height = 14,
    file = glue("{folder}/NPV plot {title}_{rule_out}.png"),
    type = "png",
    bg = "white",
    units = "in",
    dpi = 1200
  )
  
  p <- forestplot(
    sub_text_npv,
    mean = c(NA, NA, NA, data_metrics$npv),
    lower = c(NA, NA, NA, data_metrics$npv_lcl),
    upper = c(NA, NA, NA, data_metrics$npv_ucl),
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F), 11), TRUE),
    txt_gp = fpTxtGp(
      label = gpar(cex = 1.75),
      ticks = gpar(cex = 1.75),
      xlab = gpar(cex = 1.5),
      title = gpar(cex = 1.75)
    ),
    col = fpColors(
      box = "black",
      lines = "black",
      zero = "gray50"
    ),
    zero =  data_metrics[nrow(data_metrics), "npv"], #99.5,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(96, 101),
    xticks = seq(96,100, 0.5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  
} # end of function

# End