

# Forest plot function

# libraries
library(tidyverse)
#library(caret)
library(glue)
#library(metafor)
library(qwraps2)
library(forestplot)
library(berryFunctions)
library(Cairo)


# data = Dataset; predictions = "XGBoost_top_12_presentation_actual_data_2"; thres = 0.69652; title = "XGBoost_top_12_presentation_actual_data_2"

round_DD = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

forest_plot_rule_in_internal <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  
  # # Ethinicity
  # Black <- data %>% dplyr::filter(ethnicity.cat == "black")
  # Caucasian <- data %>% dplyr::filter(ethnicity.cat == "caucasian")
  # Other <-
  #   data %>% dplyr::filter(ethnicity.cat %in% c("asian", "other"))
  
  
  # # Smoking
  # Current_or_ex_smoker <-
  #   data %>% dplyr::filter(smoking == 1)
  # Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Ischemic heart disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # # Hypertension
  # Hypertension <- data %>% dplyr::filter(hypertension == 1)
  # No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # # Hyperlipidemia
  # Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  # No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # # Cerebrovascular disease
  # Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  # No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  # # Body mass index
  # bmi_less_25 <- data %>% dplyr::filter(bmi < 25)
  # bmi_25_to_30 <- data %>% dplyr::filter(bmi >= 25 & bmi < 30)
  # bmi_more_30 <- data %>% dplyr::filter(bmi >= 30)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    # Black = Black,
    # Caucasian = Caucasian,
    # Other = Other,
    # Current_or_ex_smoker = Current_or_ex_smoker,
    # Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    # Hypertension = Hypertension,
    # No_hypertension = No_hypertension,
    # Hyperlipidemia = Hyperlipidemia,
    # No_hyperlipidemia = No_hyperlipidemia,
    # Prev_cd = Prev_cd,
    # No_prev_cd = No_prev_cd
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    # bmi_less_25 = bmi_less_25,
    # bmi_25_to_30 = bmi_25_to_30,
    # bmi_more_30 = bmi_more_30,
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
      # npv = CM$stats["NPV", "Est"]*100,
      # npv_lcl = CM$stats["NPV", "LCL"]*100,
      # npv_ucl = CM$stats["NPV", "UCL"]*100,
      # ppv = CM$stats["PPV", "Est"]*100,
      # ppv_lcl = CM$stats["PPV", "LCL"]*100,
      # ppv_ucl = CM$stats["PPV", "UCL"]*100,
      # sensitivity = CM$stats["Sensitivity", "Est"]*100,
      # sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      # sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      # specificity = CM$stats["Specificity", "Est"]*100,
      # specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      # specificity_ucl = CM$stats["Specificity", "UCL"]*100,
      TN = CM$cells$true_negatives,
      FN = CM$cells$false_negatives,
      TP = CM$cells$true_positives,
      FP = CM$cells$false_positives,
      Rule_out = round(CM$cells$negatives/(CM$cells$negatives + CM$cells$positives) * 100, 0),
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
  # data_metrics2 <- data_metrics %>% 
  #   select(subgroup, npv, npv.1, npv_lcl, npv_lcl.1, npv_ucl, npv_ucl.1,TN, FN)
  
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
  
  rule_in <- data_metrics[nrow(data_metrics), "Rule_in"]
  
  # Rule_in <-
  #   round(
  #     sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"]) / sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"] + data_metrics[nrow(data_metrics), "TN"] + data_metrics[nrow(data_metrics), "FN"]) * 100,
  #     1
  #   )
  
  
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
    # "Ethnicity",
    # "  Black",
    # "  Caucasian",
    # "  Other",
    # NA,
    # "Smoking",
    # "  Current or ex-smoker",
    # "  Non-smoker",
    # NA,
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
    # "Hypertension",
    # "  Yes",
    # "  No",
    # NA,
    # "Hyperlipidemia",
    # "  Yes",
    # "  No",
    # NA,
    # "Cerebrovascular disease",
    # "  Yes",
    # "  No",
    # NA,
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
    # NA,
    # "Body mass index",
    # "  <25 (normal/underweight)",
    # "  25-30 (overweight)",
    # "  \u226530 (obese)",
    # NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  
  ### PPV ###
  
  col_ppv <- cbind(
    c("True positive",
      NA,
      NA,
      data_metrics$TP),
    c("False positive",
      NA,
      NA,
      data_metrics$FP),
    c(
      "Positive predictive value (95% CI)",
      NA,
      NA,
      data_metrics$ppv_ci
    )
  )
  
  
  sub_text_ppv <- cbind(sub_text, col_ppv)
  
  # data
  sub_data_ppv <- structure(
    list(
      value = c(NA, NA, NA, data_metrics$ppv),
      Low.value = c(NA, NA, NA, data_metrics$ppv_lcl),
      Upper.value = c(NA, NA, NA, data_metrics$ppv_ucl)
    ),
    .Names = c("value", "Low", "Upper"),
    row.names = c(NA, -35L),
    class = "data.frame"
  )
  
  Cairo(
    glue("{folder}/PPV plot {title}_{rule_in}.png"),
    height = 14,
    width = 20,
    type="png",
    bg = "white",
    units = "in",
    dpi = 1200
    # family = "Arial Unicode MS"
  )
  
  # cairo_pdf(
  #   glue("Plots/Forest plots/{folder}/PPV plot {title}.pdf"),
  #   height = 14,
  #   width = 18,
  #   onefile = FALSE,
  #   family = "Arial Unicode MS"
  # )
  
  
  p <- forestplot(
    sub_text_ppv,
    sub_data_ppv,
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F),8), T),#c(TRUE, rep(FALSE, 33), TRUE),
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
    zero = data_metrics[nrow(data_metrics), "ppv"], #75,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(50, 101),
    xticks = seq(50,100, 5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  # ### Specificity ###    
  # 
  # col_spec <- cbind(
  #   c("True negative",
  #     NA,
  #     NA,
  #     data_metrics$TN),
  #   c("False positive",
  #     NA,
  #     NA,
  #     data_metrics$FP),
  #   c(
  #     "Specificity (95% CI)",
  #     NA,
  #     NA,
  #     data_metrics$specificity_ci
  #   )
  # )
  # 
  # 
  # sub_text_spec <- cbind(sub_text, col_spec)
  # 
  # # data
  # sub_data_spec <- structure(
  #   list(
  #     value = c(NA, NA, NA, data_metrics$specificity),
  #     Low.value = c(NA, NA, NA, data_metrics$specificity_lcl),
  #     Upper.value = c(NA, NA, NA, data_metrics$specificity_ucl)
  #   ),
  #   .Names = c("value", "Low", "Upper"),
  #   row.names = c(NA, -35L),
  #   class = "data.frame"
  # )
  # 
  # Cairo(
  #   glue("{folder}/Specificity plot {title}_{rule_in}.png"),
  #   height = 14,
  #   width = 18,
  #   type="png",
  #   bg = "white",
  #   #onefile = FALSE,
  #   units = "in",
  #   dpi = 300,
  #   family = "Arial Unicode MS"
  # )
  # 
  # 
  # forestplot(
  #   sub_text_spec,
  #   sub_data_spec,
  #   graph.pos = 4,
  #   is.summary = c(T, F, rep(c(T,F,F,F),8), T), #c(TRUE, rep(FALSE, 33), TRUE),
  #   txt_gp = fpTxtGp(
  #     label = gpar(cex = 1.25),
  #     ticks = gpar(cex = 1.1),
  #     xlab = gpar(cex = 1.2),
  #     title = gpar(cex = 1.2)
  #   ),
  #   col = fpColors(
  #     box = "black",
  #     lines = "black",
  #     zero = "gray50"
  #   ),
  #   zero = 80,
  #   cex = 0.9,
  #   lineheight = "auto",
  #   boxsize = 0.5,
  #   colgap = unit(6, "mm"),
  #   lwd.ci = 2,
  #   ci.vertices = TRUE,
  #   ci.vertices.height = 0.4,
  #   clip = c(50, 101),
  #   xticks = seq(50,100, 5)
  # )
  # 
  # invisible(dev.off())
  
} # end of function


forest_plot_rule_in_perc <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  
  # # Ethinicity
  # Black <- data %>% dplyr::filter(ethnicity.cat == "black")
  # Caucasian <- data %>% dplyr::filter(ethnicity.cat == "caucasian")
  # Other <-
  #   data %>% dplyr::filter(ethnicity.cat %in% c("asian", "other"))
  
  
  # # Smoking
  # Current_or_ex_smoker <-
  #   data %>% dplyr::filter(smoking == 1)
  # Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Ischemic heart disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # # Hypertension
  # Hypertension <- data %>% dplyr::filter(hypertension == 1)
  # No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # # Hyperlipidemia
  # Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  # No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # # Cerebrovascular disease
  # Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  # No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  # # Body mass index
  # bmi_less_25 <- data %>% dplyr::filter(bmi < 25)
  # bmi_25_to_30 <- data %>% dplyr::filter(bmi >= 25 & bmi < 30)
  # bmi_more_30 <- data %>% dplyr::filter(bmi >= 30)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    # Black = Black,
    # Caucasian = Caucasian,
    # Other = Other,
    # Current_or_ex_smoker = Current_or_ex_smoker,
    # Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    # Hypertension = Hypertension,
    # No_hypertension = No_hypertension,
    # Hyperlipidemia = Hyperlipidemia,
    # No_hyperlipidemia = No_hyperlipidemia,
    # Prev_cd = Prev_cd,
    # No_prev_cd = No_prev_cd
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    # bmi_less_25 = bmi_less_25,
    # bmi_25_to_30 = bmi_25_to_30,
    # bmi_more_30 = bmi_more_30,
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
      # npv = CM$stats["NPV", "Est"]*100,
      # npv_lcl = CM$stats["NPV", "LCL"]*100,
      # npv_ucl = CM$stats["NPV", "UCL"]*100,
      # ppv = CM$stats["PPV", "Est"]*100,
      # ppv_lcl = CM$stats["PPV", "LCL"]*100,
      # ppv_ucl = CM$stats["PPV", "UCL"]*100,
      # sensitivity = CM$stats["Sensitivity", "Est"]*100,
      # sensitivity_lcl = CM$stats["Sensitivity", "LCL"]*100,
      # sensitivity_ucl = CM$stats["Sensitivity", "UCL"]*100,
      # specificity = CM$stats["Specificity", "Est"]*100,
      # specificity_lcl = CM$stats["Specificity", "LCL"]*100,
      # specificity_ucl = CM$stats["Specificity", "UCL"]*100,
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
  
  # data_metrics2 <- data_metrics %>% 
  #   select(subgroup, npv, npv.1, npv_lcl, npv_lcl.1, npv_ucl, npv_ucl.1,TN, FN)
  
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
  
  data_metrics$Rule_in_subgroup <- paste0(
    formatC(data_metrics$Rule_in, format = "f", digits = 0),
    "% [",
    data_metrics$Rule_in_N,
    "/",
    data_metrics$Total_N,
    "]")
  
  rule_in <- data_metrics[nrow(data_metrics), "Rule_in"]
  
  # Rule_in <-
  #   round(
  #     sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"]) / sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"] + data_metrics[nrow(data_metrics), "TN"] + data_metrics[nrow(data_metrics), "FN"]) * 100,
  #     1
  #   )
  
  
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
    # "Ethnicity",
    # "  Black",
    # "  Caucasian",
    # "  Other",
    # NA,
    # "Smoking",
    # "  Current or ex-smoker",
    # "  Non-smoker",
    # NA,
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
    # "Hypertension",
    # "  Yes",
    # "  No",
    # NA,
    # "Hyperlipidemia",
    # "  Yes",
    # "  No",
    # NA,
    # "Cerebrovascular disease",
    # "  Yes",
    # "  No",
    # NA,
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
    # NA,
    # "Body mass index",
    # "  <25 (normal/underweight)",
    # "  25-30 (overweight)",
    # "  \u226530 (obese)",
    # NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  
  ### PPV ###
  
  col_ppv <- cbind(
    c("True positive",
      NA,
      NA,
      data_metrics$TP),
    c("False positive",
      NA,
      NA,
      data_metrics$FP),
    c(
      "Positive predictive value (95% CI)",
      NA,
      NA,
      data_metrics$ppv_ci
    ),
    c(
      "Proportion ruled in",
      NA,
      NA,
      data_metrics$Rule_in_subgroup
    )
  )
  
  
  sub_text_ppv <- cbind(sub_text, col_ppv)
  
  # data
  sub_data_ppv <- structure(
    list(
      value = c(NA, NA, NA, data_metrics$ppv),
      Low.value = c(NA, NA, NA, data_metrics$ppv_lcl),
      Upper.value = c(NA, NA, NA, data_metrics$ppv_ucl)
    ),
    .Names = c("value", "Low", "Upper"),
    row.names = c(NA, -35L),
    class = "data.frame"
  )
  
  Cairo(
    glue("{folder}/PPV plot {title}_{rule_in}.png"),
    height = 14,
    width = 23,
    type="png",
    bg = "white",
    units = "in",
    dpi = 1200
    # family = "Arial Unicode MS"
  )
  
  # cairo_pdf(
  #   glue("Plots/Forest plots/{folder}/PPV plot {title}.pdf"),
  #   height = 14,
  #   width = 18,
  #   onefile = FALSE,
  #   family = "Arial Unicode MS"
  # )
  
  
  p <- forestplot(
    sub_text_ppv,
    sub_data_ppv,
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F),8), T),#c(TRUE, rep(FALSE, 33), TRUE),
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
    zero = data_metrics[nrow(data_metrics), "ppv"], #75,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(50, 101),
    xticks = seq(50,100, 5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  ### Specificity ###    
  
  # col_spec <- cbind(
  #   c("True negative",
  #     NA,
  #     NA,
  #     data_metrics$TN),
  #   c("False positive",
  #     NA,
  #     NA,
  #     data_metrics$FP),
  #   c(
  #     "Specificity (95% CI)",
  #     NA,
  #     NA,
  #     data_metrics$specificity_ci
  #   )
  # )
  # 
  # 
  # sub_text_spec <- cbind(sub_text, col_spec)
  # 
  # # data
  # sub_data_spec <- structure(
  #   list(
  #     value = c(NA, NA, NA, data_metrics$specificity),
  #     Low.value = c(NA, NA, NA, data_metrics$specificity_lcl),
  #     Upper.value = c(NA, NA, NA, data_metrics$specificity_ucl)
  #   ),
  #   .Names = c("value", "Low", "Upper"),
  #   row.names = c(NA, -35L),
  #   class = "data.frame"
  # )
  # 
  # Cairo(
  #   glue("{folder}/Specificity plot {title}_{rule_in}.png"),
  #   height = 14,
  #   width = 18,
  #   type="png",
  #   bg = "white",
  #   #onefile = FALSE,
  #   units = "in",
  #   dpi = 300,
  #   family = "Arial Unicode MS"
  # )
  # 
  # 
  # forestplot(
  #   sub_text_spec,
  #   sub_data_spec,
  #   graph.pos = 4,
  #   is.summary = c(T, F, rep(c(T,F,F,F),8), T), #c(TRUE, rep(FALSE, 33), TRUE),
  #   txt_gp = fpTxtGp(
  #     label = gpar(cex = 1.25),
  #     ticks = gpar(cex = 1.1),
  #     xlab = gpar(cex = 1.2),
  #     title = gpar(cex = 1.2)
  #   ),
  #   col = fpColors(
  #     box = "black",
  #     lines = "black",
  #     zero = "gray50"
  #   ),
  #   zero = 80,
  #   cex = 0.9,
  #   lineheight = "auto",
  #   boxsize = 0.5,
  #   colgap = unit(6, "mm"),
  #   lwd.ci = 2,
  #   ci.vertices = TRUE,
  #   ci.vertices.height = 0.4,
  #   clip = c(50, 101),
  #   xticks = seq(50,100, 5)
  #   # hrzl_lines = list(
  #   #   "3" = gpar(lwd = 1, col = "black"),
  #   #   "5" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "14" = gpar(
  #   #     lwd = 90,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "23" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "31" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "39" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "47" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "56" = gpar(
  #   #     lwd = 90,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   )
  #   # )
  # )
  # 
  # invisible(dev.off())
  
} # end of function




forest_plot_rule_in_external <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  
  # # Ethinicity
  # Black <- data %>% dplyr::filter(ethnicity.cat == "black")
  # Caucasian <- data %>% dplyr::filter(ethnicity.cat == "caucasian")
  # Other <-
  #   data %>% dplyr::filter(ethnicity.cat %in% c("asian", "other"))
  
  
  # # Smoking
  # Current_or_ex_smoker <-
  #   data %>% dplyr::filter(smoking == 1)
  # Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Ischemic heart disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # # Hypertension
  # Hypertension <- data %>% dplyr::filter(hypertension == 1)
  # No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # # Hyperlipidemia
  # Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  # No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # # Cerebrovascular disease
  # Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  # No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  # # Body mass index
  # bmi_less_25 <- data %>% dplyr::filter(bmi < 25)
  # bmi_25_to_30 <- data %>% dplyr::filter(bmi >= 25 & bmi < 30)
  # bmi_more_30 <- data %>% dplyr::filter(bmi >= 30)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    # Black = Black,
    # Caucasian = Caucasian,
    # Other = Other,
    # Current_or_ex_smoker = Current_or_ex_smoker,
    # Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    # Hypertension = Hypertension,
    # No_hypertension = No_hypertension,
    # Hyperlipidemia = Hyperlipidemia,
    # No_hyperlipidemia = No_hyperlipidemia,
    # Prev_cd = Prev_cd,
    # No_prev_cd = No_prev_cd
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    # bmi_less_25 = bmi_less_25,
    # bmi_25_to_30 = bmi_25_to_30,
    # bmi_more_30 = bmi_more_30,
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
      # npv = CM$stats["NPV", "Boot Est"]*100,
      # npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
      # npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
      # ppv = CM$stats["PPV", "Boot Est"]*100,
      # ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
      # ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
      # sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      # sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
      # sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
      # specificity = CM$stats["Specificity", "Boot Est"]*100,
      # specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
      # specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
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
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
  # data_metrics2 <- data_metrics %>% 
  #   select(subgroup, npv, npv.1, npv_lcl, npv_lcl.1, npv_ucl, npv_ucl.1,TN, FN)
  
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
  
  rule_in <- data_metrics[nrow(data_metrics), "Rule_in"]
  
  # Rule_in <-
  #   round(
  #     sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"]) / sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"] + data_metrics[nrow(data_metrics), "TN"] + data_metrics[nrow(data_metrics), "FN"]) * 100,
  #     1
  #   )
  
  
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
    # "Ethnicity",
    # "  Black",
    # "  Caucasian",
    # "  Other",
    # NA,
    # "Smoking",
    # "  Current or ex-smoker",
    # "  Non-smoker",
    # NA,
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
    # "Hypertension",
    # "  Yes",
    # "  No",
    # NA,
    # "Hyperlipidemia",
    # "  Yes",
    # "  No",
    # NA,
    # "Cerebrovascular disease",
    # "  Yes",
    # "  No",
    # NA,
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
    # NA,
    # "Body mass index",
    # "  <25 (normal/underweight)",
    # "  25-30 (overweight)",
    # "  \u226530 (obese)",
    # NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  
  ### PPV ###
  
  col_ppv <- cbind(
    c("True positive",
      NA,
      NA,
      data_metrics$TP),
    c("False positive",
      NA,
      NA,
      data_metrics$FP),
    c(
      "Positive predictive value (95% CI)",
      NA,
      NA,
      data_metrics$ppv_ci
    )
  )
  
  
  sub_text_ppv <- cbind(sub_text, col_ppv)
  
  # data
  sub_data_ppv <- structure(
    list(
      value = c(NA, NA, NA, data_metrics$ppv),
      Low.value = c(NA, NA, NA, data_metrics$ppv_lcl),
      Upper.value = c(NA, NA, NA, data_metrics$ppv_ucl)
    ),
    .Names = c("value", "Low", "Upper"),
    row.names = c(NA, -35L),
    class = "data.frame"
  )
  
  Cairo(
    glue("{folder}/PPV plot {title}_{rule_in}.png"),
    height = 16,
    width = 20,
    type="png",
    bg = "white",
    units = "in",
    dpi = 1200
    # family = "Arial Unicode MS"
  )
  
  # cairo_pdf(
  #   glue("Plots/Forest plots/{folder}/PPV plot {title}.pdf"),
  #   height = 14,
  #   width = 18,
  #   onefile = FALSE,
  #   family = "Arial Unicode MS"
  # )
  
  
  p <- forestplot(
    sub_text_ppv,
    sub_data_ppv,
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
    # zero = 80,
    zero = data_metrics[nrow(data_metrics), "ppv"], #75,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(50, 101),
    xticks = seq(50,100, 5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  ### Specificity ###    
  
  # col_spec <- cbind(
  #   c("True negative",
  #     NA,
  #     NA,
  #     data_metrics$TN),
  #   c("False positive",
  #     NA,
  #     NA,
  #     data_metrics$FP),
  #   c(
  #     "Specificity (95% CI)",
  #     NA,
  #     NA,
  #     data_metrics$specificity_ci
  #   )
  # )
  # 
  # 
  # sub_text_spec <- cbind(sub_text, col_spec)
  # 
  # # data
  # sub_data_spec <- structure(
  #   list(
  #     value = c(NA, NA, NA, data_metrics$specificity),
  #     Low.value = c(NA, NA, NA, data_metrics$specificity_lcl),
  #     Upper.value = c(NA, NA, NA, data_metrics$specificity_ucl)
  #   ),
  #   .Names = c("value", "Low", "Upper"),
  #   row.names = c(NA, -35L),
  #   class = "data.frame"
  # )
  # 
  # Cairo(
  #   glue("{folder}/Specificity plot {title}_{rule_in}.png"),
  #   height = 14,
  #   width = 18,
  #   type="png",
  #   bg = "white",
  #   #onefile = FALSE,
  #   units = "in",
  #   dpi = 300,
  #   family = "Arial Unicode MS"
  # )
  # 
  # 
  # p <- forestplot(
  #   sub_text_spec,
  #   sub_data_spec,
  #   graph.pos = 4,
  #   is.summary = c(TRUE, rep(FALSE, 33), TRUE),
  #   txt_gp = fpTxtGp(
  #     label = gpar(cex = 1.25),
  #     ticks = gpar(cex = 1.1),
  #     xlab = gpar(cex = 1.2),
  #     title = gpar(cex = 1.2)
  #   ),
  #   col = fpColors(
  #     box = "black",
  #     lines = "black",
  #     zero = "gray50"
  #   ),
  #   zero = 80,
  #   cex = 0.9,
  #   lineheight = "auto",
  #   boxsize = 0.5,
  #   colgap = unit(6, "mm"),
  #   lwd.ci = 2,
  #   ci.vertices = TRUE,
  #   ci.vertices.height = 0.4,
  #   clip = c(50, 101),
  #   xticks = seq(50,100, 5)
  # )
  # 
  # print(p)
  # 
  # invisible(dev.off())
  
} # end of function


forest_plot_rule_in_external_perc <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  
  # # Ethinicity
  # Black <- data %>% dplyr::filter(ethnicity.cat == "black")
  # Caucasian <- data %>% dplyr::filter(ethnicity.cat == "caucasian")
  # Other <-
  #   data %>% dplyr::filter(ethnicity.cat %in% c("asian", "other"))
  
  
  # # Smoking
  # Current_or_ex_smoker <-
  #   data %>% dplyr::filter(smoking == 1)
  # Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Ischemic heart disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # # Hypertension
  # Hypertension <- data %>% dplyr::filter(hypertension == 1)
  # No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # # Hyperlipidemia
  # Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  # No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # # Cerebrovascular disease
  # Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  # No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  # # Body mass index
  # bmi_less_25 <- data %>% dplyr::filter(bmi < 25)
  # bmi_25_to_30 <- data %>% dplyr::filter(bmi >= 25 & bmi < 30)
  # bmi_more_30 <- data %>% dplyr::filter(bmi >= 30)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    # Black = Black,
    # Caucasian = Caucasian,
    # Other = Other,
    # Current_or_ex_smoker = Current_or_ex_smoker,
    # Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    # Hypertension = Hypertension,
    # No_hypertension = No_hypertension,
    # Hyperlipidemia = Hyperlipidemia,
    # No_hyperlipidemia = No_hyperlipidemia,
    # Prev_cd = Prev_cd,
    # No_prev_cd = No_prev_cd
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    # bmi_less_25 = bmi_less_25,
    # bmi_25_to_30 = bmi_25_to_30,
    # bmi_more_30 = bmi_more_30,
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
      # npv = CM$stats["NPV", "Boot Est"]*100,
      # npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
      # npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
      # ppv = CM$stats["PPV", "Boot Est"]*100,
      # ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
      # ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
      # sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      # sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
      # sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
      # specificity = CM$stats["Specificity", "Boot Est"]*100,
      # specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
      # specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
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
  
  # data_metrics2 <- data_metrics %>% 
  #   select(subgroup, npv, npv.1, npv_lcl, npv_lcl.1, npv_ucl, npv_ucl.1,TN, FN)
  
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
  
  
  data_metrics$Rule_in_subgroup <- paste0(
    formatC(data_metrics$Rule_in, format = "f", digits = 0),
    "% [",
    data_metrics$Rule_in_N,
    "/",
    data_metrics$Total_N,
    "]")
  
  rule_in <- data_metrics[nrow(data_metrics), "Rule_in"]
  
  # Rule_in <-
  #   round(
  #     sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"]) / sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"] + data_metrics[nrow(data_metrics), "TN"] + data_metrics[nrow(data_metrics), "FN"]) * 100,
  #     1
  #   )
  
  
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
    # "Ethnicity",
    # "  Black",
    # "  Caucasian",
    # "  Other",
    # NA,
    # "Smoking",
    # "  Current or ex-smoker",
    # "  Non-smoker",
    # NA,
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
    # "Hypertension",
    # "  Yes",
    # "  No",
    # NA,
    # "Hyperlipidemia",
    # "  Yes",
    # "  No",
    # NA,
    # "Cerebrovascular disease",
    # "  Yes",
    # "  No",
    # NA,
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
    # NA,
    # "Body mass index",
    # "  <25 (normal/underweight)",
    # "  25-30 (overweight)",
    # "  \u226530 (obese)",
    # NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  
  ### PPV ###
  
  col_ppv <- cbind(
    c("True positive",
      NA,
      NA,
      data_metrics$TP),
    c("False positive",
      NA,
      NA,
      data_metrics$FP),
    c(
      "Positive predictive value\n(95% CI)",
      NA,
      NA,
      data_metrics$ppv_ci
    ),
    c(
      "Proportion ruled in",
      NA,
      NA,
      data_metrics$Rule_in_subgroup
    )
  )
  
  
  sub_text_ppv <- cbind(sub_text, col_ppv)
  
  # data
  sub_data_ppv <- structure(
    list(
      value = c(NA, NA, NA, data_metrics$ppv),
      Low.value = c(NA, NA, NA, data_metrics$ppv_lcl),
      Upper.value = c(NA, NA, NA, data_metrics$ppv_ucl)
    ),
    .Names = c("value", "Low", "Upper"),
    row.names = c(NA, -35L),
    class = "data.frame"
  )
  
  Cairo(
    glue("{folder}/PPV plot {title}_{rule_in}.png"),
    height = 14,
    width = 23,
    type="png",
    bg = "white",
    units = "in",
    dpi = 1200
    # family = "Arial Unicode MS"
  )
  
  # cairo_pdf(
  #   glue("Plots/Forest plots/{folder}/PPV plot {title}.pdf"),
  #   height = 14,
  #   width = 18,
  #   onefile = FALSE,
  #   family = "Arial Unicode MS"
  # )
  
  
  p <- forestplot(
    sub_text_ppv,
    sub_data_ppv,
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
    zero = data_metrics[nrow(data_metrics), "ppv"], #75,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(50, 101),
    xticks = seq(50,100, 5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  ### Specificity ###    
  
  # col_spec <- cbind(
  #   c("True negative",
  #     NA,
  #     NA,
  #     data_metrics$TN),
  #   c("False positive",
  #     NA,
  #     NA,
  #     data_metrics$FP),
  #   c(
  #     "Specificity (95% CI)",
  #     NA,
  #     NA,
  #     data_metrics$specificity_ci
  #   )
  # )
  # 
  # 
  # sub_text_spec <- cbind(sub_text, col_spec)
  # 
  # # data
  # sub_data_spec <- structure(
  #   list(
  #     value = c(NA, NA, NA, data_metrics$specificity),
  #     Low.value = c(NA, NA, NA, data_metrics$specificity_lcl),
  #     Upper.value = c(NA, NA, NA, data_metrics$specificity_ucl)
  #   ),
  #   .Names = c("value", "Low", "Upper"),
  #   row.names = c(NA, -35L),
  #   class = "data.frame"
  # )
  # 
  # Cairo(
  #   glue("{folder}/Specificity plot {title}_{rule_in}.png"),
  #   height = 14,
  #   width = 18,
  #   type="png",
  #   bg = "white",
  #   #onefile = FALSE,
  #   units = "in",
  #   dpi = 300,
  #   family = "Arial Unicode MS"
  # )
  # 
  # 
  # forestplot(
  #   sub_text_spec,
  #   sub_data_spec,
  #   graph.pos = 4,
  #   is.summary = c(TRUE, rep(FALSE, 33), TRUE),
  #   txt_gp = fpTxtGp(
  #     label = gpar(cex = 1.25),
  #     ticks = gpar(cex = 1.1),
  #     xlab = gpar(cex = 1.2),
  #     title = gpar(cex = 1.2)
  #   ),
  #   col = fpColors(
  #     box = "black",
  #     lines = "black",
  #     zero = "gray50"
  #   ),
  #   zero = 80,
  #   cex = 0.9,
  #   lineheight = "auto",
  #   boxsize = 0.5,
  #   colgap = unit(6, "mm"),
  #   lwd.ci = 2,
  #   ci.vertices = TRUE,
  #   ci.vertices.height = 0.4,
  #   clip = c(50, 101),
  #   xticks = seq(50,100, 5)
  #   # hrzl_lines = list(
  #   #   "3" = gpar(lwd = 1, col = "black"),
  #   #   "5" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "14" = gpar(
  #   #     lwd = 90,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "23" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "31" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "39" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "47" = gpar(
  #   #     lwd = 78,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   ),
  #   #   "56" = gpar(
  #   #     lwd = 90,
  #   #     lineend = "butt",
  #   #     columns = c(1:5),
  #   #     col = "#99999922"
  #   #   )
  #   # )
  # )
  # 
  # invisible(dev.off())
  
} # end of function



# Special forest plots for 99th -------------------------------------------

forest_plot_rule_in_99th <- function(data, predictions, thres, title, folder) {
  # Part 1: Set the population ----------------------------------------------
  
  
  # Age
  Age_less_65 <- data %>% dplyr::filter(age < 65)
  Age_more_65 <- data %>% dplyr::filter(age >= 65)
  
  # Sex
  Males <- data %>% dplyr::filter(sex == 1)
  Females <- data %>% dplyr::filter(sex == 0)
  
  
  # # Ethinicity
  # Black <- data %>% dplyr::filter(ethnicity.cat == "black")
  # Caucasian <- data %>% dplyr::filter(ethnicity.cat == "caucasian")
  # Other <-
  #   data %>% dplyr::filter(ethnicity.cat %in% c("asian", "other"))
  
  
  # # Smoking
  # Current_or_ex_smoker <-
  #   data %>% dplyr::filter(smoking == 1)
  # Non_smoker <- data %>% dplyr::filter(smoking == 0)
  
  
  # Ischemic heart disease
  Prev_ihd <- data %>% dplyr::filter(previous_ihd == 1)
  No_prev_ihd <- data %>% dplyr::filter(previous_ihd == 0)
  
  
  # Ischemic heart disease
  Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # Diabetes
  Diabetes <- data %>% dplyr::filter(diabetes == 1)
  No_diabetes <- data %>% dplyr::filter(diabetes == 0)
  
  
  # # Hypertension
  # Hypertension <- data %>% dplyr::filter(hypertension == 1)
  # No_hypertension <- data %>% dplyr::filter(hypertension == 0)
  
  
  # # Hyperlipidemia
  # Hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 1)
  # No_hyperlipidemia <- data %>% dplyr::filter(hyperlipidemia == 0)
  
  
  # # Cerebrovascular disease
  # Prev_cd <- data %>% dplyr::filter(previous_cd == 1)
  # No_prev_cd <- data %>% dplyr::filter(previous_cd == 0)
  
  
  # eGFR
  egfr_less_60 <- data %>% dplyr::filter(egfr < 60)
  egfr_more_60 <- data %>% dplyr::filter(egfr >= 60)
  
  # Ischemia
  Ischemia <- data %>% dplyr::filter(ischemia == 1)
  No_ischemia <- data %>% dplyr::filter(ischemia == 0)
  
  # Time from onset of chest pain
  Time_from_onset_no_more_than_3 <- data %>% dplyr::filter(hours_since_symptoms <= 3)
  Time_from_onset_greater_than_3 <- data %>% dplyr::filter(hours_since_symptoms > 3)
  
  
  # # Body mass index
  # bmi_less_25 <- data %>% dplyr::filter(bmi < 25)
  # bmi_25_to_30 <- data %>% dplyr::filter(bmi >= 25 & bmi < 30)
  # bmi_more_30 <- data %>% dplyr::filter(bmi >= 30)
  
  
  Overall <- data
  
  
  # Bring everything together
  subgroups <- list(
    Age_less_65 = Age_less_65,
    Age_more_65 = Age_more_65,
    Males = Males,
    Females = Females,
    # Black = Black,
    # Caucasian = Caucasian,
    # Other = Other,
    # Current_or_ex_smoker = Current_or_ex_smoker,
    # Non_smoker = Non_smoker,
    Prev_ihd = Prev_ihd,
    No_prev_ihd = No_prev_ihd,
    Prev_cd = Prev_cd,
    No_prev_cd = No_prev_cd,
    Diabetes = Diabetes,
    No_diabetes = No_diabetes,
    # Hypertension = Hypertension,
    # No_hypertension = No_hypertension,
    # Hyperlipidemia = Hyperlipidemia,
    # No_hyperlipidemia = No_hyperlipidemia,
    # Prev_cd = Prev_cd,
    # No_prev_cd = No_prev_cd
    egfr_less_60 = egfr_less_60,
    egfr_more_60 = egfr_more_60,
    Ischemia = Ischemia,
    No_ischemia = No_ischemia,
    Time_from_onset_no_more_than_3 = Time_from_onset_no_more_than_3,
    Time_from_onset_greater_than_3 = Time_from_onset_greater_than_3,
    # bmi_less_25 = bmi_less_25,
    # bmi_25_to_30 = bmi_25_to_30,
    # bmi_more_30 = bmi_more_30,
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
      # npv = CM$stats["NPV", "Boot Est"]*100,
      # npv_lcl = CM$stats["NPV", "Boot LCL"]*100,
      # npv_ucl = CM$stats["NPV", "Boot UCL"]*100,
      # ppv = CM$stats["PPV", "Boot Est"]*100,
      # ppv_lcl = CM$stats["PPV", "Boot LCL"]*100,
      # ppv_ucl = CM$stats["PPV", "Boot UCL"]*100,
      # sensitivity = CM$stats["Sensitivity", "Boot Est"]*100,
      # sensitivity_lcl = CM$stats["Sensitivity", "Boot LCL"]*100,
      # sensitivity_ucl = CM$stats["Sensitivity", "Boot UCL"]*100,
      # specificity = CM$stats["Specificity", "Boot Est"]*100,
      # specificity_lcl = CM$stats["Specificity", "Boot LCL"]*100,
      # specificity_ucl = CM$stats["Specificity", "Boot UCL"]*100,
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
      Rule_in = round(CM$cells$positives/(CM$cells$negatives + CM$cells$positives) * 100, 0)
    )
    
    data_metrics <- data_metrics %>%
      rbind(CM_metrics)
    
  }
  
  # data_metrics2 <- data_metrics %>% 
  #   select(subgroup, npv, npv.1, npv_lcl, npv_lcl.1, npv_ucl, npv_ucl.1,TN, FN)
  
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
  
  rule_in <- data_metrics[nrow(data_metrics), "Rule_in"]
  
  # Rule_in <-
  #   round(
  #     sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"]) / sum(data_metrics[nrow(data_metrics), "TP"] + data_metrics[nrow(data_metrics), "FP"] + data_metrics[nrow(data_metrics), "TN"] + data_metrics[nrow(data_metrics), "FN"]) * 100,
  #     1
  #   )
  
  
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
    # "Ethnicity",
    # "  Black",
    # "  Caucasian",
    # "  Other",
    # NA,
    # "Smoking",
    # "  Current or ex-smoker",
    # "  Non-smoker",
    # NA,
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
    # "Hypertension",
    # "  Yes",
    # "  No",
    # NA,
    # "Hyperlipidemia",
    # "  Yes",
    # "  No",
    # NA,
    # "Cerebrovascular disease",
    # "  Yes",
    # "  No",
    # NA,
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
    # NA,
    # "Body mass index",
    # "  <25 (normal/underweight)",
    # "  25-30 (overweight)",
    # "  \u226530 (obese)",
    # NA,
    "Overall"
  )
  
  # add NAs rows
  data_metrics <- berryFunctions::insertRows(data_metrics,
                                             c(3, 4, 7, 8, 11, 12, 15, 16, 19, 20,
                                               23, 24, 27, 28, 31))
  
  
  ### PPV ###
  
  col_ppv <- cbind(
    c("True positive",
      NA,
      NA,
      data_metrics$TP),
    c("False positive",
      NA,
      NA,
      data_metrics$FP),
    c(
      "Positive predictive value (95% CI)",
      NA,
      NA,
      data_metrics$ppv_ci
    )
  )
  
  
  sub_text_ppv <- cbind(sub_text, col_ppv)
  
  # data
  sub_data_ppv <- structure(
    list(
      value = c(NA, NA, NA, data_metrics$ppv),
      Low.value = c(NA, NA, NA, data_metrics$ppv_lcl),
      Upper.value = c(NA, NA, NA, data_metrics$ppv_ucl)
    ),
    .Names = c("value", "Low", "Upper"),
    row.names = c(NA, -35L),
    class = "data.frame"
  )
  
  Cairo(
    glue("{folder}/PPV plot {title}_{rule_in}.png"),
    height = 14,
    width = 21,
    type="png",
    bg = "white",
    units = "in",
    dpi = 1200
    # family = "Arial Unicode MS"
  )
  
  
  
  p <- forestplot(
    sub_text_ppv,
    sub_data_ppv,
    graph.pos = 4,
    is.summary = c(T, F, rep(c(T,F,F,F),8), T),#c(TRUE, rep(FALSE, 33), TRUE),
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
    zero = data_metrics[nrow(data_metrics), "ppv"], #75,
    cex = 0.9,
    lineheight = "auto",
    boxsize = 0.5,
    colgap = unit(6, "mm"),
    lwd.ci = 2,
    ci.vertices = TRUE,
    ci.vertices.height = 0.4,
    clip = c(30, 101),
    xticks = seq(30,100, 5)
  )
  
  print(p)
  
  invisible(dev.off())
  
  
  
} # end of function

