
library(tidyverse)
library(glue)
library(lubridate)
library(survival)
library(survminer)

## Outcomes - validation

source("Scripts/Rule in/0x - Functions/{function} Add probs and results.R")

Dataset_external_combined <- read_rds("Outputs/Data/Dataset_external_cleaned")


table(Dataset_external_combined$allcause_death, useNA = "always")
table(Dataset_external_combined$cardiac_death, useNA = "always")

table(!is.na(Dataset_external_combined$death_days))


Dataset_external_combined <- Dataset_external_combined %>%
  replace_na(list(death_days = 365,
                  cardiac_death = 0)) 

# Add probabilities

Dataset_external_combined <-
  Add_preds_to_dataset(
    dataset = Dataset_external_combined,
    model_predictions = read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_external"),
    model_name = glue("XGBoost_ind_prob_presentation_NPV_external")
  )

Dataset_external_combined <-
  Add_preds_to_dataset(
    dataset = Dataset_external_combined,
    model_predictions = read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_serial_NPV_external"),
    model_name = glue("XGBoost_ind_prob_serial_NPV_external")
  )

Dataset_external_combined <-
  Add_preds_to_dataset(
    dataset = Dataset_external_combined,
    model_predictions = read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_external"),
    model_name = glue("XGBoost_ind_prob_presentation_PPV_external")
  )

Dataset_external_combined <-
  Add_preds_to_dataset(
    dataset = Dataset_external_combined,
    model_predictions = read_rds("Outputs/Analysis/XGBoost/XGBoost_ind_prob_serial_PPV_external"),
    model_name = glue("XGBoost_ind_prob_serial_PPV_external")
  )



# create model thresholds

metrics_out_presentation = read_rds(
  "Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_NPV_final_thresholds_table"
)
metrics_in_presentation = read_rds(
  "Outputs/Analysis/XGBoost/XGBoost_ind_prob_presentation_PPV_final_thresholds_table"
)

low_thres_presentation <-
  metrics_out_presentation %>%
  dplyr::filter(npv >= 99.5 & sensitivity >= 90) %>%
  data.table::last() %>%
  dplyr::select(threshold) %>%
  unlist() / 100

high_thres_presentation <-
  metrics_in_presentation %>%
  dplyr::filter(ppv >= 80 & specificity >= 80) %>%
  data.table::first() %>%
  dplyr::select(threshold) %>%
  unlist() / 100


Dataset_external_combined <- Dataset_external_combined %>% 
  mutate(XGBoost_ind_prob_presentation_external = coalesce(XGBoost_ind_prob_presentation_NPV_external, XGBoost_ind_prob_presentation_PPV_external),
         XGBoost_ind_prob_serial_external = coalesce(XGBoost_ind_prob_serial_NPV_external, XGBoost_ind_prob_serial_PPV_external)) %>% 
  mutate(XGBoost_ind_prob_serial_external = case_when(
    XGBoost_ind_prob_presentation_external < low_thres_presentation ~ XGBoost_ind_prob_presentation_external,
    XGBoost_ind_prob_presentation_external >= high_thres_presentation ~ XGBoost_ind_prob_presentation_external,
    TRUE ~ XGBoost_ind_prob_serial_external
  ))

Dataset_external_combined <- Dataset_external_combined %>%
  mutate(
    CoDE_ACS_presentation = case_when(
      XGBoost_ind_prob_presentation_external < low_thres_presentation ~ "low-probability",
      XGBoost_ind_prob_presentation_external >= low_thres_presentation & XGBoost_ind_prob_presentation_external < high_thres_presentation ~ "intermediate-probability",
      XGBoost_ind_prob_presentation_external >= high_thres_presentation ~ "high-probability"
      
    ),
    CoDE_ACS_serial = case_when(
      XGBoost_ind_prob_serial_external < low_thres_presentation ~ "low-probability",
      XGBoost_ind_prob_serial_external >= low_thres_presentation & XGBoost_ind_prob_serial_external < high_thres_presentation ~ "intermediate-probability",
      XGBoost_ind_prob_serial_external >= high_thres_presentation ~ "high-probability"
    )
  )



### All cause mortality

# Presentation

# all cause mortality
Dataset_external_combined_death <- Dataset_external_combined %>% 
  filter(!is.na(CoDE_ACS_presentation)) %>%
  mutate(CoDE_ACS_presentation = factor(
    CoDE_ACS_presentation,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))


# 30 days
table_allcause_30days <- Dataset_external_combined_death %>% 
  mutate(allcause_death_30days = if_else(allcause_death == 1 & death_days <= 30, 1, 0)) %>% 
  tabyl(allcause_death_30days, CoDE_ACS_presentation) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_allcause_30days, caption = "All cause mortality at 30 days, stratified by the CoDE-ACS groups at presentation", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)


# 1 year
table_allcause_1year <- Dataset_external_combined_death %>%
  mutate(allcause_death_1year = if_else(allcause_death == 1 & death_days <= 365, 1, 0)) %>% 
  tabyl(allcause_death_1year, CoDE_ACS_presentation) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_allcause_1year, caption = "All cause mortality at 1 year, stratified by the CoDE-ACS groups at presentation", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)

fit_death <- survival::survfit(Surv(time = death_days, 
                                    event = allcause_death)~CoDE_ACS_presentation,
                               data = Dataset_external_combined_death)


# log-rank test

# print(survival::survdiff(Surv(time = death_days,
# event = allcause_death) ~ CoDE_ACS_presentation, # group phase
# data = Dataset_external_combined_death))


q <- ggsurvplot(fit_death, data = Dataset_external_combined_death,
                fun = "event",
                censor = FALSE,
                risk.table = TRUE,
                tables.height = 0.25,
                tables.theme = theme_cleantable(),
                risk.table.col = "black", # OR "strata"
                risk.table.title = "Number at risk",
                risk.table.pos = "out",
                legend.labs = c(
                  "low-probability",
                  "intermediate-probability",
                  "high-probability"
                ),
                legend.title = "CoDE-ACS group",
                xlim = c(0, 365),
                break.x.by = 50,
                xlab = "Days since index presentation",
                palette = c("#0072B5FF", "#7876B1FF", "#BC3C29FF")
)



q$plot <- q$plot +
scale_y_continuous(
  name = "All-cause mortality (%)",
  limits = c(0, 0.1),
  breaks = seq(0, 0.1, 0.01),
  labels = scales::percent_format(accuracy = 1)
) +
    scale_x_continuous(name="Days since index presentation", limits=c(0,365), breaks=seq(0,365,50))+
  theme_light() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(r = -170)))

q


p1 <- q$plot
p2 <- q$table
p <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(4,1))

ggsave(
  filename = glue("ci_allcause_presentation_external.png"),
  plot = p,
  path = "Outputs/Plots/Manuscript/",
  width = 12,
  height = 8,
  units = "in",
  dpi = 1200,
  type = "cairo-png"
)


# Serial all cause --------------------------------------------------------


# all cause mortality
Dataset_external_combined_death <- Dataset_external_combined %>% 
  filter(!is.na(CoDE_ACS_serial)) %>% 
  mutate(CoDE_ACS_serial = factor(
    CoDE_ACS_serial,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))

# 30 days
table_allcause_30days <- Dataset_external_combined_death %>% 
  mutate(allcause_death_30days = if_else(allcause_death == 1 & death_days <= 30, 1, 0)) %>% 
  tabyl(allcause_death_30days, CoDE_ACS_serial) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_allcause_30days, caption = "All cause mortality at 30 days, stratified by the CoDE-ACS groups at serial", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)


# 1 year
table_allcause_1year <- Dataset_external_combined_death %>%
  mutate(allcause_death_1year = if_else(allcause_death == 1 & death_days <= 365, 1, 0)) %>% 
  tabyl(allcause_death_1year, CoDE_ACS_serial) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_allcause_1year, caption = "All cause mortality at 1 year, stratified by the CoDE-ACS groups at serial", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)

fit_death <- survival::survfit(Surv(time = death_days, 
                                    event = allcause_death)~CoDE_ACS_serial,
                               data = Dataset_external_combined_death)

p_death <- survminer::ggsurvplot(fit_death, data = Dataset_external_combined_death)

tab_death <- data.frame(p_death$data.survplot)

tab_death <- tab_death %>%
  mutate(CoDE_ACS_serial = factor(
    CoDE_ACS_serial,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))

q <- ggsurvplot(fit_death, data = Dataset_external_combined_death,
                fun = "event",
                censor = FALSE,
                risk.table = TRUE,
                tables.height = 0.25,
                tables.theme = theme_cleantable(),
                risk.table.col = "black", # OR "strata"
                risk.table.title = "Number at risk",
                risk.table.pos = "out",
                legend.labs = c(
                  "low-probability",
                  "intermediate-probability",
                  "high-probability"
                ),
                legend.title = "CoDE-ACS group",
                xlim = c(0, 365),
                break.x.by = 50,
                xlab = "Days since index presentation",
                palette = c("#0072B5FF", "#7876B1FF", "#BC3C29FF")
)



q$plot <- q$plot +
  scale_y_continuous(
    name = "All-cause mortality (%)",
    limits = c(0, 0.1),
    breaks = seq(0, 0.1, 0.01),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(name="Days since index presentation", limits=c(0,365), breaks=seq(0,365,50)) +
  theme_light() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(r = -170)))

q


p1 <- q$plot
p2 <- q$table
p <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(4,1))

ggsave(
  filename = glue("ci_allcause_serial_external.png"),
  plot = p,
  path = "Outputs/Plots/Manuscript/",
  width = 12,
  height = 8,
  units = "in",
  dpi = 1200,
  type = "cairo-png"
)

### Cardiac death


# Presentation

# all cause mortality
Dataset_external_combined_death <- Dataset_external_combined %>% 
  filter(!is.na(CoDE_ACS_presentation)) %>%
  mutate(CoDE_ACS_presentation = factor(
    CoDE_ACS_presentation,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))

# 30 days
table_cardiac_30days <- Dataset_external_combined_death %>% 
  mutate(cardiac_death_30days = if_else(cardiac_death == 1 & death_days <= 30, 1, 0)) %>% 
  tabyl(cardiac_death_30days, CoDE_ACS_presentation) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_cardiac_30days, caption = "Cardiac death at 30 days, stratified by the CoDE-ACS groups at presentation", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)


# 1 year
table_cardiac_1year <- Dataset_external_combined_death %>% 
  mutate(cardiac_death_1year = if_else(cardiac_death == 1 & death_days <= 365, 1, 0)) %>% 
  tabyl(cardiac_death_1year, CoDE_ACS_presentation) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_cardiac_1year, caption = "Cardiac death at 1 year, stratified by the CoDE-ACS groups at presentation", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)

fit_death <- survival::survfit(Surv(time = death_days, 
                                    event = cardiac_death)~CoDE_ACS_presentation,
                               data = Dataset_external_combined_death)

# log-rank

# print(survival::survdiff(Surv(time = death_days,
# event = cardiac_death) ~ CoDE_ACS_presentation, # group phase
# data = Dataset_external_combined_death))

p_death <- survminer::ggsurvplot(fit_death, data = Dataset_external_combined_death)

tab_death <- data.frame(p_death$data.survplot)

tab_death <- tab_death %>%
  mutate(CoDE_ACS_presentation = factor(
    CoDE_ACS_presentation,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))



q <- ggsurvplot(fit_death, data = Dataset_external_combined_death,
                fun = "event",
                censor = FALSE,
                risk.table = TRUE,
                tables.height = 0.25,
                tables.theme = theme_cleantable(),
                risk.table.col = "black", # OR "strata"
                risk.table.title = "Number at risk",
                risk.table.pos = "out",
                legend.labs = c(
                  "low-probability",
                  "intermediate-probability",
                  "high-probability"
                ),
                legend.title = "CoDE-ACS group",
                xlim = c(0, 365),
                break.x.by = 50,
                xlab = "Days since index presentation",
                palette = c("#0072B5FF", "#7876B1FF", "#BC3C29FF")
)



q$plot <- q$plot +
  scale_y_continuous(
    name = "Cardiac death (%)",
    limits = c(0, 0.1),
    breaks = seq(0, 0.1, 0.01),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(name="Days since index presentation", limits=c(0,365), breaks=seq(0,365,50))+
  theme_light() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(r = -170)))

q


p1 <- q$plot
p2 <- q$table
p <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(4,1))

ggsave(
  filename = glue("ci_cardiac_presentation_external.png"),
  plot = p,
  path = "Outputs/Plots/Manuscript/",
  width = 12,
  height = 8,
  units = "in",
  dpi = 1200,
  type = "cairo-png"
)

# Serial ------------------------------------------------------------------


# Cardiac death
Dataset_external_combined_death <- Dataset_external_combined %>% 
  filter(!is.na(CoDE_ACS_serial)) %>%
  mutate(CoDE_ACS_serial = factor(
    CoDE_ACS_serial,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))

# 30 days
table_cardiac_30days <- Dataset_external_combined_death %>% 
  mutate(cardiac_death_30days = if_else(cardiac_death == 1 & death_days <= 30, 1, 0)) %>% 
  tabyl(cardiac_death_30days, CoDE_ACS_serial) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_cardiac_30days, caption = "Cardiac death at 30 days, stratified by the CoDE-ACS groups at serial", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)


# 1 year
table_cardiac_1year <- Dataset_external_combined_death %>%
  mutate(cardiac_death_1year = if_else(cardiac_death == 1 & death_days <= 365, 1, 0)) %>% 
  tabyl(cardiac_death_1year, CoDE_ACS_serial) %>% 
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front")


kable(table_cardiac_1year, caption = "Cardiac death at 1 year, stratified by the CoDE-ACS groups at serial", booktabs = T, linesep = "", align = "c") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                font_size = 12)

fit_death <- survival::survfit(Surv(time = death_days, 
                                    event = cardiac_death)~CoDE_ACS_serial,
                               data = Dataset_external_combined_death)

p_death <- survminer::ggsurvplot(fit_death, data = Dataset_external_combined_death)

tab_death <- data.frame(p_death$data.survplot)

tab_death <- tab_death %>%
  mutate(CoDE_ACS_serial = factor(
    CoDE_ACS_serial,
    levels = c(
      "low-probability",
      "intermediate-probability",
      "high-probability"
    )
  ))


q <- ggsurvplot(fit_death, data = Dataset_external_combined_death,
                fun = "event",
                censor = FALSE,
                risk.table = TRUE,
                tables.height = 0.25,
                tables.theme = theme_cleantable(),
                risk.table.col = "black", # OR "strata"
                risk.table.title = "Number at risk",
                risk.table.pos = "out",
                legend.labs = c(
                  "low-probability",
                  "intermediate-probability",
                  "high-probability"
                ),
                legend.title = "CoDE-ACS group",
                xlim = c(0, 365),
                break.x.by = 50,
                xlab = "Days since index presentation",
                palette = c("#0072B5FF", "#7876B1FF", "#BC3C29FF")
)



q$plot <- q$plot +
  scale_y_continuous(
    name = "Cardiac death (%)",
    limits = c(0, 0.1),
    breaks = seq(0, 0.1, 0.01),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(name="Days since index presentation", limits=c(0,365), breaks=seq(0,365,50))+
  theme_light() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(r = -170)))

q


p1 <- q$plot
p2 <- q$table
p <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(4,1))

ggsave(
  filename = glue("ci_cardiac_serial_external.png"),
  plot = p,
  path = "Outputs/Plots/Manuscript/",
  width = 12,
  height = 8,
  units = "in",
  dpi = 1200,
  type = "cairo-png"
)

# End
