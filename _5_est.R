
### packages ###
library(tidyverse)
library(labelled)
library(splines)
library(splitstackshape)
library(lmtest)
library(sandwich)
library(sqldf)
library(survival)
library(survminer)
library(patchwork)


### impute missing ###
df.bl <- readRDS("df_bl.rds") %>%
  mutate(ADI_NATRANK = ifelse(is.na(ADI_NATRANK), median(ADI_NATRANK, na.rm = T), ADI_NATRANK),
         potassium_BL = ifelse(is.na(potassium_BL), median(potassium_BL, na.rm = T), potassium_BL)) %>%
  mutate(exp = case_when(Arm == "ARNI" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())

### set reference level ###
df.bl <- fun_ref(df.bl)


### Logistic regression-based propensity score approach ###
# HR
fun_itt_LR(df.bl, timevar = Death_itt_time, eventvar = Death_itt_event, cov = cov_priori_ns, coxHR = TRUE)

# RD/RR
est1 <- fun_itt_boot(df.bl, 
                     time = Death_itt_time, event = Death_itt_event, 
                     covariate = cov_priori_ns,
                     approach = "LR",
                     coxHR = FALSE, seeds = 7, nboot = 500)


### GBM-based propensity score approach ###

# keep the hyperparameters from the tuning process!!!
# learning_rate <- hyper_grid$learning_rate[hyper_grid$RMSE == min(hyper_grid$RMSE)]
# depth <- hyper_grid2$interaction.depth[hyper_grid2$RMSE == min(hyper_grid2$RMSE)]
# nodesize <- hyper_grid2$n.minobsinnode[hyper_grid2$RMSE == min(hyper_grid2$RMSE)]
# ntrees <- gbm.perf(gbm.fit.final, method = "cv")


fun_itt_gbm(df.bl, Death_itt_time, Death_itt_event, cov = cov_priori_gbm,
            # n.trees = ntrees,
            # interaction.depth = depth,
            # shrinkage = learning_rate,
            # n.minobsinnode = nodesize,
            coxHR = TRUE)

est2 <- fun_itt_boot(df.bl, 
                     time = Death_itt_time, event = Death_itt_event, 
                     covariate = cov_priori_gbm,   # for data-driven GBM approach, use 'cov_datadriven_gbm'
                     approach = "gbm",
                     coxHR = FALSE, seeds = 7, nboot = 2)


### create cumulative incidence plot ###
g1 <- fun_cumIncPlot(df = df.bl, df_boot = est,
                    expmed = "ARNI", timevar = "Death_itt_time", eventvar = "Death_itt_event",
                    expmedcolor = "#ED4B4B", ymax = 45, ybreaks = 10)

g2 <- fun_cumIncPlot(df = df.bl, df_boot = est2,
                     expmed = "ARNI", timevar = "Death_itt_time", eventvar = "Death_itt_event",
                     expmedcolor = "#ED4B4B", ymax = 45, ybreaks = 10)

ggsave(g1, filename = "death_itt_LR.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(g2, filename = "death_itt_gbm_priori.tiff", width = 7.5, height = 7.5, bg = "transparent")
