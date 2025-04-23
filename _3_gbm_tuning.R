
### packages ###
library(tidyverse)
library(gbm)


### impute missing ###
df.bl <- readRDS("df_bl.rds") %>%
  mutate(ADI_NATRANK = ifelse(is.na(ADI_NATRANK), median(ADI_NATRANK, na.rm = T), ADI_NATRANK),
         potassium_BL = ifelse(is.na(potassium_BL), median(potassium_BL, na.rm = T), potassium_BL)) %>%
  mutate(exp = case_when(Arm == "ARNI" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())

### set reference level ###
df.bl <- fun_ref(df.bl)


### tuning gbm hyperparameters ###

# gbm with a priori confounder selection
f_gbm1 <- as.formula(paste("Arm == 'ARNI' ~ ", paste(cov_priori_gbm, collapse = " + "), sep = ""))

# gbm with a data-driven confounder selection
f_gbm2 <- as.formula(paste("Arm == 'ARNI' ~ ", paste(cov_datadriven_gbm, collapse = " + "), sep = ""))


# initial gbm fit
set.seed(723)
gbm.fit <- gbm(formula = f_gbm1,
               distribution = "bernoulli",
               data = df.bl,
               n.trees = 5000,
               interaction.depth = 1,
               shrinkage = 0.1,
               cv.folds = 10,
               n.cores = NULL,
               verbose = FALSE)


print(gbm.fit)
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

# tuning learning rate (shrinkage)
hyper_grid <- expand.grid(
  learning_rate = c(0.3, 0.1, 0.05, 0.01, 0.005),
  RMSE = NA,
  trees = NA,
  time = NA
)

for(i in seq_len(nrow(hyper_grid))) {
  
  set.seed(723)
  
  train_time <- system.time({
    m <- gbm(formula = f_gbm1,
             distribution = "bernoulli",
             data = df.bl,
             n.trees = 50000,
             interaction.depth = 3,
             shrinkage = hyper_grid$learning_rate[i],
             cv.folds = 10,
             n.cores = NULL,
             verbose = FALSE)
  })
  
  hyper_grid$RMSE[i] <- sqrt(min(m$cv.error))
  hyper_grid$trees[i] <- which.min(m$cv.error)
  hyper_grid$time[i] <- train_time[["elapsed"]]
}

learning_rate <- hyper_grid$learning_rate[hyper_grid$RMSE == min(hyper_grid$RMSE)]

# search depth, minobsinnode
hyper_grid2 <- expand.grid(
  n.trees = 10000,
  shrinkage = learning_rate,
  interaction.depth = c(2, 3, 5, 7),
  n.minobsinnode = c(10, 20, 30, 50, 75, 100, 150)
)

tuning_gbm <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) {
  
  set.seed(723)
  m <- gbm(formula = f_gbm1,
           distribution = "bernoulli",
           data = df.bl,
           n.trees = n.trees,
           interaction.depth = interaction.depth,
           shrinkage = shrinkage,
           n.minobsinnode = n.minobsinnode,
           cv.folds = 10,
           n.cores = NULL,
           verbose = FALSE)
  
  sqrt(min(m$cv.error))
}

hyper_grid2$RMSE <- pmap_dbl(
  hyper_grid2,
  ~ tuning_gbm(
    n.trees = ..1,
    shrinkage = ..2,
    interaction.depth = ..3,
    n.minobsinnode = ..4
  )
)


depth <- hyper_grid2$interaction.depth[hyper_grid2$RMSE == min(hyper_grid2$RMSE)]
nodesize <- hyper_grid2$n.minobsinnode[hyper_grid2$RMSE == min(hyper_grid2$RMSE)]


### final gbm model ###
set.seed(13)
gbm.fit.final <- gbm(f_gbm1,
                     distribution = "bernoulli",
                     data = df.bl,
                     n.trees = 2000,
                     interaction.depth = 2,
                     shrinkage = 0.005,
                     n.minobsinnode = 100,
                     cv.folds = 10,
                     n.cores = NULL,
                     verbose = FALSE)

print(gbm.fit.final)
sqrt(min(gbm.fit.final$cv.error))

ntrees <- gbm.perf(gbm.fit.final, method = "cv")


# variable importance plot
imp1 <- summary.gbm(gbm.fit.final) %>%
  mutate(var_label = fct_recode(var,
                                "Month of baseline" = "trial_num",
                                "HF duration" = "time_HF",
                                "Age" = "Age",
                                "New/Prevalent users" = "UserType",
                                "HF hospitalization" = "HFH_BL_cat",
                                "eGFR" = "eGFR_BL",
                                "Ejection fraction%" = "LVEF_BL",
                                "MRAs" = "RX_MRA_BL",
                                "Body mass index" = "BMI",
                                "Area deprivation index" = "ADI_NATRANK",
                                "Potassium" = "potassium_BL",
                                "Systolic BP" = "SBP_BL",
                                "Diuretics" = "RX_Diuretics_BL",
                                "Resynchronization therapy" = "CRT",
                                "Race" = "Race3cat",
                                "Valvular heart disease" = "VHD_BL",
                                "Smoking" = "SMK",
                                "Atrial fibrillation" = "AFibFlutter_BL",
                                "Cardiomyopathy" = "Cardiomyopathy_BL",
                                "Flu vaccine" = "flushot_1y",
                                "Digoxin" = "RX_Digoxin_BL",
                                "COPD" = "COPD_BL",
                                "Chronic kidney disease" = "CKD_BL",
                                "Myocardial infraction" = "MI_BL",
                                "Diabetes" = "DM_BL",
                                "Depression" = "MDD_BL",
                                "Peripheral artery disease" = "PAD_BL")) %>%
  # filter(rel.inf <= 1) %>%
  filter(rel.inf > 0)


imp.plot1 <- ggplot(imp, aes(x= reorder(var_label, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", width = 1, color = alpha(c("#193E8F"), 1), fill = alpha(c("#193E8F"), 0.8)) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  xlab("") +
  ylab("Relative influence") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 40, 10), limits = c(0, 40)) +
  # scale_y_continuous(expand = c(0,0), breaks = seq(0, 1, 0.2), limits = c(0, 1.1)) +
  theme(panel.grid = element_blank(),
        axis.line = element_line(size=1),
        axis.ticks = element_line(size=0.5))

ggsave(imp.plot1, filename = "Var_imp.tiff", width = 8, height = 10, dpi = 300, units = "in")

# keep learning_rate, depth, nodesize, ntrees for later use!!!
rm(gbm.fit, hyper_grid, hyper_grid2, m, i, train_time, 
   tuning_gbm, gbm.fit.final, imp1, imp.plot1)

gc()


