
### labels ###
varLabelList <- list(Arm = "Treatment strategy",
                     UserType = "User Type",
                     Age = "Age",
                     Gender = "Gender",
                     Race = "Race",
                     Race3cat = "Race",
                     Ethnicity = "Ethnicity",
                     # rurality = "Rurality",
                     # Employment = "Employment status",
                     ADI_NATRANK = "Area deprivation index",
                     SMK = "Smoking",
                     BMI = "Body mass index",
                     BMICat = "Body mass index",
                     LVEF_BL = "Ejection fraction%",
                     SBP_BL = "Systolic BP",
                     DBP_BL = "DBP",
                     creatinine_BL = "Creatinine, mg/dL",
                     eGFR_BL = "eGFR",
                     potassium_BL = "Potassium",
                     BNP_BL = "BNP, pg/mL",
                     BNP_cat = "BNP, pg/mL",  
                     proBNP_BL = "NT-proBNP, pg/mL",
                     proBNP_cat = "NT-proBNP, pg/mL",
                     # albumin_BL = "Albumin, g/dL", 
                     # hb_BL = "Hemoglobin, g/dL",
                     # BNP_comb = "BNP/NT-proBNP",
                     # proBNP_convert = "Converted NT-proBNP",
                     # proBNP_convert_log = "Converted NT-proBNP, log",
                     HFH_BL_num = "HF hospitalization",
                     HFH_BL_cat = "HF hospitalization",
                     year_index = "Year of index",
                     AFibFlutter_BL = "Atrial fibrillation",
                     Cardiomyopathy_BL = "Cardiomyopathy",
                     CKD_BL = "Chronic kidney disease",
                     COPD_BL = "COPD",
                     DM_BL = "Diabetes",
                     DysLipid_BL = "Dyslipidemia",
                     HTN_BL = "Hypertension",
                     MDD_BL = "Depression",
                     MI_BL = "Myocardial infraction",
                     # OSA_BL = "Obstructive sleep apnea",
                     PAD_BL = "Peripheral artery disease",
                     VHD_BL = "Valvular heart disease",
                     # Anemia_BL = "Anemia",
                     # malignancy_BL = "Malignancy",
                     # ARD_BL = "ARD",
                     # DVT_PE_BL = "DVT/PE",
                     # SRD_BL = "SRD",
                     # Dementia_BL = "Dementia",
                     # Flu_BL = "Flu",
                     # Fracture_BL = "Fracture",
                     # HemoStroke_BL = "ICH",
                     # IschemicStroke_BL = "Ischemic Stroke",
                     # SAH_BL = "SAH",
                     # TIA_BL = "TIA",
                     RX_betaBlocker_BL = "\U03B2-blockers",
                     RX_MRA_BL = "MRAs",
                     RX_SGLT2_BL = "SGLT-2 inhibitors",
                     RX_Diuretics_BL = "Diuretics",
                     RX_Diuretics_loop_BL = "Diuretic, loop",
                     RX_Diuretic_TZD_BL = "Diuretic, thiazide",
                     RX_Digoxin_BL = "Digoxin",
                     RX_GLP1_BL = "GLP-1",
                     RX_statin_BL = "Statin",
                     flushot_1y = "Flu vaccine",
                     time_implant = "T since implantation, mo",
                     time_HF = "HF duration",
                     CRT = "Resynchronization therapy",
                     Death_itt_event = "Death, intention-to-treat",
                     Death_itt_time = "Follow-up months, intention-to-treat")


### set reference levels ###
fun_ref <- function(df) {
  df.bl$Arm <- relevel(df.bl$Arm, ref = "ACEI/ARB")
  df.bl$Gender <- relevel(df.bl$Gender, ref = "Men")
  df.bl$Race <- relevel(df.bl$Race, ref = "White")
  df.bl$Race3cat <- relevel(df.bl$Race3cat, ref = "White")
  df.bl$Ethnicity <- relevel(df.bl$Ethnicity, ref = "Non-Hispanics")
  df.bl$SMK <- relevel(df.bl$SMK, ref = "Never")
  df.bl$AFibFlutter_BL <- relevel(df.bl$AFibFlutter_BL, ref = "No")
  df.bl$Cardiomyopathy_BL <- relevel(df.bl$Cardiomyopathy_BL, ref = "No")
  df.bl$CKD_BL <- relevel(df.bl$CKD_BL, ref = "No")
  df.bl$COPD_BL <- relevel(df.bl$COPD_BL, ref = "No")
  df.bl$DM_BL <- relevel(df.bl$DM_BL, ref = "No")
  df.bl$HTN_BL <- relevel(df.bl$HTN_BL, ref = "No")
  df.bl$MDD_BL <- relevel(df.bl$MDD_BL, ref = "No")
  df.bl$MI_BL <- relevel(df.bl$MI_BL, ref = "No")
  df.bl$PAD_BL <- relevel(df.bl$PAD_BL, ref = "No")
  df.bl$VHD_BL <- relevel(df.bl$VHD_BL, ref = "No")
  df.bl$RX_betaBlocker_BL <- relevel(df.bl$RX_betaBlocker_BL, ref = "No")
  df.bl$RX_MRA_BL <- relevel(df.bl$RX_MRA_BL, ref = "No")
  df.bl$RX_SGLT2_BL <- relevel(df.bl$RX_SGLT2_BL, ref = "No")
  df.bl$RX_Diuretics_BL <- relevel(df.bl$RX_Diuretics_BL, ref = "No")
  df.bl$RX_Digoxin_BL <- relevel(df.bl$RX_Digoxin_BL, ref = "No")
  df.bl$RX_GLP1_BL <- relevel(df.bl$RX_GLP1_BL, ref = "No")
  df.bl$flushot_1y <- relevel(df.bl$flushot_1y, ref = "No")
  df.bl$HFH_BL_cat <- relevel(df.bl$HFH_BL_cat, ref = "0")
  df.bl$CRT <- relevel(df.bl$CRT, ref = "No")
  df.bl$year_index <- relevel(df.bl$year_index, ref = "2020")
  
  return(df)
}


### confounders from a priori approach with ns() ###
cov_priori_ns <- c("ns(trial_num, 3)",
                   "ns(Age, df = 3)",
                   "Gender",
                   "Race3cat",
                   "Ethnicity",
                   "ns(ADI_NATRANK, 3)",
                   "SMK",
                   "ns(BMI, 3)", 
                   "ns(SBP_BL, df = 3)", 
                   "ns(LVEF_BL, df = 3)", 
                   "ns(eGFR_BL, df = 3)",
                   "ns(potassium_BL, df = 3)",
                   "AFibFlutter_BL", 
                   "Cardiomyopathy_BL", 
                   "CKD_BL", 
                   "COPD_BL",
                   "DM_BL", 
                   "HTN_BL", 
                   "MDD_BL", 
                   "MI_BL", 
                   "PAD_BL", 
                   "VHD_BL", 
                   "RX_betaBlocker_BL", 
                   "RX_MRA_BL", 
                   "RX_SGLT2_BL", 
                   "RX_Diuretics_BL", 
                   "RX_Digoxin_BL", 
                   "flushot_1y", 
                   "HFH_BL_cat",
                   "CRT", 
                   "UserType", 
                   "ns(time_HF, 3)")


cov_priori_gbm <- c("trial_num",
                    "Age",
                    "Gender",
                    "Race3cat",
                    "Ethnicity",
                    "ADI_NATRANK",
                    "SMK",
                    "BMI", 
                    "SBP_BL", 
                    "LVEF_BL", 
                    "eGFR_BL",
                    "potassium_BL",
                    "AFibFlutter_BL", 
                    "Cardiomyopathy_BL", 
                    "CKD_BL", 
                    "COPD_BL",
                    "DM_BL", 
                    "HTN_BL", 
                    "MDD_BL", 
                    "MI_BL", 
                    "PAD_BL", 
                    "VHD_BL", 
                    "RX_betaBlocker_BL", 
                    "RX_MRA_BL", 
                    "RX_SGLT2_BL", 
                    "RX_Diuretics_BL", 
                    "RX_Digoxin_BL", 
                    "flushot_1y", 
                    "HFH_BL_cat",
                    "CRT", 
                    "UserType", 
                    "time_HF")

cov_datadriven_gbm <- c("trial_num",
                        "Age",
                        "Gender",
                        "Race3cat",
                        "Ethnicity",
                        "ADI_NATRANK",
                        "SMK",
                        "BMI", 
                        "SBP_BL", 
                        "LVEF_BL", 
                        "eGFR_BL",
                        "potassium_BL",
                        "AFibFlutter_BL", 
                        "Cardiomyopathy_BL", 
                        "CKD_BL", 
                        "COPD_BL",
                        "DM_BL", 
                        "HTN_BL", 
                        "MDD_BL", 
                        "MI_BL", 
                        "PAD_BL", 
                        "VHD_BL", 
                        "RX_betaBlocker_BL", 
                        "RX_MRA_BL", 
                        "RX_SGLT2_BL", 
                        "RX_Diuretics_BL", 
                        "RX_Digoxin_BL", 
                        "flushot_1y", 
                        "HFH_BL_cat",
                        "CRT", 
                        "UserType", 
                        "time_HF",
                        # predictors
                        "rurality", 
                        "Employment", 
                        "OSA_BL", 
                        "DysLipid_BL", 
                        "Anemia_BL", 
                        "malignancy_BL", 
                        "DVT_PE_BL", 
                        "Dementia_BL", 
                        "Flu_BL", 
                        "Fracture_BL", 
                        "cerebral_BL", 
                        "albumin_BL", 
                        "hb_BL", 
                        "time_implant")

### function for logistic regression-based propensity score ###
fun_IPTW_LR <- function(df, cov) {
  
  # formula
  f_num <- paste("exp == 1  ~ 1", sep = "")
  f_denom <- paste("exp == 1 ~ ", paste(cov, collapse = " + "), sep = "")
  
  # IPTW numerator
  p_num <- glm(formula = f_num,
               family = "binomial",
               data = df, x = FALSE, y = FALSE)
  
  # IPTW denominator
  p_denom <- glm(formula = f_denom,
                 family = "binomial",
                 data = df, x = FALSE, y = FALSE)
  
  # print(summary(p_denom))
  
  df$pn <- predict(p_num, type = "response")
  df$pd <- predict(p_denom, type = "response")
  
  # calculate IPTW
  df$IPTW <- ifelse(df$exp == 1, df$pn/df$pd,
                    (1-df$pn)/(1-df$pd))
  
  output <- df %>%
    select(ScrSSN, Arm, exp, startdate, id, pn, pd, IPTW)
  
  return(output)
  
}

# intention-to-treat estimates
fun_itt_LR <- function(df,
                       timevar,
                       eventvar,
                       cov,
                       coxHR = FALSE) {
  
  df <- df %>%
    mutate(id = row_number())
  
  df.weights <- fun_IPTW_LR(df = df,
                            cov = cov)
  
  df.itt <- df %>%
    select(ScrSSN, Arm, exp, startdate, id, {{timevar}}, {{eventvar}}) %>%
    mutate(surv = {{timevar}}) %>%
    expandRows("surv", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == surv & {{eventvar}} == 1, 1, 0)) 
  
  model_data <- inner_join(df.itt,
                           select(df.weights, id, IPTW),
                           by = c("id"))
  
  if(coxHR) {
    fit.itt.hr <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24)),
                      family = "quasibinomial", data = model_data,
                      weights = IPTW)
    
    estimates <- coeftest(fit.itt.hr, vcov = sandwich)
    
    print(estimates)
    print(coefci(fit.itt.hr, vcov = sandwich))
    
    
  } else {
    events <- model_data %>%
      group_by(time, exp) %>%
      summarise(events = sum(event), .groups = "keep") %>%
      ungroup() %>%
      pivot_wider(names_from = exp, values_from = events) %>%
      rename(events_0 = "0",
             events_1 = "1") %>%
      mutate(events_0 = cumsum(events_0),
             events_1 = cumsum(events_1))
    
    fit.itt.risk <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24)) + 
                          exp*ns(time, knots = c(6, 12, 24)),
                        family = "quasibinomial", data = model_data, 
                        weights = IPTW, x = FALSE, y = FALSE)  
    
    # create table shell
    arm_0 <- data.frame(0, seq(0, 41))
    arm_1 <- data.frame(1, seq(0, 41))
    
    colnames(arm_0) <- c("exp", "time")
    colnames(arm_1) <- c("exp", "time")
    
    # 1 - prob
    arm_0$p_0 <- 1 - predict(fit.itt.risk, arm_0, type = "response")
    arm_1$p_1 <- 1 - predict(fit.itt.risk, arm_1, type = "response")
    
    # survival
    arm_0$s_0 <- cumprod(arm_0$p_0)
    arm_1$s_1 <- cumprod(arm_1$p_1)
    
    # merge and get cumulative incident
    estimates <- merge(arm_0, arm_1, by = "time") %>%
      merge(events, by = "time") %>%
      mutate(cuminc_0 = 1 - s_0,
             cuminc_1 = 1 - s_1,
             rd = cuminc_1 - cuminc_0,
             rr = cuminc_1/cuminc_0) %>%
      arrange(time) %>%
      select(time, events_0, events_1, cuminc_0, cuminc_1, rd, rr)
    
    rm(model_data, events, fit.itt.risk, arm_0, arm_1)
    gc()
    
    return(estimates)
  }
}



fun_itt_gbm <- function(df,
                        timevar,
                        eventvar,
                        cov,
                        n.trees = ntrees,
                        interaction.depth = depth,
                        shrinkage = learning_rate,
                        n.minobsinnode = nodesize,
                        coxHR = FALSE) {
  
  df <- df %>%
    mutate(id = row_number())
  
  f_num <- paste("exp == 1  ~ 1", sep = "")
  
  # IPTiW numerator
  fit_num <- glm(formula = f_num,
                 family = "binomial",
                 data = df, x = FALSE, y = FALSE)
  
  # shrinkage 0.005, interaction.depth 2, n.minobsinnode 100
  gbm.formula <- as.formula(paste("Arm == 'ARNI' ~ ", paste(cov, collapse = " + "), sep = ""))
  
  set.seed(13)
  gbm.fit <- gbm(gbm.formula,
                 distribution = "bernoulli",
                 data = df.bl,
                 n.trees = n.trees,
                 interaction.depth = interaction.depth,
                 shrinkage = shrinkage,
                 n.minobsinnode = n.minobsinnode,
                 cv.folds = 0,
                 n.cores = NULL,
                 verbose = FALSE)
  
  df.itt <- df %>%
    select(ScrSSN, Arm, exp, startdate, id, {{timevar}}, {{eventvar}}, everything()) %>%
    mutate(pn = predict(fit_num, type = "response"),
           link_gbm = predict.gbm(gbm.fit, type = "link"),
           pd_gbm = exp(link_gbm)/(1 + exp(link_gbm)),
           IPTW_gbm = case_when(exp == 1 ~ pn/pd_gbm,
                                TRUE ~ (1-pn)/(1-pd_gbm))) %>%
    mutate(surv = {{timevar}}) %>%
    expandRows("surv", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == surv & {{eventvar}} == 1, 1, 0)) 
  
  
  if(coxHR) {
    fit.itt.hr <- glm(event == 1 ~ exp + # Cardiomyopathy_BL + RX_Diuretics_BL + UserType + ns(trial_num, 3) +   # <== doubly robust adjustment
                      ns(time, knots = c(6, 12, 24)),
                      family = "quasibinomial", data = df.itt,
                      weights = IPTW_gbm)
    
    print(summary(fit.itt.hr))
    
    estimates <- coeftest(fit.itt.hr, vcov = sandwich)
    
    print(estimates)
    print(coefci(fit.itt.hr, vcov = sandwich))
    
  }  else {
    
    fit.itt.risk <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24)) + 
                        # Cardiomyopathy_BL + RX_Diuretics_BL + UserType + ns(trial_num, 3) +      # <== doubly robust adjustment
                          exp*ns(time, knots = c(6, 12, 24)),
                        family = "quasibinomial", data = df.itt, 
                        weights = IPTW_gbm, x = FALSE, y = FALSE)  
    
    # create shells
    arm_0 <- data.frame(0, seq(0, 41))
    arm_1 <- data.frame(1, seq(0, 41))
    # doubly robust adjustment
    # arm_0 <- data.frame(0, seq(0, 41), factor("No"), factor("No"), factor("Prevalent"), 37.14)
    # arm_1 <- data.frame(1, seq(0, 41), factor("No"), factor("No"), factor("Prevalent"), 37.14)
    
    colnames(arm_0) <- c("exp", "time")
    colnames(arm_1) <- c("exp", "time")
    # colnames(arm_0) <- c("exp", "time", "Cardiomyopathy_BL", "RX_Diuretics_BL", "UserType", "trial_num")
    # colnames(arm_1) <- c("exp", "time", "Cardiomyopathy_BL", "RX_Diuretics_BL", "UserType", "trial_num)
    
    # 1 - prob
    arm_0$p_0 <- 1 - predict(fit.itt.risk, arm_0, type = "response")
    arm_1$p_1 <- 1 - predict(fit.itt.risk, arm_1, type = "response")
    
    # survival
    arm_0$s_0 <- cumprod(arm_0$p_0)
    arm_1$s_1 <- cumprod(arm_1$p_1)
    
    # merge and cum inc
    estimates <- merge(arm_0, arm_1, by = "time") %>%
      mutate(cuminc_0 = 1 - s_0,
             cuminc_1 = 1 - s_1,
             rd = cuminc_1 - cuminc_0,
             rr = cuminc_1/cuminc_0) %>%
      arrange(time) %>%
      select(time, cuminc_0, cuminc_1, rd, rr)
    
    rm(df, gbm.fit, fit.itt.risk, arm_0, arm_1)
    gc()
    
    return(estimates)
    
  }
}



### function for bootstrapping ###
fun_resample <- function(dat2) {dat2[sample(1:nrow(dat2), nrow(dat2), replace = TRUE), ]}

fun_bootstrap <- function(seed, n, df, timevar, eventvar, cov, coxHR, approach) {
  
  if(approach == "LR") {
    fun_itt <- fun_itt_LR
  } else {
    fun_itt <- fun_itt_gbm
  }
  
  set.seed(seed)
  
  replicate(n, fun_resample(df), FALSE) %>%
    map(~ fun_itt(df = .,
                  timevar = {{timevar}},
                  eventvar = {{eventvar}},
                  cov = {{cov}},
                  coxHR = {{coxHR}})) %>%
    bind_rows() %>%
    group_by(time) %>%
    summarise(cuminc_0_lcl = quantile(cuminc_0, probs = 0.025, na.rm = T),
              cuminc_0_ucl = quantile(cuminc_0, probs = 0.975, na.rm = T),
              cuminc_1_lcl = quantile(cuminc_1, probs = 0.025, na.rm = T),
              cuminc_1_ucl = quantile(cuminc_1, probs = 0.975, na.rm = T),
              rd_lcl = quantile(rd, probs = 0.025, na.rm = T),
              rd_ucl = quantile(rd, probs = 0.975, na.rm = T),
              rr_lcl = quantile(rr, probs = 0.025, na.rm = T),
              rr_ucl = quantile(rr, probs = 0.975, na.rm = T),
              .groups = "keep")
}

fun_itt_boot <- function(dat,
                         time,
                         event,
                         covariate,
                         approach,
                         coxHR = FALSE,
                         seeds,
                         nboot) {
  
  if(approach == "LR") {
    fun_itt <- fun_itt_LR
  } else {
    fun_itt <- fun_itt_gbm
  }
  
  est <- fun_itt(df = dat, timevar = {{time}}, eventvar = {{event}}, cov = covariate, coxHR = FALSE)
  
  ci <- fun_bootstrap(seed = seeds, n = nboot, df = dat, timevar = {{time}}, eventvar = {{event}}, 
                      cov = covariate, coxHR = FALSE, approach = approach)
  
  final <- inner_join(est, ci, by = c("time"))
  
  return(final)
}


### adjusted cumulative incidence plot ###
fun_cumIncPlot <- function(df, 
                           df_boot,
                           expmed = "ARNI",
                           timevar = flu_itt_time, eventvar = flu_itt_event,
                           expmedcolor = "#ED4B4B",
                           ymax = 12, ybreaks = 3) {
  
  d0 <- df_boot
  
  data.km <- sqldf("SELECT 0 as exp, time, 100*cuminc_0 as cuminc, 100*cuminc_0_lcl as lcl, 100*cuminc_0_ucl as ucl
                      FROM d0
                 UNION
                 SELECT 1 as exp, time, 100*cuminc_1 as cuminc, 100*cuminc_1_lcl as lcl, 100*cuminc_1_ucl as ucl
                      FROM d0") %>%
    mutate(Arm = factor(case_when(exp == 0 ~ "ACEI/ARB",
                                  TRUE ~ expmed),
                        levels = c(expmed, "ACEI/ARB")))
  
  surv.formula <- as.formula(paste("Surv(", {{timevar}}, ", ", {{eventvar}}, ") ~ Arm", sep = ""))
  
  survfit <- survfit(surv.formula, data = df)
  survfit$call$formula <- surv.formula
  survfit$call$data <- df
  
  
  survplot <- ggsurvplot(survfit, fun = "event", censor = T,  risk.table = T, risk.table.col = "strata",
                         xlim = c(0, 42), break.x.by = 6)
  
  table.atrisk <- survplot$table$data %>%
    mutate(Arm = factor(case_when(Arm == "ACEI/ARB" ~ "ACEI/ARB",
                                  TRUE ~ expmed), 
                        levels = c(expmed, "ACEI/ARB")))
  
  p.atrisk <- ggplot(table.atrisk, aes(x = time, y = Arm, label = n.risk, colour = Arm)) +
    geom_text(size = 4.5) +
    scale_y_discrete(limits = c("ACEI/ARB", expmed), labels = c("ACEI/ARB", expmed)) +
    ylab(" ") +
    xlab(" ") +
    scale_color_manual(values = c(expmedcolor, "#5596E6")) +
    theme_minimal() +
    ggtitle("Num at risk") +
    coord_cartesian(xlim = c(0, 42),
                    clip = "on") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = c("#5596E6", expmedcolor), size = 13, face = "bold"),
          legend.position = "none",
          plot.margin = unit(c(-0.5, 1, 0, 0.5), "lines"),
          plot.title = element_text(face = "bold", size = 13, hjust = 0))
  
  p.curv <- ggplot(data.km, aes(x = time, y = cuminc, group = Arm)) +
    geom_line(aes(color = Arm), lwd = 1.2) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Arm), alpha = 0.3) +
    theme_minimal() +
    scale_color_manual(values = c(expmedcolor, "#5596E6")) +
    scale_fill_manual(values = c(expmedcolor, "#5596E6")) +
    scale_x_continuous(limits = c(0, 42), breaks = seq(0, 42, 6)) +
    scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, ybreaks)) +
    ylab("Cumulative Incidence (%)") +
    xlab("Months") +
    coord_cartesian(xlim = c(0, 42),
                    clip = "on") +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.position = "none",
          legend.text = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13, face = "bold"))
  
  g <- (p.curv/p.atrisk) + 
    plot_layout(heights = c(1, 0.25)) &
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  return(g)
}


