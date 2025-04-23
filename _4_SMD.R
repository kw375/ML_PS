
### packages ###
library(tidyverse)
library(labelled)
library(splines)
library(splitstackshape)
library(survey)
library(tableone)


### impute missing ###
df.bl <- readRDS("df_bl.rds") %>%
  mutate(ADI_NATRANK = ifelse(is.na(ADI_NATRANK), median(ADI_NATRANK, na.rm = T), ADI_NATRANK),
         potassium_BL = ifelse(is.na(potassium_BL), median(potassium_BL, na.rm = T), potassium_BL)) %>%
  mutate(exp = case_when(Arm == "ARNI" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())

### set reference level ###
df.bl <- fun_ref(df.bl)


### Logistic regression-based propensity score ###
PS_LR <- fun_IPTW_LR(df.bl, cov_priori_ns)
df.bl.lr <- inner_join(df.bl,
                       select(PS_LR, ScrSSN, Arm, pd, IPTW),
                       by = c("ScrSSN", "Arm"))


### GBM-based propensity score ###
fit_num <- glm(Arm == "ARNI" ~ 1, data = df.bl, family = "binomial")
df.bl.gbm <- df.bl %>%
  mutate(pn = predict(fit_num, type = "response"),
         link_gbm = predict.gbm(gbm.fit.final, type = "link"),
         pd_gbm = exp(link_gbm)/(1 + exp(link_gbm)),
         IPTW_gbm = case_when(Arm == "ARNI" ~ pn/pd_gbm,
                              TRUE ~ (1-pn)/(1-pd_gbm)))


# check propensity score distribution
h.lr <- ggplot(df.bl.lr, aes(x = pd, color = Arm, fill = Arm)) + 
  geom_histogram(binwidth = 0.025, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#5596E6", "#ED4B4B")) +
  scale_fill_manual(values = c("#5596E6", "#ED4B4B")) +
  xlab("Propensity Score") +
  ylab("Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

h.gbm <- ggplot(df.bl.gbm, aes(x = pd_gbm, color = Arm, fill = Arm)) + 
  geom_histogram(binwidth = 0.025, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#5596E6", "#ED4B4B")) +
  scale_fill_manual(values = c("#5596E6", "#ED4B4B")) +
  xlab("Propensity Score") +
  ylab("Count") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggsave(h.lr, filename = "Hist_lr.tiff", width = 8, height = 6, dpi = 300, units = "in")
ggsave(h.gbm, filename = "Hist_gbm.tiff", width = 8, height = 6, dpi = 300, units = "in")


### Standardized mean difference ###

var_label(df.bl.lr) <- varLabelList
# var_label(df.bl.gbm) <- varLabelList

SMD.vars <- c("Age", "Gender", "Race3cat", "Ethnicity", "ADI_NATRANK",
              "SMK", "BMI", "SBP_BL", "LVEF_BL", 
              "eGFR_BL", "potassium_BL", 
              "AFibFlutter_BL", "Cardiomyopathy_BL", "CKD_BL", "COPD_BL", "DM_BL", "DysLipid_BL", "HTN_BL", 
              "MDD_BL",  "MI_BL", "PAD_BL", "VHD_BL",
              "RX_betaBlocker_BL", "RX_MRA_BL", "RX_SGLT2_BL",  "RX_Diuretics_BL", "RX_Digoxin_BL", 
              "flushot_1y", "HFH_BL_num",
              "CRT", "time_HF", "UserType")

# SMD without weighting
noweightedtbl <- CreateTableOne(vars = SMD.vars, 
                                strata = "Arm",
                                data = df.bl.lr,
                                test = FALSE)

# add IPTW
weighted_lr <- svyCreateTableOne(vars = SMD.vars,
                                 strata = "Arm",
                                 data = svydesign(ids = ~ 1, data = df.bl.lr, weights = ~ IPTW),
                                 test = FALSE)

weighted_gbm <- svyCreateTableOne(vars = SMD.vars,
                                  strata = "Arm",
                                  data = svydesign(ids = ~ 1, data = df.bl.gbm, weights = ~ IPTW_gbm),
                                  test = FALSE)

# create SMD plot
SMD.plot <- data.frame(variable = unlist(noweightedtbl$MetaData$varLabels),
                       Unadjusted = as.numeric(ExtractSmd(noweightedtbl)),
                       Weighted_lr = as.numeric(ExtractSmd(weighted_lr)),
                       Weighted_gbm = as.numeric(ExtractSmd(weighted_gbm)))


SMD.plot.melt <- reshape2::melt(data = SMD.plot,
                                id.vars = "variable",
                                variable.name = "Method",
                                value.name = "SMD") %>%
  mutate(variable_new = fct_relevel(variable, 
                                    "Year of index",
                                    "User Type",
                                    "T since HF, mo",
                                    "CRT",
                                    "No. HFH, past 12 mo",
                                    "Influenza vaccine, past 12 mo",
                                    "Digoxin",
                                    "Diuretics",
                                    "SGLT-2 inhibitors",
                                    "MRA",
                                    "\U03B2-blockers",
                                    "Valvular heart disease",
                                    "Peripheral artery disease",
                                    "Myocardial infraction",
                                    "Depression",
                                    "Hypertension",
                                    "Dyslipidemia",
                                    "Diabetes",
                                    "COPD",
                                    "Chronic kidney disease",
                                    "Cardiomyopathy",
                                    "A-fib/flutter",
                                    "Potassium, mEq/L",
                                    "eGFR, mL/min/1.73 m2",
                                    "LVEF, %",
                                    "SBP, mm Hg",
                                    "Body mass index",
                                    "Smoking status",
                                    "ADI",
                                    "Ethnicity",
                                    "Race",
                                    "Gender",
                                    "Age")) %>%
  mutate(Method = fct_recode(Method,
                             "Unadjusted" = "Unadjusted",
                             "LR-based" = "Weighted_lr",
                             "GBM-based" = "Weighted_gbm"))



SMDplot <- ggplot(SMD.plot.melt, aes(x = variable_new, y = SMD, group = Method, color = Method)) +
  geom_point(aes(shape = Method), size = 4) +
  geom_hline(aes(yintercept = 0.1), color = "black", linewidth = 1.2) +
  coord_flip() + 
  theme_minimal() +
  xlab(" ") +
  ylab("Standardized mean difference") +
  scale_color_manual(values = c("#ED4B4B", "#5596E6", "#E6A555")) +
  scale_shape_manual(values = c(17, 16, 18)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(linewidth=0.5),
        panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "darkgrey", linetype = "dashed"),
        axis.text = element_text(color = "black", size = 10),
        text = element_text(color = "black", size = 10))

ggsave(SMDplot, width = 7, height = 8, dpi = 450, units = "in",
       filename = paste("SMD_", Sys.Date(), ".tiff", sep = ""))


rm(noweightedtbl)
rm(weighted_lr)
rm(weighted_gbm)
rm(SMD.plot)
rm(SMD.plot.melt)
rm(SMD.vars)
rm(SMDplot)
