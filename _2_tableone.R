
### packages ###
library(tidyverse)
library(tableone)


df.bl <- readRDS("df_bl.rds")

vars.tbl.bl <- c("Age", "Gender", "Race", "Ethnicity", "ADI_NATRANK",
                 "SMK", "BMI", "BMICat", "SBP_BL", "DBP_BL", "LVEF_BL", 
                 "creatinine_BL", "eGFR_BL", "potassium_BL", 
                 "BNP_BL", "BNP_cat", "proBNP_BL", "proBNP_cat",
                 "AFibFlutter_BL", "Cardiomyopathy_BL", "CKD_BL", "COPD_BL", "DM_BL", "HTN_BL", 
                 "MDD_BL",  "MI_BL", "PAD_BL", "VHD_BL",
                 "RX_betaBlocker_BL", "RX_MRA_BL", "RX_SGLT2_BL",  "RX_Diuretics_BL", "RX_Digoxin_BL", 
                 "RX_GLP1_BL", "flushot_1y", "HFH_BL_cat",
                 "CRT", "time_implant", "time_HF",
                 "UserType", "year_index",
                 "Anemia_BL", "malignancy_BL", "DVT_PE_BL", "Dementia_BL", 
                 "Flu_BL", "Fracture_BL", "cerebral_BL", 
                 "albumin_BL", "hb_BL")

medianvars <- c("LVEF_BL", "creatinine_BL", "potassium_BL", "BNP_BL", "proBNP_BL", "albumin_BL", "hb_BL",
                "time_implant", "time_HF")


tbl.bl <- CreateTableOne(vars = vars.tbl.bl,
                         data = df.bl,
                         strata = "Arm",
                         includeNA = TRUE,
                         addOverall = T,
                         test = FALSE)

tblPrint <- print(tbl.bl,
                  nonnormal = medianvars,
                  showAllLevels = TRUE,
                  contDigits = 1,
                  test = FALSE,
                  missing = TRUE,
                  quote = FALSE,
                  varLabels = TRUE,
                  dropEqual = TRUE,
                  noSpaces = TRUE)


# write to .txt file
write.table(tblPrint,
            file = paste("table1_", Sys.Date(), ".txt", sep = ""),
            sep = "\t",
            quote = F,
            row.names = T,
            col.names = F)

