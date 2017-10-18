# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(dts.quality)

# Data Input -------------------------------------------------------------

bias <- read_excel("W:/Quality Documents Controlled - General Access/Chemistry/MU/Bias Data Compilation.xlsx", 
           col_types = c("text", "text", "date", 
                         "text", "text", "text", "text", "numeric", 
                         "numeric", "numeric", "numeric", 
                         "numeric", "numeric", "numeric", 
                         "numeric", "numeric", "text"))

bias <- bias[-c(1:9),1:14]

colnames(bias) <- c("Type", "Reference", "Date", "Test_Code", "Analyte", "Unit", "Matrix","Reference_Mean", "sd","no","Lab_Result","pct_sd", "Bias", "pct_Bias")


# Data Cleaning ----------------------------------------------------------




# Bias--------------------------------------------------------------------


summary_bias <- bias %>% 
        group_by(Test_Code, Analyte, Matrix) %>% 
        mutate(pct_Bias = outliers(pct_Bias)) %>% 
        summarize(n = sum(!is.na(pct_Bias)), u_ref = mean(pct_sd, na.rm = TRUE)/sqrt(mean(no, na.rm = TRUE)), av_bias = mean(pct_Bias, na.rm = TRUE), sd_bias = sd(pct_Bias, na.rm = TRUE),UoB = sqrt(u_ref^2 + sd_bias^2))


summary_bias <- summary_bias %>% 
        filter(n >1)
summary_bias <- summary_bias[,c(1,2,3,4,6,8)]

summary_bias

write.csv(summary_bias, "MU_Bias_Summary2.csv")
