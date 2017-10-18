
# Data Input -------------------------------------------------------------

bias <- GC_longer

bias <- bias[-c(1:9),1:14]

#colnames(bias) <- c("Type", "Reference", "Date", "Test_Code", "Analyte", "Unit", "Matrix","Reference_Mean", "sd","no","Lab_Result"##,"pct_sd", "Bias", "pct_Bias")


# Data Cleaning ----------------------------------------------------------




# Bias--------------------------------------------------------------------


summary_bias <- bias %>% 
        group_by(Compound, Matrix) %>% 
        mutate(pct_bias = outliers(pct_bias)) %>% 
        summarize(n = sum(!is.na(pct_bias)), u_ref = mean(pct_sd, na.rm = TRUE)/sqrt(mean(n, na.rm = TRUE)), av_bias = mean(pct_bias, na.rm = TRUE), sd_bias = sd(pct_bias, na.rm = TRUE),UoB = sqrt(u_ref^2 + sd_bias^2))


summary_bias <- summary_bias %>% 
        filter(n >1)
summary_bias <- summary_bias[,c(1:7)]

summary_bias

write.csv(summary_bias, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/outputs/Bias_Summary.csv")
