#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

#### Spreadsheet information -----------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        path <- "H:/GitHub Projects/C6-Pesticide-Validation/data/LCMSMS Validation Data.xlsx" 
} else { 
        path <- "~/Documents/GitHub/C6 Pesticide Validation/data/LCMSMS Validation Data.xlsx"
}

sheets <- excel_sheets(path = path)

#### Data Input -----------------------------

LC_longer2 <- data.frame(
        Compound = as.character(),
        Matrix = as.character(),
        Batch = as.numeric(),
        Result = as.numeric(),
        Mean = as.numeric(),
        SD = as.numeric(),
        n = as.numeric(),
        Value = as.numeric(),
        pct_sd = as.numeric(),
        bias = as.numeric(),
        pct_bias = as.numeric()
        )


for (j in seq(from = 3, to = 15, by = 2)){

LC_data <- read_excel(path, sheet = sheets[j], skip = 3)
        
#### Data Cleaning -----------------------------
        
n <- ncol(LC_data)
        
LC_data1 <- LC_data[,c(1,2:8)]
LC_data1$Batch <- 1


LC_data2 <- LC_data[,c(1, 10:16)]

for (i in 2:8){
        colnames(LC_data2)[i] = i-1 
}

LC_data2$Batch <- 2

LC_data3 <- LC_data[,c(1, 18:24)]

for (i in 2:8){
        colnames(LC_data3)[i] = i-1 
}
LC_data3$Batch <- 3

LC_long <- rbind (LC_data1, LC_data2, LC_data3)

LC_long <- LC_long[,c(1,9,2:8)]

LC_longer <- tidyr::gather(data = LC_long, key = Result, value = Value, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)

LC_longer$Value <- sapply(LC_longer$Value, as.numeric)



split <- strsplit(sheets[j], split="-")
LC_longer$Matrix <- split[[1]][1]


LC_longer$Mean <- 0.075
LC_longer$SD <- 0.001
LC_longer$n <- 20
LC_longer$Units <- "mg/kg"
LC_longer <- LC_longer[, c(1,5,2,3,6:8,4)]
LC_longer$pct_sd <- 100*LC_longer$SD/LC_longer$Mean
LC_longer$bias <- LC_longer$Value-LC_longer$Mean
LC_longer$pct_bias <- 100*LC_longer$bias/LC_longer$Mean

LC_longer2 <- rbind(LC_longer2,LC_longer)

}


# Data Input -------------------------------------------------------------

bias <- LC_longer2


# Bias--------------------------------------------------------------------


summary_bias <- bias %>% 
        group_by(Compound, Matrix) %>% 
        mutate(pct_bias = outliers(pct_bias)) %>% 
        summarize(n = sum(!is.na(pct_bias)), u_ref = mean(pct_sd, na.rm = TRUE)/sqrt(mean(n, na.rm = TRUE)), av_bias = mean(pct_bias, na.rm = TRUE), sd_bias = sd(pct_bias, na.rm = TRUE),UoB = sqrt(u_ref^2 + sd_bias^2))



## ------------------------------------------------------------------------


summary_bias <- summary_bias[,c(1:7)]


summary_bias$Significance <- "temp"

summary_bias <- na.omit(summary_bias)

nrsb <- nrow(summary_bias)

for (l in 1:nrsb){

        if ((2*summary_bias$UoB[l] > abs(summary_bias$av_bias[l])) == TRUE){
                
                summary_bias$Significance[l] = "Insignificant"
} else {
                summary_bias$Significance[l] = "Significant" 
        }
}
       

write.csv(summary_bias, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/LC outputs/LC_Bias_Summary.csv")

