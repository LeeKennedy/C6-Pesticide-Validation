#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

#### Spreadsheet information -----------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        path <- "H:/GitHub Projects/C6-Pesticide-Validation/data/GCMSMS Validation Data.xlsx" 
} else { 
        path <- "~/Documents/GitHub/C6 Pesticide Validation/data/GCMSMS Validation Data.xlsx"
}

sheets <- excel_sheets(path = path)

#### Data Input -----------------------------

GC_longer2 <- data.frame(
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


for (j in 1:7){


GC_data <- read_excel(path, sheet = sheets[j], skip = 4)
        
#### Data Cleaning -----------------------------
        
n <- ncol(GC_data)
        
if(n == 29) {
        GC_data1 <- GC_data[,c(1,3:9)]
} else {
        GC_data1 <- GC_data[,c(1,2:8)]
}

GC_data1$Batch <- 1


if(n == 29) {
        GC_data2 <- GC_data[,c(1, 12:18)]
} else {
        GC_data2 <- GC_data[,c(1, 11:17)]
}

for (i in 2:8){
        colnames(GC_data2)[i] = i-1 
}

GC_data2$Batch <- 2

if(n == 29) {
        GC_data3 <- GC_data[,c(1, 21:27)]
} else {
        GC_data3 <- GC_data[,c(1, 20:26)]
}

for (i in 2:8){
        colnames(GC_data3)[i] = i-1 
}
GC_data3$Batch <- 3

GC_long <- rbind (GC_data1, GC_data2, GC_data3)

GC_long <- GC_long[,c(1,9,2:8)]

GC_longer <- tidyr::gather(data = GC_long, key = Result, value = Value, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)

GC_longer$Value <- sapply(GC_longer$Value, as.numeric)

GC_longer$Matrix <- sheets[j]
GC_longer$Mean <- 0.075
GC_longer$SD <- 0.001
GC_longer$n <- 20
GC_longer$Units <- "mg/kg"
GC_longer <- GC_longer[, c(1,5,2,3,6:8,4)]
GC_longer$pct_sd <- 100*GC_longer$SD/GC_longer$Mean
GC_longer$bias <- GC_longer$Value-GC_longer$Mean
GC_longer$pct_bias <- 100*GC_longer$bias/GC_longer$Mean

GC_longer2 <- rbind(GC_longer2,GC_longer)

}


# Data Input -------------------------------------------------------------

bias <- GC_longer2


# Bias--------------------------------------------------------------------


summary_bias <- bias %>% 
        group_by(Compound, Matrix) %>% 
        mutate(pct_bias = outliers(pct_bias)) %>% 
        summarize(n = sum(!is.na(pct_bias)), u_ref = mean(pct_sd, na.rm = TRUE)/sqrt(mean(n, na.rm = TRUE)), av_bias = mean(pct_bias, na.rm = TRUE), sd_bias = sd(pct_bias, na.rm = TRUE),UoB = sqrt(u_ref^2 + sd_bias^2))



## ------------------------------------------------------------------------


summary_bias <- summary_bias[,c(1:7)]

nrsb <- nrow(summary_bias)
summary_bias$Significance <- "temp"

for (l in 1:nrsb){

        if ((2*summary_bias$UoB[l] > abs(summary_bias$av_bias[l])) == TRUE ){
                
                summary_bias$Significance[l] = "Insignificant"
} else {
                summary_bias$Significance[l] = "Significant" 
        }
}
       

write.csv(summary_bias, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/outputs/Bias_Summary.csv")

