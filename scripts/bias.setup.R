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

GC_data <- read_excel(path, sheet = sheets[1], skip = 4)
        
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

GC_longer$Matrix <- sheets[1]
GC_longer$Mean <- 0.075
GC_longer$SD <- 0.005
GC_longer$n <- 20
GC_longer$Units <- "mg/kg"
GC_longer <- GC_longer[, c(1,5,2,3,6:8,4)]
GC_longer$pct_sd <- 100*GC_longer$SD/GC_longer$Mean
GC_longer$bias <- GC_longer$Value-GC_longer$Mean
GC_longer$pct_bias <- 100*GC_longer$bias/GC_longer$Mean
