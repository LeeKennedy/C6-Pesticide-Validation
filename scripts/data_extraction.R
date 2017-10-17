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

GC_All <- data.frame(
        Compound = as.character(),
        Replicate = as.character(),
        Result = as.numeric(),
        Batch = as.numeric(),
        Matrix = as.character())


for (j in 1:7){

GC_data <- read_excel(path, sheet = sheets[j], skip = 4)

#### Data Cleaning -----------------------------

n <- ncol(GC_data)

if(n == 29) {
        GC_data1 <- GC_data[,c(1,3:9)]
} else {
        GC_data1 <- GC_data[,c(1,2:8)]
}

GC_data_long1 <- tidyr::gather(data = GC_data1, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
GC_data_long1$Batch <- 1


if(n == 29) {
        GC_data2 <- GC_data[,c(1,12:18)]
} else {
        GC_data2 <- GC_data[,c(1,11:17)]
}



for (i in 2:8){
    colnames(GC_data2)[i] = i-1 
}

GC_data_long2 <- tidyr::gather(data = GC_data2, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
GC_data_long2$Batch <- 2

GC_All1 <- rbind(GC_data_long1, GC_data_long2)


if(n == 29) {
        GC_data3 <- GC_data[,c(1,21:27)]
} else {
        GC_data3 <- GC_data[,c(1,20:26)]
}

for (i in 2:8){
        colnames(GC_data3)[i] = i-1 
}

GC_data_long3 <- tidyr::gather(data = GC_data3, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
GC_data_long3$Batch <- 3

GC_All1 <- rbind(GC_All1, GC_data_long3)

GC_All1$Matrix <- sheets[j]

GC_All <- rbind(GC_All, GC_All1)
}

GC_All$Result <- as.numeric(GC_All$Result)

### Summary ---------------------------------

GC_summary <- GC_All %>% 
        group_by(Matrix, Batch, Compound) %>% 
        summarise(Recovery = round(100*mean(Result, na.rm=TRUE)/0.075,1))

GC_summary <- as.data.frame(GC_summary)

GC_wide <- spread(GC_summary, Matrix, Recovery)
                  
                  
