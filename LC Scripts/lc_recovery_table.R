#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

options (stringsAsFactors = FALSE)

#### Spreadsheet information -----------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        path <- "H:/GitHub Projects/C6-Pesticide-Validation/data/LCMSMS Validation Data.xlsx" 
} else { 
        path <- "~/Documents/GitHub/C6 Pesticide Validation/data/LCMSMS Validation Data.xlsx"
}

sheets <- excel_sheets(path = path)

#### Data Input -----------------------------

LC_All <- data.frame(
        Compound = as.character(),
        Replicate = as.character(),
        Result = as.numeric(),
        Batch = as.numeric(),
        Matrix = as.character())


for (j in c(3,5,7,9,11,13,15)){


LC_data <- read_excel(path, sheet = sheets[j], skip = 3)

#### Data Cleaning -----------------------------

LC_data1 <- LC_data[,c(1,2:8)]

LC_data_long1 <- tidyr::gather(data = LC_data1, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
LC_data_long1$Batch <- 1



LC_data2 <- LC_data[,c(1,10:16)]

for (i in 2:8){
    colnames(LC_data2)[i] = i-1 
}

LC_data_long2 <- tidyr::gather(data = LC_data2, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
LC_data_long2$Batch <- 2

LC_All1 <- rbind(LC_data_long1, LC_data_long2)

LC_data3 <- LC_data[,c(1,18:24)]

for (i in 2:8){
        colnames(LC_data3)[i] = i-1 
}
LC_data_long3 <- tidyr::gather(data = LC_data3, key = Replicate, value = Result, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)
LC_data_long3$Batch <- 3

LC_All1 <- rbind(LC_All1, LC_data_long3)

split <- strsplit(sheets[j], split="-")
LC_All1$Matrix <- split[[1]][1]

LC_All <- rbind(LC_All, LC_All1)
}

GC_All$Result <- as.numeric(GC_All$Result)

### Summary ---------------------------------

LC_summary <- LC_All %>% 
        group_by(Matrix, Compound) %>% 
        summarise(Recovery = round(100*mean(Result, na.rm=TRUE)/0.075,1))

LC_summary <- as.data.frame(LC_summary)

LC_wide <- spread(LC_summary, Matrix, Recovery)

  
#write_csv(LC_wide, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/LC outputs/Recovery_Matrix.csv")                
                  
if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(LC_wide, "H:/GitHub Projects/C6-Pesticide-Validation/LC outputs/LC_Recovery_Matrix.csv")
} else { 
        write_csv(LC_wide, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/LC outputs/LC_Recovery_Matrix.csv")
        
}