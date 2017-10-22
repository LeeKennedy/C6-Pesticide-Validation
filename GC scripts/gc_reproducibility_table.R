#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

options(stringsAsFactors = FALSE)

#### Spreadsheet information -----------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        path <- "H:/GitHub Projects/C6-Pesticide-Validation/data/GCMSMS Validation Data.xlsx" 
} else { 
        path <- "~/Documents/GitHub/C6 Pesticide Validation/data/GCMSMS Validation Data.xlsx"
}

sheets <- excel_sheets(path = path)

GC_precision2 <- data.frame(
        Compound = as.character(),
        Matrix = as.character(),
        Repeatability = as.numeric(),
        Reproducibility = as.numeric())

#### Data Input -----------------------------

for (m in c(1,2,3,4,5,6,7)){

GC_data <- read_excel(path, sheet = sheets[m], skip = 4)

n_rows <- nrow(GC_data)


####   Taking one line ---------------------------------------

for (k in 1:n_rows) {
        
n <- ncol(GC_data)
        
if(n == 29) {
        batch1 <- GC_data[k,c(3:9)]
} else {
        batch1 <- GC_data[k,c(2:8)]
}


if(n == 29) {
        batch2 <- GC_data[k,c(12:18)]
} else {
        batch2 <- GC_data[k,c(11:17)]
}

for (i in 1:7){
        colnames(batch2)[i] = i 
}

if(n == 29) {
        batch3 <- GC_data[k,c(21:27)]
} else {
        batch3 <- GC_data[k,c(20:26)]
}


for (i in 1:7){
        colnames(batch3)[i] = i 
}

batch_set <- rbind(batch1, batch2, batch3)
batch_set <- as.data.frame(sapply(batch_set, as.numeric))
batch_set$Batch <- c("A", "B", "C")
batch_set <- batch_set[,c(8,1:7)]
batch_set
batchlong <- tidyr::gather(data = batch_set, key = Run, value = Value, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)

batch_aov <- aov(Value~Batch, data = batchlong, na.rm = TRUE)

#Repeatability & Reproducibility
mean.sqr <- summary(batch_aov)[1][[1]][[3]]
ncount <- as.numeric(length(batch_aov$effects))/as.numeric(length(batch_aov$coefficients))
Repeatability <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
Reproducibility <- sqrt(Repeatability^2 + interim^2)



GC_precision <- as.data.frame(cbind(Repeatability, Reproducibility))

GC_precision$Matrix <- sheets[m]
comp <- as.list((GC_data)[k,1])
GC_precision$Compound <- comp[[1]]

GC_precision <- GC_precision[,c(4,3,1,2)]
GC_precision2 <- rbind(GC_precision2, GC_precision)
}
}

GC_precision2$Precision <- 2*GC_precision2$Reproducibility

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(GC_wide, "H:/GitHub Projects/C6-Pesticide-Validation/GC outputs/Reproducibility.csv")
} else { 
        write_csv(GC_precision2, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/GC outputs/Reproducibility.csv")
        
}