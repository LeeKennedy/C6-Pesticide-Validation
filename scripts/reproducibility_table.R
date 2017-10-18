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

#GC_precision <- data.frame(
#        Compound = as.character(),
#        Matrix = as.character(),
#        Repeatability = as.numeric(),
#        Reproducibility = as.numeric())

####   Taking one line ---------------------------------------



batch1 <- GC_data[1,c(3:9)]

batch2 <- GC_data[1,c(12:18)]
for (i in 1:7){
        colnames(batch2)[i] = i 
}

batch3 <- GC_data[1,c(21:27)]
for (i in 1:7){
        colnames(batch3)[i] = i 
}

batch_set <- rbind(batch1, batch2, batch3)
batch_set$Batch <- c("A", "B", "C")
batch_set <- batch_set[,c(8,1:7)]
batch_set
batchlong <- tidyr::gather(data = batch_set, key = Run, value = Value, na.rm = FALSE, `1`, `2`, `3`, `4`, `5`, `6`, `7`)

batch_aov <- aov(Value~Batch, data = batchlong)
summary(batch_aov)

#Repeatability & Reproducibility
mean.sqr <- summary(batch_aov)[1][[1]][[3]]
ncount <- as.numeric(length(batch_aov$effects))/as.numeric(length(batch_aov$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)

GC_precision$Compound <- as.character(GC_precision$Compound)
GC_precision$Matrix <- as.character(GC_precision$Matrix)

GC_precision <- as.data.frame(cbind(sdr, sdR))
GC_precision$Matrix <- sheets[1]
GC_precision$Compound <- as.list((GC_data)[1,1])
GC_precision <- GC_precision[,c(4,3,1,2)]
