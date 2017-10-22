#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

options(stringsAsFactors = FALSE)

#### Spreadsheet information -----------------------------

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        path <- "H:/GitHub Projects/C6-Pesticide-Validation/data/LCMSMS Validation Data.xlsx" 
} else { 
        path <- "~/Documents/GitHub/C6 Pesticide Validation/data/LCMSMS Validation Data.xlsx"
}

sheets <- excel_sheets(path = path)

LC_precision2 <- data.frame(
        Compound = as.character(),
        Matrix = as.character(),
        Repeatability = as.numeric(),
        Reproducibility = as.numeric())

#### Data Input -----------------------------

for (m in seq(from=3, to=15, by=2)){

LC_data <- read_excel(path, sheet = sheets[m], skip = 3)

n_rows <- nrow(LC_data)


####   Taking one line ---------------------------------------

for (k in 1:n_rows) {
        
n <- ncol(LC_data)
        

batch1 <- LC_data[k,c(2:8)]


batch2 <- LC_data[k,c(10:16)]

for (i in 1:7){
        colnames(batch2)[i] = i 
}

batch3 <- LC_data[k,c(18:24)]

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



LC_precision <- as.data.frame(cbind(Repeatability, Reproducibility))

split <- strsplit(sheets[m], split="-")
LC_precision$Matrix <- split[[1]][1]

comp <- as.list((LC_data)[k,1])
LC_precision$Compound <- comp[[1]]

LC_precision <- LC_precision[,c(4,3,1,2)]
LC_precision2 <- rbind(LC_precision2, LC_precision)
}
}

LC_precision2$Precision <- 2*LC_precision2$Reproducibility

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(LC_precision2, "H:/GitHub Projects/C6-Pesticide-Validation/LC outputs/LC_Reproducibility.csv")
} else { 
        write_csv(LC_precision2, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/LC outputs/LC_Reproducibility.csv")
        
}