#### Clean Up environment -----------------------------
rm(list=ls())

#### Packages -----------------------------
library(readxl)
library(tidyverse)
library(dts.quality)

#### Functions -----------------------------


#### Data Input -----------------------------

precision <- read_csv("LC outputs/LC_Reproducibility.csv")
bias <- read_csv("LC outputs/LC_Bias_Summary.csv")


#### Data Merging -----------------------------

data_MU <- merge(precision, bias)

nr <- nrow(data_MU)

data_MU <- data_MU[, c(1:5,8:12)]

for (x in 1:nr) {
        if ((data_MU$Significance[x] == "Significant") == TRUE) {
                data_MU$MU[x] <- 2*sqrt(data_MU$Reproducibility[x]^2+(0.075*data_MU$UoB[x]/100)^2) + abs(0.075*data_MU$av_bias[x]/100)
        } else {
                data_MU$MU[x] <- 2*sqrt(data_MU$Reproducibility[x]^2+(0.075*data_MU$UoB[x]/100)^2) 
        } 
}

data_MU$pct_MU <- 100*data_MU$MU/0.075


if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(data_MU, "H:/GitHub Projects/C6-Pesticide-Validation/LC outputs/LC_MU_Combined.csv")
} else { 
        write_csv(data_MU, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/LC outputs/LC_MU_Combined.csv")
        
}
