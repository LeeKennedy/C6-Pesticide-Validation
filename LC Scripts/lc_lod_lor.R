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

### Apple -----------------------------------------------------------------

lod_apple <- read_excel(path, sheet = "LOR_LOD Data_Apple", skip = 4)
lod_apple$C <- as.numeric(lod_apple$C)
colnames(lod_apple)[1] <- "Compound"
lod_apple$Matrix <- "Apple"
lod_apple <- lod_apple[,c(1,12,2:8)]

nr <- nrow(lod_apple)

lod_apple$SD <- 1

for (xx in 1:nr) {
        temp <- (lod_apple[xx,3:9])
        lod_apple$SD[xx] <- sd(temp, na.rm = TRUE)  
}

lod_apple$LOD <- 3*lod_apple$SD
lod_apple$LOR <- 10*lod_apple$SD

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(lod_apple, "H:/GitHub Projects/C6-Pesticide-Validation/outputs/LOD_LOR_Apple.csv")
} else { 
        write_csv(lod_apple, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/outputs/LOD_LOR_Apple.csv")
        
}

### Spinach ---------------------------------------------------------------


lod_spinach <- read_excel(path, sheet = "LOR_LOD Data_Spinach", col_types = c("text", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric", 
                                                                              "numeric", "numeric", "numeric"), 
                          skip = 4)


colnames(lod_spinach)[1] <- "Compound"
lod_spinach$Matrix <- "Spinach"
lod_spinach <- lod_spinach[,c(1,11,2:8)]

nr <- nrow(lod_spinach)

lod_spinach$SD <- 1

for (xx in 1:nr) {
        temp <- (lod_spinach[xx,3:9])
        lod_spinach$SD[xx] <- sd(temp, na.rm = TRUE)  
}

lod_spinach$LOD <- 3*lod_spinach$SD
lod_spinach$LOR <- 10*lod_spinach$SD

if("Windows" %in% Sys.info()['sysname'] == TRUE){ 
        
        write_csv(lod_spinach, "H:/GitHub Projects/C6-Pesticide-Validation/outputs/LOD_LOR_Spinach.csv")
} else { 
        write_csv(lod_spinach, "/Users/Study Old/Documents/GitHub/C6 Pesticide Validation/outputs/LOD_LOR_Spinach.csv")
        
}

