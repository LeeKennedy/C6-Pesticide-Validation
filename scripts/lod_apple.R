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


lod_apple <- read_excel(path, sheet = "LOR_LOD Data_Apple", skip = 4)
lod_apple$C <- as.numeric(lod_apple$C)
colnames(lod_apple)[1] <- "Compound"
lod_apple$Matrix <- "Apple"
lod_apple <- lod_apple[,c(1,12,2:8)]



