
# Bush 631-603
# Week 3 R script

# Edit the script and save 

# Upload libraries (after install)
library(readxl)
library(haven)
library(tidyverse)

# Upload data - use the drop down menu or use read_dta for STATA file (include file directory - where the file is saved in YOUR machine!!)
insurgentdata <- read_dta("/Week3_Causality_II/Data_ClassTask1.dta")
View(insurgentdata)

## Ways to look at data (click the data on the right panel - opens new tab with data)

# Top 6 rows
head(insurgentdata)
head(insurgentdata, n=10)

# list all columns (variables)
glimpse(insurgentdata)

# slice - Top 10 rows
slice(insurgentdata)

