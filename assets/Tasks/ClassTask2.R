
# Bush 631-603
# Class Task II

# Edit the script and save 

# Upload libraries
library(readxl)
library(haven)
library(tidyverse)

# Upload data - use the drop down menu or use read_dta for STATA file (include file directory - where the file is saved in YOUR machine!!)
insurgentdata <- read_dta("/Week3_Causality_II/Data_ClassTask1.dta")
View(insurgentdata)

# Subsets
sub_iraq <- subset(insurgentdata, subset = (hbase == "Iraq"))
sub_somalia <- subset(insurgentdata, subset = (hbase == "Somalia"))

# Table size and stick strategy
table(sub_iraq$size_rec, sub_iraq$stick)

# Diff-in-means
mean(sub_iraq$rev_fatlties) - mean(sub_somalia$rev_fatlties) 

# prop.table
prop.table(table(sub_iraq$reli))
prop.table(table(insurgentdata$reli))
