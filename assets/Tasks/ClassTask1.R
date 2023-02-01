
# Bush 631-603
# Class Task I

# Edit the script and save 

# Upload libraries (after install)
library(readxl)
library(haven)
library(tidyverse)

# Upload data - use the drop down menu or use read_dta for STATA file (include file directory - where the file is saved in YOUR machine!!)
insurgentdata <- read_dta("/Week3_Causality_II/Data_ClassTask1.dta")
View(insurgentdata)

# Show rows (155,215,235,411), columns (group name, home base, year, age,)
insurgentdata[c(155,215,235,411),c(3,4,5,21)]

# Functions
# mean & median
mean(insurgentdata$rev_fatlties)
median(insurgentdata$rev_fatlties)

# min & max
min(insurgentdata$ucdpbd)
max(insurgentdata$ucdpbd)
range(insurgentdata$ucdpbd)

# sum all
summary(insurgentdata)

# sum: rev_fatlties; ucdpbd
summary(insurgentdata$rev_fatlties)
summary(insurgentdata$ucdpbd)
