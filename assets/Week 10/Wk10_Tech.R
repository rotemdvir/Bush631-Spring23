
# Bush 631-603
# Week 10 R script

# Edit the script and save 

# Upload libraries (after install)
library(readxl)
library(haven)
library(tidyverse)

# Upload data - use the drop down menu or use read_dta for STATA file (include file directory - where the file is saved in YOUR machine!!)
insurgentdata <- read_dta("/Week3_Causality_II/Data_ClassTask1.dta")
View(insurgentdata)

# Subset groups operating in Iraq
sub.insurg <- insurgentdata %>% 
  filter(hbase == "Iraq")

sub.insurg

# Subset all groups but the ones operating in Iraq
sub.insurg2 <- insurgentdata %>% 
  filter(hbase != "Iraq")

# Subset groups operating in Iraq or Somalia
sub.insurg3 <- insurgentdata %>% 
  filter(hbase == "Iraq" | hbase == "Somalia")

sub.insurg3

# Create new var
# Define name, use ifelse to define values
insurgentdata <- insurgentdata %>%
  mutate(yrs90 = ifelse(year < 2000, "90s Rock","You're too old"))

prop.table(table(NewVar_Proportions = insurgentdata$yrs90))

# Organize column (variable) by value (numeric or text)
insurgentdata %>% dplyr::select(year,hbase,rev_fatlties) %>% head(n=4)

insurgentdata %>% dplyr::select(year,hbase,rev_fatlties) %>% 
  arrange(rev_fatlties) %>% head(n=4)

# New dataset with summary stats for selected variables per group
new.dat <- insurgentdata %>% 
  group_by(group) %>% 
  summarise(fatal.mean = mean(rev_fatlties, na.rm = T),
            fatal.med = median(rev_fatlties, na.rm = T),
            mx.battle = max(ucdpbd, na.rm = T),
            mn.battle = min(ucdpbd, na.rm = T))
new.dat

