
# Bush 631-603
# Week 2 R script

# How to use comments 

# Upload libraries (after install)
library(readxl)
library(haven)
library(tidyverse)

# Create objects
obj1 <- 55

obj2 <- 55 * 5

word1 <- "Test"

# Create vectors
vec1 <- c(1:11)

vec2 <- c("a","b","c")

vec3 <- c(seq(from = 10, to = 1000, by = 5))  

# Math on vectors
vec3_math <- sqrt(vec3)

vec3_math2 <- (vec3^2)/3.5

# Functions on vectors
range(vec3_math2)

sum(vec3_math)

# Build mini data with multiple vectors
dat1 <- c(vec3,vec3_math2)
print(dat1)

# Logical values
vec4 <- c(FALSE,TRUE,TRUE,FALSE,FALSE)

mean(vec4)
sum(vec4)

"Aggies" == "Aggies"

vec1 > 5

vec1 != 10




