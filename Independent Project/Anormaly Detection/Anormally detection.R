# Load the dataset
#
path <- "D:/Moringa/Class/R/IP/IP Week 14/Supermarket_Sales_Forecasting - Sales.csv"

data <- read.csv(path)
View(data)
head(data, 7)

# Installing anomalize package
# ---
# 
install.packages("anomalize")

# Load tidyverse and anomalize 
# ---
# 
library(dplyr)
library(tidyverse)
library(anomalize)
library(AnomalyDetection)

# Detecting our anomalies
# ----
data$Date <- as.Date(data$Date, "%m/%d/%Y")
data_anom = data %>%
  as_tibble() %>%
  as_tbl_time(Date) %>%
  arrange(Date) %>%
  as_period("daily")

data_anom

# We identified several anomalies during the year 2019.
# On the first day of the year we notice an anomaly of value 457.
