# Install packages.
#
install.packages("tidyverse")
install.packages("modelr")
install.packages("broom")

# Load Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs


# Loading the dataset.
#
carrefour = read.csv("http://bit.ly/CarreFourDataset")
head(carrefour)
View(carrefour)

# Before you start data cleaning, you need to understand the structure of the data.
class(carrefour)

# We can see the dimensions of the weather dataframe using the dim() function.
dim(carrefour)

# We can see the dataframe column names using names() function.
names(carrefour)


# It will show a  summary of the object's internal structure.
str(carrefour)

# The glimpse() function shows the same information as str()
glimpse(carrefour)

# Check the top rows.
head(carrefour)



# Check Bottom rows of the dataframe.
tail(carrefour)

## Univariate Analysis
### Data Visualization
dev.off()

#### Histogram Plots of numeric variables.
# Unit.Price.

unit.price <- c(carrefour$Unit.price)
hist(unit.price)

# The Unit Price of most of the products below 100
# Have a frequency of 100 and above.


# Quantity.

Quantity <- c(carrefour$Quantity)
hist(Quantity)

# Quantities between the range 2 and 10 have frequencies ranging between 75 and 120.
# Quantity of value 1 have a frequency of 200.

# Tax.

Tax <- c(carrefour$Tax)
hist(Tax)

# We observe that an increase in Tax leads to a decrease in the frequency of the product tax.

# Gross Margin Percentage.

gross.margin.percentage <- c(carrefour$gross.margin.percentage)
hist(gross.margin.percentage)

# There is no significant difference between the frequencies of gross margin percentage

# Gross Income.

gross.income <- c(carrefour$gross.income)
hist(gross.income)

# We observe that an increase in the gross income leads to a decrease in it's frequency.


# Rating.

Rating <- c(carrefour$Rating)
hist(Rating)

# We notice that the frequencies of ratings range between 70 and 100.

# Total.

Total <- c(carrefour$Total)
hist(Total)

# The frequencies of most of the total cash on product is above 100 
# With the exception of total amount of 100 with a frequency of 90.



## Bivariate Analysis.
# Unit Price Against Total.
plot(Total, unit.price)

# An increase in the unit price leads to an increase in the total amount.


# Quantity against Total.
plot(Total, Quantity)

# An increase in the Quantity leads to an increase in the total amount.


# Tax against Total.
plot(Total, Tax)
# An increase in the amount of tax leads to an increase in the total amount.


# Rating against Total
plot(Total, Rating)
# Plot of Total amount and Rating is not directly proprotional.
# Hence an increase in rating does not lead to an increase in total.




















# Installing and loading our caret package
# ---
# 
suppressWarnings(
  suppressMessages(if
                   (!require(caret, quietly=TRUE))
    install.packages("caret")))
library(caret)

# Installing and loading the corrplot package for plotting
# ---
# 
suppressWarnings(
  suppressMessages(if
                   (!require(corrplot, quietly=TRUE))
    install.packages("corrplot")))
library(corrplot)

df <- carrefour[c(2:8,10,11)]
head(df)

Branch <- df$Branch
Customer.type <- df$Branch
Gender <- df$Branch
Product.line <- df$Product.line
Payment <- df$Payment


### Label Encoding Categorical varibles.
Branch <- as.numeric(Branch)
Customer.type < as.numeric(Customer.type)
Gender <- as.numeric(c(Gender))
Product.line <- as.numeric(Product.line)
Payment < as.numeric(Payment)

# Numerical data
library(dplyr)
num <- select_if(df, is.numeric)
num

cat <- select_if(df, is.factor)
cat
total_df <- carrefour$Total

# Merge the Categorical dataframes to the numeric dataframe.

data <- cbind(Branch, Customer.type, Gender, Product.line, Payment)

head(data)

summary(data)

class(data)

# merge two data frames by ID and Country
total <- merge(num, data)

total <- merge(total, total_df)
head(total)


## Dimensionality Reduction
## tSNE

# Installing Rtnse package
# 
install.packages("Rtsne")

# Loading our tnse library
# 
library(Rtsne)

head(total)

 # Curating the database for analysis 
# 
Labels<-total$Total
carrefour$Total<-as.factor(carrefour$Total)


# For plotting
#
colors = rainbow(length(unique(carrefour$Total)))
names(colors) = unique(carrefour$Total)

# Executing the algorithm on curated data
# 
tsne <- Rtsne(carrefour[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

# Getting the duration of execution
# 
exeTimeTsne <- system.time(Rtsne(carrefour[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

# Plotting our graph and closely examining the graph
# 
par(mar=c(1,1,1,1))
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=carrefour$Total, col=colors[carrefour$Total])





## Feature Selection

# Calculating the correlation matrix
# ---
#
correlationMatrix <- cor(total[,-1])

# Find attributes that are highly correlated
# ---
#
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# Highly correlated attributes
# ---
# 
highlyCorrelated

names(total[,highlyCorrelated])
# We obsearve that Tax and Branch have the highest Correlation


# Removing Redundant Features 
# ---
# 
Dataset2<-total[-highlyCorrelated]

colSums(is.na(Dataset2))


na.omit(Dataset2)

# We removed redundant features and in our case they were Tax and Branch features.


# Performing our graphical comparison
# ---
#
dev.off()
par(mfrow = c(1, 2))
corrplot(correlationMatrix)
corrplot(cor(Dataset2))


