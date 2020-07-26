# We first we install the required arules library 
#
install.packages("arules")

# Loading the arules library
#
library(arules)

# Loading our Supermarket's dataset from our csv file
# ---
path<-"http://bit.ly/SupermarketDatasetII" 

Dataset<-read.transactions(path, sep=',')

head(Dataset, 10)
View(Dataset)

# Verifying the object's class
# ---
# This should show us transactions as the type of data that we will need
# ---
# 
class(Dataset)

# If we wanted to preview the items that make up our dataset,
# alternatively we can do the following
# ---
# 
items<-as.data.frame(itemLabels(Dataset))
colnames(items) <- "Item"
head(items, 10) 

# distribution of the item sets (no. of items purchased in each transaction), etc.
# ---
# 
summary(Dataset)


# some operation in percentage terms of the total transactions 
# 
itemFrequency(Dataset[, 40:50],type = "absolute")
round(itemFrequency(Dataset[, 40:50],type = "relative")*100,2)

# Displaying top 10 most common items in the transactions dataset 
# and the items whose relative importance is at least 10%
# 
par(mfrow = c(1, 2))

# plot the frequency of items
itemFrequencyPlot(Dataset, topN = 1,col="darkgreen")
itemFrequencyPlot(Dataset, support = 0.01,col="darkred")


# Building a model based on association rules 
# using the apriori function 
# ---
# We use Min Support as 0.001 and confidence as 0.8
# ---
# 
rules <- apriori (Dataset, parameter = list(supp = 0.001, conf = 0.8))
rules


# ---
# 
summary(rules)

# Observing rules built in our model i.e. first 5 model rules
# ---
# 
inspect(rules[1:5])


# Interpretation of the first rule:
# ---
# If someone buys frozen smoothie, they are 88.9% likely to buy mineral water.


# We can also use different criteria such as: (by = "lift" or by = "support")
# 
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])


