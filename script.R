# setwd("~/Documents/Master2/data_mining")
rm(list=ls())
library(data.table)
library(plotly)
library(arules)
library(RColorBrewer)
library(arulesViz)

df <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
recipes <- merge(df,reviews,by.x = "id",by.y = "recipe_id")

# Association
# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# https://eric.univ-lyon2.fr/~ricco/cours/slides/data%20frame%20avec%20r.pdf

# Apriori Algorithm
ingredients <- recipes$ingredients[1:1000]
ingredients <- gsub("\\[", "", ingredients)
ingredients <- gsub("\\]", "", ingredients)
ingredients <- gsub("'", "", ingredients)
for (i in 1:length(ingredients)) {
  ingredients[[i]] <- strsplit(ingredients[[i]], split = ', ')  
}
ingredientsFile <- "datas/ingredients.csv"

### Our implementation of the algorithms
# Set the minimum support and minimum confidence thresholds
min_sup <- 0.001
min_conf <- 0.8

# Count support for each item
support <- array()
for (itemSet in ingredients) {
  for (item in itemSet) {
    if(length(item) > 1) {
      print(item)
    }
    if (is.na(support[item])) {
      support[item] <- 1
    }
    else  {
      support[item] <- support[item] + 1
    }  
  } 
}

support[60:80]
support["gingersnap crumbs"]
is.na(support["gingersnap crumbs"])
# Filter items over the minimum support threshold
# Combine filtered items to obtain next item-sets
# Filter and combine until you can't go further

# Hash Tree






### Test existing algorithms
write.csv(x = ingredients, file = ingredientsFile, quote = FALSE, row.names = FALSE)
ingredients_tr <- read.transactions(file = ingredientsFile, format = 'basket', sep = ',')

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(ingredients_tr, topN=20, type="absolute", col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(ingredients_tr, parameter = list(supp=0.002, conf=0.8,maxlen=10))
inspect(association.rules[1:10])

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]

#Plot SubRules
plotly_arules(subRules)
