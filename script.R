setwd("~/Documents/Master2/data_mining")
rm(list=ls())
library(data.table)
library(plotly)
library(arules)
library(RColorBrewer)
library(arulesViz)
library(dplyr)

recipes <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
datas <- merge(recipes, reviews, by.x = "id", by.y = "recipe_id")

### Association rules
# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# https://eric.univ-lyon2.fr/~ricco/cours/slides/data%20frame%20avec%20r.pdf

# Data cleaning
recipes <- datas$ingredients[1:100]
recipes <- gsub("\\[", "", recipes)
recipes <- gsub("\\]", "", recipes)
recipes <- gsub("'", "", recipes)
for (i in 1:length(recipes)) {
  recipes[[i]] <- (strsplit(recipes[[i]], split = ', '))  
}
unlisted_ing <- unique(unlist(recipes))
sets <- list()
sets[[1]] <- data.frame(item_set=unlisted_ing, support=c(integer(length(unlisted_ing))))

### Our implementation of the algorithms
# Set the minimum support and minimum confidence thresholds
min_sup <- 0.1
min_conf <- 0.8

# Count support for each item
calculate_support <- function(recipe_sets, item_sets) {
  print("Support")
  i <- 0
  # For each transaction (combination of items)...
  for (item_set in item_sets$item_set) {
    i <- i +1
    if(i %% 1000 == 0) {
      print(i) 
    }
    # For each recipe...
    for (recipe_set in recipe_sets) {
      # Does the recipe contains each ingredient of the transaction ?
      if(all(item_set %in% unlist(recipe_set))) {
        # If yes, add one to the transaction count
        item_sets$support[i] <- item_sets$support[i] + 1
      }
    }
  }
  
  i <- 0
  nb_recipes <- length(recipe_sets)
  # For each transaction...
  for (sup in item_sets$support) {
    i <- i + 1
    # Divide the count by the total number of recipe, to obtain the support
    item_sets$support[i] <- sup / nb_recipes
  }
  return(item_sets)
}

# Filter item-sets over the given support  threshold
filter_items <- function(item_sets, threshold) {
  print("Filtering")
  return (item_sets[item_sets$support >= threshold,])
}

# Combine item-sets with n items to obtain n+1 items item_sets
combine_sets <- function(item_sets, base_sets) {
  print("Combining")
  dimension <- length(item_sets$item_set[[1]])
  print(dimension)
  temp_set <- (merge(x = item_sets, y = base_sets, by = NULL))
  vect <- c()
  j <- 0
  for (i in 1:length(temp_set[[1]])) {
    temp <- union(temp_set[[1]][i], temp_set[[3]][i])
    if(length(unlist(temp)) > dimension) {
      j <- j + 1
      vect[[j]] <- temp 
    }
  }
  return (data.frame(item_set=I(vect), support=integer(length(vect))))
}

#while() {
#}
sets[[1]] <- calculate_support(recipes, sets[[1]])
sets[[1]] <- filter_items(sets[[1]], min_sup)
i <- 1
while(nrow(sets[[i]]) > 1) {
  i <- i + 1
  sets[[i]] <- combine_sets(sets[[i-1]], sets[[1]])
  sets[[i]]  <- calculate_support(recipes, sets[[i]])
  sets[[i]] <- filter_items(sets[[i]], min_sup)
}

# Filter items over the minimum support threshold
# Combine filtered items to obtain next item-sets
# Filter and combine until you can't go further

# Hash Tree



### Test existing algorithms
ingredientsFile <- "datas/ingredients.csv"
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
