rm(list=ls())
library(data.table)

df <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
recipes = merge(df,reviews,by.x = "id",by.y = "recipe_id")[2:length(recipes)]
