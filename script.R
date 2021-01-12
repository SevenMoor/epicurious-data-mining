rm(list=ls())
library(data.table)
library(plotly)

df <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
recipes <- merge(df,reviews,by.x = "id",by.y = "recipe_id")

clusterized <- data.frame(matrix(ncol = 5, nrow = 0))
cluster_df_cols <- c("name","fat","sugar","protein","cluster")
colnames(clusterized) <- cluster_df_cols

for (i in 1:nrow(recipes)){
  nutrition <- recipes[i,"nutrition"]
  nutrition <- sub("\\[","",nutrition[1])
  nutrition <- sub("\\]","",nutrition)
  nutrition <- strsplit(nutrition,",")
  print(i)
  clusterized[i] <- c(recipes[i,"name"],nutrition[2],nutrition[3],nutrition[5],0)
}
