rm(list=ls())
library(data.table)
library(plotly)

df <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
recipes <- merge(df,reviews,by.x = "id",by.y = "recipe_id")

clusterized <- data.frame(matrix(ncol = 5, nrow =0))
cluster_df_cols <- c("name","fat","sugar","protein","cluster")
colnames(clusterized) <- cluster_df_cols



#Formatting data to get measures for the clusters
#------------------------------------------------
for (i in 1:1000){
  nutrition <- recipes[i,"nutrition"]
  nutrition <- sub("\\[","",nutrition[1])
  nutrition <- sub("\\]","",nutrition)
  nutrition <- unlist(strsplit(nutrition,","))
  print(i)
  print(nutrition)
  clusterized <- rbind(clusterized, data.frame(name=recipes[i,"name"],fat=as.numeric(nutrition[2]), sugar=as.numeric(nutrition[3]), protein=as.numeric(nutrition[5]), cluster=0))
}


#Initializing centroids and seeding clusters
#-------------------------------------------
centroids <- matrix(ncol=3,nrow=0)

cluster_count <- 5
if(cluster_count<nrow(clusterized)){
  pos <- sample(1:nrow(clusterized),cluster_count)
  print(pos)
  for(rec in pos){
    clusterized[rec,5] <- i 
    if(nrow(centroids)!=0){
      centroids <- rbind(centroids,c(clusterized[rec,2],clusterized[rec,3],clusterized[rec,4]))
    }
    else{
      centroids <- rbind(c(clusterized[rec,2],clusterized[rec,3],clusterized[rec,4]))
    }
  }
}


#Defining distance function
#--------------------------
distance <- function(a,center){
  return(sqrt((center[1]-a[2])**2+(center[2]-a[3])**2+(center[3]-a[4])**2))
}


#Iteration tracker
iteration <- 0

#Main iterations
#---------------
repeat{
  centroids_buffer <- centroids
  
  #Cluster assignment
  #------------------
  for(i in 1:nrow(clusterized)){
    closest <- NA
    distance_to_closest <- 0
    cat("Iteration ",iteration,", datapoint ",i,"\n")
    for(j in 1:cluster_count){
      if(is.na(closest) || distance_to_closest>distance(clusterized[i,],centroids[j,])){
        distance_to_closest <- distance(clusterized[i,],centroids[j,])
        closest <- j
      }
    }
    clusterized[i,5] <- closest
  }
  
  #Centroid update
  #---------------
  for(i in 1:cluster_count){
    x <- 0
    y <- 0
    z <- 0
    counter <- 0
    for(j in 1:nrow(clusterized)){
      if(clusterized[j,5]==i){
        counter <- counter+1
        x <- x+clusterized[j,2]
        y <- y+clusterized[j,3]
        z <- z+clusterized[j,4]
      }
    }
    centroids[i] <- c(x/counter,y/counter,z/counter)
  }
  
  if(centroids==centroids_buffer){
    break
  }
  iteration <- iteration+1
}

plot <- plot_ly(data = clusterized, 
                x=~fat,
                y=~sugar,
                z=~protein,
                color=~cluster,
                text=~paste("Recipe:",name,"\n=====\nfat:",fat,"\nsugar:",sugar,"\nprotein:",protein),
                marker=list(
                  size=3
                  )
                )
plot
htmlwidgets::saveWidget(as_widget(plot), "index.html")
