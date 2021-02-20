rm(list=ls())
library(data.table)
library(plotly)

df <- read.csv(file='datas/RAW_recipes.csv',sep=",",colClasses = c(NA,NA,"NULL","NULL","NULL","NULL",NA,"NULL","NULL","NULL",NA,"NULL"))
reviews <- read.csv(file ='datas/RAW_interactions.csv',sep=",",colClasses = c("NULL",NA,"NULL",NA,"NULL"))
reviews <- aggregate(reviews[2],reviews[1],mean)
recipes <- merge(df,reviews,by.x = "id",by.y = "recipe_id")

clusterized <- data.frame(matrix(ncol = 5, nrow =0))
cluster_df_cols <- c("id","fat","sugar","protein","cluster")
colnames(clusterized) <- cluster_df_cols



#Formatting data to get measures for the clusters
#------------------------------------------------
for (i in 1:1000){
  nutrition <- recipes[i,"nutrition"]
  nutrition <- sub("\\[","",nutrition[1])
  nutrition <- sub("\\]","",nutrition)
  nutrition <- unlist(strsplit(nutrition,","))
  print(i)
  clusterized <- rbind(clusterized, data.frame(id=recipes[i,"id"],fat=as.numeric(nutrition[2]), sugar=as.numeric(nutrition[3]), protein=as.numeric(nutrition[5]), cluster=0))
}


#Preprocessing of the data
#-------------------------
get_outlier_thresholds <- function(df,lower,upper,column){
  return(quantile( x = df[,  column], c(lower,upper), na.rm = TRUE))
}

fat_bounds <- get_outlier_thresholds(clusterized,.05,.95,2)
sugar_bounds <- get_outlier_thresholds(clusterized,.05,.95,3)
protein_bounds <- get_outlier_thresholds(clusterized,.05,.95,4)

clusterized<-clusterized[!(clusterized$fat<fat_bounds[1] | clusterized$fat> fat_bounds[2]),]
clusterized<-clusterized[!(clusterized$sugar<sugar_bounds[1] | clusterized$sugar> sugar_bounds[2]),]
clusterized<-clusterized[!(clusterized$protein<protein_bounds[1] | clusterized$protein> protein_bounds[2]),]

#Initializing centroids and seeding clusters
#-------------------------------------------
centroids <- matrix(ncol=3,nrow=0)

cluster_count <- 3
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
  
  moved <- 0
  
  cat("Iteration ",iteration,"\n")
  centroids_buffer <- centroids
  
  #Cluster assignment
  #------------------
  for(i in 1:nrow(clusterized)){
    closest <- NA
    distance_to_closest <- 0
    for(j in 1:cluster_count){
      if(is.na(closest) || distance_to_closest>distance(clusterized[i,],centroids[j,])){
        distance_to_closest <- distance(clusterized[i,],centroids[j,])
        closest <- j
      }
    }
    if(clusterized[i,5]!=closest){
      moved <- moved+1
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
  
  if(centroids==centroids_buffer|moved<max(1,nrow(clusterized)*0.001)){
    break
  }
  iteration <- iteration+1
}


#Printing scatter plot
#---------------------
clusterized <- exploitable
clusterized$cluster <- as.character(clusterized$cluster)

plot <- plot_ly(data = clusterized, 
                x=~fat,
                y=~sugar,
                z=~protein,
                color=~cluster,
                text=~paste("Recipe:",id,"\n=====\nfat:",fat,"\nsugar:",sugar,"\nprotein:",protein),
                marker=list(
                  size=3
                  ),
                colors=c("#A846DD","#4357AD","#00A6A6","#DB5461","#FFD97D","#36382E")
                )
plot <- plot %>% layout(title = 'Generated clusters')
plot


#Saving data for later use
#-------------------------
save(clusterized,file="clusters.Rda")
htmlwidgets::saveWidget(as_widget(plot), "index.html")
