dataset <- read.csv(file.choose(),header = TRUE)
#dataset <- dataset[ ,1:(ncol(dataset)-1)]
dataset


k = 2

Centroids <- data.frame()
for (i in 1:k){
  j = as.numeric(readline(paste("Enter the row number of the data point to be considered as the centroid  of cluster ",i,": ")))
  Centroids <- rbind(Centroids,dataset[j, ])
}



cont = TRUE
cluster_mat<- matrix(c(0),nrow = nrow(dataset),ncol = 1)


counter = 0
while(cont == TRUE){
  for (i in 1:nrow(dataset)){
    dist <- c()
    for (j in 1:k){
      dist[j] = sqrt(sum((dataset[i, ] - Centroids[j, ])**2))
    }
    cluster_mat[i,1] = which( dist == min(dist) )
  }
  new_centroids <- data.frame()
  for (i in 1:k){
    for (j in 1:ncol(dataset)){
      new_centroids[i,j] = mean(dataset[which(cluster_mat[,1]==i),j])
    }
  }
  dist <- matrix(c(0),nrow = k,ncol = 1)
  for(i in 1:k){
    for(j in 1:ncol(dataset)){
      if(Centroids[i,j]!=new_centroids[i,j]){
        dist[i,1] = 1
      }
    }
  }
  Centroids <- new_centroids
  if(all(dist[,1]==0)){
    cont = FALSE
  }
  else{
    counter = counter+1
    print(paste("Iteration : ",counter))
  }
}

for (i in 1:k){
  print(paste("Cluster ",i," : "))
  print(dataset[which(cluster_mat[,1]==i), ])
  cat("\n")
}

for (i in 1:k){
  print(paste("Cluster ",i," contains ",nrow(dataset[which(cluster_mat[,1]==i), ])," data points"))
  cat("\n")
}

cluster_mat

nkk <- data.frame()
nkk <- cbind(dataset[ ,ncol(dataset)])

nkf <-data.frame()
nkf <- cbind(dataset[,1])
data_set <- data.frame()

dataset <- dataset[ ,2:(ncol(dataset)-1)]

Centroids <- Centroids[ ,2:(ncol(Centroids)-1)]


for(j in 1:k)
{
  nk <- data.frame()
  cen <- c()
  d<- c()
for(i in 1:nrow(dataset))
{
  if(cluster_mat[i,1] == j)
  {
    nk <- rbind(nk,dataset[i, ])
    
    d[i] = i
  }
  cen = Centroids[j,]
}
  
for(l in 1:ncol(nk))
{
  
  nk[,l] = replace(nk[,l],which(nk[,l] == 0),cen[,l])
  
}
  
  d = as.numeric(na.omit(d))
  for(x in 1:length(d))
  {
    dataset[d[x], ] <- nk[x,]
  }
}


data_set <- cbind(nkf,dataset,nkk)
names(data_set)[1] <- "No of times pregnant"
names(data_set)[9] <-  "Class variable"
write.csv(data_set,"removed_noise.csv")
