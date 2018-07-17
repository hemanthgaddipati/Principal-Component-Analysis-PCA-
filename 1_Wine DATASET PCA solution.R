## Loading Wine dataset

  mydata<-read.csv("C:/Users/SRIRAMA/Desktop/Data Science/Assignments and stuff/PCA/wine.csv") 
  View(mydata)


## the first column in mydata has Type number

  View(mydata[-1]) 

## mydata[-1] -> Considering only values that matter for applying PCA

  data <- mydata[-1]
  
  attach(data)
  
  cor(data)
  
  pcaObj<-princomp(mydata[-1], cor = TRUE, scores = TRUE, covmat = NULL)
  
  summary(pcaObj)
  
  str(pcaObj)
  
  loadings(pcaObj)

  plot(pcaObj) # graph showing importance of principal components 

  # Comp.1 having highest importance (highest variance)

    biplot(pcaObj)

  #pcaObj$loadings

    pcaObj$scores[,1:3] # Top 3 PCA Scores which represents almostall the data

## cbind used to bind the data in column wise

## Considering top 3 principal component scores and binding them with mydata

    mydata<-cbind(mydata,pcaObj$scores[,1:3])

    View(mydata)

## preparing data for clustering (considering only pca scores as they represent the entire data)

    clus_data<-mydata[,15:17]

## Normalizing the data 

    norm_clus<-scale(clus_data) # Scale function is used to normalize data

    dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance(here I am considering Euclidean distance)

# Clustering the data using hclust function --> Hierarchical

    fit1<-hclust(dist1,method="complete") # method here is complete linkage

    plot(fit1) # Displaying Dendrogram

    ### finding K value for k means clustering
    normalized.data <- scale(mydata[,2:14])
    kvalue = (nrow(normalized.data)-1)*sum(apply(normalized.data, 2, var))		 # Determine number of clusters by scree-plot 
    for (i in 2:14) kvalue[i] = sum(kmeans(normalized.data, centers=i)$withinss)
    plot(1:14, kvalue, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
    title(sub = "K-Means Clustering Scree-Plot")
    
    groups<-cutree(fit1,3) # Cutting the dendrogram for 3 clusters

    membership_1<-as.matrix(groups) # cluster numbering 

    View(membership_1)

    final1<-cbind(membership_1,mydata) # binding column wise with orginal data
    
    View(final1)

    View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) 

    plot(final1)

# drawn from the aggregate of the wine members data on membership_1

  write.csv(final1,file="wineDATASET_clustered.csv",row.names = F,col.names = F)

  getwd()
  