library(readxl)
## Load the Data set

Rest_of_World_Data <- read_excel("~/Impact of Integration/Rest_of_World_Data_Cluster.xlsx", sheet = "Sheet1")
View(Rest_of_World_Data)

Germany <- subset(Rest_of_World_Data,Site_Country == 'DE')

colnames(Germany)

## Create a data set with only 4 variables that is used for clustering
subgroup_Germany<-Germany[c(4,5,8,9)]
subgroup2_Germany <- data.frame(scale(subgroup_Germany)) ## Standardized values for the 4 variables

# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- subgroup2_Germany
wss <- sapply(1:k.max, 
              function(k)
                {kmeans(data, k, nstart=20 )$tot.withinss}
              )
km.res_Germany <- kmeans(data,5,nstart = 20)
km.res_Germany$cluster
Germany_output <- data.frame(Germany,km.res_Germany$cluster)
View(Germany_output)
## Export to CSV
write.csv(Germany_output, "C:/Users/310269304/Documents/Impact of Integration/Germany_Clusters_4.csv")