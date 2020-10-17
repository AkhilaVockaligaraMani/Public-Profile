pkgs <- c("factoextra",  "NbClust")
install.packages(pkgs)
install.packages("tidyr")
library(cluster)
library(fclust)
library(tidyr)
library(mice)
library(dplyr)
library(dbscan)
library(data.table)
library(fpc)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
library(lubridate)
library(ggplot2)
library(devtools)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(NbClust)
library(mice)
library(VIM)
library(dplyr)
library(dbscan)
library(data.table)
library(fpc)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(ggplot2)
library(NbClust)
library(tidyverse)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)

# read data from csv
setwd("C:\\Users\\akhil\\OneDrive\\Desktop\\ThesisWork\\2 Analysis")
# load data from csv
table_from_csv <- read.csv(file = "DL_DATA_6123F_01MIN_2017.csv",header =FALSE,sep =",", 
                           stringsAsFactors=FALSE )
colnames(table_from_csv) <- c("INTERVAL_01_MINS","UM10","UM40")
View(table_from_csv)
ncol(table_from_csv)
nrow(table_from_csv)
#########################################################################################
table_from_csv <- na.omit(table_from_csv)
#Mean of all columns
numdata<-table_from_csv[sapply(table_from_csv, is.numeric)]  
sapply(numdata, mean, na.rm = T)  # Returns a vector
lapply(numdata, mean, na.rm = T)

#impute variable mean for each missing value
for(i in 1:ncol(table_from_csv)){
  table_from_csv[,i]=ifelse(is.na(table_from_csv[,i]),
                            ave(table_from_csv[,i],FUN=function(y) mean(y, na.rm = TRUE)),
                            table_from_csv[,i])
}
View(table_from_csv)
table_imputed_matrix <- table_from_csv
View(table_imputed_matrix)
summary(table_imputed_matrix)
#######################################################################################
# Historgram for each attribute
table_imputed_matrix %>% 
  gather(Attributes, value, 2:3) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency")
#######################################################################################
table_imputed_matrix$Year <- factor(year(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Month <- factor(month(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Day <- factor(day(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Weekday <- factor(wday(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Hour <- factor(hour(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Minute <- factor(minute(table_imputed_matrix$INTERVAL_01_MINS))
table_imputed_matrix$Hour_Minute = paste(table_imputed_matrix$Hour, 
                                         table_imputed_matrix$Minute, sep=":")
#############################
df <- table_imputed_matrix %>% 
  filter(Month=='10',Day=='6')

#df <- table_imputed_matrix
#scatter plot data
attach(df)

pairs(~UM10+UM40,data=df,
      main="Simple Scatterplot Matrix",col=rgb(0,100,0,50,maxColorValue=255), pch=20)
######################################################################################
# Find optimal k
set.seed(123)
# function to compute total within-cluster sum of squares
fviz_nbclust(df[,2:3], kmeans, method = "wss", k.max = 50) + theme_minimal() + 
  ggtitle("the Elbow Method")

# Silhouette method
fviz_nbclust(df[,2:3], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df[,2:3], kmeans, nstart = 25,k=30,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
#########################################################################################
#fuzzy cmeans
res.fcm <- fcm(df[,2:3,centers=3])


###################################################################
##############################
#KMeans
set.seed(123)
#km.res <- kmeans(df, 100, nstart = 25)
km.res <- kmeans(df[,2:3], 4, iter.max = 1000, nstart = 1,
                 algorithm = c("Lloyd"), trace=FALSE)
df <- cbind(df, cluster = km.res$cluster)
plotcluster(df[,2:3],km.res$cluster)
#Plot clusters
#############################
#DBScan
# Find the suitable eps
kNNdistplot(df[,2:3], k=100)
abline(h=20,lty=2)

set.seed(123)
#res <- fpc::dbscan(table_imputed_matrix, eps = .9, MinPts = 10)
resdb <- dbscan::dbscan(df[,2:3], eps =20, minPts  = 100)
resdb$cluster
plot(df[,2:3], col=resdb$cluster)
points(df[resdb$cluster==0,], pch = 3, col = "grey")
hullplot(df[,2:3], resdb)
df <- cbind(df, cluster = resdb$cluster)
plotcluster(df[,2:3],resdb$cluster)
#Analyzing clusters
###########################################################################################################
#Yearly Analysis
plotdata_grp <- df %>% 
  filter(Year=="2018")%>%
  group_by(Month,Day,Hour,cluster) %>% 
  summarise(Sum = sum(cluster),FrequencyYear = n())
View(plotdata_grp)
# Plot with frequency and different colours
p <- ggplot(plotdata_grp, aes(Month,Day,Hour,cluster))
p + geom_point()
p + geom_point(aes(colour = factor(cluster),size = FrequencyYear))

# Frequency of each Cluster in a hour--2019/7/29, 31/7/2019, 31/8/2019
plotdata_month <- df %>% 
  filter(Year=="2018" & Month=="1")%>%
  group_by(Day,Hour,cluster) %>% 
  summarise(Sum = sum(cluster),FrequencyMonth = n())
View(plotdata_month)
# Plot with frequency and different colours
p <- ggplot(plotdata_month, aes(Day,Hour,cluster))
p + geom_point()
p + geom_point(aes(colour = factor(cluster),size = FrequencyMonth))

#Frequency of each Cluster in a hour--2019/7/29, 31/7/2019, 31/8/2019
plotdata_hour <- df %>% 
  filter(Year=="2017" & Month=="8" & Day== "1")%>%
  group_by(Hour,Minute,cluster) %>% 
  summarise(Sum = sum(cluster),FrequencyHour = n())
View(plotdata_hour)
#Plot with frequency and different colours
p <- ggplot(plotdata_hour, aes(Hour,Minute,cluster))
p + geom_point()
p + geom_point(aes(colour = factor(cluster),size = FrequencyHour))
##########################################################################################################
#Yearly Analysis
plotdata_grp <- df %>% 
  group_by(Hour,Minute,cluster) %>% 
  summarise(Sum = sum(cluster),Frequency1 = n())
View(plotdata_grp)
# Plot with frequency and different colours
p <- ggplot(plotdata_grp, aes(Hour,Minute,cluster))
p + geom_point()
p + geom_point(aes(colour = factor(cluster),size = Frequency1))
#########################################################################################################
avg_sil <- function(k) {
  km.res <- kmeans(df[,2:4], centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df[,2:4]))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
