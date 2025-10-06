
# Load necessary libraries
library(ncdf4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(networkD3)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(rockchalk)
library(dplyr)
library(gtsummary)
library(car)
library(ordinal)
library(finalfit)
library(dplyr)
library(mice)
library(ggplot2)
library(plm)
library(flextable)
library(gt)
library(kableExtra)
library(pglm)
library(broom.mixed)
library(huxtable)
library(cluster)
library(NbClust)
library(factoextra)
library(lcmm)
library(flexmix)
library(mclust)
library(poLCA)
library(lavaan)
library(msm)
library(stats19)
library(agricolae)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(vcd)
library(networkD3)
library(poLCA)
library(psych)
library(nnet)
library(randomForest)
library(xfun)
library(pdp)
library(dunn.test)
library(RColorBrewer)
library(scales)
library(ggpubr)
library(ggExtra)
library(gridExtra)
library(gtsummary)
library(flextable)
library(officer)
# Load library
library(stats)
library(psych)

set.seed(123)

#clustering

fviz_nbclust(standardized_data,kmeans,method="wss")+labs(subtitle="Elbow method")
fviz_nbclust(standardized_data,kmeans,method="silhouette")+labs(subtitle="Sil method")
set.seed(200)
gap_stat<-clusGap(standardized_data,FUN=kmeans,nstart=25,K.max=10,B=50)
print(gap_stat,method="firstmax")
fviz_gap_stat(gap_stat)

set.seed(20)
data.matrix(standardized_data)
TIPOC3c<-(na.omit(data.matrix(standardized_data)))
Tipo3c<-dist(standardized_data,method="euclidean")
hclust(Tipo3c,method="ward.D")
tipo3C<-hclust(Tipo3c,method="ward.D")

#windows()
plot(tipo3C)

# Plot and save the dendrogram
png("Ddendrogram_plot.png", width = 800, height = 600)  # Open a PNG device
plot(tipo3C, main = "Dendrogram", xlab = "Observations", sub = "", ylab = "Height")  # Plot the dendrogram
rect.hclust(tipo3C, k = 4, border = "red")  # Add rectangles to the plot for 3 clusters


kc<-kmeans(standardized_data,4)
kc
kc$centers
write.csv(kc$centers,file="clusterpca4.csv")



dataset<-cbind(F4,kc$cluster) 
View(dataset)
dataset$cluster<-kc$cluster
View(dataset)

View(dataset)

cluster_summary <- dataset %>%group_by(cluster) %>%summarise(Mean_Variable1 = mean(age),SD_Variable1 = sd(age),Mean_Variable2 = mean(Duration),SD_Variable2 = sd(Duration),Mean_Variable3= mean(Occupation),SD_Variable3 = sd(Occupation),Mean_Variable4 = mean(Education),SD_Variable4 = sd(Education),Count = n())
cluster_summary
print(cluster_summary)
write.csv(cluster_summary,file="clustersummary.csv")

F6<-cbind(dataset,F5)
View(F6)
