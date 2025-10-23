
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

#load survey data (simulated)

SS1 <- read.csv("./data/survey/SS1.csv")
F4<-SS1%>%dplyr::select(Education,Occupation,Duration,age)
F5<-SS1%>%dplyr::select(que_6,que_15,que_24,Location,Point)

# Run KMO and Bartlett's Test
kmo_result <- KMO(F4)
bartlett_result <- cortest.bartlett(cor(F4), n = nrow(F4))

# Print results
print(kmo_result)
print(bartlett_result)

set.seed(123)
Data1<-as.data.frame.matrix(F4)

#PCA


pca_result <- prcomp(Data1, center = TRUE, scale = TRUE)
summary(pca_result)
#screeplot(pca_result,type="l")
print(pca_result$rotation)
eigenvectors<-pca_result$loadings
eigenvalues<-pca_result$sdev*pca_result$sdev
eigenvalues

pca_result <- principal(Data1, nfactors = 2, rotate = "varimax")
pca_result

fit.varimax<-principal(Data1,nfactors=2,rotate="varimax")
fit.varimax
loadings <- fit.varimax$loadings
loadings1<-loadings(fit.varimax)
loadings1
component_scores <- as.matrix(Data1) %*% loadings
normalized_scores <- scale(component_scores)
composite_data <- data.frame(RC1 = normalized_scores[, 1], RC2 = normalized_scores[, 2])
View(composite_data)
standardized_data <- scale(composite_data)

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

#windows()
# Plot and save the dendrogram
png("Ddendrogram_plot.png", width = 800, height = 600)  # Open a PNG device
plot(tipo3C, main = "Dendrogram", xlab = "Observations", sub = "", ylab = "Height")  # Plot the dendrogram
rect.hclust(tipo3C, k = 4, border = "red")  # Add rectangles to the plot for 3 clusters





kc<-kmeans(standardized_data,4)
kc
kc$centers
#write.csv(kc$centers,file="clusterpca4.csv")



dataset<-cbind(F4,kc$cluster) 
View(dataset)
dataset$cluster<-kc$cluster
View(dataset)

View(dataset)

cluster_summary <- dataset %>%group_by(cluster) %>%summarise(Mean_Variable1 = mean(age),SD_Variable1 = sd(age),Mean_Variable2 = mean(Duration),SD_Variable2 = sd(Duration),Mean_Variable3= mean(Occupation),SD_Variable3 = sd(Occupation),Mean_Variable4 = mean(Education),SD_Variable4 = sd(Education),Count = n())
cluster_summary
print(cluster_summary)
#write.csv(cluster_summary,file="clustersummary.csv")


F6<-cbind(dataset,F5)
View(F6)



View(F6)
######################################################################################################################
# CALCULATING THE SLOPE OF EACH PARTICIPANT 

bovill_data <- read.csv("./data/point_data/tmmx.csv")
bovill_data <- bovill_data %>%
  filter(coord == "Deary")
summary(bovill_data)

F6_Bovill<-F6 %>%
  filter(Location == "Deary")
summary(F6_Bovill)

summary(F6_Bovill)

View(F6_Bovill)


F6_Bovill <- F6_Bovill %>%
  rowwise() %>%
  mutate(
    Slope = {
      duration_years <- Duration
      start_year <- max(1979, 2025 - duration_years)
      end_year <- 2025
      
      # Use start and end year to define the range, but calculate from bovill_data
      if (sum(bovill_data$year >= start_year & bovill_data$year <= end_year) >= 2) {
        yearly_avg <- dplyr::summarize(
          dplyr::group_by(bovill_data, year),
          mean_temp = mean(tmmx, na.rm = TRUE)
        ) %>%
          filter(year >= start_year, year <= end_year)
        
        if (nrow(yearly_avg) >= 2) {
          model <- lm(mean_temp ~ year, data = yearly_avg)
          coef(model)[["year"]]
        } else {
          NA_real_
        }
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup()

View(F6_Bovill)


#########################################################################################################
#Plotting the graphs 


t1<-ggplot(F6_Bovill, aes(x = que_6, y = Slope)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Relationship between que_6 and Temperature Trend (Slope)",
    x = "que_6",
    y = "Slope of Temperature Trend (°C/year)"
  ) +
  theme_classic()
#windows()
t1





t2<-ggplot(F6_Bovill, aes(x = que_6, y = cluster)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Relationship between que_6 and cluster",
    x = "que_6",
    y = "cluster"
  ) +
  theme_classic()
#windows()
t2

t3<-ggplot(F6_Bovill, aes(x = factor(cluster), y = que_6)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Cluster", y = "Response to que_6", title = "Distribution of que_6 by Cluster") +
  theme_classic()

#windows()
t3


t4<-ggplot(F6_Bovill, aes(x = factor(cluster), y = que_6)) +
  geom_violin(fill = "lightgreen") +
  labs(x = "Cluster", y = "Response to que_6", title = "Violin Plot of que_6 by Cluster") +
  theme_minimal()

#windows()
t4


t5<-ggplot(F6_Bovill, aes(x = factor(que_6), fill = factor(cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Response to que_6", y = "Count", fill = "Cluster", title = "Responses to que_6 by Cluster") +
  theme_light()

#windows()
t5



#------


# Suppose you have:
# survey_df$perception   # 1–7 Likert
# survey_df$sens_slope   # Sen’s slope for that location

# Min-max scale both to [0,1]
F6_Bovill$que_6_scaled <- scales::rescale(F6_Bovill$que_6, to = c(0,1))
F6_Bovill$Slope_scaled       <- scales::rescale(F6_Bovill$Slope, to = c(0,1))
F6_Bovill$delta <- F6_Bovill$que_6_scaled - F6_Bovill$Slope_scaled

# Alternatively, standardize to mean 0, sd 1
F6_Bovill$que_6_scaled_alt <- scale(F6_Bovill$que_6)
F6_Bovill$Slope_scaled_alt       <- scale(F6_Bovill$Slope)
F6_Bovill$delta_alt <- F6_Bovill$que_6_scaled_alt - F6_Bovill$Slope_scaled_alt



