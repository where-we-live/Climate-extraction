
# Load required libraries
library(ncdf4)
library(dplyr)
library(lubridate)

# Function to extract daily climate data for multiple coordinates
extract_climate_data_multiple <- function(opendap_url, coordinates, var_type) {
  
  # Initialize an empty data frame to collect all data
  all_climate_data <- data.frame()
  
  # Open the NetCDF file once
  nc_data <- nc_open(opendap_url)
  
  # Identify variable name (assumes the first is the target variable)
  var_name <- names(nc_data$var)[1]
  
  # Extract coordinate and time information
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat")
  time <- ncvar_get(nc_data, "day")  # Update if your time var is named differently
  
  # Convert time to actual dates
  time_units <- ncatt_get(nc_data, "day", "units")$value
  origin_date <- as.Date(strsplit(time_units, " since ")[[1]][2])
  dates <- origin_date + time
  
  # Loop over each coordinate
  for (i in 1:nrow(coordinates)) {
    target_lon <- coordinates$lon[i]
    target_lat <- coordinates$lat[i]
    coord_label <- coordinates$label[i]
    
    # Find the nearest grid point
    lon_ind <- which.min(abs(lon - target_lon))
    lat_ind <- which.min(abs(lat - target_lat))
    
    # Define extraction parameters
    start <- c(lon_ind, lat_ind, 1)
    count <- c(1, 1, -1)  # -1 to get all time steps
    
    # Extract the variable data
    var_array <- ncvar_get(nc_data, var_name, start = start, count = count)
    
    # Create data frame for the current coordinate
    climate_df <- data.frame(
      date = dates,
      value = as.vector(var_array),
      coord = coord_label
    )
    
    # Convert units if needed
    if (var_type == "temperature") {
      climate_df$value <- climate_df$value - 273.15  # Kelvin to Celsius
    }
    
    # Add year, month, and day
    climate_df <- climate_df %>%
      mutate(year = year(date), month = month(date), day = day(date))
    
    # Append to the full dataset
    all_climate_data <- bind_rows(all_climate_data, climate_df)
  }
  
  # Close NetCDF connection
  nc_close(nc_data)
  
  return(all_climate_data)
}


# Define coordinates
coordinates <- data.frame(
  lon = c(-116.394, -116.647, -116.556, -116.77),
  lat = c(46.859, 46.614, 46.799, 46.737),
  label = c("Bovill", "Kendrick", "Deary", "Troy")
)

# NetCDF OPeNDAP URL (min temperature example)
opendap_url_temp <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_tmmn_1979_CurrentYear_CONUS.nc#fillmismatch"

# Extract climate data
daily_temp_data <- extract_climate_data_multiple(opendap_url_temp, coordinates, "temperature")

# View the first few rows
head(daily_temp_data)


bovill_data <- daily_temp_data %>%
  filter(coord == "Bovill")
summary(bovill_data)

colnames(bovill_data)
str(bovill_data)

yearly_avg <- dplyr::summarize(
  dplyr::group_by(bovill_data, year),
  mean_temp = mean(value, na.rm = TRUE)
)
head(yearly_avg)

# Step 1: Fit the model to get the slope
model <- lm(mean_temp ~ year, data = yearly_avg)
slope_value <- coef(model)["year"]
slope_label <- paste0("Slope = ", round(slope_value, 3), " °C/year")


library(ggplot2)
# Step 2: Create the plot with the slope label
p<-ggplot(yearly_avg, aes(x = year, y = mean_temp)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", 
           x = min(yearly_avg$year) + 2, 
           y = max(yearly_avg$mean_temp) - 0.5,
           label = slope_label,
           hjust = 0,
           size = 5,
           color = "red") +
  labs(
    title = "Trend in Annual Average Temperature in Bovill",
    x = "Year",
    y = "Mean Annual Temperature (°C)"
  ) +
  theme_classic()
windows()
p


##############################################################################################

home=setwd("C:\\Users\\frusere\\OneDrive - University of Idaho\\Desktop\\UOI\\DATA")

SS1<-read.csv("SS1.csv")


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
library(psych)
View(SS1)

F4<-SS1%>%dplyr::select(Education,Occupation,Duration,age)
F5<-SS1%>%dplyr::select(que_6,que_15,que_24,Location,Point)

View(F4)

summary(F4)

# Run KMO and Bartlett's Test
kmo_result <- KMO(F4)
bartlett_result <- cortest.bartlett(cor(F4), n = nrow(F4))

# Print results
print(kmo_result)
print(bartlett_result)

attach(F4)
F1c<-F4

Data1<-F1c
set.seed(123)
Data1<-as.data.frame.matrix(Data1)
View(Data1)

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

windows()
plot(tipo3C)

windows()
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



View(F6)
######################################################################################################################
# CALCULATING THE SLOPE OF EACH PARTICIPANT 

F6_Bovill<-F6 %>%
  filter(Location == "Bovill")
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
          mean_temp = mean(value, na.rm = TRUE)
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
windows()
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
windows()
t2

t3<-ggplot(F6_Bovill, aes(x = factor(cluster), y = que_6)) +
  geom_boxplot(fill = "skyblue") +
  labs(x = "Cluster", y = "Response to que_6", title = "Distribution of que_6 by Cluster") +
  theme_classic()

windows()
t3


t4<-ggplot(F6_Bovill, aes(x = factor(cluster), y = que_6)) +
  geom_violin(fill = "lightgreen") +
  labs(x = "Cluster", y = "Response to que_6", title = "Violin Plot of que_6 by Cluster") +
  theme_minimal()

windows()
t4


t5<-ggplot(F6_Bovill, aes(x = factor(que_6), fill = factor(cluster))) +
  geom_bar(position = "dodge") +
  labs(x = "Response to que_6", y = "Count", fill = "Cluster", title = "Responses to que_6 by Cluster") +
  theme_light()

windows()
t5