
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

#load survey data (simulated)

SS1 <- read.csv("./data/survey/SS1.csv")
F4<-SS1%>%dplyr::select(Education,Occupation,Duration,age)
F5<-SS1%>%dplyr::select(que_6,que_15,que_24,Location,Point)
Data1<-as.data.frame.matrix(F4)
F6<-cbind(Data1,F5)

######################################################################################################################
# CALCULATING THE SLOPE OF EACH PARTICIPANT 

climate_data <- read.csv("./data/point_data/tmmx.csv")
climate_data <- climate_data %>%
  filter(coord == "Deary")
summary(climate_data)

survey_data<-F6 %>%
  filter(Location == "Deary")
summary(survey_data)

survey_data <- survey_data %>%
  rowwise() %>%
  mutate(
    Slope = {
      duration_years <- Duration
      start_year <- max(1979, 2025 - duration_years)
      end_year <- 2025
      
      # Use start and end year to define the range, but calculate from climate_data
      if (sum(climate_data$year >= start_year & climate_data$year <= end_year) >= 2) {
        yearly_avg <- dplyr::summarize(
          dplyr::group_by(climate_data, year),
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

#------


# Suppose you have:
# survey_df$perception   # 1–7 Likert
# survey_df$sens_slope   # Sen’s slope for that location

# Min-max scale both to [0,1]
survey_data$que_6_scaled <- scales::rescale(survey_data$que_6, to = c(0,1))
survey_data$Slope_scaled       <- scales::rescale(survey_data$Slope, to = c(0,1))
survey_data$delta <- survey_data$que_6_scaled - survey_data$Slope_scaled

# Alternatively, standardize to mean 0, sd 1
survey_data$que_6_scaled_alt <- scale(survey_data$que_6)
survey_data$Slope_scaled_alt       <- scale(survey_data$Slope)
survey_data$delta_alt <- survey_data$que_6_scaled_alt - survey_data$Slope_scaled_alt


#########################################################################################################
#Plotting the graphs 


t1<-ggplot(survey_data, aes(x = que_6, y = Slope)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = "Relationship between que_6 and Temperature Trend (Slope)",
    x = "que_6",
    y = "Slope of Temperature Trend (°C/year)"
  ) +
  theme_classic()
t1


ggplot(survey_data, aes(x = que_6, y = delta)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
  scale_x_continuous(breaks = sort(unique(survey_data$que_6))) +
  labs(x = "que_6", y = "delta") +
  theme_minimal()

#duration vs delta
ggplot(survey_data, aes(x = Duration, y = delta)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
  scale_x_continuous(breaks = sort(unique(survey_data$Duration))) +
  labs(x = "que_6", y = "delta") +
  theme_minimal()

