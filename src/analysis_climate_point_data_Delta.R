analysis_climate_point_data_Delta <- function(location, clim) {
  
library(ggplot2)

survey_data <- read.csv(paste("./data/delta/", "survey_delta_", clim, "_", location, ".csv", sep=""))

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


ggplot(survey_data, aes(x = que_24, y = que_24_delta)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
  scale_x_continuous(breaks = sort(unique(survey_data$que_24))) +
  labs(x = "que_24", y = "delta") +
  theme_minimal()

analysis_climate_point_data_Delta <- function()

#duration vs delta
ggplot(survey_data, aes(x = Duration, y = que_24_delta)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
  scale_x_continuous(breaks = sort(unique(survey_data$Duration))) +
  labs(x = "que_6", y = "delta") +
  theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

# pick the two columns you want on the x-axis
cols <- c("que_6", "que_24")   # change to any two, e.g., c("que_6","que_15")

survey_data %>%
  pivot_longer(all_of(cols), names_to = "question", values_to = "xval") %>%
  ggplot(aes(x = xval, y = delta, color = question)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.12, height = 0)) +
  scale_x_continuous(breaks = sort(unique(unlist(survey_data[cols])))) +
  labs(x = "Response value", y = "delta", color = "Column") +
  theme_minimal()

}