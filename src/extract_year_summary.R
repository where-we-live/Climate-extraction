extract_year_summary <- function(location, var_type) {

  source("./src/extract_climate_point_data.R")

  extracted_data <- extract_climate_point_data(var_type)
  extracted_data$coord<-as.factor(extracted_data$coord)
  
  location_data <- extracted_data %>%
    filter(coord == location)
  
yearly_avg <- dplyr::summarize(
  dplyr::group_by(location_data, year),
  climate_var = mean(.data[[ names(location_data)[2] ]], na.rm = TRUE)
)

# Step 1: Fit the model to get the slope

library(trend)
sen_result <- sens.slope(yearly_avg$climate_var, yearly_avg$year)
slope_value <- sen_result$estimates  # This is the Sen's slope per year
slope_label <- paste0("Slope = ", round(slope_value, 3), " change/year")


}

library(ggplot2)
# Step 2: Create the plot with the slope label
p<-ggplot(yearly_avg, aes(x = year, y = climate_var)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", 
           x = min(yearly_avg$year) + 2, 
           y = max(yearly_avg$climate_var) - 0.5,
           label = slope_label,
           hjust = 0,
           size = 5,
           color = "red") +
  labs(
    title = "Trend in Annual Average Climate Variable in Bovill",
    x = "Year",
    y = "Mean Value"
  ) +
  theme_classic()
p
