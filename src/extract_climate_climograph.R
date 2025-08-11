



extract_climate_climograph <- function(var_type) {

  library(dplyr)
  library(lubridate)
  library(ggplot2)
  
  source("./src/extract_climate_point_data.R")
  
  extracted_data <- extract_climate_point_data(var_type)
  extracted_data$coord<-as.factor(extracted_data$coord)
  
  
  if(var_type=="tmmx"){
    plottitle = "Climograph of Average Monthly Maximum Temperature"
    yaxis = "Average Minimum Temperature (°C)"
    
    climate_summary <- extracted_data %>%
      group_by(coord, month) %>%
      summarise(climate_variable = mean(extracted_data[,2], na.rm = TRUE), .groups = "drop")
    
    # Create the climograph with all coordinates in one graph
    plot <- ggplot(climate_summary, aes(x = month, y = climate_variable, color = coord, group = coord)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12, labels = month) +
      labs(title = plottitle,
           x = "Month",
           y = yaxis,
           color = "Coordinate") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    
    } else{}
 
  if(var_type=="tmmn"){
    plottitle = "Climograph of Average Monthly Minimum Temperature"
    yaxis = "Average Minimum Temperature (°C)"
    
    climate_summary <- extracted_data %>%
      group_by(coord, month) %>%
      summarise(climate_variable = mean(extracted_data[,2], na.rm = TRUE), .groups = "drop")
    
    # Create the climograph with all coordinates in one graph
    plot <- ggplot(climate_summary, aes(x = month, y = climate_variable, color = coord, group = coord)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12, labels = month) +
      labs(title = plottitle,
           x = "Month",
           y = yaxis,
           color = "Coordinate") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else{}
  
  if(var_type=="pr"){
    plottitle = "Climograph of Total Monthly Preciptation"
    yaxis = "Total Precipitation Amount (mm)"
    
    
    climate_summary <- extracted_data %>%
      mutate(
        year = year(date)
      ) %>%
      group_by(coord, year, month) %>%
      summarize(monthly_total = sum(precipitation_amount, na.rm = TRUE), .groups = "drop") %>%
      group_by(month, coord) %>%
      summarize(mean_monthly_precip = mean(monthly_total, na.rm = TRUE), .groups = "drop") %>%
      arrange(match(month, month.abb))  # order months Jan-Dec
    
    
    # Create the climograph with all coordinates in one graph
    plot <- ggplot(climate_summary, aes(x = month, y = mean_monthly_precip, color = coord, group = coord)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12, labels = month) +
      labs(title = plottitle,
           x = "Month",
           y = yaxis,
           color = "Coordinate") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else{}
  
  if(var_type=="vs"){
    plottitle = "Climograph of Average Wind Speed"
    yaxis = "Average Wind Speed (m/s)"
    
    climate_summary <- extracted_data %>%
      group_by(coord, month) %>%
      summarise(climate_variable = mean(extracted_data[,2], na.rm = TRUE), .groups = "drop")
    
    # Create the climograph with all coordinates in one graph
    plot <- ggplot(climate_summary, aes(x = month, y = climate_variable, color = coord, group = coord)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12, labels = month) +
      labs(title = plottitle,
           x = "Month",
           y = yaxis,
           color = "Coordinate") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  
  } else{}
  
  return(plot)
  
}
