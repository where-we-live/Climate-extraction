model_climate_point_data_Delta <- function(location, clim) {
  
library(dplyr)    
library(tidyr)    
library(ggplot2)  
library(scales)   
  
set.seed(123)

#load survey data (simulated)

SS1 <- read.csv("./data/survey/SS1.csv")
SS1$Location <- as.factor(SS1$Location)
levels(SS1$Location)[levels(SS1$Location) == "Elk River"] <- "Kendrick"
F4<-SS1%>%dplyr::select(Education,Occupation,Duration,age)
F5<-SS1%>%dplyr::select(que_6,que_15,que_24,Location,Point)
Data1<-as.data.frame.matrix(F4)
F6<-cbind(Data1,F5)

#plot slopes

df <- read.csv(paste("./data/point_data/", clim, ".csv", sep=""))

library(dplyr)
library(lubridate)
library(ggplot2)

# assume your data frame is called df with columns: date, tmmx, coord (optional)
# if year/month already exist, they'll be used; otherwise they're derived from `date`

#DJF

djf_by_season <- df %>%
  mutate(
    date = as.Date(date),
    mon  = if ("month" %in% names(.)) month else month(date),
    yr   = if ("year"  %in% names(.)) year  else year(date),
    season_year = ifelse(mon == 12, yr + 1L, yr)  # label by Jan year
  ) %>%
  filter(mon %in% c(12, 1, 2)) %>%
  group_by(season_year, coord) %>%                  # one bar per city × season
  summarise(djf_mean = mean(tmmx, na.rm = TRUE), .groups = "drop") %>%
  arrange(season_year, coord)


# If you have multiple locations in `coord`, either facet:
p_all <- ggplot(djf_by_season, aes(factor(season_year), djf_mean)) +
  geom_col() +
  facet_wrap(~ coord, scales = "free_y") +
  labs(x = "Year", y = "Mean tmmx (Jun–Aug)", title = "JJA average tmmx by year") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ...or color by coord in a single panel:
city_plots <- ggplot(djf_by_season, aes(factor(season_year), djf_mean, fill = coord)) +
  geom_col(position = "dodge") +
  labs(x = "Year", y = "Mean tmmx (Jun–Aug)", title = "JJA average tmmx by year", fill = "Location") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(grobs = c(list(p_all), city_plots), ncol = 2)

#JJA

jja_by_year <- df %>%
  mutate(
    date  = as.Date(date),
    yr    = if ("year"  %in% names(.)) year else year(date),
    mon   = if ("month" %in% names(.)) month else month(date)
  ) %>%
  filter(mon %in% 6:8) %>%
  group_by(yr, coord) %>%                 # drop `coord` here if you don't have it
  summarise(jja_mean = mean(tmmx, na.rm = TRUE), .groups = "drop")


# If you have multiple locations in `coord`, either facet:
p_all <- ggplot(jja_by_year, aes(factor(yr), jja_mean)) +
  geom_col() +
  facet_wrap(~ coord, scales = "free_y") +
  labs(x = "Year", y = "Mean tmmx (Jun–Aug)", title = "JJA average tmmx by year") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ...or color by coord in a single panel:
city_plots <- ggplot(jja_by_year, aes(factor(yr), jja_mean, fill = coord)) +
  geom_col(position = "dodge") +
  labs(x = "Year", y = "Mean tmmx (Jun–Aug)", title = "JJA average tmmx by year", fill = "Location") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(grobs = c(list(p_all), city_plots), ncol = 2)

# CALCULATING THE SLOPE OF EACH PARTICIPANT 

climate_data <- read.csv(paste("./data/point_data/", clim, ".csv", sep=""))
climate_data <- climate_data %>%
  filter(coord == location)
summary(climate_data)

survey_data<-F6 %>%
  filter(Location == location)
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

survey_data$Slope_scaled  <- scales::rescale(survey_data$Slope, to = c(0,1))
survey_data$Slope_scaled_alt  <- scale(survey_data$Slope)

#calculate slope and delta for all questions

for (i in colnames(survey_data[, 5:7])) {
  message(i)
  # scale the actual column vector, not a string
  survey_data[[paste0(i, "_scaled")]] <- scales::rescale(survey_data[[i]], to = c(0, 1))
  survey_data[[paste0(i, "_delta")]] <- survey_data[[paste0(i, "_scaled")]] - survey_data$Slope_scaled
  
  survey_data[[paste0(i, "_scaled_alt")]] <- as.numeric(scale(survey_data[[i]]))
  survey_data[[paste0(i, "_delta_alt")]]  <- survey_data[[paste0(i, "_scaled_alt")]] - survey_data$Slope_scaled_alt

write.csv(survey_data, file= paste("./data/delta/", "survey_delta_", clim, "_", location, ".csv", sep=""), row.names=FALSE)

}

#Plotting the graphs 

for (i in colnames(survey_data[, 5:7])) {

t1<-ggplot(survey_data, aes(x=as.numeric(.data[[i]]), y = Slope)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(
    title = paste("Relationship between ", i, " and ", clim, " Trend (Slope)", sep=""),
    x = i,
    y = paste("Slope of ", clim, " Trend", sep="")
  ) +
  theme_classic()
ggplot2::ggsave(paste("./plots/", i, "_", location, "_slopetrend.png", sep=""), t1, width = 7, height = 5, dpi = 300, device = ragg::agg_png)
}


for (i in names(survey_data)[5:7]) {
  ycol <- paste0(i, "_delta")
  if (!ycol %in% names(survey_data)) next  # skip if the delta column isn't there
  
  t1 <- ggplot(survey_data, aes(x = .data[[i]], y = .data[[ycol]])) +
    geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
    # if x is numeric (e.g., 1–6), give nice breaks
    { if (is.numeric(survey_data[[i]])) 
      scale_x_continuous(breaks = sort(unique(survey_data[[i]]))) 
      else 
        scale_x_discrete() } +
    labs(
      title = paste("Relationship between", i, "and delta"),
      x = i,
      y = "delta"
    ) +
    theme_minimal()

  ggplot2::ggsave(
    filename = paste0("./plots/", i, "_", location, "_deltatrend.png"),
    plot = t1,
    width = 7, height = 5, dpi = 300,
    device = ragg::agg_png
  )
}

for (i in names(survey_data)[5:7]) {
  ycol <- paste0(i, "_delta")
  if (!ycol %in% names(survey_data)) next  # skip if delta not present
  
  t1 <- ggplot(survey_data, aes(x = Duration, y = .data[[ycol]])) +
    geom_point(alpha = 0.7, position = position_jitter(width = 0.1, height = 0)) +
    {
      if (is.numeric(survey_data$Duration)) {
        scale_x_continuous(breaks = sort(unique(survey_data$Duration)))
      } else {
        scale_x_discrete()
      }
    } +
    labs(
      title = paste("Duration vs", ycol),
      x = "Duration",
      y = "delta"
    ) +
    theme_minimal()
  
  ggplot2::ggsave(
    filename = paste0("./plots/Duration_vs_", i, "_", location, "_delta.png"),
    plot = t1,
    width = 7, height = 5, dpi = 300,
    device = ragg::agg_png
  )
}

}

