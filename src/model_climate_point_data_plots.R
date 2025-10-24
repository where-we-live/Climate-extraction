model_climate_point_data_plots <- function(location, clim) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(lubridate)
  library(gridExtra)  # for grid.arrange
  library(rlang)      # for as_label
  
  set.seed(123)
  
  # load survey data (left as-is)
  SS1 <- read.csv("./data/survey/SS1.csv")
  SS1$Location <- as.factor(SS1$Location)
  levels(SS1$Location)[levels(SS1$Location) == "Elk River"] <- "Kendrick"
  F4 <- SS1 %>% dplyr::select(Education, Occupation, Duration, age)
  F5 <- SS1 %>% dplyr::select(que_6, que_15, que_24, Location, Point)
  Data1 <- as.data.frame.matrix(F4)
  F6 <- cbind(Data1, F5)
  
  # load point data; `clim` is the file stem (e.g., "tmmx" or "pr")
  df <- read.csv(paste0("./data/point_data/", clim, ".csv"))
  
  # helpful label for axis/title from the passed column
  value_lab <- as_label(enquo(clim))
  
  # ---------- DJF ----------
  djf_by_season <- df %>%
    mutate(
      date = as.Date(date),
      mon  = if ("month" %in% names(.)) .data$month else lubridate::month(date),
      yr   = if ("year"  %in% names(.)) .data$year  else lubridate::year(date),
      season_year = ifelse(mon == 12, yr + 1L, yr)  # label by Jan year
    ) %>%
    filter(mon %in% c(12, 1, 2)) %>%
    group_by(season_year, coord) %>%
    summarise(djf_mean = mean(.data[[clim]], na.rm = TRUE), .groups = "drop") %>%
    arrange(season_year, coord)
  
  p_all_djf <- ggplot(djf_by_season, aes(factor(season_year), djf_mean)) +
    geom_col() +
    facet_wrap(~ coord, scales = "free_y") +
    labs(x = "Year",
         y = paste0("Mean ", value_lab, " (DJF)"),
         title = paste0("DJF average ", value_lab, " by year")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_city_djf <- ggplot(djf_by_season, aes(factor(season_year), djf_mean, fill = coord)) +
    geom_col(position = "dodge") +
    labs(x = "Year",
         y = paste0("Mean ", value_lab, " (DJF)"),
         title = paste0("DJF average ", value_lab, " by year"),
         fill = "Location") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # ---------- JJA ----------
  jja_by_year <- df %>%
    mutate(
      date = as.Date(date),
      mon  = if ("month" %in% names(.)) .data$month else lubridate::month(date),
      yr   = if ("year"  %in% names(.)) .data$year  else lubridate::year(date)
    ) %>%
    filter(mon %in% 6:8) %>%
    group_by(yr, coord) %>%
    summarise(jja_mean = mean(.data[[clim]], na.rm = TRUE), .groups = "drop") %>%
    arrange(yr, coord)
  
  p_all_jja <- ggplot(jja_by_year, aes(factor(yr), jja_mean)) +
    geom_col() +
    facet_wrap(~ coord, scales = "free_y") +
    labs(x = "Year",
         y = paste0("Mean ", value_lab, " (JJA)"),
         title = paste0("JJA average ", value_lab, " by year")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_city_jja <- ggplot(jja_by_year, aes(factor(yr), jja_mean, fill = coord)) +
    geom_col(position = "dodge") +
    labs(x = "Year",
         y = paste0("Mean ", value_lab, " (JJA)"),
         title = paste0("JJA average ", value_lab, " by year"),
         fill = "Location") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # show DJF (left: faceted; right: combined) and JJA (left/right)
  grid.arrange(
    grobs = list(p_all_djf, p_city_djf, p_all_jja, p_city_jja),
    ncol = 2
  )
  
  # (optional) return the data and plots for further use
  invisible(list(
    djf = list(data = djf_by_season, p_all = p_all_djf, p_city = p_city_djf),
    jja = list(data = jja_by_year,   p_all = p_all_jja,  p_city = p_city_jja)
  ))
}
