library(tidycensus)
library(tidyverse)

# Load your Census API Key
census_api_key("e0f23a6547146c1afe5ef60848c557b67dfd3acb", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")




census_extract <- function(year, type, state) {
# Check available variables for a given year and dataset
vars <- load_variables(year, type, cache = TRUE)

# View the first few variables (optional)
# View(vars)

# Get all variable names
all_vars <- vars$name

# Download data for all counties in Idaho for all variables
idaho_data <- get_acs(
  geography = "county",
  variables = all_vars,
  state = state,
  year = year,
  survey = type,
  output = "long"  # output in wide format so each variable is a column
)
}


#multistate------------------

census_extract_multistate <- function(year, type) {

# Define the states you want to loop through (use abbreviations or full names)
states_to_download <- c("ID", "MT", "WY", "UT")  # Example: Idaho, Montana, Wyoming, Utah

# Load all variable names for ACS 5-year data
acs_vars <- load_variables(year, type, cache = TRUE)
all_vars <- acs_vars$name

# Initialize an empty list to store data frames
all_states_data <- list()

# Loop through each state and download ACS data by county
for (state_abbr in states_to_download) {
  message("Downloading data for: ", state_abbr)
  
  # Try-catch in case of download errors
  try({
    state_data <- get_acs(
      geography = "county",
      variables = all_vars,
      state = state_abbr,
      year = 2022,
      survey = "acs5",
      output = "wide"
    ) %>%
      mutate(state = state_abbr)  # Add a column for the state abbreviation
    
    all_states_data[[state_abbr]] <- state_data
  }, silent = TRUE)
}

# Combine all states into one data frame
combined_data <- bind_rows(all_states_data)

# Optional: write to file
# write_csv(combined_data, "acs_5yr_county_data_selected_states.csv")
}



census_extract_allstate <- function(year, type) {
  

# Define all U.S. state abbreviations (excluding DC and territories for now)
all_states <- state.abb

# Load all variable names for ACS 5-year data
acs_vars <- load_variables(year, type, cache = TRUE)
all_vars <- acs_vars$name

# Initialize list to store results
all_states_data <- list()

# Loop through states and download county-level data
for (state_abbr in all_states) {
  message("Downloading data for: ", state_abbr)
  
  try({
    state_data <- get_acs(
      geography = "county",
      variables = all_vars,
      state = state_abbr,
      year = 2022,
      survey = "acs5",
      output = "wide"
    ) %>%
      mutate(state = state_abbr)
    
    all_states_data[[state_abbr]] <- state_data
  }, silent = TRUE)
}

# Combine into a single data frame
combined_data <- bind_rows(all_states_data)

# Optional: Save to CSV
# write_csv(combined_data, "acs_5yr_2022_county_data_all_states.csv")

}

#rename columns to be more descriptive-----------



census_extract_rename <- function(year, type) {
  
library(tidycensus)
library(tidyverse)
library(janitor)

# Load ACS metadata for 2022
acs_vars <- load_variables(year, type, cache = TRUE)

# Clean up variable labels for use in column names
acs_vars_clean <- acs_vars %>%
  mutate(
    label_clean = label %>%
      str_replace_all("!!", "_") %>%   # Replace hierarchy separators
      str_replace_all("[^A-Za-z0-9_]", "_") %>%  # Remove special chars
      str_replace_all("_+", "_") %>%   # Remove double underscores
      str_trim() %>%
      tolower()
  )

# Assume your wide-format dataframe is called `combined_data`
# Get the columns that need renaming (those with E or M suffixes)
wide_cols <- names(combined_data)[str_detect(names(combined_data), "_[EM]$")]

# Build renaming map
rename_map <- tibble(old = wide_cols) %>%
  mutate(
    varcode = str_remove(old, "[EM]$"),
    suffix = str_extract(old, "[EM]$"),
    new = paste0(
      acs_vars_clean$label_clean[match(varcode, acs_vars_clean$name)],
      ifelse(suffix == "E", "_estimate", "_moe")
    )
  ) %>%
  drop_na(new)

# Apply the renaming
combined_data_renamed <- combined_data %>%
  rename_with(.fn = ~ set_names(rename_map$new, rename_map$old)[.], .cols = all_of(rename_map$old))

}



census_extract(2022, "acs5")


