library(fs)
library(readr)
library(dplyr)
library(tidyr)

# Read file with variable names -------------------------------------------

path_variables_dir <- path("data", "variables")
variable_names_file <- "variable-names.csv"
path_variable_names_file <- path(path_variables_dir, variable_names_file)
variable_names <- read_csv(path_variable_names_file)

# Read data set without names ---------------------------------------------

path_interim_data_dir <- path("data", "interim")
dataset_wo_names_file <- "dataset-without-names.csv"
path_dataset_wo_names_file <- path(path_interim_data_dir, dataset_wo_names_file)
dataset_wo_names <- read_csv(path_dataset_wo_names_file)

# Set variable names ------------------------------------------------------

nrow_variable_names <- nrow(variable_names)
dataset <- dataset_wo_names

for (row in 1:nrow_variable_names) {
  variable_name <- variable_names$variable[row]
  new_variable_name <- variable_names$variable_name[row]
  colnames(dataset)[colnames(dataset) == variable_name] <- new_variable_name
}

colnames(dataset) <- tolower(colnames(dataset))

# Get values for all variables --------------------------------------------

variables_to_filter_for_pivot <- c(
  "caseid", "wealth_index_scoring_factor", "weighting_factor", "year", 
  "total_children_ever_born", "sons_who_have_died", "daughters_who_have_died",
  "births_last_five_years", "births_past_year", "births_month_interview", 
  "date_first_birth", "age_respondent_first_birth", "duration_current_pregnancy",
  "number_living_children", "living_children_current_pregnancy", 
  "marriage_fisrt_birth_interval", "last_birth_interview_interval",
  "length_time_knows_last_partner"
)

values_per_variable_no_filter <- 
  dataset %>%
  # select() %>%
  pivot_longer(
    cols = !any_of(variables_to_filter_for_pivot),
    names_to = "variable_name",
    values_to = "value"
  ) %>%
  group_by(variable_name) %>%
  summarise(values = sort(unique(value)), .groups = "drop") %>%
  left_join(variable_names, by = "variable_name") %>%
  select(survey, variable, variable_name, values) %>%
  arrange(survey, variable, values) 

values_per_variable <- 
  values_per_variable_no_filter %>% 
  filter

# Read variable values ----------------------------------------------------

variable_values_file <- "variable-values.csv"
path_variable_values_file <- path(path_variables_dir, variable_values_file)
variable_values <- read_csv(path_variable_values_file)

# Set variable values -----------------------------------------------------

variable_values_path <- path(variables_path, "variable-values.csv")
variable_values <- read_csv(variable_values_path)

for (i in 1:nrow(variable_values)) {
  variable_name <- variable_values$variable_name[i]
  value <- as.character(variable_values$value[i])
  value_description <- variable_values$value_description[i]
  
  dataset[variable_name][dataset[variable_name] == value] <- value_description
}

# is_numeric function------------------------------------------------------

# is_numeric <- function(x) {
#   if (sum(grepl("^[0a-zA-Z].*", x)) > 0) {
#     FALSE
#   } else {
#     TRUE
#   }
# }


# Filter and parsing (numeric) variables ----------------------------------

# Export wrangled dataset

write.csv(dataset, "data/processed/hiv-ninis-dataset.csv", row.names = FALSE)