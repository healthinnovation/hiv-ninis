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
dataset_wo_names <- read_csv(
  path_dataset_wo_names_file, col_types = cols(.default = "c")
)

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
  "no_living_children", "living_children_current_pregnancy", 
  "marriage_fisrt_birth_interval", "last_birth_interview_interval", "age", 
  "no_household_members", "no_children_6_yo", 
  "no_eligible_women_15_to_49_yo", "age_household_head", "age_household_member",
  "mother_line_no", "father_line_no", "single_years_edu_partner",
  "no_sex_partners_other_partner_last_12_months", 
  "no_sex_partners_including_partner_last_12_months",
  "years_lived_place_residence", "age_first_sexual_partner",
  "total_lifetime_sex_partners_last_12_months", "highest_year_edu_woman",
  "time_get_primary_source_drinking_water", "highest_year_edu", 
  "edu_single_years", "grade_edu_current_year", 
  "edu_single_years_current_year", "grade_edu_previous_year",
  "edu_single_years_previous_year", "length_time_knows_last_partner"
)

dataset %>%
  select(-all_of(variables_to_filter_for_pivot)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "variable_name",
    values_to = "value"
  ) %>%
  group_by(variable_name) %>%
  summarise(value = sort(unique(value)), .groups = "drop") %>%
  left_join(variable_names, by = "variable_name") %>%
  select(survey, variable, variable_name, value) %>%
  arrange(survey, variable, value) ->
  values_per_variable

# Read variable values ----------------------------------------------------

variable_values_file <- "variable-values.csv"
path_variable_values_file <- path(path_variables_dir, variable_values_file)
variable_values <- read_csv(path_variable_values_file)

# Filter variables without value description ------------------------------

values_per_variable %>% 
  anti_join(
    variable_values, by = c("survey", "variable", "variable_name", "value")
  ) ->
  variable_values_to_fill

variable_values_to_fill$value_description <- rep(NA, nrow(variable_values_to_fill))

# Export variable values to fill ------------------------------------------

variables_out_path <- path("data", "variables", "raw")
output_file_name <- "variable-values-to-fill.csv"
output_file_path <- path(variables_out_path, output_file_name)
write_csv(variable_values_to_fill, output_file_path, na = "")

# Export data set ---------------------------------------------------------

data_out_path <- path("data", "interim")
data_out_file_name <- "dataset-without-values.csv"
data_out_file_path <- path(data_out_path, data_out_file_name)
write_csv(dataset, data_out_file_path, na = "")
