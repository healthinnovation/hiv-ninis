library(fs)
library(readr)
library(dplyr)
library(tidyr)

# Read file with variable values ------------------------------------------

path_variables_dir <- path("data", "variables")
variable_values_file <- "variable-values.csv"
path_variable_values_file <- path(path_variables_dir, variable_values_file)
variable_values_all <- read_csv(path_variable_values_file)

# Read data set without values --------------------------------------------

path_interim_data_dir <- path("data", "interim")
dataset_wo_values_file <- "dataset-without-values.csv"
path_dataset_wo_values_file <- path(path_interim_data_dir, dataset_wo_values_file)
dataset_wo_values <- read_csv(
  path_dataset_wo_values_file, col_types = cols(.default = "c")
)

# Filter variable names ---------------------------------------------------

variable_values_all %>% 
  filter(variable_name %in% names(dataset_wo_values)) -> variable_values

# Set variable values -----------------------------------------------------

dataset <- dataset_wo_values

for (row in 1:nrow(variable_values)) {
  variable_name <- variable_values$variable_name[row]
  value <- as.character(variable_values$value[row])
  value_description <- variable_values$value_description[row]
  
  dataset[variable_name][dataset[variable_name] == value] <- value_description
}

# Check number of rows and number of unique CASEID ------------------------

dataset %>% 
  group_by(year) %>% 
  summarise(n = n(), n_caseid = length(unique(caseid)))

# Export wrangled data set ------------------------------------------------

data_out_path <- path("data", "processed")
data_out_file_name <- "dataset.csv"
data_out_file_path <- path(data_out_path, data_out_file_name)
write_csv(dataset, data_out_file_path, na = "")