library(fs)
library(haven)
library(purrr)
library(dplyr)
library(data.table)
library(readr)
library(stringi)
library(tidyr)

#TODO: Check V833B

# Read surveys and variables ----------------------------------------------

variables_path <- path("data", "variables")
variables_file <- "variables.csv"
variables_file_path <- path(variables_path, variables_file)
variables_dtfr <- read_csv(variables_file_path)

endes_surveys <- unique(variables_dtfr$survey)

# Read raw surveys --------------------------------------------------------

raw_path <- path("data", "raw")
endes_path <- path(raw_path, "endes")
endes_files <- dir_ls(endes_path, recurse = T, glob = "*.sav")
pattern <- paste(endes_surveys, collapse = "|")
endes_files <- endes_files[grepl(pattern, endes_files)]
surveys_raw <- map(endes_files, read_sav)

# Select variables --------------------------------------------------------

variables <- split(variables_dtfr$variable, variables_dtfr$survey)

# variables_path <- path("data", "variables")
# variables_files <- dir_ls(variables_path, recurse = TRUE, glob = "*.txt")
# variables <- map(variables_files, scan, what = character(), quiet = TRUE)
# names(variables) <- path_ext_remove(path_file(variables_files))
# 
# survey_dtfrs <- list()
# for (survey_name in names(variables)) {
#   survey <- rep(survey_name, length(variables[[survey_name]]))
#   survey_dtfrs[[survey_name]] <- 
#     tibble(survey, variable = variables[[survey_name]])
# }
# survey_variables <- bind_rows(survey_dtfrs)

surveys_select <- surveys_raw

for (survey_path in names(surveys_select)) {
  message(path_ext_remove(survey_path))
  names(surveys_select[[survey_path]]) <- 
    toupper(names(surveys_select[[survey_path]]))
  
  survey_name <- path_ext_remove(path_file(survey_path))
  
  if (!(survey_name %in% names(variables))) {
    message(paste(survey_name, "is missing.\n"))
  } else {
    surveys_select[[survey_path]] <- 
      surveys_select[[survey_path]] %>% 
      select(all_of(variables[[survey_name]])) %>% 
      # as_factor() %>% 
      mutate(across(everything(), as.character)) %>% 
      mutate(across(everything(), ~ trimws(.x, which = "both")))
  }
  
  surveys_select[[survey_path]]$PATH <- survey_path
}

# Bind data frames by survey name -----------------------------------------

surveys <- list()
for (survey in endes_surveys) {
  surveys[[survey]] <- list()
  for (survey_year in names(surveys_select)) {
    if (grepl(survey, survey_year)) {
      surveys[[survey]][[survey_year]] <- surveys_select[[survey_year]]
    }
  }
}

surveys_bind <- list()
for (survey in names(surveys)) {
  message(survey)
  surveys_bind[[survey]] <- 
    bind_rows(surveys[[survey]]) %>% 
    mutate(
      YEAR = substr(PATH, 16, 19),
      MODULE = path_file(path_dir(PATH)),
      SURVEY = path_ext_remove(path_file(PATH))
    )
}

# Merge surveys -----------------------------------------------------------

surveys_bind$CSALUD01 <- 
  surveys_bind$CSALUD01 %>% 
  mutate(
    CASEID = ifelse(
      nchar(QSNUMERO) == 1,
      paste0(HHID, "  ", QSNUMERO),
      paste0(HHID, " ", QSNUMERO)
    )
  )

surveys_bind$RECH1 <- 
  surveys_bind$RECH1 %>% 
  mutate(
    CASEID = ifelse(
      nchar(HVIDX) == 1,
      paste0(HHID, "  ", HVIDX),
      paste0(HHID, " ", HVIDX)
    )
  )

full_dataset <- 
  surveys_bind %>%
  reduce(left_join, by = c("CASEID", "YEAR"))

# Filter some unnecessary columns ----------------------------------------

dataset_raw <- 
  full_dataset %>% 
  select(
    !starts_with(c("PATH", "MODULE", "SURVEY", "HHID", "HVIDX", "QSNUMERO"))
  )

# Filter variables with high rate of missing values ------------------------

vars_na_percent <- apply(
  dataset_raw, 2, function(x) sum(is.na(x)) / nrow(dataset_raw)
)

null_vars <- names(which(vars_na_percent == 1))

dataset_select <- select(dataset_raw, !any_of(null_vars))
dataset_select <- select(dataset_select, CASEID, YEAR, everything())

# Export variables --------------------------------------------------------

variable_names_file <- "variable-names.csv"
variable_names_file_path <- path(variables_path, variable_names_file)
variable_names <- read_csv(variable_names_file_path)

variables_to_select <- setdiff(names(dataset_select), c("CASEID", "YEAR"))

variables_to_fill <- setdiff(variables_to_select, variable_names$variable)

variables_to_export <- filter(variables_dtfr, variable %in% variables_to_fill)
variables_to_export$variable_name <- rep(NA, nrow(variables_to_export))

variables_out_path <- path("data", "variables", "raw")
variables_out_file_name <- "variable-names-to-fill.csv"
variables_out_file_path <- path(variables_out_path, variables_out_file_name)
write_csv(variables_to_export, variables_out_file_path, na = "")

# Export data set ---------------------------------------------------------

data_out_path <- path("data", "interim")
data_out_file_name <- "dataset-without-names.csv"
data_out_file_path <- path(data_out_path, data_out_file_name)
write_csv(dataset_select, data_out_file_path, na = "")

