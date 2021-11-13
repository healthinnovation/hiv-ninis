library(fs)
library(haven)
library(purrr)
library(dplyr)
library(data.table)
library(readr)
library(stringi)
library(tidyr)

# Read raw surveys --------------------------------------------------------

raw_path <- path("data", "raw")
endes_path <- path(raw_path, "endes")
endes_files <- dir_ls(endes_path, recurse = T, glob = "*.sav")
endes_surveys <- c("CSALUD01", "RE516171", "RE758081", "REC0111", "REC91")
pattern <- paste(endes_surveys, collapse = "|")
endes_files <- endes_files[grepl(pattern, endes_files)]
surveys_raw <- map(endes_files, read_sav)

# Read variables files ----------------------------------------------------

variables_path <- path("data", "variables")
variables_files <- dir_ls(variables_path, recurse = TRUE, glob = "*.txt")
variables <- map(variables_files, scan, what = character(), quiet = TRUE)
names(variables) <- path_ext_remove(path_file(variables_files))

survey_dtfrs <- list()
for (survey_name in names(variables)) {
  survey <- rep(survey_name, length(variables[[survey_name]]))
  survey_dtfrs[[survey_name]] <- 
    tibble(survey, variable = variables[[survey_name]])
}
survey_variables <- bind_rows(survey_dtfrs)

# Initial format and merge by survey name ---------------------------------

for(survey in names(surveys_raw)) {
  print(survey)
  names(surveys_raw[[survey]]) <- toupper(names(surveys_raw[[survey]]))
  
  survey_name <- path_ext_remove(path_file(survey))
  if (!(survey_name %in% names(variables))) {
    cat(paste(survey_name, "is missing.\n"))
  } else {
    surveys_raw[[survey]] <- 
      surveys_raw[[survey]] %>% 
      select(all_of(variables[[survey_name]]))
  }
  
  surveys_raw[[survey]]$PATH <- survey
  
  surveys_raw[[survey]] <- 
    surveys_raw[[survey]] %>% 
    # as_factor() %>% 
    mutate(across(everything(), as.character)) %>% 
    mutate(across(everything(), ~ trimws(.x, which = "both")))
}

surveys <- list()
for (survey in endes_surveys) {
  surveys[[survey]] <- list()
  for (survey_raw in names(surveys_raw)) {
    if (grepl(survey, survey_raw)) {
      surveys[[survey]][[survey_raw]] <- surveys_raw[[survey_raw]]
    }
  }
}

surveys_merged <- list()
for (survey in names(surveys)) {
  print(survey)
  surveys_merged[[survey]] <- 
    bind_rows(surveys[[survey]]) %>% 
    mutate(
      YEAR = substr(PATH, 16, 19),
      MODULE = path_file(path_dir(PATH)),
      SURVEY = path_ext_remove(path_file(PATH))
    )
}

# Join surveys ------------------------------------------------------------

surveys_merged$CSALUD01 <- 
  surveys_merged$CSALUD01 %>% 
  mutate(
    CASEID = paste(HHID, QSNUMERO)
  )

full_dataset <- 
  surveys_merged$REC0111 %>% 
  left_join(surveys_merged$REC91, by = c("CASEID", "YEAR")) %>% 
  left_join(surveys_merged$RE516171, by = c("CASEID", "YEAR")) %>% 
  left_join(surveys_merged$RE758081, by = c("CASEID", "YEAR")) %>% 
  left_join(surveys_merged$CSALUD01, by = c("CASEID", "YEAR"))

# Filter rows -------------------------------------------------------------

dataset_raw <- 
  full_dataset %>% 
  filter(!is.na(V781)) %>% 
  select(!starts_with(c('PATH', 'MODULE', 'SURVEY')))


# Filter variables with high rate of missing values ------------------------

na_percent_raw <- apply(
  dataset_raw, 2, function(x) sum(is.na(x)) / nrow(dataset_raw)
)

to_filter <-
  names(which(na_percent_raw > 0.8))[
    !names(which(na_percent_raw > 0.8)) %in% c(
      'V770A', 'V770B', 'V770G', 'V770H', 'V770I', 'V770J', 'V770K', 'V770M', 
      'V770P', 'V770Q', 'V770S', 'V770U', 'V770V', 'V770W', 'V770X'
    )
  ]

dataset_values <- select(dataset_raw, !any_of(to_filter))


# Check rate of missing values --------------------------------------------

# na_percent <- apply(
#   dataset, 2, function(x) round(100 * sum(is.na(x)) / nrow(dataset), 2)
# ) 
# 
# na_percent_dtfrm <- 
#   tibble(variable = names(dataset), na_percent) %>% 
#   arrange(na_percent)


# Set variable names ------------------------------------------------------

variable_names_path <- path(variables_path, 'variable-names.csv')
variable_names <- read_csv(variable_names_path)

for (i in seq(nrow(variable_names))) {
  names(dataset_values)[names(dataset_values) == variable_names$variable[i]] <- 
    variable_names$variable_name[i]
}

# Set variable values -----------------------------------------------------

variable_values_path <- path(variables_path, 'variable-values.csv')
variable_values <- read_csv(variable_values_path)

dataset <- tibble(dataset_values)

for (i in 1:nrow(variable_values)) {
  variable_name <- variable_values$variable_name[i]
  value <- as.character(variable_values$values[i])
  value_description <- variable_values$value_description[i]
  
  dataset[variable_name][dataset[variable_name] == value] <- value_description
}


# Get values for all variables --------------------------------------------

# dataset %>% 
#   select(!c(wealth_index_scoring_factor, weighting_factor, year)) %>% 
#   pivot_longer(
#     cols = !caseid,
#     names_to = "variable_name",
#     values_to = "value"
#   ) %>% 
#   group_by(variable_name) %>% 
#   summarise(values = sort(as.numeric(unique(value))), .groups = "drop") %>% 
#   left_join(variable_names, by = "variable_name") %>% 
#   left_join(survey_variables, by = "variable") %>% 
#   select(survey, variable, variable_name, values) %>% 
#   arrange(survey, variable, values) %>% 
#   write.csv("data/interim/variable-values.csv", row.names = FALSE)



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

write.csv(dataset, 'data/processed/hiv-ninis-dataset.csv', row.names = FALSE)

