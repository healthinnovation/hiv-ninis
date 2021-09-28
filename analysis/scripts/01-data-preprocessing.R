library(fs)
library(haven)
library(purrr)
library(dplyr)
library(data.table)

raw_path <- path('data', 'raw')
endes_path <- path(raw_path, 'endes')
endes_files <- dir_ls(endes_path, recurse = T, glob = '*.sav')
surveys <- c('CSALUD01', 'RE516171', 'RE758081', 'REC0111', 'REC91')
pattern <- paste(surveys, collapse = '|')
endes_files <- endes_files[grepl(pattern, endes_files)]
dfrs_raw <- map(endes_files, read_sav)
# names(dfrs_raw) <- path_ext_remove(path_file(endes_files))

for(dfr in names(dfrs_raw)) {
  dfrs_raw[[dfr]]$path <- dfr
  dfrs_raw[[dfr]] <- as_factor(dfrs_raw[[dfr]])
}

dfrs <- list()
for (survey in surveys) {
  dfrs[[survey]] <- list()
  for (dfr in names(dfrs_raw)) {
    if (grepl(survey, dfr)) {
      dfrs[[survey]][[dfr]] <- dfrs_raw[[dfr]]
    }
  }
}

dfrs_merged <- list()
for (survey in names(dfrs)) {
  dfrs_merged[[survey]] <- rbindlist(dfrs[[survey]], fill = T)
}
