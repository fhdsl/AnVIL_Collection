#!/usr/bin/env Rscript

library(optparse)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --------- Get the Github Token ---------

option_list <- list(
  optparse::make_option(
    c("--git_pat"),
    type = "character",
    default = NULL,
    help = "GitHub personal access token",
  )
)

# Read the GH_PAT argument
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
git_pat <- opt$git_pat

# ------ Function to get book title ------

get_book_title <- function(df) {
  # Create dummy column
  df$book_title <- ""
  
  for (i in 1:nrow(df)) {
    # Make raw content url
    base_url <-
      str_replace(df[i, ]$html_url, "github.com", "raw.githubusercontent.com")
    readlines_url <- paste0(base_url, "/main/index.Rmd")
    
    # Readlines from url
    index_data <- readLines(readlines_url)
    
    # Get book metadata
    metadata_lines <- grep("---", index_data)
    book_metadata <-
      index_data[(metadata_lines[1] + 1):(metadata_lines[2] - 1)]
    
    # Extract title
    book_title <- book_metadata[grep("title:",  book_metadata)]
    
    # Strip extra characters
    book_title <- str_replace(book_title, 'title: \"', '')
    book_title <- str_replace(book_title, '\"', '')
    
    # Append
    df$book_title[i] <- book_title
  }
  
  return(df)
}

# --------- Set url and token ---------

message(paste("Querying Github API..."))

# Request search results specific to jhudsl + fhdsl + DataTrail organizations
# Also allows us to pull in repos forked into these organizations
url <- "https://api.github.com/search/repositories?q=user:jhudsl+user:fhdsl+fork:true&per_page=50"

# Provide the appropriate GH token & Make the request
req <- GET(url = url, config = add_headers(Authorization = paste("token", git_pat)))

if (!(httr::http_error(req))) {
  message(paste("API request successful!"))
} else {
  stop("API request failed!")
}

# --------- Traverse pages ---------

# Pull out the last page number of the request
last <- str_extract(req$headers$link, pattern = '.(?=>; rel=\"last\")')

full_repo_df <- tibble()
for (page in 1:last){

  url <- paste0("https://api.github.com/search/repositories?q=user:jhudsl+user:fhdsl+fork:true&per_page=50&page=", page)
  message(paste("Gathering results from:", url))
  req <- GET(url = url, config = add_headers(Authorization = paste("token", git_pat)))
  repo_dat <-
    jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE)
  message(paste("... Gathered", nrow(repo_dat$items), "repositories."))

  repo_df <-
    tibble(repo_dat$items) %>%
    select(full_name, homepage, html_url, description, private) %>%
    separate(full_name, into = c("org", "name"), sep = "/") %>%

    # Collapse topics so they can be printed
    bind_cols(tibble(topics = unlist(
      lapply(repo_dat$items$topics, paste, collapse = ", ")
    ))) %>%

    # Drop private repos and remove org column
    filter(!(private)) %>%
    select(!c(private, org)) %>%

    # Rearrange columns
    relocate(description, .before = topics) %>%

    # Keep only those with homepages and descriptions
    filter(!(is.na(homepage)), homepage != "",!(is.na(description))) %>%
  
    # Keep only AnVIL and GDSCN related content
    # Exclude templates
    mutate(is_anvil = str_detect(topics, "anvil")) %>% 
      mutate(is_gdscn = str_detect(topics, "gdscn")) %>%
      mutate(is_template = str_detect(topics, "template")) %>%
      filter(is_anvil | is_gdscn) %>% 
      filter(!is_template) %>% 
      select(!c(is_anvil, is_gdscn, is_template)) %>% 
    
    # Get repository book titles
    get_book_title()

  full_repo_df <- rbind(full_repo_df, repo_df) %>%
    dplyr::arrange(name)
}

# --------- Save the collection ---------

# Create an artifact file containing the repos, else write an empty file
if (!dir.exists("resources")) {
  dir.create("resources")
}
if (nrow(full_repo_df) > 0) {
  readr::write_tsv(full_repo_df, file.path('resources', 'collection.tsv'))
} else {
  readr::write_tsv(tibble(), file.path('resources', 'collection.tsv'))
}
