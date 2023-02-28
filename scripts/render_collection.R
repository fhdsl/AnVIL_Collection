library(dplyr)
library(stringr)

make_collection_table <- function(exclude = NULL) {
  # Read in repos found by GHA
  df <- tryCatch(
    # Check for the file created by GHA
    expr = {
      df <-
        readr::read_tsv("resources/collection.tsv")
      
      # Do some cleaning of strings
      df$name <-
        df$name %>%
        stringr::str_replace_all("_Book_", ": ") %>%
        stringr::str_replace_all("_", " ")
      
      # Concatenate columns to create links
      df <-
        df %>%
        mutate(`Book Name` = paste0("[", book_title, "](", homepage, ") ([github](", html_url, "))")) %>%
        rename(Description = description, Topics = topics) %>%
        select(`Book Name`, Description, Topics)
      
      # Remove duplicates if necessary
      df <- distinct(df)
      
      return(df)
    },
    # Will error out if file doesn't exist - provides a blank tibble instead
    error = function(e) {
      df <-
        tibble(`Book Name` = "none",
               Description = "none",
               Topics = "none")
      
      return(df)
    }
  )
  
  return(df)
}
