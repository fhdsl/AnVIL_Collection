library(dplyr)
library(stringr)

make_collection_table <- function(make_gsdcn_only_table = FALSE, exclude_gdscn_from_table = FALSE) {
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
      
      # Filter gdscn if desired
      if(make_gsdcn_only_table & !(exclude_gdscn_from_table)){
        df <- df %>% filter(is_gdscn == TRUE)
      }
      if(exclude_gdscn_from_table & !(make_gsdcn_only_table)){
        df <- df %>% filter(is_gdscn == FALSE)
      }
      if(make_gsdcn_only_table & exclude_gdscn_from_table){
        message("Cannot have both 'GDSCN only' and 'exclude GDSCN' options selected")
      }
      
      # Concatenate columns to create links
      df <-
        df %>%
        dplyr::arrange(book_title) %>% 
        mutate(`Book Name` = paste0("[", book_title, "](", homepage, ")")) %>%
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
