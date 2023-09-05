library(dplyr)
library(stringr)

make_youtube_shorts_table <- function() {
  # Read in videos from the Youtube Shorts Playlist
  df <- tryCatch(
    # Check for the file created by GHA
    expr = {
      df <-
        readr::read_tsv("resources/youtube_shorts_data.tsv")
      
      df <- df %>% 
        select(snippet.title, snippet.description, snippet.playlistId, snippet.resourceId.videoId)
      
      # Create actual url of video
      df <- df %>% 
        mutate(`Video` = paste0("[",snippet.title,"](https://www.youtube.com/watch?v=", snippet.resourceId.videoId, "&list=", snippet.playlistId, ")"))
        
      # Extract slides link
      df$`Google Slides` <-
        df$snippet.description %>% 
        str_extract_all("https://docs.google.com/presentation/d/.*") %>%
        str_remove_all("\\.$") # Remove trailing period
      
      # Create Google Slides link w/ markdown magic
      df <- df %>% 
        mutate(`Google Slides` = paste0("[Go to slides](", `Google Slides`, ")")) %>% 
        select(Video, `Google Slides`)
      
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
