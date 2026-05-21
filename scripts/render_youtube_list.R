library(dplyr)
library(stringr)

#' Make YouTube Table from `write_playlist_details()`
#'
#' @param infile string, the path of the csv file containing youtube playlist data from `write_playlist_details()`
#'
#' @return a tibble
#' @export
#'
#' @examples
#' make_youtube_table(infile = "resources/youtube_shorts_data.csv")
make_youtube_table <- function(infile) {
  # Read in videos from the Youtube Shorts Playlist
  df <- tryCatch(
    # Check for the file created by GHA
    expr = {
      df <-
        readr::read_csv(infile)
      
      df <- df %>% 
        filter(status.privacyStatus == "public") %>% # Don't print unlisted videos
        select(snippet.title, contentDetails.videoPublishedAt, snippet.description, snippet.playlistId, snippet.resourceId.videoId)
      
      # Create actual url of video
      df <- df %>% 
        mutate(`Video` = paste0("[",snippet.title,"](https://www.youtube.com/watch?v=", snippet.resourceId.videoId, "&list=", snippet.playlistId, ")"))
      
      # Create date posted
      df <- df %>% 
        mutate(Date = format(contentDetails.videoPublishedAt, "%Y-%m-%d"))
        
      # Extract slides link
      df$Slides <-
        df$snippet.description %>% 
        str_extract_all("https://docs.google.com/presentation/d/.*|https://drive.google.com/file/d/.*") %>%
        str_remove_all("\\.$") # Remove trailing period
      
      # Create Google Slides link w/ markdown magic
      df <- df %>% 
        mutate(Slides = paste0("[Go to slides](", Slides, ")")) %>% 
        select(Video, Date, Slides) %>% 
        mutate(Slides = case_when(Slides == "[Go to slides](character(0))" ~ "N/A", # Often, there are no slides on the video description
                                  Slides == "[Go to slides](NA)" ~ "N/A",
                                  TRUE ~ Slides))
      
      # Remove duplicates if necessary
      df <- distinct(df) %>% 
        arrange(desc(Date))
      
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
