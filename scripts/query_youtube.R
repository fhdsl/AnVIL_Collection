#!/usr/bin/env Rscript

library(optparse)
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)
# library(stringr)

# --------- Get the Youtube API key ---------

option_list <- list(
  optparse::make_option(
    c("--yt_key"),
    type = "character",
    default = NULL,
    help = "Youtube API key",
  )
)

# Read the GH_PAT argument
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
key <- opt$yt_key

message(is.character(key))

# --------- Relevant constants ---------

base_api <- "https://www.googleapis.com/youtube/v3/"
AnVIL_channel_id <- "UCBbHCj7kUogAMFyBAzzzfUw" # https://www.youtube.com/channel/UCBbHCj7kUogAMFyBAzzzfUw
shorts_playlist_id <- "PL6aYJ_0zJ4uCABkMngSYjPo_3c-nUUmio"

# --------- Set url and key as first test ---------

message(paste("Querying Youtube API..."))

# Request results from AnVIL Channel
url <- paste0(
  base_api,
  "channels?key=",
  key,
  "&id=",
  AnVIL_channel_id,
  "&part=snippet,contentDetails,statistics"
)

# Make the request
req <- GET(url = url)

if (!(httr::http_error(req))) {
  message(paste("API request successful!"))
} else {
  stop("API request failed!")
}

# --------- Get basic channel info ---------

channel_dat <-
  jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE)

# Save relevant statistics in table format to write to artifact
uploads <- tibble(
  playlist_id = channel_dat$items$contentDetails.relatedPlaylists.uploads,
  view_count = channel_dat$items$statistics.viewCount,
  sub_count = channel_dat$items$statistics.subscriberCount,
  vid_count = channel_dat$items$statistics.videoCount
)

# --------- Save the metadata ---------

# Create an artifact file containing the repos, else write an empty file
if (!dir.exists("resources")) {
  dir.create("resources")
}
if (nrow(uploads) > 0) {
  readr::write_tsv(uploads, file.path('resources', 'youtube_metadata.tsv'))
} else {
  readr::write_tsv(tibble(), file.path('resources', 'youtube_metadata.tsv'))
}

# --------- Get the shorts playlist ---------

# temporary variables
nextPageToken <- ""
shorts_df <- NULL

# Loop through the playlist while there is still a next page
while (!is.null(nextPageToken)) {

  # Request results from the shorts playlist
  url <- paste0(
    base_api,
    "playlistItems?key=",
    key,
    "&playlistId=",
    shorts_playlist_id,
    "&part=snippet,contentDetails,status&maxResults=50"
  )
  
  # Add the page token for page 2 onwards
  if (nextPageToken != "") {
    url <- paste0(url, "&pageToken=", nextPageToken)
  }
  
  req <- GET(url)
  
  channel_dat <-
    jsonlite::fromJSON(httr::content(req, as = "text"), flatten = TRUE)
  
  # Determine if next page is present
  nextPageToken <- channel_dat$nextPageToken
  
  page_df <- as.data.frame(channel_dat$items)
  if (is.null(shorts_df)) {
    shorts_df <- page_df
  } else {
    shorts_df <- bind_rows(shorts_df, page_df)
  }
}

# --------- Save the playlist data ---------

# Create an artifact file containing the repos, else write an empty file
if (!dir.exists("resources")) {
  dir.create("resources")
}
if (nrow(shorts_df) > 0) {
  readr::write_tsv(shorts_df, file.path('resources', 'youtube_shorts_data.tsv'))
} else {
  readr::write_tsv(tibble(), file.path('resources', 'youtube_shorts_data.tsv'))
}
