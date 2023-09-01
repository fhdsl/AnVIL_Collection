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

# --------- Set url and token ---------

message(paste("Querying Youtube API..."))

# Request results from AnVIL Channel

AnVIL_channel_id <- "UCBbHCj7kUogAMFyBAzzzfUw" # https://www.youtube.com/channel/UCBbHCj7kUogAMFyBAzzzfUw
base <- "https://www.googleapis.com/youtube/v3/"

url <-
  paste0(
    "https://www.googleapis.com/youtube/v3/channels?key=",
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
uploads <- tibble(
  playlist_id = channel_dat$items$contentDetails.relatedPlaylists.uploads,
  view_count = channel_dat$items$statistics.viewCount,
  sub_count = channel_dat$items$statistics.subscriberCount,
  vid_count = channel_dat$items$statistics.videoCount
)

# --------- Save the collection ---------

# Create an artifact file containing the repos, else write an empty file
if (!dir.exists("resources")) {
  dir.create("resources")
}
if (nrow(uploads) > 0) {
  readr::write_tsv(uploads, file.path('resources', 'youtube_metadata.tsv'))
} else {
  readr::write_tsv(tibble(), file.path('resources', 'youtube_metadata.tsv'))
}
