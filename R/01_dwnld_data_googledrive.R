# download project data from Google Drive

## help from: https://community.rstudio.com/t/how-to-download-a-google-drives-contents-based-on-drive-id-or-url/16896

library(googledrive)
library(purrr)
library(here)
library(stringr)

setwd(here::here('data')) # 2021-01-28 doing this for now because I can't figure out a work around

# store the URL for the project data

folder_url <- "https://drive.google.com/drive/folders/1_DjiVtWL3VLHoOms8rEUhiUfbVdz83MH" 

## identify this folder on Drive
## Let `googledrive` know this is a file ID or URL, as opposed to file name
folder <- googledrive::drive_get(as_id(folder_url))

# find files in folder
files = googledrive::drive_ls(folder)

# loop dirs and download files inside them
# this will also skip files already downloaded

for (i in seq_along(files$name)) {
  
  #list files
  i_dir = googledrive::drive_ls(files[i, ])
  
  #mkdir
  dir.create(files$name[i])
  
  #download files
  for (file_i in seq_along(i_dir$name)) {
    
    # fails if already exists
    try({
      drive_download(
        as_id(i_dir$id[file_i]),
        path = stringr::str_c(files$name[i], "/", i_dir$name[file_i])
        )
    })
  }
}

rm(list = ls())

