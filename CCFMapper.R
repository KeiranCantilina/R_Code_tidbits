library(curl)
library(RCurl)
library(rio)
library(sendmailR)
library(stringr)
library(magick)

## Destination file
destfolder <- "D://CCFMaps//"

## Strings
prefix <- "https://iwebcr.ccf.org/etrakmaps/"
suffix <- ".png"
resolution <- 12

## Stop flag
stop_flag <- FALSE
invalid_flag <- 0

## Candidate buildings number code range to  search
buildings_start <- 100
buildings_end <- 1506

## Iterate through buildings
for (current_building in buildings_start:buildings_end){
  
  ## Try a building code
  resolution <- 12
  startimage <- try(image_read(paste(prefix, current_building, "_files/",resolution,"/0_0.png", sep="")), silent=TRUE)
  
  ## If the building code works, try upping resolution
  if(!class(startimage) == "try-error"){
    startimage <- try(image_read(paste(prefix, current_building, "_files/13/0_0.png", sep="")), silent=TRUE)
    
    ## If the high resolution building code works, set resolution to 13, else 12.
    if(!class(startimage) == "try-error"){
      resolution <- 13
    }
    
    ## Use working resolution and try stepping through tiles to download
   
    ## Step through Y
    ytiles <- c()
    current_ytile = 0
    for (current_ytile in 0:20){
      
      ## Step through X
      xtiles <- c()
      current_xtile = 0
      for (current_xtile in 0:20){
        image <- try(image_read(paste(prefix, current_building, "_files/",resolution,"/",current_xtile,"_",current_ytile,".png", sep="")), silent=TRUE)
        
        ## If tile is invalid, and we're not at x = 0, assume we x'd out and go to next y row after setting xmax
        if(class(image) == "try-error" && current_xtile != 0){
          invalid_flag <- 1
          break
        }
        
        ## If tile is invalid and we're at x = 0, assume we y'd out and stop after setting ymax
        if(class(image) == "try-error" && current_xtile == 0){
          stop_flag <- TRUE
          invalid_flag <- 2
          break
        }
        
        ## Else, let's save the tile to the current row vector!
        if (!stop_flag){
          xtiles <- append(xtiles, image)
        }
      }
      
      ## We finished a row! Let's glob it together add it to the vector of rows
      if (!stop_flag){
        xtiles <- image_append(xtiles)
      }
      ytiles <- append(ytiles, xtiles)
      
      ## If we y'd out, exit the loop after globbing the rows together and saving the image
      if(stop_flag){
        full_image <- image_append(ytiles, stack = TRUE)
        ## Save meeee
        image_write(full_image, path = paste(destfolder,current_building,".png"), format = "png")
        invalid_flag <- 3
        break
      }
    }
    
    
    
    ## Reset stop flag
    stop_flag <- FALSE
    
    ## If we're here, either we're done with the image or there wasn't one.
    gc()
  }

  
  
  
}
