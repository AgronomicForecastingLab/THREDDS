library(ncdf4)
library(tidyverse)
library(tidync)
library(furrr)
library(callr)
plan(multisession, workers = 8)
setwd("C:/Users/hamzed/GoogleDrive")
url <- "http://localhost/thredds/dodsC/testAll/NC/"
#https://opendap.github.io/documentation/QuickStart.html  
tile.info <- read.csv("https://raw.githubusercontent.com/AgronomicForecastingLab/THREDDS/main/LANDSAT/Tile_extents.csv",
                      sep=",")

OpenDap_LANDSAT <- function(url, ymax, ymin, xmin, xmax) {
  
  # Finding all the lat/long before subsetting
  lon <- ncdf4::ncvar_get(nc_open(url),'x')
  lat <- ncdf4::ncvar_get(nc_open(url),'y')
  
  
  #find the index of the bounds 
  ind.y.min <- which.min(abs(ymax - lat))
  ind.y.max <- which.min(abs(ymin - lat))
  ind.x.min <- which.min(abs(xmin - lon))
  ind.x.max <- which.min(abs(xmax - lon))
  #make the range of indices
  ydx <-  ind.y.min:ind.y.max
  xdx <- ind.x.min:ind.x.max
  
  #create the subset url
  subset.url <-
    paste0(
      url,
      '?elevation[0:1:11][',ind.y.min,':1:',ind.y.max,']',
      '[',ind.x.min,':1:', ind.x.max ,']'
    )
  
  #read the subset url and replace lat/lon indices to real lat long
  p <- tidync::tidync(subset.url) %>%
    tidync::hyper_tibble(na.rm = FALSE) %>%
    dplyr::mutate(x=x+ind.x.min-1,
                  y=y+ind.y.min-1)%>%
    dplyr::mutate(x=replace(x, is.nan(elevation), lon[x]
    ),
    y=replace(y, is.nan(elevation), lat[y]
    )
    )%>%
    dplyr::select(value=elevation, lon=x, lat=y, band) %>%
    dplyr::filter(is.finite(value))
  
  return(p)
}
Becks_sites <- read.csv("C:/Users/hamzed/OneDrive - University of Illinois - Urbana/UIUC/Data/Beck'Yield data/Becks_site.csv")

run.threds <- function() {
  tryCatch({
    Sys.sleep(1)
    RCurl::getURL('localhost:80/thredds/catalog.html')
  }, error = function(cond) {
    print("THREDDS is down !")
    system(
      "docker run --rm --name tds -d -p 80:8080 -v C:/Users/hamzed/GoogleDrive/content:/usr/local/tomcat/content unidata/thredds-docker",
      intern = TRUE
    )
    Sys.sleep(20)
    run.threds()
  })
}


#-------------------------------------------
Becks_LANDSAT <- Becks_sites %>%
  #head(1) %>%
  dplyr::select(Lat = Latitude, Lon = Longitude, TestPlotId, YearCode) %>%
  pwalk(possibly(function(Lat, Lon, TestPlotId, YearCode) {
    print(TestPlotId)
    #Find the tiles that this site will fit in
    avail.img <- tile.info %>%
      filter(Lat > ymin, Lat < ymax, Lon > xmin, Lon < xmax)# %>%
    # filter(lubridate::year(Date)==YearCode)
    
    #if files exists go to the next
    if(file.exists(paste0("RDS/",TestPlotId, ".RDS"))) return(NULL)
    
    #check if the thredds is up. Otherwise bring it up
    run.threds()
    
    # Get those tiles from the THREDDS
    data.s <- avail.img$File %>%
      future_map2(avail.img$Date, possibly(function(.x, datef) {
        ncfilen <- paste0(url,
                          tools::file_path_sans_ext(.x),
                          ".nc")
        # send the operation to an external R session to avoid memory leakage
        callr::r(function(url, Lat, Lon, ncfilen, datef){
          library(dplyr)
          OpenDap_LANDSAT <- function(url, ymax, ymin, xmin, xmax, datef=NULL) {
            
            # Finding all the lat/long before subsetting
            lon <- ncdf4::ncvar_get(ncdf4::nc_open(url),'x')
            lat <- ncdf4::ncvar_get(ncdf4::nc_open(url),'y')
            
            
            #find the index of the bounds 
            ind.y.min <- which.min(abs(ymax - lat))
            ind.y.max <- which.min(abs(ymin - lat))
            ind.x.min <- which.min(abs(xmin - lon))
            ind.x.max <- which.min(abs(xmax - lon))
            #make the range of indices
            ydx <-  ind.y.min:ind.y.max
            xdx <- ind.x.min:ind.x.max
            
            #create the subset url
            subset.url <-
              paste0(
                url,
                '?elevation[0:1:11][',ind.y.min,':1:',ind.y.max,']',
                '[',ind.x.min,':1:', ind.x.max ,']'
              )
            
            #read the subset url and replace lat/lon indices to real lat long
            p <- tidync::tidync(subset.url) %>%
              tidync::hyper_tibble(na.rm = FALSE) %>%
              dplyr::mutate(x=x+ind.x.min-1,
                            y=y+ind.y.min-1)%>%
              dplyr::select(value=elevation, lon=x, lat=y, band) %>%
              dplyr::filter(is.finite(value))
            
            p$lon<- lon[p$lon]
            p$lat<- lat[p$lat]
            
            return(list(p, MetaData=list(ymax=ymax,
                                         ymin=ymin,
                                         xmin=xmin,
                                         xmax=xmax,
                                         File=url,
                                         Date=datef) ))
          }
          
          OpenDap_LANDSAT(ncfilen,
                          Lat + 0.001,
                          Lat - 0.001,
                          Lon - 0.001,
                          Lon + 0.001, 
                          datef) 
          
        },args=list(url, Lat, Lon, ncfilen, datef), show=FALSE)
      },otherwise = NULL, quiet = FALSE),.progress = TRUE) 
    
    
    saveRDS(data.s, paste0("RDS/",TestPlotId, ".RDS"))
    #return(data.s)
    gc()
  }, otherwise = NULL,
  quiet = FALSE)
  )


