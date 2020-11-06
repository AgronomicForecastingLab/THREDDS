library(ncdf4)
library(tidyverse)
library(tidync)
xmin <- -93
xmax <- -92.75
ymin <- 38.75
ymax <- 39
url <- "http://192.17.59.89:8080/thredds/dodsC/testAll/2013-05-131-0000009472-0000009472.nc"
#https://opendap.github.io/documentation/QuickStart.html  

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


OpenDap_LANDSAT(url, ymax, ymin, xmin, xmax)
