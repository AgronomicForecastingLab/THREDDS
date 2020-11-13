library(tidyverse)
library(raster)
#-----------------
S30789 <- S30789 %>%
  discard(~ nrow(.x[[1]])==0)

all.imgs <- S30789 %>%
  map(possibly(function(p.date){
    range.x <- range(p.date[[1]]$lon)
    range.y <- range(p.date[[1]]$lat)
    
    r <- p.date[[1]] %>%
      split(.$band) %>%
      map(function(one.day.data){
        # get the (last) indices
        #browser()
        r0 <- rasterFromXYZ(one.day.data[,c("lon","lat","value")]) %>%
          `extent<-`(c(range.x, range.y))
        
        names(r0) <- paste0("B", unique(one.day.data$band))
        return(r0)
      }) %>%
      raster::stack()
    

    return(
      list(
        Raster=r,
        Date=p.date$MetaData$Date
      )
    )
  }, otherwise = NULL)) %>%
  discard(is.null)




all.imgs %>%
  walk(possibly( function(img){
    par(oma=c(2,5,2,2), mar=c(1,1,1,1))
    print(plotRGB(
      img$Raster, 4, 3, 2 , axes = TRUE, scale = max(apply(values(img$Raster), 2, max)[2:4]),
      main=img$Date
    ))
  }, otherwise = NULL))
