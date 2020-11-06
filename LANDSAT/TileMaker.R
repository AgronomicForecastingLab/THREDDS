library(tidyverse)
library(raster)
library(lubridate)
library(gdalUtils)
library(rgdal)
library(furrr)
plan(multiprocess)
img.folder<-"LANDSAT"
gdal_setInstallation(search_path = )
getOption("gdalUtils_gdalPath")

ff <- list.files(img.folder)


File.Date <- ff %>%
  map_dfr(function(x){
    date.img <- (strsplit(x,"-00")[[1]])[1]
    date.img<- as.Date(date.img, format = "%Y-%M-%j")
    
    data.frame(
      Date=date.img,
      File=x
    )
  })


File.ext <- File.Date %>%
  split(.$File) %>%
  future_map_dfr(possibly(function(one.f){
    # browser()
    ext <- brick(file.path("LANDSAT",one.f$File)) %>%
      extent()
    
    one.f %>%
      bind_cols(xmin=ext[1],
                xmax=ext[2],
                ymin=ext[3],
                ymax=ext[4])
  },otherwise = NULL),.progress = TRUE)



File.Date %>%
  split(.$File) %>%
  map(function(one.f){
    browser()
    
    writeRaster(
      brick(file.path("LANDSAT",one.f$File)),
      file.path("NC", paste0(tools::file_path_sans_ext(one.f$File),".nc4")),
      overwrite = TRUE,
      format = "CDF",
      varname = "Temperature",
      varunit = "degC",
      longname = "Temperature -- raster stack to netCDF",
      xname = "Longitude",
      yname = "Latitude",
      zname = "Time (Month)"
    )
    
  })

File.Date %>%
  split(.$Date) %>%
  map(function(one.day){
 
    
    raster.list <-one.day$File[3:4] %>%
      map(~stack(file.path("LANDSAT",.x))) %>%
      setNames(one.day$File[3:4])

    names(raster.list)[1:2] <- c('x', 'y')
    raster.list$fun <- mean
    raster.list$na.rm <- TRUE
    
    y <- do.call(mosaic, raster.list)
       browser()
       
       
       writeRaster(
         y,
         "rstack.nc",
         overwrite = TRUE,
         format = "CDF",
         varname = "Temperature",
         varunit = "degC",
         longname = "Temperature -- raster stack to netCDF",
         xname = "Longitude",
         yname = "Latitude",
         zname = "Time (Month)"
       )
  })
