from multiprocessing import Pool, TimeoutError
from multiprocessing.dummy import Pool as ThreadPool
import arcpy
from arcpy import env
from arcpy.sa import *
import os

arcpy.env.workspace="C:/Users/hamzed/GoogleDrive"
arcpy.CheckOutExtension("Spatial")
arcpy.env.overwriteOutput = True

def raster_to_nc(x):

    inRaster = os.path.join(arcpy.env.workspace, "LANDSAT",x+".tif")
    outNetCDFFile = os.path.join(arcpy.env.workspace, "NC",x+".nc")
            # Set local variables
    
    # Process: RasterToNetCDF
    arcpy.RasterToNetCDF_md(inRaster, outNetCDFFile, "elevation",
                        "meter","x","y", "",compression_level= 9)
    print(inRaster)


if __name__ == '__main__':
    address=r'C:\Users\Para2x\Desktop\1-Tifs'
    pool = Pool(8)               # start 4 worker processes
    #####################
################### Reading my rasters
    rastersadd=[]
    rastersname=[]
    rastersfullname=[]
    rasterfolders=[]
    try:
        for root, dirs, files in os.walk(arcpy.env.workspace):
            for f in files:
                if f.endswith('.tif'):
                    rastersadd.append(os.path.join(root, f))
                    rastersname.append(f.split(".")[0])
                    rastersfullname.append(f)
                    rasterfolders.append(root)
    except arcpy.ExecuteError:
        print(arcpy.GetMessages())
        

    ######################
    pool.map(raster_to_nc, rastersname)
    pool.close()
    pool.join()