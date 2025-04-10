#### Function to cut and crop a raster based on a drawn polygon ####
## author: Florian Kunz
## created: 23.11.23, based on extract.by.mask and dra.polygon
## adapted: 

## ras_in: raster that should be cut according to a reference raster
## ras_ref: input raster but with spatial extent, crs and cell dimensions of reference raster
## categorical: logical, whether data is categorical
## Default for resample: nearest for class/bilinear for continuous

fun.extract.by.draw.polygon <- function(in_ras) {
  require("terra")
  require("tmaptools")
  plot(in_ras)
  print("Choose your polyon by clicking.")
  print("Then click 'Finish' in the upper right corner to connect the last point to the first point")
  polygon_draw <- draw(x= "polygon", sp=TRUE, col='red', lwd=3) # draw polygon on the tif in the plotting window
  plot(in_ras)
  plot(polygon_draw, add =T)
  print("calling terra::crop")
  in_ras <- terra::crop(in_ras, polygon_draw) # this will fit the extent to the ref raster
  print("calling terra::mask")
  in_ras <- terra::mask(in_ras, polygon_draw) # this will cut the raster (cell dimensions) to the ref raster
  plot(in_ras)
  return(in_ras)
}
