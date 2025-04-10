#### Function to cut, project and resample a raster ####
### convencience function, inspired by old ArcGIS tool "extract by mask" ###
## author: Florian Kunz
## created: 10.08.2022
## adapted: 16.01.2025, version 1.1

## ras_in: raster that should be cut according to a reference raster
## ras_ref: input raster but with spatial extent, crs and cell dimensions of reference raster
## categorical: logical, whether data is categorical
## Default for resample: nearest for class/bilinear for continuous

fun.extract.by.mask <- function(ras_in, ras_ref, categorical) {
  require("terra")
  if(categorical==TRUE) {print(noquote("raster set as CATEGORICAL, using nearest neighbour"))
  } else{print(noquote("raster set as CONTINUOUS, using bilinear"))}
  if(identical(crs(ras_in), crs(ras_ref))==FALSE) { # this will transform into crs from ref_ras, if needed
    if(categorical==TRUE) {
      print(noquote("calling terra::project"))
      ras_in <- terra::project(ras_in, ras_ref, method="near")
    } else {
      print(noquote("calling terra::project"))
      ras_in <- terra::project(ras_in, ras_ref)}
  }
  noquote("calling terra::resample")
  if(categorical==TRUE) {
    ras_in <- terra::resample(ras_in, ras_ref, method="near")
  } else {
    ras_in <- terra::resample(ras_in, ras_ref, method="bilinear")}# this will align cells (snap them)
  print(noquote("calling terra::crop"))
  ras_in <- terra::crop(ras_in, ras_ref, snap="near") # this will fit the extent to the ref raster
  print(noquote("calling terra::mask"))
  ras_in <- terra::mask(ras_in, ras_ref) # this will cut the raster (cell dimensions) to the ref raster
  print(ras_in)
  return(ras_in)
}
