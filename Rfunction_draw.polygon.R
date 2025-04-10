#### Function to cut, project and resample a raster ####
## author: Florian Kunz, based on Fabian Knufinke
## created: 21.02.2023
## ref_ras: this must be the raster that is visualized, based on which the user will draw the polygon

fun.draw.polygon <- function(ref_ras) {
  require("terra")
  require("tmaptools")
  plot(ref_ras)
  print("Choose your polyon by clicking.")
  print("Then click 'Finish' in the upper right corner to connect the last point to the first point")
  polygon_draw <- draw(x= "polygon", sp=TRUE, col='red', lwd=3) # draw polygon on the tif in the plotting window
  plot(ref_ras)
  plot(polygon_draw, add =T)
  return(polygon_draw)
}