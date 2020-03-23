#' @title Rasterize suitable habitat
#' @description An internal function to rasterize suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a data frame from \code{\link{suitable.habitat}} function
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @import sp raster
#' @keywords internal
#' @export

rasterize.suitable.habitat <- function(data, proj4, drop.crumbs, res) {
  
  x <- data[!is.na(data$habitat),] # Remove non-habitat
  
  # To points
  
  p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("habitat")])
  
  # Rasterize
  
  ext <- raster::extent(p)
  r <- raster::raster(ext, ncol = res, nrow = res)
  y <- raster::rasterize(p, r, p$habitat, fun = mean)
  sp::proj4string(y) <- proj4
  
  # Clump disconnected regions
  
  r <- raster::clump(y) # Uses Queen's case by default
  
  # Calculate area for the clumps and subset
  
  area.tab <- tapply(suppressWarnings(raster::area(r, na.rm = TRUE)), r[], sum)/1e6
  area.tab <- data.frame(id = names(area.tab), area = unname(area.tab))
  area.tab <- area.tab[area.tab$area >= drop.crumbs,]
  
  # Subset and replace the value by area
  
  raster::subs(r, area.tab)
}
