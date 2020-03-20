#' @title Polygonize suitable habitat
#' @description An internal function to polygonize suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a data frame from \code{\link{suitable.habitat}} function
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @import sp raster rgeos rgdal sf stars units
#' @keywords internal
#' @importFrom grDevices chull
#' @importFrom stats na.omit

polygonize.suitable.habitat <- function(data, buffer.width, proj4, drop.crumbs, res) {
  
  x <- stats::na.omit(data)
  x <- x[x$habitat,]
  
  p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("habitat")])
  
  ext <- raster::extent(p)
  r <- raster::raster(ext, ncol = res, nrow = res)
  
  y <- raster::rasterize(p, r, p$habitat, fun=mean)
  
  # sp::proj4string(y) <- "+proj=longlat +datum=WGS84"
  # y <- raster::projectRaster(y, crs = proj4)
  
  pol <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y), as_points = FALSE, merge = TRUE))
  sp::proj4string(pol) <- proj4
  
  ## Merge adjacent polygons ###
  
  pol <- rgeos::gBuffer(pol, byid = TRUE, width = buffer.width)
  pol <- rgeos::gBuffer(pol, byid = TRUE, width = -buffer.width)
  
  pol2 <- ksmooth_polys(x = pol, k = 12, N = 5L) # From helpers
  
  ## Return
  
  if (drop.crumbs != 0 & !is.na(drop.crumbs)) {
    smoothr::drop_crumbs(pol2, units::set_units(drop.crumbs, km^2))
  } else {
    pol2
  }

}

