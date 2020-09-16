#' @title Polygonize suitable habitat
#' @description An internal function to polygonize suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a raster object from the \code{\link{rasterize.suitable.habitat}} function.
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @import sp raster rgeos rgdal sf stars units
#' @keywords internal
#' @importFrom grDevices chull
#' @importFrom stats na.omit
#' @export

polygonize.suitable.habitat <- function(data, buffer.width, drop.crumbs) {
  
  pol <- suppressWarnings(sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(data), as_points = FALSE, merge = TRUE)))
  
  ## Merge adjacent polygons ###
  
  pol <- rgeos::gBuffer(pol, byid = FALSE, width = buffer.width)
  pol <- rgeos::gBuffer(pol, byid = FALSE, width = -buffer.width)
  
  pol2 <- suppressWarnings(ksmooth_polys(x = pol, k = 12, N = 5L)) # From helpers
  
  ## Return
  
  if (drop.crumbs != 0 & !is.na(drop.crumbs)) {
    suppressWarnings(smoothr::drop_crumbs(pol2, units::set_units(drop.crumbs, km^2)))
  } else {
    pol2
  }

}

