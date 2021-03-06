#' @title Rasterize suitable habitat
#' @description An internal function to rasterize suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a data frame from \code{\link{suitable.habitat}} function
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param mod.extent The oceanographic model extent
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal.
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @import sp raster
#' @keywords internal
#' @export

rasterize.suitable.habitat <- function(data, proj4, mod.extent = mod.ext, drop.crumbs, res) {
  
  if("robustness" %in% names(data)) { # Sensitivity analysis switch
    data$habitat <- data$robustness
  }
  
  x <- data[!is.na(data$habitat),] # Remove non-habitat
  
  # To points
  
  p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("habitat")])
  
  # Rasterize
  
  ext <- raster::extent(mod.extent)
  r <- raster::raster(ext, ncol = res, nrow = res)
  y <- raster::rasterize(p, r, p$habitat, fun = mean)
  suppressWarnings(sp::proj4string(y) <- proj4)
  
  # Clump disconnected regions
  
  r <- raster::clump(y) # Uses Queen's case by default
  
  # Calculate area for the clumps and subset
  
  area.tab <- tapply(suppressWarnings(raster::area(r, na.rm = TRUE)), r[], sum)/1e6
  area.tab <- data.frame(id = names(area.tab), area = unname(area.tab))
  area.tab <- area.tab[area.tab$area >= drop.crumbs,]
  
  # Subset and replace the value by area
  
  out <- raster::subs(r, area.tab)
  
  # Replace values by robusness (if sensitivity analysis is applied)
  
  if("robustness" %in% names(data)) {
    out@data@values <- ifelse(!is.na(out@data@values), y@data@values, NA)
    out@data@names <- "robustness"
    out@data@min <- min(out@data@values, na.rm = TRUE)
    out@data@max <- max(out@data@values, na.rm = TRUE)
  }
  
  ## Return
  
  out
}
