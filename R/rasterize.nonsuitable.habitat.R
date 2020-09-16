#' @title Rasterize non-suitable habitat to find limiting factors
#' @description An internal function to rasterize non-suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a data frame from \code{\link{suitable.habitat}} function
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @import sp raster
#' @keywords internal
#' @seealso \code{\link{limiting.factors}} \code{\link{plot.SHmod}}  
#' @export

# data = spdt; proj4 = proj4; mod.extent = mod.ext; drop.crumbs = drop.crumbs; res = res
rasterize.nonsuitable.habitat <- function(data, proj4, mod.extent = mod.ext, drop.crumbs, res) {
  
  # Data
  
  x <- data[!is.na(data$lim.factor),] 
  x <- droplevels(x)
  
  ## Limiting factor levels
  
  lim.factor.levels <- data.frame(ID = c(0, seq_along(levels(x$lim.factor))), VALUE = c("crumbs", levels(x$lim.factor)))
  x$lim.factor <- as.integer(x$lim.factor)
  
  ### Convert to points
  
  p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("habitat", "lim.factor")])
  
  ## Common raster setup
  ext <- raster::extent(mod.extent)
  base.raster <- raster::raster(ext, ncol = res, nrow = res)
  
  # Suitable habitat
  ## Rasterize
  
  y <- raster::rasterize(p, base.raster, p$habitat, fun = mean)
  
  suppressWarnings(sp::proj4string(y) <- proj4)
  
  ## Clump disconnected regions
  
  r <- raster::clump(y) # Uses Queen's case by default
  
  ## Calculate area for the clumps and subset
  
  area.tab <- tapply(suppressWarnings(raster::area(r, na.rm = TRUE)), r[], sum)/1e6
  area.tab <- data.frame(id = names(area.tab), area = unname(area.tab) >= drop.crumbs)
  
  ## Subset and replace the value by area
  
  rashab <- raster::subs(r, area.tab)
  
  ### Non-suitable habitat
  
  y <- raster::rasterize(p, base.raster, p$lim.factor)
  suppressWarnings(sp::proj4string(y) <- proj4)
  
  # Add other limiting factors
  
  tmp <- data.frame(rashab = rashab@data@values, limfac = y@data@values)
  tmp$limfac[tmp$limfac == lim.factor.levels[lim.factor.levels$VALUE == "suitable", "ID"] & !is.na(tmp$limfac)] <- NA
  
  rashab@data@values <- ifelse(!is.na(tmp$rashab), tmp$rashab, ifelse(!is.na(tmp$limfac), tmp$limfac, NA))
  
  # Factorize ####
  
  rashab <- raster::as.factor(rashab)
  levels(rashab) <- list(lim.factor.levels)
  
  # Return
  
  rashab
}
