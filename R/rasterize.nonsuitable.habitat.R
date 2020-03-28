#' @title Rasterize non-suitable habitat to find limiting factors
#' @description An internal function to rasterize non-suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a data frame from \code{\link{suitable.habitat}} function
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @import sp raster
#' @keywords internal
#' @export

# data = spdt; proj4 = proj4; mod.extent = mod.ext; drop.crumbs = drop.crumbs; res = res
rasterize.nonsuitable.habitat <- function(data, proj4, mod.extent = mod.ext, drop.crumbs, res) {
  
  ## Non suitable habitat
  
  x <- data[!is.na(data$lim.factor),] 
  # x <- data[is.na(data$habitat) & !is.na(data$lim.factor),] 
  
  lim.factor.levels <- data.frame(ID = seq_along(levels(x$lim.factor)), level = levels(x$lim.factor))
  
  x$lim.factor <- as.integer(x$lim.factor)
  
  ## Suitable habitat
  
  # y <- data[!is.na(data$habitat),]

  # To points
  
  p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("lim.factor")])
  # ps <- sp::SpatialPointsDataFrame(coords = y[c("lon", "lat")], data = y[c("habitat")])
  
  # Rasterize
  
  ext <- raster::extent(mod.extent)
  r <- raster::raster(ext, ncol = res, nrow = res)
  y <- raster::rasterize(p, r, p$lim.factor)
  sp::proj4string(y) <- proj4
  
  # rs <- raster::raster(ext, ncol = res, nrow = res)
  # ys <- raster::rasterize(ps, rs, ps$habitat)
  # 
  # Clump disconnected regions
  
  # rs <- raster::clump(ys) # Uses Queen's case by default
  
  # Calculate area for the clumps and subset
  
  # area.tab <- tapply(suppressWarnings(raster::area(rs, na.rm = TRUE)), rs[], sum)/1e6
  # area.tab <- data.frame(id = names(area.tab), area = unname(area.tab))
  # area.tab <- area.tab[area.tab$area >= drop.crumbs,]
  # 
  # Subset and replace the value by area
  
  # ys <- raster::subs(rs, area.tab)
  
  # Factorize
  
  y <- raster::as.factor(y)
  levels(y) <- list(lim.factor.levels)
  
  # Return
  
  y
}
