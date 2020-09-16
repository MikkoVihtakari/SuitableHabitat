#' @title Plot a NEMO data on a map
#' @description Plot method for \code{\link[=NEMOdata]{NEMOdata}} objects.
#' @param x \code{NEMOdata} object
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" limits the map using coordinate range of \code{x}. Alternatively use a numeric vector as described in \code{\link[ggOceanMaps]{basemap}} documentation.
#' @param type Character argument defining the types of model data to be plotted: 
#' \itemize{
#' \item \code{"depth"} Depth data as raster
#' \item \code{"temp"} Temperature data as raster
#' \item \code{"sal"} Salinity data as raster
#' \item \code{"all"} All of the above
#' }
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @param land.col Character code specifying the color of land.
#' @param midpoints Named vector defining the midpoints for color scales of plotted variables. See \code{\link[ggplot2]{scale_colour_gradient}}.
#' @param ncol Integer specifying the number of columns when all variables are plotted (\code{type = "all"}).
#' @param ... Additional arguments passed to \code{\link[ggOceanMaps]{basemap}}.
#' @method plot NEMOdata
#' @author Mikko Vihtakari
#' @import ggplot2
#' @importFrom ggOceanMaps basemap LS round_any reorder_layers
#' @importFrom scales squish
#' @export

# limits = 40; type = "all"; base_size = 8; land.col = "grey80"; midpoints = c("temp" = 7, "depth" = 1500, "sal" = 34)
plot.NEMOdata <- function(x, limits = 40, type = "all", midpoints = c("temp" = 7, "depth" = 1500, "sal" = 34.5), ncol = 1, base_size = 8, land.col = "grey80", ...) {
  
  # Tests
  
  # if(!type %in% c("raw", "raster", "polygon", "hexagon", "raw&raster", "lim.fact", "default")) stop("Invalid type argument. Use one of following: 'raw', 'raster', 'polygon', 'hexagon', 'raw&raster', 'lim.fact' or 'default'")
  
  limits.lon <- 1e5
  limits.lat <- 1e5
  
  # Data manipulation
  
  dt <- data.frame(lon = c(x$lon), lat = c(x$lat), depth = c(x$depth), temp = c(x$temp), sal = c(x$sal))
  
  if(length(limits) == 1 && limits < 90 && limits > 30) {
    dt <- dt[dt$lat > limits,]
  }
  
  dt[dt$depth < 1, "temp"] <- NA
  dt[dt$depth < 1, "sal"] <- NA
  
  dt <- dt[order(dt$lon, dt$lat),]
  
  sps <- sp::SpatialPointsDataFrame(coords = dt[c("lon", "lat")], data = dt[c("depth", "temp", "sal")], proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sps <- sp::spTransform(sps, sp::CRS("+init=epsg:3995"))
  spdt <- data.frame(sps)
  
  # Basemap
  
  if(limits == "auto") limits <- c(ggOceanMaps::round_any(min(spdt$lon), limits.lon, floor), ggOceanMaps::round_any(max(spdt$lon), limits.lon, ceiling), ggOceanMaps::round_any(min(spdt$lat), limits.lat, floor), ggOceanMaps::round_any(max(spdt$lat), limits.lat, ceiling))
  
  bm <- suppressMessages(ggOceanMaps::basemap(limits = limits, shapefiles = "Arctic", land.col = land.col, base_size = base_size, verbose = FALSE))
  
  # Add data on maps
  
  if(type == "temp" | type == "all") {
    
    col.breaks <- sort(unique(c(-2, 0, 2.5, 5, 7.5, 10, 12.5, 15, midpoints[["temp"]])))
    
    pt <- bm + 
      geom_tile(data = spdt, aes(x = lon, y = lat, color = temp, fill = temp), width = 1.5e4, height = 1.5e4) +
      scale_color_gradient2(name = "Bottom\ntemperature", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoints[["temp"]]) + 
      scale_fill_gradient2(name = "Bottom\ntemperature", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoints[["temp"]])
    
    pt <- ggOceanMaps::reorder_layers(pt)
  } 
  
  if(type == "depth" | type == "all") {
    
    col.breaks <- sort(unique(c(1, 1000, 3000, 5000, 7000, midpoints[["depth"]])))
    
    pd <- bm + 
      geom_tile(data = spdt, aes(x = lon, y = lat, color = depth, fill = depth), width = 1.5e4, height = 1.5e4) +
      scale_color_gradient2(name = "Bottom\ndepth", low = "#F7FBFF", mid = "#7F95B5", high = "#08306B", na.value = "grey", breaks = col.breaks, midpoint = midpoints[["depth"]]) + 
      scale_fill_gradient2(name = "Bottom\ndepth", low = "#F7FBFF", mid = "#7F95B5", high = "#08306B", na.value = "grey", breaks = col.breaks, midpoint = midpoints[["depth"]])
    
    pd <- ggOceanMaps::reorder_layers(pd)
  }
  
  if(type == "sal" | type == "all") {
    
    col.breaks <- sort(unique(c(31, 32, 33, 34, 35, 36, midpoints[["sal"]])))
    
    ps <- bm + 
      geom_tile(data = spdt, aes(x = lon, y = lat, color = sal, fill = sal), width = 1.5e4, height = 1.5e4) +
      scale_colour_viridis_b(name = "Bottom\nsalinity", limits = c(31, 36), oob = scales::squish, breaks = col.breaks, na.value = "grey") + 
      scale_fill_viridis_b(name = "Bottom\nsalinity", limits = c(31, 36), oob = scales::squish, breaks = col.breaks, na.value = "grey") 
    
    ps <- ggOceanMaps::reorder_layers(ps)
  } 
  
  # Plot
  
  if(type == "all") {
    cowplot::plot_grid(pt, pd, ps, ncol = ncol, labels = c("Temperature", "Depth", "Salinity"))
  } else if (type == "temp") {
    pt
  } else if (type == "depth") {
    pd
  } else if (type == "sal") {
    ps
  }
}
