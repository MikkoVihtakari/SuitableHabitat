#' @title Plot a NEMO data on a map
#' @description Plot method for \code{\link[=NEMOdata]{NEMOdata}} objects.
#' @param x \code{NEMOdata} object
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" limits the map using coordinate range of \code{x}. Alternatively use a numeric vector as described in \code{\link[PlotSvalbard]{basemap}} documentation.
#' @param type Character argument defining the types of model data to be plotted: 
#' \itemize{
#' \item \code{"depth"} Depth data as raster
#' \item \code{"temp"} Temperature data as raster
#' \item \code{"sal"} Salinity data as raster
#' \item \code{"all"} All of the above
#' }
#' @param ... Additional arguments. Required by R build checks. Ignore.
#' @method plot NEMOdata
#' @author Mikko Vihtakari
#' @import ggplot2
#' @importFrom PlotSvalbard basemap LS
#' @importFrom broom tidy
#' @export

# type = "default"
plot.NEMOdata <- function(x, limits = 40, type = "all", midpoint = NA, cutpoints = NA, ...) {
  
  # Tests
  
  # if(!type %in% c("raw", "raster", "polygon", "hexagon", "raw&raster", "lim.fact", "default")) stop("Invalid type argument. Use one of following: 'raw', 'raster', 'polygon', 'hexagon', 'raw&raster', 'lim.fact' or 'default'")
  
  # Data manipulation
  
  dt <- data.frame(lon = c(x$lon), lat = c(x$lat), depth = c(x$depth), temp = c(x$temp), sal = c(x$sal))
  dt <- dt[dt$lat > 40,]
  dt[dt$depth < 1, "temp"] <- NA
  dt[dt$depth < 1, "sal"] <- NA
  
  dt <- dt[order(dt$lon, dt$lat),]
  
  sps <- sp::SpatialPointsDataFrame(coords = dt[c("lon", "lat")], data = dt[c("depth", "temp", "sal")], proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sps <- spTransform(sps, CRS(polarStereographic))
  spdt <- data.frame(sps)
  
  
  if(!is.na(cutpoints)) {
    
    spdt[[type]]
  }
  
  
  if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = spdt)
  
  bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey80", base_size = 8)
  
  if(type == "temp") {
    
    if(is.na(midpoint)) midpoint <- 7
    
    col.breaks <- sort(unique(c(-2, 0, 2.5, 5, 7.5, 10, 12.5, 15, midpoint)))
    
    bm + 
      geom_tile(data = spdt, aes(x = lon, y = lat, color = temp, fill = temp), width = 1e4, height = 1e4) +
      scale_color_gradient2(name = "Bottom\ntemperature", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoint) + 
      scale_fill_gradient2(name = "Bottom\ntemperature", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoint) + 
      add_land()
    
  } else if(type == "depth") {
    
    if(is.na(midpoint)) midpoint <- 1500
    
    col.breaks <- sort(unique(c(0, 250, 500, 750, 1000, 2000, 3000, 4000, 6000, midpoint)))
    
    bm + 
      geom_tile(data = spdt, aes(x = lon, y = lat, color = depth, fill = depth), width = 1e4, height = 1e4) +
      scale_color_gradient2(name = "Bottom\ndepth", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoint) + 
      scale_fill_gradient2(name = "Bottom\ndepth", low = "blue4", high = scales::muted("red"), na.value = "grey", breaks = col.breaks, midpoint = midpoint) + 
      add_land()
    
  } else if(type == "default") {
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = x$raw)
    
    rawdt <- x$raw[!is.na(x$raw$habitat),]
    
    rasdt <- data.frame(raster::coordinates(x$raster), area = x$raster@data@values)
    rasdt <- rasdt[!is.na(rasdt$area),]
    names(rasdt)[names(rasdt) == "x"] <- "lon"
    names(rasdt)[names(rasdt) == "y"] <- "lat"
    
    poldt <- suppressMessages(suppressWarnings(broom::tidy(x$polygon)))
    names(poldt)[names(poldt) == "long"] <- "lon"
    
  } else if(type == "raster") {
    
    rasdt <- data.frame(raster::coordinates(x$raster), area = x$raster@data@values)
    rasdt <- rasdt[!is.na(rasdt$area),]
    names(rasdt)[names(rasdt) == "x"] <- "lon"
    names(rasdt)[names(rasdt) == "y"] <- "lat"
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = rasdt)
    
  } else if(type == "polygon") {
    
    poldt <- suppressMessages(suppressWarnings(broom::tidy(x$polygon)))
    names(poldt)[names(poldt) == "long"] <- "lon"
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = poldt)
    
  } else if(type == "hexagon") {
    
    hexdt <- data.frame(hexbin::hcell2xy(x$hexagon), count_bin1 = x$hexagon@count, cell_bin1 = x$hexagon@cell)
    names(hexdt)[names(hexdt) == "x"] <- "lon"
    names(hexdt)[names(hexdt) == "y"] <- "lat"
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = hexdt)
    
  } else if(type == "lim.fact") {
    
    rasdt <- data.frame(raster::coordinates(x$lim.fact), level = as.factor(x$lim.fact@data@values))
    rasdt <- rasdt[!is.na(rasdt$level),]
    
    tmp <- x$lim.fact@data@attributes[[1]]
    levels(rasdt$level)[tmp$ID] <- as.character(tmp$level)
    
    names(rasdt)[names(rasdt) == "x"] <- "lon"
    names(rasdt)[names(rasdt) == "y"] <- "lat"
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = rasdt)
    
  }
  
  ## Basemap 
  
  bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = 8)
  
  ## Maps
  
  if(type == "raw") {
    
    mp <- bm + 
      geom_tile(data = rawdt, aes(x = lon, y = lat), color = "#FF5F68", fill = "#FF5F68", width = 1e4, height = 1e4, alpha = 0.1)
    
    
  } else if(type == "raw&raster") {
    
    mp <- bm + 
      geom_tile(data = rawdt, aes(x = lon, y = lat), color = "#FF5F68", fill = "#FF5F68", width = 1.5e4, height = 1.5e4) +
      geom_tile(data = rasdt, aes(x = lon, y = lat), fill = "#82C893", alpha = 0.7)
    
  } else if(type == "default") {
    
    mp <- bm + 
      geom_tile(data = rawdt, aes(x = lon, y = lat), color = "#FF5F68", fill = "#FF5F68", width = 1.5e4, height = 1.5e4) +
      geom_tile(data = rasdt, aes(x = lon, y = lat), fill = "#82C893") +
      geom_polygon(data = poldt, aes(x = lon, y = lat, group = group), fill = NA, color = "#449BCF", size = PlotSvalbard::LS(1))
    
  } else if(type == "raster") {
    
    mp <- bm + 
      geom_tile(data = rasdt, aes(x = lon, y = lat), fill = "#82C893", alpha = 0.7)
    
  } else if(type == "polygon") {
    
    mp <- bm + 
      ggspatial::geom_spatial_polygon(data = poldt, aes(x = lon, y = lat, group = group), fill = "#449BCF", color = "#449BCF", size = PlotSvalbard::LS(1), alpha = 0.3, crs = 4326)
    
  } else if(type == "hexagon") {
    
    mp <- bm + 
      geom_hex(data = hexdt, aes(x = lon, y = lat), fill = "#D696C8", stat = "identity", size = PlotSvalbard::LS(0.5)) 
    
  } else if(type == "lim.fact") {
    
    mp <- bm + geom_tile(data = rasdt, aes(x = lon, y = lat, fill = level)) + 
      scale_fill_manual("Limiting factor", 
                        breaks = c("all", "depth", "depth&sal", "sal", "temp", "temp&depth"), 
                        labels = c("All", "Depth", "Depth &\nSalinity", "Salinity", "Temperature", "Temperture &\nDepth"),
                        values = c("#D696C8", "#449BCF", "#82C893", "#056A89", "#FF5F68", "#FF9252", "#FFC95B"))
    
  }
  
  
  ## Add common specs
  
  mp + PlotSvalbard::add_land()
  
}
