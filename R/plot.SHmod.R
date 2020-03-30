#' @title Plot a suitable habitat model on a map
#' @description Plot method for \code{\link[=suitable.habitat]{SHmod}} objects.
#' @param x \code{SHmod} object from \code{\link{suitable.habitat}} function.
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" (default) limits the map using coordinate range of \code{mod}. Alternatively use a numeric vector as described in \code{\link[PlotSvalbard]{basemap}} documentation.
#' @param type Character argument defining the types of model data to be plotted: 
#' \itemize{
#' \item \code{"raw"} Only raw data. I.e. the grid cell data directly from the oceanographic model.
#' \item \code{"raster"} Only raster data. See \code{\link{suitable.habitat}} and \code{\link{rasterize.suitable.habitat}}
#' \item \code{"polygon"} Only polygon data. See \code{\link{suitable.habitat}} and \code{\link{polygonize.suitable.habitat}}
#' \item \code{"hexagon"} Only hexagon data. See \code{\link{suitable.habitat}} and \code{\link{hexagonize.suitable.habitat}}
#' \item \code{"raw&raster"} Options \code{"raw"} and \code{"raster"} on top of each other.
#' \item \code{"lim.fact"} Limiting factors as raster data.
#' \item \code{"default"} Options \code{"raw"}, \code{"raster"}, and \code{"polygon"} on top of each other.
#' }
#' @param ... Additional arguments. Required by R build checks. Ignore.
#' @method plot SHmod
#' @seealso \code{\link{suitable.habitat}} \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ggplot2 ggspatial 
#' @importFrom PlotSvalbard basemap LS
#' @importFrom broom tidy
#' @export

# type = "default"
plot.SHmod <- function(x, limits = "auto", type = "default", legend.position = "right", ...) {
  
  # Tests
  
  if(!type %in% c("raw", "raster", "polygon", "hexagon", "raw&raster", "lim.fact", "default")) stop("Invalid type argument. Use one of following: 'raw', 'raster', 'polygon', 'hexagon', 'raw&raster', 'lim.fact' or 'default'")
  
  if(type == "lim.fact" & !x$parameters$lim.factors) stop("No limiting factor data in x. Run the model suitable.habitat function using find.lim.factors = TRUE.")
  
  # Data manipulation
  
  if(type == "raw") {
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = x$raw)
    
    rawdt <- x$raw[!is.na(x$raw$habitat),]
    
  } else if(type == "raw&raster") {
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = x$raw)
    
    rawdt <- x$raw[!is.na(x$raw$habitat),]
    
    rasdt <- data.frame(raster::coordinates(x$raster), area = x$raster@data@values)
    rasdt <- rasdt[!is.na(rasdt$area),]
    names(rasdt)[names(rasdt) == "x"] <- "lon"
    names(rasdt)[names(rasdt) == "y"] <- "lat"
    
  } else if(type == "default") {
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = x$raw)
    
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
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = rasdt)
    
  } else if(type == "polygon") {
    
    poldt <- suppressMessages(suppressWarnings(broom::tidy(x$polygon)))
    names(poldt)[names(poldt) == "long"] <- "lon"
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = poldt)
    
  } else if(type == "hexagon") {
    
    hexdt <- data.frame(hexbin::hcell2xy(x$hexagon), count_bin1 = x$hexagon@count, cell_bin1 = x$hexagon@cell)
    names(hexdt)[names(hexdt) == "x"] <- "lon"
    names(hexdt)[names(hexdt) == "y"] <- "lat"
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = hexdt)
    
  } else if(type == "lim.fact") {
    
    rasdt <- data.frame(raster::coordinates(x$lim.fact), level = as.factor(x$lim.fact@data@values))
    rasdt <- rasdt[!is.na(rasdt$level),]
    
    tmp <- x$lim.fact@data@attributes[[1]]
    levels(rasdt$level) <- as.character(tmp$VALUE)
    
    names(rasdt)[names(rasdt) == "x"] <- "lon"
    names(rasdt)[names(rasdt) == "y"] <- "lat"
    
    if(limits == "auto") limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = rasdt)
    
  }
  
  ## Basemap 
  
  bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey80", base_size = 8)
  
  ## Maps
  
  if(type == "raw") {
    
    mp <- bm + 
      geom_tile(data = rawdt, aes(x = lon, y = lat), color = "#FF5F68", fill = "#FF5F68", width = 1e4, height = 1e4, alpha = 0.1)
    
    # bm + geom_tile(data = rawdt, aes(x = lon, y = lat, color = lim.factor, fill = lim.factor), width = 1e4, height = 1e4) + add_land()
    
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
                        breaks = c("suitable", "crumbs", "depth", "depth&sal", "temp", "temp&depth", "temp&&depth", "temp&depth&sal", "temp&&depth&sal", "sal"), 
                        labels = c("None", "Habitat\nfragment", "Depth", "Depth &\nSalinity", "Temperature", "Temperature &\nDepth\n(simultanous)", "Temperature &\nDepth\n(orthogonal)", "Salinity &\nTemperature &\nDepth\n(simultanous)", "Salinity &\nTemperature &\nDepth\n(orthogonal)", "Salinity"),
                        values = c("#FFCC72", "#FF9252", "#449BCF", "#056A89", "#FF5F68", "#D696C8", "#B27DA6", "#accf44", "#8FAC38", "#44cf67")
      ) + 
      theme(legend.position = legend.position, 
            legend.key.height = unit(1.2, "cm")
      )
    
  }
  
  
  ## Add common specs
  
  mp + PlotSvalbard::add_land()
  
}
