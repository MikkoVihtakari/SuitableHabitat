#' @title Plot a suitable habitat model on a map
#' @description Plot method for \code{\link[=model.fit]{SHfit}} objects.
#' @param x \code{SHfit} object from the \code{\link{model.fit}} function.
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" (default) limits the map using coordinate range of \code{mod}. Alternatively use a numeric vector as described in \code{\link[PlotSvalbard]{basemap}} documentation.
#' @param type Character argument defining the types of model data to be plotted: 
#' \itemize{
#' \item \code{"polygon"} Model fit based on polygons and spatial points,
#' \item \code{"hexagon"} Hexagon griding based model fit.
#' \item \code{"raster"} Model fit based on raster grid.
#' }
#' @param ... Additional arguments to \code{\link[PlotSvalbard]{basemap}}
#' @method plot SHfit
#' @seealso \code{\link{suitable.habitat}} \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ggplot2 ggspatial 
#' @importFrom PlotSvalbard basemap LS
#' @importFrom broom tidy
#' @export

# type = "default"
plot.SHfit <- function(x, limits = "auto", type = "hexagon", base_size = 8, ...) {
  
  # Tests
  
  if(!type %in% c("raster", "polygon", "hexagon")) stop("Invalid type argument. Use one of following: 'raster', 'polygon', or 'hexagon'")
  
  # Data manipulation
  
  if(type == "raster") {
    
    stop("raster has not been implemented yet")
    
  } else if(type == "polygon") {
    
    pointdt <- as.data.frame(coordinates(x$polygon$points))
    names(pointdt)[1] <- "lon"
    names(pointdt)[2] <- "lat"
    
    pointdt <- cbind(pointdt, x$polygon$points@data)
    
    poldt <- suppressMessages(suppressWarnings(broom::tidy(x$polygon$model.polygon)))
    names(poldt)[names(poldt) == "long"] <- "lon"
    
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = pointdt)
    
  } else if(type == "hexagon") {
    
    tmp <- lapply(x$hexagon$all, function(j) {
      matrix(PlotSvalbard::auto_limits("panarctic", limits = c("lon", "lat"), data = j), nrow = 1)
    })
    
    tmp <- data.frame(do.call(rbind, tmp))
    
    limits <- c(min(tmp[1]), max(tmp[2]), min(tmp[3]), max(tmp[4]))
    
  } 
  
  ## Basemap 
  
  bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = base_size, ...)
  # debug alternative: bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = 8)
  
  ## Maps
  
  if(type == "raster") {
    
  } else if(type == "polygon") {
    
    mp <- bm + 
      ggspatial::geom_spatial_polygon(data = poldt, aes(x = lon, y = lat, group = group), fill = "#449BCF", color = "#449BCF", size = PlotSvalbard::LS(1), alpha = 0.3, crs = 4326) +
      geom_point(data = pointdt[pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#82C893") +
      geom_point(data = pointdt[!pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#FF5F68") 
    
  } else if(type == "hexagon") {
    
    mp <- bm +
      geom_hex(data = x$hexagon$all$unique_mod, 
               aes(x = lon, y = lat), fill = "#449BCF", stat = "identity", size = LS(0.5)) +
      geom_hex(data = x$hexagon$all$unique_obs, 
               aes(x = lon, y = lat), fill = "#FF5F68", stat = "identity", size = LS(0.5)) +
      geom_hex(data = x$hexagon$all$overlapping, 
               aes(x = lon, y = lat), fill = "#82C893", stat = "identity", size = LS(0.5))
    
  } 
  
  
  ## Add common specs
  
  mp + PlotSvalbard::add_land()
  
}
