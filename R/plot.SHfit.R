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
#' @importFrom cowplot plot_grid
#' @export

# type = "default"
plot.SHfit <- function(x, limits = "auto", type = x$parameters$fit.method, region.plot = x$parameters$regions, capitalize.region.names = TRUE, axis.labels = TRUE, base_size = 8, ...) {
  
  # Tests
  
  if(!type %in% c("raster", "polygon", "hexagon")) stop("Invalid type argument. Use one of following: 'raster', 'polygon', or 'hexagon'")
  
  # Data manipulation for non-region plots
  
  if(type == "raster") {
    
    stop("raster has not been implemented yet")
    
  } else if(type == "polygon") {
    
    ## Data manipulation
    
    if(!region.plot) {
      
      pointdt <- as.data.frame(coordinates(x$polygon$points))
      names(pointdt)[1] <- "lon"
      names(pointdt)[2] <- "lat"
      
      pointdt <- cbind(pointdt, x$polygon$points@data)
      
      poldt <- suppressMessages(suppressWarnings(broom::tidy(x$polygon$model.polygon)))
      names(poldt)[names(poldt) == "long"] <- "lon"
      
      limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("lon", "lat"), data = pointdt) 
    }
    
  } else if(type == "hexagon") {
    
    if(!region.plot) {
      
      tmp <- lapply(x$hexagon$all, function(j) {
        matrix(PlotSvalbard::auto_limits("panarctic", limits = c("lon", "lat"), data = j), nrow = 1)
      })
      
      tmp <- data.frame(do.call(rbind, tmp))
      
      limits <- c(min(tmp[1]), max(tmp[2]), min(tmp[3]), max(tmp[4]))
    }
  } 
  
  ## Basemap 
  
  if(!region.plot) {
    bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = base_size, ...)
    # debug alternative: bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = 8)
  }
  
  ## Maps
  
  if(type == "raster") {
    
  } else if(type == "polygon") {
    
    if(!region.plot) {
      
      bm + 
        ggspatial::geom_spatial_polygon(data = poldt, aes(x = lon, y = lat, group = group), fill = "#449BCF", color = "#449BCF", size = PlotSvalbard::LS(1), alpha = 0.3, crs = 4326) +
        geom_point(data = pointdt[pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#82C893") +
        geom_point(data = pointdt[!pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#FF5F68") +
        add_land()
      
    } else {
      stop("not implemented yet")
    }
    
  } else if(type == "hexagon") {
    
    if(!region.plot) {
      
      bm +
        geom_hex(data = x$hexagon$all$unique_mod, 
                 aes(x = lon, y = lat), fill = "#449BCF", stat = "identity", size = LS(0.5)) +
        geom_hex(data = x$hexagon$all$unique_obs, 
                 aes(x = lon, y = lat), fill = "#FF5F68", stat = "identity", size = LS(0.5)) +
        geom_hex(data = x$hexagon$all$overlapping, 
                 aes(x = lon, y = lat), fill = "#82C893", stat = "identity", size = LS(0.5)) + 
        add_land()
      
    } else {
      
      reg.plots <- lapply(x$hexagon$regions, function(k) {
        
        reg <- as.character(unique(k[[1]]$region))
        
        ## Region borders
        
        reg.pol <- x$parameters$region.polygons[x$parameters$region.polygons@data$region == reg,]
        reg.pol.dt <- suppressMessages(suppressWarnings(broom::tidy(reg.pol)))
        names(reg.pol.dt)[names(reg.pol.dt) == "long"] <- "lon"
        
        limits <- PlotSvalbard::auto_limits("panarctic", limits = c("lon", "lat"), data = reg.pol.dt)
        
        if(capitalize.region.names) reg <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", reg, perl=TRUE)
        
        ## Plot
        
        basemap("panarctic", limits = limits, base_size = base_size) +
          geom_hex(data = k[["unique_mod"]], aes(x = lon, y = lat), fill = "#449BCF", stat = "identity", size = LS(0.5)) +
          geom_hex(data = k[["unique_obs"]], aes(x = lon, y = lat), fill = "#FF5F68", stat = "identity", size = LS(0.5)) +
          geom_hex(data = k[["overlapping"]], aes(x = lon, y = lat), fill = "#82C893", stat = "identity", size = LS(0.5)) +
          geom_polygon(data = reg.pol.dt, aes(x = lon, y = lat, group = group), fill = NA, color = "black") +
          ggtitle(reg) +
          add_land() + {
            if(!axis.labels) theme(axis.title = element_blank(), axis.text = element_blank())
          }
        
      })
      
      cowplot::plot_grid(plotlist = reg.plots)
    }
    
  } 
  
}
