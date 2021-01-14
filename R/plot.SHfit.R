#' @title Plot a suitable habitat model on a map
#' @description Plot method for \code{\link[=model.fit]{SHfit}} objects.
#' @param x \code{SHfit} object from the \code{\link{model.fit}} function.
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" (default) limits the map using coordinate range of \code{mod}. Alternatively use a numeric vector as described in \code{\link[ggOceanMaps]{basemap}} documentation.
#' @param type Character argument defining the types of model data to be plotted: 
#' \itemize{
#' \item \code{"polygon"} Model fit based on polygons and spatial points,
#' \item \code{"hexagon"} Hexagon griding based model fit.
#' \item \code{"raster"} Model fit based on raster grid.
#' }
#' @param title "capitalize", NA, anything else
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @param land.col Character code specifying the color of land.
#' @param region.plot Logical indicating whether region fit should be plotted
#' @param axis.labels Logical indicating whether axis labels should be included to the maps.
#' @param ... Additional arguments passed to \code{\link[ggOceanMaps]{basemap}}.
#' @method plot SHfit
#' @seealso \code{\link{suitable.habitat}} \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ggplot2 ggspatial 
#' @importFrom ggOceanMaps basemap LS
#' @importFrom broom tidy
#' @importFrom cowplot plot_grid
#' @export

# x = juv_mod_fit
# limits = "auto"; type = x$parameters$fit.method; region.plot = x$parameters$regions; axis.labels = TRUE; title = "capitalize"; base_size = 8; land.col = "grey80"
plot.SHfit <- function(x, limits = "auto", type = x$parameters$fit.method, region.plot = x$parameters$regions, axis.labels = TRUE, title = "capitalize", base_size = 8, land.col = "grey80", ...) {
  
  # Tests & definitions ####
  
  if(!type %in% c("raster", "polygon", "hexagon")) stop("Invalid type argument. Use one of following: 'raster', 'polygon', or 'hexagon'")
  
  limits.lon <- 1e5
  limits.lat <- 1e5
  
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
      
      limits <- suppressWarnings(ggOceanMaps::auto_limits(pointdt, proj.in = "+init=epsg:3995", proj.out = "+init=epsg:3995", verbose = FALSE))$projLimits
    }
    
  } else if(type == "hexagon") {
    
    if(!region.plot) {
      
      tmp <- lapply(x$hexagon$all, function(j) {
        matrix(suppressWarnings(ggOceanMaps::auto_limits(j, proj.in = "+init=epsg:3995", proj.out = "+init=epsg:3995", verbose = FALSE))$projLimits, nrow = 1)
        # c(ggOceanMaps::round_any(min(j$lon), limits.lon, floor), ggOceanMaps::round_any(max(j$lon), limits.lon, ceiling), ggOceanMaps::round_any(min(j$lat), limits.lat, floor), ggOceanMaps::round_any(max(j$lat), limits.lat, ceiling))
      })
      
      tmp <- data.frame(do.call(rbind, tmp))
      
      limits <- c(min(tmp[1]), max(tmp[2]), min(tmp[3]), max(tmp[4]))
      
    } else {
      
      reg.dat <- lapply(x$hexagon$regions, function(k) {
        # print(as.character(unique(k$unique_mod$region)))
        reg <- as.character(unique(k[[1]]$region))
        
        ## Region borders
        
        reg.pol <- x$parameters$region.polygons[x$parameters$region.polygons@data$region == reg,]
        reg.pol.dt <- suppressMessages(suppressWarnings(broom::tidy(reg.pol)))
        names(reg.pol.dt)[names(reg.pol.dt) == "long"] <- "lon"
        
        limits <- suppressWarnings(ggOceanMaps::auto_limits(reg.pol.dt, proj.in = "+init=epsg:3995", proj.out = "+init=epsg:3995", verbose = FALSE))$projLimits
        
        if(!is.na(title)) {
          if(title == "capitalize") reg <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", reg, perl=TRUE)
        }
        
        list(limits = limits, reg.pol.dt = reg.pol.dt, reg = reg)
        
      })
      
      ## Aspect ratio
      
      asp.ratio <- lapply(reg.dat, function(k) {
        lon.diff <- diff(k$limits[1:2])
        lat.diff <- diff(k$limits[3:4])
        
        lon.diff/lat.diff
      })
      
      smallest.asp <- names(which.min(abs(unlist(asp.ratio) - 1)))
      
      ## Increase the y limits to match the closest to 1 aspect ratio (makes plots equal size and more square)
      
      reg.dat[names(reg.dat)[!names(reg.dat) %in% smallest.asp]] <- lapply(reg.dat[names(reg.dat)[!names(reg.dat) %in% smallest.asp]], function(k) {
        
        target.lat <- diff(k$limits[1:2])/asp.ratio[[smallest.asp]]
        current.lat <- diff(k$limits[3:4])
        lat.increase <- (target.lat - current.lat) / 2
        
        k$limits[3] <- k$limits[3] - lat.increase
        k$limits[4] <- k$limits[4] + lat.increase
        
        list(limits = k$limits, reg.pol.dt = k$reg.pol.dt, reg = k$reg)
      })
      
      
    }
  } 
  
  ## Basemap ####
  
  if(!region.plot) {
    bm <- ggOceanMaps::basemap(limits = limits, shapefiles = "Arctic", base_size = base_size, land.col = land.col, verbose = FALSE, ...)
    # debug alternative: bm <- ggOceanMaps::basemap(limits = limits, shapefiles = "Arctic", base_size = base_size, land.col = land.col, verbose = FALSE)
  }
  
  ## Maps
  
  if(type == "raster") {
    
  } else if(type == "polygon") {
    
    if(!region.plot) {
      
      bm + 
        ggspatial::geom_spatial_polygon(data = poldt, aes(x = lon, y = lat, group = group), fill = "#449BCF", color = "#449BCF", size = ggOceanMaps::LS(1), alpha = 0.3, crs = 4326) +
        geom_point(data = pointdt[pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#82C893") +
        geom_point(data = pointdt[!pointdt$fit,], aes(x = lon, y = lat), pch = 21, color = "#FF5F68") +{
          if(!axis.labels) theme(axis.title = element_blank(), 
                                 axis.text = element_blank(), 
                                 axis.ticks = element_blank(),
                                 axis.ticks.x = element_blank(), 
                                 axis.ticks.y = element_blank()
          )
        }
      
    } else {
      stop("not implemented yet")
    }
    
  } else if(type == "hexagon") {
    
    if(!region.plot) {
      
      p <- bm +
        geom_hex(data = x$hexagon$all$unique_mod, 
                 aes(x = lon, y = lat), fill = "#449BCF", stat = "identity", size = ggOceanMaps::LS(0.5)) +
        geom_hex(data = x$hexagon$all$unique_obs, 
                 aes(x = lon, y = lat, alpha = count_bin2), fill = "#FF5F68", stat = "identity", size = ggOceanMaps::LS(0.5)) +
        geom_hex(data = x$hexagon$all$overlapping, 
                 aes(x = lon, y = lat), fill = "#82C893", stat = "identity", size = ggOceanMaps::LS(0.5)) + 
        scale_alpha("N", range = c(0.2, 1), trans = "log") + {
          if(!axis.labels) theme(axis.title = element_blank(), 
                                 axis.text = element_blank(), 
                                 axis.ticks = element_blank(),
                                 axis.ticks.x = element_blank(), 
                                 axis.ticks.y = element_blank()
          )
        } +
        theme(legend.position = "none")
      
      ggOceanMaps::reorder_layers(p)
      
    } else {
    
      reg.plots <- lapply(seq_along(reg.dat), function(i) {
        
        ## Plot
        
        p <- ggOceanMaps::basemap(limits = reg.dat[[i]]$limits, shapefiles = "Arctic", base_size = base_size, land.col = land.col, verbose = FALSE, ...) +
          geom_hex(data = x$hexagon$regions[[i]][["unique_mod"]], 
                   aes(x = lon, y = lat), fill = "#449BCF", stat = "identity", size = ggOceanMaps::LS(0.5)) +
          geom_hex(data = x$hexagon$regions[[i]][["unique_obs"]], 
                   aes(x = lon, y = lat, alpha = count_bin2), fill = "#FF5F68", stat = "identity", size = ggOceanMaps::LS(0.5)) +
          geom_hex(data = x$hexagon$regions[[i]][["overlapping"]], 
                   aes(x = lon, y = lat), fill = "#82C893", stat = "identity", size = ggOceanMaps::LS(0.5)) +
          geom_polygon(data = reg.dat[[i]]$reg.pol.dt, 
                       aes(x = lon, y = lat, group = group), 
                       fill = NA, color = "black", size = ggOceanMaps::LS(0.5), linetype = "longdash") +
          scale_alpha("N", range = c(0.2, 1), trans = "log") + {
            if(!is.na(title)) ggtitle(reg.dat[[i]]$reg) } + {
              if(!axis.labels) theme(axis.title = element_blank(), 
                                     axis.text = element_blank(), 
                                     axis.ticks = element_blank(),
                                     axis.ticks.x = element_blank(), 
                                     axis.ticks.y = element_blank()
              )
            } +
          theme(legend.position = "none")
        
      })
      
      cowplot::plot_grid(plotlist = reg.plots)
    }
    
  } 
  
}


### Scrap code:

# # k <- df[[1]]
# 
# 
# ## Alpha cut
# # 
# # tmp <- sapply(df, function(k) {
# #   if(any(names(k) %in% "count_bin2")) {
# #     range(k$count_bin2)
# #   }
# # })
# # 
# # tmp <- range(unname(unlist(tmp)))
# # alpha.cut <- pretty(tmp)
# # 
# # df <- lapply(df, function(k) {
# #   
# #   if(any(names(k) %in% "count_bin2")) {
# #     k$alpha <- as.numeric(cut(k$count_bin2, alpha.cut))
# #     k
# #   } else {k}
# #   
# # })
# # 
