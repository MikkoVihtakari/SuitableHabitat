#' @title Examine fit of observations to a suitable habitat model
#' @description Compares a suitable habitat model to observations specified as spatial points.
#' @param mod A \link[=suitable.habitat]{suitable habitat model object}.
#' @param pts \link[sp]{SpatialPoints} object containing observations of the species. If \code{proj4string} is missing, it is assumed to be equal to \code{mod}.
#' @param method hexagon, raster, polygon
#' @param regions A \link[sp]{SpatialPolygonsDataFrame} defining the region boundaries. The region identifier in \code{\@data} \strong{must be} named "region".
#' @details ...
#' @return Returns a list containing suitable habitat estimates in following formats: 1) \strong{\code{$raw}:} oceanographic model (\code{oceangr.model}) grid cells, 2) \strong{\code{$raster}:} rasterized cells whose resolution is defined by the \code{res} with disconnected regions smaller than the \code{drop.crumbs} parameter removed (in km2), 3) \strong{\code{$polygon}:} polgyonized raster data, and 4) \strong{\code{$hexagon}:} hexagonized raster data. See \code{\link[hexbin]{hexbin}}.
#' @import sp raster rgeos rgdal hexbin
#' @importFrom stats na.omit sd
#' @export


# mod = gen_mod; pts = SpatialPoints(pres_dt[,c("lon.utm", "lat.utm")], proj4string = CRS(map_projection("panarctic"))); method = "hexagon"; regions = rgdal::readOGR("../../GIS/Pan-Arctic map/ghl_major_regions.shp")
model.fit <- function(mod, pts, regions = NULL, method = "hexagon") {
  
  ## Checks ###
  
  ### Check that crs is equal for all spatial objects 
  
  mod.proj <- mod$parameters$model.proj
  
  if(sp::proj4string(pts) != mod.proj) {
    if(is.na(sp::proj4string(pts))) {
      sp::proj4string(pts) <- mod.proj
      message("missing pts projection. Assumed the same than mod")
    } else {
      pts <- sp::spTransform(pts, CRSobj = CRS(mod.proj))
      message("pts projection transformed to match that of mod")
    }
  }
  
  if(!is.null(regions)) {
    if(!class(regions) %in% c("SpatialPolygons", "SpatialPolygonsDataFrame")) stop("regions have to be defined as SpatialPolygons or SpatialPolygonsDataFrame")
    
    if(sp::proj4string(regions) != mod.proj) {
      if(is.na(sp::proj4string(regions))) {
        sp::proj4string(regions) <- mod.proj
        message("missing regions projection. Assumed the same than mod")
      } else {
        pts <- sp::spTransform(regions, CRSobj = CRS(mod.proj))
        message("regions projection transformed to match that of mod")
      }
    }
  }
  
  ## Remove observation points outside the model extent
  
  tmp <- sp::over(pts, mod$parameters$model.extent) 
  
  if(sum(is.na(tmp)) > 0) message("Note that there were observations outside the model extent. They are removed from the model fit examination.")
  
  pts <- pts[!is.na(tmp)]
  
  message("1/4. Setup done...")
  
  ##########################
  ## The hexagon method ####
  
  if(method %in% c("hexagon", "all")) {
    
    x <- as.data.frame(coordinates(pts))
    names(x)[1] <- "lon"
    names(x)[2] <- "lat"
    
    x <- x[x$lon <= max(mod$hexagon@xbnds) & x$lon >= min(mod$hexagon@xbnds) & x$lat <= max(mod$hexagon@ybnds) & x$lat >= min(mod$hexagon@ybnds),]
    
    obsh <- hexbin::hexbin(x$lon, x$lat, xbins = mod$hexagon@xbins, xbnds = mod$hexagon@xbnds, ybnds = mod$hexagon@ybnds)
    
    df <- hdiffcoords(mod$hexagon, obsh)
    names(df) <- c("unique_mod", "unique_obs", "overlapping")
    
    ## Fit table
    
    tab <- data.frame(region = "all", nhex_noobs = nrow(df$unique_mod), nhex_out = nrow(df$unique_obs), nhex_in = nrow(df$overlapping), hex_n_out = sum(df$unique_obs$count_bin2), hex_n_in = sum(df$overlapping$count_bin2))
    
    tab$nhex <- tab$nhex_noobs + tab$nhex_out + tab$nhex_in
    tab$pr_hex_out <- 100*tab$nhex_out/(tab$nhex_in + tab$nhex_noobs + tab$nhex_out)
    tab$pr_hex_in <- 100*tab$nhex_in/(tab$nhex_in + tab$nhex_noobs)
    tab$pr_n_in <- 100*tab$hex_n_in/(tab$hex_n_in + tab$hex_n_out)
    
    fit_tab <- tab
    
    ## Regions ####
    
    if(!is.null(regions)) {
      
      df <- lapply(df, function(k) {
        
        sps <- sp::SpatialPoints(k[c("lon", "lat")], proj4string = sp::CRS(mod.proj))
        
        reg <- sp::over(sps, regions)
        reg_ord <- levels(reg$region)
        reg$region <- as.character(reg$region)
        reg$region[is.na(reg$region)] <- "other"
        reg$region <- factor(reg$region, c(reg_ord, "other"))
        
        k$region <- reg$region
        
        k
      })
      
      region.list <- unique(as.character(unname(unlist(lapply(df, function(k) unique(k$region))))))
      region.list <- region.list[!region.list %in% "other"]
      
      regdf <- lapply(region.list, function(j) {
        
        lapply(df, function(k) {
          k[k$region == j,]
        })
        
      })
      
      names(regdf) <- region.list
      
      ## Fit table
      
      # k <- regdf[[1]]
      reg_tabs <- lapply(regdf, function(k) {
        
        tab <- data.frame(region = unique(k[[1]]$region), nhex_noobs = nrow(k$unique_mod), nhex_out = nrow(k$unique_obs), nhex_in = nrow(k$overlapping), hex_n_out = sum(k$unique_obs$count_bin2), hex_n_in = sum(k$overlapping$count_bin2))
        
        tab$nhex <- tab$nhex_noobs + tab$nhex_out + tab$nhex_in
        tab$pr_hex_out <- 100*tab$nhex_out/(tab$nhex_in + tab$nhex_noobs + tab$nhex_out)
        tab$pr_hex_in <- 100*tab$nhex_in/(tab$nhex_in + tab$nhex_noobs)
        tab$pr_n_in <- 100*tab$hex_n_in/(tab$hex_n_in + tab$hex_n_out)
        
        tab
      })
      
      fit_tab <- rbind(fit_tab, do.call(rbind, reg_tabs))
      rownames(fit_tab) <- 1:nrow(fit_tab)
    } else {
      
      regdf <- NULL
      
    }
    
    hex.dat <- list(summary = fit_tab, all = df, regions = regdf)
    
  } else {
    hex.dat <- NULL
  }
  
  message("2/4. Hexagon done...")
  
  ##########################
  ## The polygon method ####
  
  if(method %in% c("polygon", "all")) {
    
    pts <- SpatialPointsDataFrame(pts, data = data.frame(fit = sp::over(pts, mod$polygon)[[1]]))
    pts@data$fit <- !is.na(pts@data$fit)
    
    message("3/4. Spatial points done...")
    
    ## Fit table
    
    fit_tab <- data.frame(region = "all", N = nrow(pts@data), n_in = sum(pts@data$fit), n_out = sum(!pts@data$fit))
    fit_tab$pr_in <- 100*fit_tab$n_in/fit_tab$N
    fit_tab$pr_out <- 100*fit_tab$n_out/fit_tab$N
    
    if(!is.null(regions)) {
      
      pts@data$region <- sp::over(pts, regions)[[2]]
      
      ## Fit table
      
      reg_tabs <- lapply(levels(pts@data$region), function(j) {
        
        tp <- pts[pts@data$region %in% j,]
        
        tab <- data.frame(region = j, N = nrow(tp@data), n_in = sum(tp@data$fit), n_out = sum(!tp@data$fit))
        tab$pr_in <- 100*tab$n_in/tab$N
        tab$pr_out <- 100*tab$n_out/tab$N
        
        tab
      })
      
      fit_tab <- rbind(fit_tab, do.call(rbind, reg_tabs))
      rownames(fit_tab) <- 1:nrow(fit_tab)
    } 
    
    # no_fits <- pts[pts@data$fit,]
    # fits <- pts[pts@data$fit,]
    
    pol.dat <- list(summary = fit_tab, points = pts, model.polygon = mod$polygon)
    
    message("4/4. Polygons done...")
    
  } else {
    pol.dat <- NULL
  }
  
  #########################
  ## The raster method ####
  
  # Rasterize the observations
  # (unfinished)
  # r <- raster::raster(raster::extent(mod$raster), ncol = mod$parameters$raster.resolution, nrow = mod$parameters$raster.resolution)
  # sp::proj4string(r) <- mod.proj
  # 
  # r2 <- raster::rasterize(pts, mod$raster, pts$fit, fun = mean)
  # sp::proj4string(r2) <- mod.proj
  # 
  # r1 <- mod$raster
  # 
  # rr <- raster::resample(r1, r2)
  # 
  # ex = extent(r1)
  # r2 = crop(r2, ex)
  # 
  # table(y@data@values)
  # 
  # tmp <- data.frame(raster.fit = !is.na(extract(mod$raster, pts)))
  # tmp <- cbind(tmp, pts@data)
  # 
  # tab <- data.frame(region = "all", N = nrow(tmp), n_in = sum(tmp$raster.fit), n_out = sum(!tmp$raster.fit))
  # tab$pr_in <- 100*tab$n_in/tab$N
  # tab$pr_out <- 100*tab$n_out/tab$N
  # 
  # length(tmp)
  # sum(is.na(tmp))
  
  ##############
  ## Return ####
  
  out <- list(polygon = pol.dat, hexagon = hex.dat, 
              parameters = c(fit.method = method, regions = !is.null(regions), region.polygons = regions, mod$parameters)
  )
  
  class(out) <- "SHfit"
  
  out
  
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
