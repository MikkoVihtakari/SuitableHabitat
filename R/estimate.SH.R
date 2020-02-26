#' @title Estimate a suitable habitat for a species from NEMO output
#' @description Estimates suitable habitat for a species from European Modeling of the Ocean (NEMO) physical oceanography model output based on limitations set by temperature, salinity and bottom depth.
#' @param lon Two dimensional longitude \strong{matrix} from a NEMO model. Must contain similar \code{\link[base]{dim}}ensions than \code{lat}
#' @param lat Two dimensional latitude \strong{matrix} from a NEMO model. Must contain similar \code{\link[base]{dim}}ensions than \code{lon}
#' @param depth Bottom depth matrix from a NEMO model. Must contain similar \code{\link[base]{dim}}ensions than \code{lon} and \code{lat}.
#' @param temp Temperature matrix from a NEMO model. Must contain similar \code{\link[base]{dim}}ensions than \code{lon} and \code{lat}.
#' @param sal Temperature matrix from a NEMO model. Must contain similar \code{\link[base]{dim}}ensions than \code{lon} and \code{lat}. Optional if \code{sal.range} is \code{c(NA, NA)}.
#' @param limit.polygon A \link[sp]{SpatialPolygons} object setting the temperature (x-axis) and depth (y-axis) limits for non-rectangular temperature-depth preferences.
#' @param depth.range Numeric vector of two values setting the minimum and maximum depth to limit the suitable habitat. Use \code{NA} ignore a limit. Ignored if \code{limit.polygon} is specified.
#' @param temp.range Numeric vector of two values setting the minimum and maximum temperature to limit the suitable habitat. Use \code{NA} ignore a limit. Ignored if \code{limit.polygon} is specified.
#' @param sal.range Numeric vector of two values setting the minimum and maximum salinity to limit the suitable habitat. Use \code{NA} ignore a limit. 
#' @param polygonize Logical indicating whether the NEMO grid should be smoothed to polygons. Requires functioning gdal_polygonize.py installed on computer. See details.
#' @param log.transform.depth Logical indicating whether y-axis of the TD space (depth) should be log10 transformed. Use this option if the limit.polygon is calculated in log-space. Otherwise, set to FALSE. Ignored if \code{limit.polygon = NULL}.  
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param lat.lim Single numeric value defining a latitude value which is used to cut the model data.
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @param find.lim.factos Logical indicating whether the function should which factors (if any) make the NEMO grid cell non-suitable habitat. 
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @details The \code{\link[base]{dim}}ensions of NEMO input matrices must be equal.
#' 
#' If \code{polygonize} is set to \code{TRUE}, you need a functioning \code{gdal_polygonize.py} installed on your computer. The easiest way to install the script is to install QGIS 2.18 (earlier or later versions won't do).
#' @import sp raster rgeos smoothr rgdal

# Test parameters
# lon = lon; lat = lat; depth = dpt; temp = d00s$temp$med; sal = d00s$sal$med; buffer.width = 15000; proj4 = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"; lat.lim = 40; res = 400; limit.polygon = NULL; log.transform.depth = TRUE; find.lim.factors = FALSE; drop.crumbs = 1000
# temp.range = c(lims$temp.min, lims$temp.max); depth.range = c(lims$depth.min, lims$depth.max); sal.range = c(lims$sal.min, lims$sal.max); polygonize = TRUE
# limit.polygon = sdm.lims$Small$chull.shp; sal.range = c(lims$sal.min, lims$sal.max); polygonize = TRUE; depth.range = NULL; temp.range = NULL
estimate.SH <- function(lon, lat, depth, temp, sal = NULL, limit.polygon = NULL, depth.range = NULL, temp.range = NULL, sal.range = c(NA, NA), polygonize = FALSE, buffer.width = 15000, proj4 = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", lat.lim = 40, res = 400, log.transform.depth = TRUE, find.lim.factors = FALSE, drop.crumbs = 1000) {
  
  ## Checks
  
  if(all(c(is.null(limit.polygon), is.null(depth.range), is.null(temp.range)))) stop("either limit.polygon or depth.range and temp.range has to be specified.")
  
  ## Make the data frame ####
  
  dt <- as.data.frame(cbind(lon = c(lon), lat = c(lat), depth = c(depth), temp = c(temp), sal = c(sal)))
  var.cols <- names(dt)[!names(dt) %in% c("lon", "lat")]
  if (!is.null(lat.lim)) {dt <- dt[dt$lat > lat.lim,]}
  dt[dt$depth < 1, "temp"] <- -999
  dt[is.na(dt$sal), "sal"] <- -999
  dt <- dt[order(dt$lon, dt$lat),]
  
  ## Suitable habitat
  
  if(is.null(limit.polygon)) {
    
    ###############################
    ## RECTANGULAR PREFERENCES ####
    
    if(length(depth.range) != 2) stop("depth.range must be length of 2. Use NA to ignore limits")
    if(length(temp.range) != 2) stop("temp.range must be length of 2. Use NA to ignore limits")
    if(!is.numeric(depth.range)) stop("depth.range must be a numeric vector")
    if(!is.numeric(temp.range)) stop("temp.range must be a numeric vector")
    
    if (is.null(sal)) {
      
      limits <- list(depth.min = depth.range[1], depth.max = depth.range[2], temp.min = temp.range[1], temp.max = temp.range[2])
      
    } else {
      
      if(length(sal.range) != 2) stop("sal.range must be length of 2. Use NA to ignore limits")
      if(!is.numeric(sal.range)) stop("sal.range must be a numeric vector")
      
      limits <- list(depth.min = depth.range[1], depth.max = depth.range[2], temp.min = temp.range[1], temp.max = temp.range[2], sal.min = sal.range[1], sal.max = sal.range[2])
    }
    
    # i <- 3
    tmp <- lapply(seq_along(limits), function(i) {
      tmp <- limits[[i]]
      
      if (is.na(tmp)) {
        var <- unlist(strsplit(names(limits)[i], "\\."))[1]
        type <- unlist(strsplit(names(limits)[i], "\\."))[2]
        rng <- range(dt[[var]], na.rm = TRUE)
        
        if (type == "min") {
          rng[1] - 1
        } else {
          rng[2] + 1
        }
        
      } else {
        tmp
      }
    })
    
    names(tmp) <- names(limits)
    limits <- tmp
    
    if(is.null(sal)) {
      dt$habitat <- ifelse(dt$depth == 0, NA, ifelse(dt$depth <= limits$depth.max & dt$depth >= limits$depth.min & dt$temp <= limits$temp.max & dt$temp >= limits$temp.min, TRUE, NA))
    } else {
      dt$habitat <- ifelse(dt$depth == 0, NA, ifelse(dt$depth <= limits$depth.max & dt$depth >= limits$depth.min & dt$temp <= limits$temp.max & dt$temp >= limits$temp.min & dt$sal <= limits$sal.max & dt$sal >= limits$sal.min, TRUE, NA))
    }
    
    rownames(dt) <- 1:nrow(dt)
    
    if(find.lim.factors) stop("find.lim.factors = TRUE has not been implemented for rectangular TD-spaces yet. Use limit.polygon to enable the feature or set find.limfactors = FALSE.")
    
  } else { 
    
    #####################
    ## LIMIT POLYGON ####
    
    if(class(limit.polygon) != "SpatialPolygons") stop("limit.polygon has to be a SpatialPolygons object.")
    
    tmp <- dt[c("temp", "depth")]
    if(log.transform.depth) tmp$depth <- log10(tmp$depth)
    
    sps <- sp::SpatialPoints(tmp)
    
    habtab <- sp::over(sps, limit.polygon)
    
    if (is.null(sal)) {
      dt$habitat <- ifelse(!is.na(habtab), TRUE, NA)
    } else {
      dt$habitat <- ifelse(!is.na(habtab) & dt$sal <= sal.range[2] & dt$sal >= sal.range[1], TRUE, NA)
    }
    
    if (find.lim.factors) {
      
      var.cols <- c(var.cols, "lim.factor")
      
      temp.lims <- unname(limit.polygon@bbox[1,])
      depth.lims <- unname(limit.polygon@bbox[2,])
      
      if(is.null(sal)) {
        
        limfacdt <- data.frame(temp = tmp$temp >= temp.lims[1] & tmp$temp <= temp.lims[2], depth = tmp$depth >= depth.lims[1] & tmp$depth <= depth.lims[2])
        dt$lim.factor <- ifelse(rowSums(limfacdt) == 2, "suitable", ifelse(limfacdt$temp, "depth", ifelse(limfacdt$depth, "temp", "both")))
        
        dt[is.na(dt$habitat) & dt$lim.factor == "suitable", "lim.factor"] <- "temp&depth"
        dt[dt$temp == -999, "lim.factor"] <- NA
        
      } else {
        
        limfacdt <- data.frame(temp = tmp$temp >= temp.lims[1] & tmp$temp <= temp.lims[2], depth = tmp$depth >= depth.lims[1] & tmp$depth <= depth.lims[2], sal = dt$sal >= sal.range[1] & dt$sal <= sal.range[2])
        
        bla <- apply(limfacdt, 1, function(k) {
          tmp <- c("temp", "depth", "sal")[!c("temp", "depth", "sal") %in% names(which(k))]
          
          if(length(tmp) == 0) {
            "suitable"
          } else if(length(tmp) == 1) {
            tmp
          } else if(length(tmp) == 3) {
            "all"
          } else {
            paste(tmp, collapse = "&")
          }
        })
        
        dt$lim.factor <- unlist(bla)
        
        dt[is.na(dt$habitat) & dt$lim.factor == "suitable", "lim.factor"] <- "temp&depth"
        dt[dt$sal == -999, "lim.factor"] <- NA
        dt[dt$temp == -999, "lim.factor"] <- NA
      }
    } 
  }
  
  
  
  
  ##########################################
  ### DATA PREPARATION FOR BOTH OPTIONS ####
  
  ### Actual model output ###
  
  sps <- sp::SpatialPointsDataFrame(coords = dt[c("lon", "lat")], data = dt[c(var.cols, "habitat")], proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sps <- sp::spTransform(sps, sp::CRS(proj4))
  spdt <- data.frame(sps)
  
  ### Polygonize the modeled distribution ###
  
  if (polygonize) {
    
    x <- na.omit(dt)
    x <- x[x$habitat,]
    
    p <- sp::SpatialPointsDataFrame(coords = x[c("lon", "lat")], data = x[c("habitat")])
    
    ext <- raster::extent(p)
    r <- raster::raster(ext, ncol = res, nrow = res)
    
    y <- raster::rasterize(p, r, p$habitat, fun=mean)
    sp::proj4string(y) <- "+proj=longlat +datum=WGS84"
    
    y <- projectRaster(y, crs = proj4)
    
    # pol <- gdal_polygonizeR(y)
    pol <- raster::rasterToPolygons(y, dissolve = TRUE)
    
    # if(is.na(proj4string(pol))) sp::proj4string(pol) <- "+proj=longlat +datum=WGS84"
    # pold <- sp::spTransform(pol, sp::CRS(proj4))
    
    #### Merge adjacent polygons ###
    
    # pols <- sf::st_as_sf(pol)
    
    #### Crash site under ####
    
    # pols2 <- clusterSF(pols, units::set_units(500, km)) # From helpers
    
    # pold <- as(pols2, 'Spatial')
    
    pol <- rgeos::gBuffer(pol, byid = TRUE, width = buffer.width)
    pol <- rgeos::gBuffer(pol, byid = TRUE, width = -buffer.width)
    
    ## Find optimal smooth parameters ####
    # plot(pol, border = "red")
    # plot(ksmooth_polys(x = pol, k = 20, N = 5L), add = T, border = "blue")
    ## ####
    
    pol2 <- ksmooth_polys(x = pol, k = 12, N = 5L) # From helpers
    
    #### Find optimal crumb drop size ####
    # plot(pol, border = "red")
    # plot(smoothr::drop_crumbs(pol, units::set_units(1000, km^2)), add = T, border = "blue")
    #### ####
    
    #distr_poly <- pol2
    
    if (drop.crumbs != 0 | !is.na(drop.crumbs)) {
      distr_poly <- smoothr::drop_crumbs(pol2, units::set_units(drop.crumbs, km^2))
    }
  }
  
  ##############
  ## Return ####
  
  if (polygonize) {
    list(polygons = distr_poly, actual = spdt)
  } else {
    list(polygons = NULL, actual = spdt)
  }
  
}

