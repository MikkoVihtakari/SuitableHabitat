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
#' @import sp raster rgeos smoothr rgdal sf stars
#' @export

# Test parameters
# depth = dpt; temp = d00s$temp$mean; sal = d00s$sal$mean; habitat.space = out; polygonize = FALSE; buffer.width = 15000; proj4 = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"; lat.lim = 40; res = 400; log.transform.depth = TRUE; find.lim.factors = FALSE; drop.crumbs = 1000

estimate.SH <- function(lon, lat, depth, temp, sal = NULL, habitat.space, polygonize = FALSE, buffer.width = 15000, proj4 = "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", lat.lim = 40, res = 400, log.transform.depth = TRUE, find.lim.factors = FALSE, drop.crumbs = 1000) {
  
  ## Checks
  
  ## Make the data frame ####
  
  dt <- as.data.frame(cbind(lon = c(lon), lat = c(lat), depth = c(depth), temp = c(temp), sal = c(sal)))
  var.cols <- names(dt)[!names(dt) %in% c("lon", "lat")]
  if (!is.null(lat.lim)) {dt <- dt[dt$lat > lat.lim,]}
  dt[dt$depth < 1, "temp"] <- -999
  dt[is.na(dt$sal), "sal"] <- -999
  dt <- dt[order(dt$lon, dt$lat),]
  
  ## Suitable habitat
  
  tmp <- dt[c("temp", "depth")]
  if(log.transform.depth) tmp$depth <- log10(tmp$depth)
  
  sps <- sp::SpatialPoints(tmp)
  
  habtab <- sp::over(sps, habitat.space$habitat.space.sp)
  
  if (is.null(habitat.space$ovars)) {
    dt$habitat <- ifelse(!is.na(habtab), TRUE, NA)
  } else {
    if(length(habitat.space$ovars) == 1 && habitat.space$ovars == "sal" && !is.null(sal)) {
      sal.range <- range(habitat.space$cvars[[habitat.space$ovars]])
      dt$habitat <- ifelse(!is.na(habtab) & dt$sal <= sal.range[2] & dt$sal >= sal.range[1], TRUE, NA)
    } else {
      stop("No other ovars implemented than sal (salinity)")
    }
  }
  
  ### Limiting factors
  
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
    
    y <- raster::projectRaster(y, crs = proj4)
    
    pol <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y), as_points = FALSE, merge = TRUE))
    
    #### Merge adjacent polygons ###
    
    pol <- rgeos::gBuffer(pol, byid = TRUE, width = buffer.width)
    pol <- rgeos::gBuffer(pol, byid = TRUE, width = -buffer.width)
    
    pol2 <- ksmooth_polys(x = pol, k = 12, N = 5L) # From helpers
    
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

