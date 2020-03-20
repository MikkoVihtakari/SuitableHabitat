#' @title Estimate a suitable habitat for a species from NEMO output
#' @description Estimates suitable habitat for a species from European Modeling of the Ocean (NEMO) physical oceanography model output based on limitations set by temperature, salinity and bottom depth.
#' @param habitat.space ...
#' @param oceangr.model ...
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param lat.lim Single numeric value defining a latitude value which is used to cut the model data.
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @param find.lim.factors Logical indicating whether the function should which factors (if any) make the NEMO grid cell non-suitable habitat. 
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @details The \code{\link[base]{dim}}ensions of NEMO input matrices must be equal.
#' @import sp raster rgeos smoothr rgdal sf stars units
#' @importFrom grDevices chull
#' @importFrom stats na.omit sd
#' @export

# Test parameters
# habitat.space = td.general[[1]]; oceangr.model = NEMOdata; buffer.width = 15000; proj4 = polarStereographic; lat.lim = 40; res = 400; find.lim.factors = FALSE; drop.crumbs = 1000

suitable.habitat <- function(habitat.space, oceangr.model = NEMOdata, buffer.width = 15000, proj4 = polarStereographic, lat.lim = 40, res = 400, find.lim.factors = FALSE, drop.crumbs = 1000) {
  
  ## Checks
  
  ## Make the data frame ####
  
  dt <- as.data.frame(cbind(lon = c(oceangr.model$lon), lat = c(oceangr.model$lat), depth = c(oceangr.model$depth), temp = c(oceangr.model$temp), sal = c(oceangr.model$sal)))
  
  var.cols <- names(dt)[!names(dt) %in% c("lon", "lat")]
  
  if (!is.null(lat.lim)) {dt <- dt[dt$lat > lat.lim,]}
  dt[dt$depth < 1, "temp"] <- -999
  dt[is.na(dt$sal), "sal"] <- -999
  dt <- dt[order(dt$lon, dt$lat),]
  
  ## Suitable habitat
  
  svars <- habitat.space$svars
  svars[svars == "bdepth"] <- "depth"
  
  tmp <- dt[svars]
  
  log.transform.vars <- sapply(habitat.space$svar.info, function(k) k$log.transform)
  
  if(any(log.transform.vars)) {
    tmp[log.transform.vars] <- lapply(tmp[log.transform.vars], function(k) log10(k))
  }
  
  sps <- sp::SpatialPoints(tmp)
  
  habtab <- sp::over(sps, habitat.space$habitat.space.sp)
  
  if (is.null(habitat.space$ovars)) {
    dt$habitat <- ifelse(!is.na(habtab[[1]]), TRUE, NA)
  } else {
    if(length(habitat.space$ovars) == 1 && habitat.space$ovars == "sal") {
      sal.range <- range(habitat.space$cvars[[habitat.space$ovars]])
      dt$habitat <- ifelse(!is.na(habtab[[1]]) & dt$sal <= sal.range[2] & dt$sal >= sal.range[1], TRUE, NA)
    } else {
      stop("No other ovars implemented than sal (salinity)")
    }
  }
  
  ### Limiting factors
  
  if (find.lim.factors) {
    stop("Limiting factors code has not been finished")
    # 
    # var.cols <- c(var.cols, "lim.factor")
    # 
    # temp.lims <- unname(limit.polygon@bbox[1,])
    # depth.lims <- unname(limit.polygon@bbox[2,])
    # 
    # if(is.null(sal)) {
    #   
    #   limfacdt <- data.frame(temp = tmp$temp >= temp.lims[1] & tmp$temp <= temp.lims[2], depth = tmp$depth >= depth.lims[1] & tmp$depth <= depth.lims[2])
    #   dt$lim.factor <- ifelse(rowSums(limfacdt) == 2, "suitable", ifelse(limfacdt$temp, "depth", ifelse(limfacdt$depth, "temp", "both")))
    #   
    #   dt[is.na(dt$habitat) & dt$lim.factor == "suitable", "lim.factor"] <- "temp&depth"
    #   dt[dt$temp == -999, "lim.factor"] <- NA
    #   
    # } else {
    #   
    #   limfacdt <- data.frame(temp = tmp$temp >= temp.lims[1] & tmp$temp <= temp.lims[2], depth = tmp$depth >= depth.lims[1] & tmp$depth <= depth.lims[2], sal = dt$sal >= sal.range[1] & dt$sal <= sal.range[2])
    #   
    #   bla <- apply(limfacdt, 1, function(k) {
    #     tmp <- c("temp", "depth", "sal")[!c("temp", "depth", "sal") %in% names(which(k))]
    #     
    #     if(length(tmp) == 0) {
    #       "suitable"
    #     } else if(length(tmp) == 1) {
    #       tmp
    #     } else if(length(tmp) == 3) {
    #       "all"
    #     } else {
    #       paste(tmp, collapse = "&")
    #     }
    #   })
    #   
    #   dt$lim.factor <- unlist(bla)
    #   
    #   dt[is.na(dt$habitat) & dt$lim.factor == "suitable", "lim.factor"] <- "temp&depth"
    #   dt[dt$sal == -999, "lim.factor"] <- NA
    #   dt[dt$temp == -999, "lim.factor"] <- NA
    # }
  } 
  
  ##########################################
  ### DATA PREPARATION FOR BOTH OPTIONS ####
  
  ### Actual model output ###
  
  sps <- sp::SpatialPointsDataFrame(coords = dt[c("lon", "lat")], data = dt[c(var.cols, "habitat")], proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sps <- sp::spTransform(sps, sp::CRS(proj4))
  spdt <- data.frame(sps)
  spdt <- spdt[names(spdt) != "optional"]
  
  ### Polygonize the modeled distribution ###
  
  distr_poly <- polygonize.suitable.habitat(data = spdt, buffer.width = buffer.width, proj4 = proj4, drop.crumbs = drop.crumbs, res = res)
  
  ##############
  ## Return ####
  
  out <- list(polygons = distr_poly, actual = spdt)
  
  class(out) <- "SHmod"
  
  out
}

