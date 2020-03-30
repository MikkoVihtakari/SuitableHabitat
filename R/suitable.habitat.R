#' @title Estimate a suitable habitat for a species from NEMO output
#' @description Estimates suitable habitat for a species from European Modeling of the Ocean (NEMO) physical oceanography model output based on limitations set by temperature, salinity and bottom depth.
#' @param habitat.space An object from the \code{\link{habitat.space}} function.
#' @param oceangr.model A list of oceanographic model data. The format should follow that examplified in the \link{NEMOdata} object.
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param lat.lim Single numeric value defining a latitude value which is used to cut the model data.
#' @param res Vertical and horizontal resolution of the smoothed polygon output. Setting this parameter makes the polygons more connected cutting corners in the raster based model. 
#' @param find.lim.factors Logical indicating whether the function should which factors (if any) make the NEMO grid cell non-suitable habitat. 
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param hexbins A number of (xbins) used for the hexagonization. See \code{\link[hexbin]{hexbin}}.
#' @details The suitable habitat is estimated using the oceanographic model (\code{oceangr.model}) grid cells. The grid cell output are further manipulated using the \code{\link{rasterize.suitable.habitat}}, \code{\link{polygonize.suitable.habitat}}, and \code{\link{hexagonize.suitable.habitat}} functions.
#' 
#' The \code{\link[base]{dim}}ensions of NEMO input matrices must be equal. 
#' @return Returns a list containing suitable habitat estimates in following formats: 1) \strong{\code{$raw}:} oceanographic model (\code{oceangr.model}) grid cells, 2) \strong{\code{$raster}:} rasterized cells whose resolution is defined by the \code{res} with disconnected regions smaller than the \code{drop.crumbs} parameter removed (in km2), 3) \strong{\code{$polygon}:} polgyonized raster data, and 4) \strong{\code{$hexagon}:} hexagonized raster data. See \code{\link[hexbin]{hexbin}}.
#' @import sp raster rgeos smoothr rgdal sf stars units hexbin
#' @importFrom grDevices chull
#' @importFrom stats na.omit sd
#' @export

# Test parameters
# habitat.space = td.general[[1]]; oceangr.model = NEMOdata; buffer.width = 15000; proj4 = polarStereographic; lat.lim = 40; res = 350; find.lim.factors = TRUE; drop.crumbs = 3e4; hexbins = 100

suitable.habitat <- function(habitat.space, oceangr.model = NEMOdata, proj4 = polarStereographic, lat.lim = 40, res = 350, drop.crumbs = 3e4, buffer.width = 1.5e4, hexbins = 100, find.lim.factors = TRUE) {
  
  ## Checks
  
  if(proj4 != "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") stop("Only the 'panarctic' projection in the PlotSvalbard package is currently supported.")
  
  ## Make the data frame ####
  
  dt <- as.data.frame(cbind(lon = c(oceangr.model$lon), lat = c(oceangr.model$lat), depth = c(oceangr.model$depth), temp = c(oceangr.model$temp), sal = c(oceangr.model$sal)))
  
  var.cols <- names(dt)[!names(dt) %in% c("lon", "lat")]
  
  if (!is.null(lat.lim)) {dt <- dt[dt$lat > lat.lim,]}
  dt[dt$depth < 1, "temp"] <- -999
  dt[is.na(dt$sal), "sal"] <- -999
  dt <- dt[order(dt$lon, dt$lat),]
  
  ## Model extent
  
  x <- dt[c("lon", "lat")]
  x$lon_cut <- cut(x$lon, seq(-180,180,1))
  levels(x$lon_cut) <- sapply(strsplit(gsub("\\(|\\]", "", levels(x$lon_cut)), ","), function(k) mean(as.numeric(k)))
  x$lon_cut <- as.numeric(as.character(x$lon_cut))
  
  y <- x %>% group_by(lon_cut) %>% summarise(lat = min(lat))
  y <- plyr::rename(y, c("lon_cut" = "lon"))
  
  z <- sp::SpatialLines(list(sp::Lines(sp::Line(y), ID = 1)))
  sp::proj4string(z) <- "+proj=longlat +datum=WGS84"
  
  z <- sp::spTransform(z, sp::CRS(proj4))
  
  mod.ext <- sp::Polygons(list(sp::Polygon(coordinates(z))), ID = 1)
  mod.ext <- sp::SpatialPolygons(list(mod.ext))
  proj4string(mod.ext) <- proj4
  
  ## Suitable habitat
  
  svars <- habitat.space$svars
  svars[svars == "bdepth"] <- "depth"
  
  svars.dt <- dt[svars]
  
  log.transform.vars <- sapply(habitat.space$svar.info, function(k) k$log.transform)
  
  if(any(log.transform.vars)) {
    svars.dt[log.transform.vars] <- lapply(svars.dt[log.transform.vars], function(k) log10(k))
  }
  
  sps <- sp::SpatialPoints(svars.dt)
  
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
  
  ## Limiting factors ###
  
  if (find.lim.factors) {
    dt <- limiting.factors(habitat.space = habitat.space, var.cols = var.cols, dt = dt, svars.dt = svars.dt)
    var.cols <- c(var.cols, "lim.factor")
  } 
  
  
  ### Raw model output ###
  
  sps <- sp::SpatialPointsDataFrame(coords = dt[c("lon", "lat")], data = dt[c(var.cols, "habitat")], proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  sps <- sp::spTransform(sps, sp::CRS(proj4))
  spdt <- data.frame(sps)
  spdt <- spdt[names(spdt) != "optional"]
  
  ### Rasterize and clump the modeled habitat ####
  
  ras_hab <- rasterize.suitable.habitat(data = spdt, proj4 = proj4, mod.extent = mod.ext, drop.crumbs = drop.crumbs, res = res)
  
  ### Add limiting factors
  
  if(find.lim.factors) {
    
    lim_fact <- rasterize.nonsuitable.habitat(data = spdt, proj4 = proj4, mod.extent = mod.ext, drop.crumbs = drop.crumbs, res = res)  
    
  } else {
    
    lim_fact <- NULL
      
  }
  
  ### Hexagonize the rasterized habitat ###
  
  hex_hab <- hexagonize.suitable.habitat(data = ras_hab, hexbins = hexbins)
  
  ### Polygonize the modeled distribution ###
  
  distr_poly <- polygonize.suitable.habitat(data = ras_hab, buffer.width = buffer.width, drop.crumbs = drop.crumbs)
  
  ##############
  ## Return ####
  
  out <- list(raw = spdt, raster = ras_hab, polygon = distr_poly, hexagon = hex_hab, lim.fact = lim_fact, parameters = 
                list(model.extent = mod.ext, model.proj = proj4, latitude.limit = lat.lim, raster.resolution = res,
                     drop.crumbs = drop.crumbs, buffer.width = buffer.width, hexagon.resolution = hexbins, lim.factors = find.lim.factors)
  )
  
  class(out) <- "SHmod"
  
  out
}

