#' @title Combine suitable habitat models based on geographic criteria
#' @description Used to combine (currently only two) suitable habitat models based on (currently linear) geographic criteria
#' @param mod1,mod2 \link[=suitable.habitat]{Suitable habitat models} to be be combined.
#' @param x.breaks,y.breaks Longitude and/or latitude breaks for the model combination as a character vector of length 2 including logical operators. The corresponding y/x intercept will be used to combine the model parts. Should be given as the model \code{\link[=suitable.habitat]{proj4}} units. For example \code{c("<2.5e6", ">=2.5e6")}
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @param hexbins A number of (xbins) used for the hexagonization. See \code{\link[hexbin]{hexbin}}.
#' @details The function finds limiting factors if both \code{mod1} and \code{mod2} contain them.
#' @import sp raster 
#' @export
 
# y.breaks = c("<2.5e6", ">=2.5e6"); x.breaks = NULL; buffer.width = 1.5e4; drop.crumbs = 3e4; res = 350; hexbins = 100
combine.models <- function(mod1, mod2, y.breaks = NULL, x.breaks = NULL, buffer.width = 1.5e4, drop.crumbs = 3e4, res = 350, hexbins = 100) {
  
  # Progress bar ####
  
  pb <-  txtProgressBar(min = 0, max = 8, initial = 0, style = 3) 
  
  ## Tests ####
  
  if(sp::proj4string(mod1$raster) != sp::proj4string(mod2$raster)) stop("projection (proj4) has to be identical for mod1 and mod2")
  
  mod.proj <- sp::proj4string(mod1$raster)
  
  if(!is.null(x.breaks)) {
    if(!is.character(x.breaks)) stop("x.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
    if(length(x.breaks) != 2) stop("x.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
  }
  
  if(!is.null(y.breaks)) {
    if(!is.character(y.breaks)) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
    if(length(y.breaks) != 2) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
  }
  
  find.lim.factors <- all(c(mod1$parameters$lim.factors, mod2$parameters$lim.factors))
  
  setTxtProgressBar(pb, 1)
  
  ## Subset
  
  if(!is.null(y.breaks)) {
    dt1 <- mod1$raw[eval(parse(text=paste("mod1$raw$lat", y.breaks[1]))),]
    dt2 <- mod2$raw[eval(parse(text=paste("mod1$raw$lat", y.breaks[2]))),]
    spdt <- rbind(dt1, dt2)
  }
  
  if(!is.null(x.breaks)) {
    dt1 <- mod1$raw[eval(parse(text=paste("mod1$raw$lon", x.breaks[1]))),]
    dt2 <- mod2$raw[eval(parse(text=paste("mod1$raw$lon", x.breaks[2]))),]
    spdt <- rbind(dt1, dt2)
  }
  
  setTxtProgressBar(pb, 2)
  
  ## Model extent
  
  x <- sp::SpatialPoints(coords = spdt[c("lon", "lat")], proj4string = sp::CRS(mod.proj))
  x <- sp::spTransform(x, sp::CRS("+proj=longlat +datum=WGS84"))
  x <- data.frame(coordinates(x))
  x$lon_cut <- cut(x$lon, seq(-180,180,1))
  levels(x$lon_cut) <- sapply(strsplit(gsub("\\(|\\]", "", levels(x$lon_cut)), ","), function(k) mean(as.numeric(k)))
  x$lon_cut <- as.numeric(as.character(x$lon_cut))
  
  y <- x %>% group_by(lon_cut) %>% summarise(lat = min(lat))
  y <- plyr::rename(y, c("lon_cut" = "lon"))
  
  z <- sp::SpatialLines(list(sp::Lines(sp::Line(y), ID = 1)))
  sp::proj4string(z) <- "+proj=longlat +datum=WGS84"
  
  z <- sp::spTransform(z, sp::CRS(mod.proj))
  
  mod.ext <- sp::Polygons(list(sp::Polygon(coordinates(z))), ID = 1)
  mod.ext <- sp::SpatialPolygons(list(mod.ext))
  proj4string(mod.ext) <- mod.proj
  
  setTxtProgressBar(pb, 3)
  
  ### Rasterize and clump the modeled habitat ####
  
  ras_hab <- rasterize.suitable.habitat(data = spdt, proj4 = mod.proj, mod.extent = mod.ext, drop.crumbs = drop.crumbs, res = res)
  
  setTxtProgressBar(pb, 4)
  
  ### Add limiting factors
  
  if(find.lim.factors) {
    
    lim_fact <- rasterize.nonsuitable.habitat(data = spdt, proj4 = mod.proj, mod.extent = mod.ext, drop.crumbs = drop.crumbs, res = res)  
    
  } else {
    
    lim_fact <- NULL
    
  }
  
  setTxtProgressBar(pb, 5)
  
  ### Hexagonize the rasterized habitat ###
  
  hex_hab <- hexagonize.suitable.habitat(data = ras_hab, hexbins = hexbins)
  
  setTxtProgressBar(pb, 6)
  
  ### Polygonize the modeled distribution ###
  
  distr_poly <- polygonize.suitable.habitat(data = ras_hab, buffer.width = buffer.width, drop.crumbs = drop.crumbs)
  
  setTxtProgressBar(pb, 7)
  
  ##############
  ## Return ####
  
  out <- list(raw = spdt, raster = ras_hab, polygon = distr_poly, hexagon = hex_hab, lim.fact = lim_fact,
              parameters = 
                list(model.extent = mod.ext, model.proj = mod.proj, 
                     latitude.limit = c(mod1$parameters$latitude.limit, mod2$parameters$latitude.limit), 
                     raster.resolution = res,
                     drop.crumbs = drop.crumbs, buffer.width = buffer.width, hexagon.resolution = hexbins,
                     lim.factors = find.lim.factors,
                     x.breaks = x.breaks, y.breaks = y.breaks)
  )
              
  
  class(out) <- "SHmod"
  
  setTxtProgressBar(pb, 8)
  
  out
  
}
