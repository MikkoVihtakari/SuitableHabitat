#' @title Combine suitable habitat models based on geographic criteria
#' @description Used to combine (currently only two) suitable habitat models based on (currently linear) geographic criteria
#' @param mod1,mod2 \link[=suitable.habitat]{Suitable habitat models} to be be combined.
#' @param x.breaks,y.breaks Longitude and/or latitude breaks for the model combination as a character vector of length 2 including logical operators. The corresponding y/x intercept will be used to combine the model parts. Should be given as the model \code{\link[=suitable.habitat]{proj4}} units. For example \code{c("<2.5e6", ">=2.5e6")}
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @param hexbins A number of (xbins) used for the hexagonization. See \code{\link[hexbin]{hexbin}}.
#' @import sp raster 
#' @export
 
combine.models <- function(mod1, mod2, y.breaks = NULL, x.breaks = NULL, buffer.width = 15000, drop.crumbs = 1000, res = 350, hexbins = 100) {
  
  ## Tests
  
  if(sp::proj4string(mod1$raster) != sp::proj4string(mod2$raster)) stop("projection (proj4) has to be identical for mod1 and mod2")
  
  if(!is.null(x.breaks)) {
    if(!is.character(x.breaks)) stop("x.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
    if(length(x.breaks) != 2) stop("x.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
  }
  
  if(!is.null(y.breaks)) {
    if(!is.character(y.breaks)) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
    if(length(y.breaks) != 2) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
  }
  
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
  
  ### Rasterize and clump the modeled habitat ###
  
  ras_hab <- rasterize.suitable.habitat(data = spdt, proj4 =  sp::proj4string(mod1$raster), drop.crumbs = drop.crumbs, res = res)
  
  ### Hexagonize the rasterized habitat ###
  
  hex_hab <- hexagonize.suitable.habitat(data = ras_hab, hexbins = hexbins)
  
  ### Polygonize the modeled distribution ###
  
  distr_poly <- polygonize.suitable.habitat(data = ras_hab, buffer.width = buffer.width, drop.crumbs = drop.crumbs, res = res)
  
  ##############
  ## Return ####
  
  out <- list(raw = spdt, raster = ras_hab, polygon = distr_poly, hexagon = hex_hab)
  
  class(out) <- "SHmod"
  
  out
  
}
