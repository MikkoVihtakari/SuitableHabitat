#' @title Combine suitable habitat models based on geographic criteria
#' @description Used to combine (currently only two) suitable habitat models based on (currently linear) geographic criteria
#' @param mod1,mod2 ...
#' @param y.breaks Latitude break for the model combination as a character vector including logical operators. For example \code{c("<2.5e6", ">=2.5e6")}
#' @param x.breaks Not implemented. 
#' @param buffer.width Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.
#' @param proj4 Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function. 
#' @param res Vertical and horizontal resolution of the smoothed polygon output.
#' @export
 
combine.models <- function(mod1, mod2, y.breaks = NULL, x.breaks = NULL, buffer.width = 15000, drop.crumbs = 1000, res = 500) {
  
  ## Tests
  
  if(sp::proj4string(mod1$polygons) != sp::proj4string(mod2$polygons)) stop("projection (proj4) has to be identical for mod1 and mod2")
  
  if(!is.null(x.breaks)) stop("x.breaks not implemented yet")
  
  if(!is.null(y.breaks)) {
    if(!is.character(y.breaks)) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
    if(length(y.breaks) != 2) stop("y.breaks has to be a character vector of length 2. Use numbers and logical operators to define the limits, e.g. '>=2.5e6'")
  }
  
  ## Subset
  
  if(!is.null(y.breaks)) {
    dt1 <- mod1$actual[eval(parse(text=paste("mod1$actual$lat", y.breaks[1]))),]
    dt2 <- mod2$actual[eval(parse(text=paste("mod1$actual$lat", y.breaks[2]))),]
    spdt <- rbind(dt1, dt2)
  }
  
  ## Polygonize
  
  distr_poly <- polygonize.suitable.habitat(data = spdt, buffer.width = buffer.width, proj4 = sp::proj4string(mod1$polygons), drop.crumbs = drop.crumbs, res = res)
  
  ##############
  ## Return ####
  
  out <- list(polygons = distr_poly, actual = spdt)
  
  class(out) <- "SHmod"
  
  out
  
  
}
