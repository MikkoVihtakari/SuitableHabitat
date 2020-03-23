#' @title Hexagonize suitable habitat
#' @description An internal function to hexagonize suitable habitat data from the \code{\link{suitable.habitat}} function
#' @param data a raster object from the \code{\link{rasterize.suitable.habitat}} function.
#' @param hexbins A number of (xbins) used for the hexagonization. See \code{\link[hexbin]{hexbin}}.
#' @import raster hexbin
#' @keywords internal
#' @export

hexagonize.suitable.habitat <- function(data, hexbins) {
  
  x <- data.frame(raster::coordinates(data), area = data@data@values)
  x <- x[!is.na(x$area),]
  names(x)[names(x) == "x"] <- "lon"
  names(x)[names(x) == "y"] <- "lat"
  
  hexbin::hexbin(x$lon, x$lat, xbins = hexbins, xbnds = range(x$lon), ybnds = range(x$lat))
  
  # A reminder:
  # tmp <- data.frame(hexbin::hcell2xy(hmod), count_bin1 = hmod@count, cell_bin1 = hmod@cell)
  
}