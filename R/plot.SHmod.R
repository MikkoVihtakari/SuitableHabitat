#' @title Plot a suitable habitat model on a map
#' @description Plot method for \code{\link[=suitable.habitat]{SHmod}} objects.
#' @param x \code{SHmod} object from \code{\link{suitable.habitat}} function.
#' @param limits Map limits. See \code{\link{basemap}}. The option "auto" (default) limits the map using coordinate range of \code{mod}. Alternatively use a numeric vector as described in \code{\link[PlotSvalbard]{basemap}} documentation.
#' @param ... Additional arguments. Required by R build checks. Ignore.
#' @method plot SHmod
#' @seealso \code{\link{suitable.habitat}} \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ggplot2 
#' @importFrom PlotSvalbard basemap
#' @importFrom broom tidy
#' @export


plot.SHmod <- function(x, limits = "auto", ...) {
  
  spdt <- x$actual
  distr_dt <- suppressMessages(suppressWarnings(broom::tidy(x$polygons)))
  
  if(limits == "auto") {
    limits <- PlotSvalbard::auto_limits(type = "panarctic", limits = c("long", "lat"), data = distr_dt)
  }
  
  ## Basemap 
  
  bm <- PlotSvalbard::basemap("panarctic", limits = limits, land.col = "grey", base_size = 8)
  
  ## Map
  
  bm + 
    geom_tile(data = spdt, aes(x = lon, y = lat, fill = habitat, color = habitat), width = 10000, height = 10000, alpha = 0.1) + 
    geom_polygon(data = distr_dt, aes(x = long, y = lat, group = group), fill = NA, color = "blue", size = PlotSvalbard::LS(1)) + 
    scale_fill_discrete(na.value = NA) + 
    scale_color_discrete(na.value = NA) + 
    PlotSvalbard::add_land() + 
    theme(legend.position = "none")
  
}
