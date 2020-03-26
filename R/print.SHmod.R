##' @title Print suitable habitat model (\code{SHmod}) objects
##' @description \code{\link{print}} function for \code{\link[=suitable.habitat]{SHmod}} objects
##' @param x object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print SHmod
##' @keywords internal
##' @export
##' @author Mikko Vihtakari

print.SHmod <- function(x, ...) {
  
  cat("Suitable habitat model object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Consists of following lists:")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste("Projection:", sp::proj4string(x$raster)), sep = "\n")
  cat(paste("Extent:", round(raster::extent(x$raster), 0)), sep = "\n")
  cat(NULL, sep = "\n")
  cat("$raw ", sep = " ")
  cat(paste0("(nrow: ", nrow(x$raw), ", ncol: ", ncol(x$raw), ")"), sep = "\n")
  print(head(x$raw))
  cat(NULL, sep = "\n")
  cat("$raster", sep = "\n")
  print(x$raster)
  cat("$polygon", sep = "\n")
  print(x$polygon)
  cat(NULL, sep = "\n")
  cat("$hexagon", sep = "\n")
  print(x$hexagon)
  
}

