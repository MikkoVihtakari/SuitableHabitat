##' @title Print habitat space (\code{habitatSpace}) objects
##' @description \code{\link{print}} function for \code{\link[=habitat.space]{habitatSpace}} objects
##' @param x object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print habitatSpace
##' @keywords internal
##' @export
##' @author Mikko Vihtakari
##' @seealso \code{\link{deg_grid}} \code{\link{basemap}}

print.habitatSpace <- function(x, ...) {
  
  #Range <- data.frame(lon.deg = range(x$lon$lon), lat.deg = range(x$lat$lat), lon.utm = range(x$lon$lon.utm), lat.utm = range(x$lat$lat.utm))
  
  cat("Habitat space object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat(paste("Method:", x$method), sep = "\n")
  cat(paste("Dimensions:", x$dimensions), sep = "\n")
  cat("Simultanously constraining variables: ")
  cat(x$svars, sep = ", ")
  cat(NULL, sep = "\n")
  cat("Orthogonally constraining variables: ")
  if(length(x$ovars) == 0) {
    cat("NA")
  } else {
    cat(x$ovars, sep = ", ")
  }
  cat(NULL, sep = "\n")
  cat(paste("Convex hull correction:", x$chull.correction), sep = "\n")
  cat(paste0("Observations inside the model: ", round(x$model.fit$pr.in, 2), "%"), sep = "\n")
}

