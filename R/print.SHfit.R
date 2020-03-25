##' @title Print suitable habitat model fit (\code{SHfit}) objects
##' @description \code{\link{print}} function for \code{\link[=model.fit]{SHfit}} objects
##' @param x object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print SHfit
##' @keywords internal
##' @export
##' @author Mikko Vihtakari

print.SHfit <- function(x, ...) {
  
  cat("Suitable habitat model fit object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Consists of following lists: ")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste("Method:", x$parameters$fit.method), sep = "")
  cat(ifelse(x$parameters$regions, " with regions", ""), sep = "\n")
  cat(paste("Projection:", x$parameters$model.proj), sep = "\n")
  cat(paste("Extent:", round(raster::extent(x$parameters$model.extent), 0)), sep = "\n")
  cat(NULL, sep = "\n")
  if(!is.null(x$hexagon)) {
    cat("$hexogon", sep = "\n")
    cat("Elements: ")
    cat(names(x$hexagon), sep = ", ")
    cat(NULL, sep = "\n")
    cat("Fit:", sep = "\n")
    print(x$hexagon$summary)
  }
  if(!is.null(x$polygon)) {
    cat("$polygon", sep = "\n")
    cat("Elements: ")
    cat(names(x$polygon), sep = ", ")
    cat(NULL, sep = "\n")
    cat("Fit:", sep = "\n")
    print(x$polygon$summary)
  }
}

