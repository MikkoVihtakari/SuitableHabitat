##' @title Print NEMO data (\code{NEMOdata}) objects
##' @description \code{\link{print}} function for \code{\link[=suitable.habitat]{NEMOdata}} objects
##' @param x object to be printed.
##' @param ... further arguments passed to \code{\link{print}}.
##' @method print NEMOdata
##' @keywords internal
##' @importFrom utils object.size
##' @export
##' @author Mikko Vihtakari

print.NEMOdata <- function(x, ...) {
  cat("NEMOdata object.", sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(names(x), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("File size ", format(utils::object.size(x), units = "Mb")), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Bottom temperature data (use $temp to index):", sep = "\n")
  cat(paste0(nrow(x$temp), " rows. ", ncol(x$temp), " columns."), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Bottom salinity data (use $sal to index):", sep = "\n")
  cat(paste0(nrow(x$sal), " rows. ", ncol(x$sal), " columns."), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("Decade: ", x$decade), sep = "\n")
  cat("Years: ")
  cat(x$years, sep = ", ")
}



