#' @title Plot a habitat space
#' @description Plot method for \code{\link[=habitat.space]{habitatSpace}} objects.
#' @param x \code{habitatSpace} object from \code{\link{habitat.space}} function.
#' @param ... Additional graphical arguments. Required by R build checks. Ignore.
#' @method plot habitatSpace
#' @seealso \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ks sp graphics
#' @export


plot.habitatSpace <- function(x, ...) {
  
  plot(x = c(x$svar.info[[1]]$min.limit, x$svar.info[[1]]$max.limit),
       y = c(x$svar.info[[2]]$min.limit, x$svar.info[[2]]$max.limit), 
       type = "n",
       xlab = x$svars[1],
       ylab = x$svars[2],
       yaxt = "n",
       xaxt = "n",
       ...
  )
  
  if(x$svar.info[[1]]$log.transform) {
    Ticks <- graphics::axTicks(1)
    graphics::axis(1, at = Ticks, labels = round(10^Ticks, 0))
  } else {
    graphics::axis(1)
  }
  
  if(x$svar.info[[2]]$log.transform) {
    Ticks <- graphics::axTicks(2)
    graphics::axis(2, at = Ticks, labels = round(10^Ticks, 0))
  } else {
    graphics::axis(2)
  }
  
  points(x$kde.object$x, pch = ".", col = "grey")
  
  plot(x$kde.object, cont = c(1, 10, 25, 50, 75, 90, 99), add = T, ...)
  
  sp::plot(x$habitat.space.sp, add = T, border = "red")
  
  if(x$chull.correction) {
    sp::plot(x$habitat.space.chull.sp, add = T, border = "blue")
  }

}
