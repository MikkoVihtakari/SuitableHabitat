#' @title Plot a habitat space
#' @description Plot method for \code{\link[=habitat.space]{habitatSpace}} objects.
#' @param mod \code{habitatSpace} object from \code{\link{habitat.space}} function.
#' @param ... Additional graphical arguments. Required by R build checks. Ignore.
#' @method plot habitatSpace
#' @seealso \code{\link{habitat.space}}
#' @author Mikko Vihtakari
#' @import ks sp
#' @export


plot.habitatSpace <- function(mod, ...) {
  
  plot(x = c(mod$svar.info[[1]]$min.limit, mod$svar.info[[1]]$max.limit),
       y = c(mod$svar.info[[2]]$min.limit, mod$svar.info[[2]]$max.limit), 
       type = "n",
       xlab = mod$svars[1],
       ylab = mod$svars[2],
       yaxt = "n",
       xaxt = "n",
       ...
  )
  
  if(mod$svar.info[[1]]$log.transform) {
    Ticks <- axTicks(1)
    axis(1, at = Ticks, labels = round(10^Ticks, 0))
  } else {
    axis(1)
  }
  
  if(mod$svar.info[[2]]$log.transform) {
    Ticks <- axTicks(2)
    axis(2, at = Ticks, labels = round(10^Ticks, 0))
  } else {
    axis(2)
  }
  
  points(mod$kde.object$x, pch = ".", col = "grey")
  
  ks::plot.kde(mod$kde.object, cont = c(1, 10, 25, 50, 75, 90, 99), add = T, ...)
  
  sp::plot(mod$habitat.space.sp, add = T, border = "red")
  
  if(mod$chull.correction) {
    sp::plot(mod$habitat.space.chull.sp, add = T, border = "blue")
  }

}
