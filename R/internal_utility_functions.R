#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal  
#' @author Hadley Wickham

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Something
#' @import sf

clusterSF <- function(sfpolys, thresh){ ## From https://gis.stackexchange.com/questions/254519/group-and-union-polygons-that-share-a-border-in-r
  dmat = sf::st_distance(sfpolys)
  hc = hclust(as.dist(dmat>thresh), method="single")
  groups = cutree(hc, h=0.5)
  d = sf::st_sf(
    geom = do.call(c,
                   lapply(1:max(groups), function(g){
                     sf::st_union(sfpolys[groups==g,])
                   })
    )
  )
  d$group = 1:nrow(d)
  d
}

#' @title Wrapper to \code{\link[smoothr]{smooth_ksmooth}} SpatialPolygonsDataFrames
#' @param x SpatialPolygonsDataFrame
#' @param k The \code{smoothness} parameter in \code{\link[smoothr]{smooth_ksmooth}}
#' @param N The \code{n} parameter in \code{\link[smoothr]{smooth_ksmooth}}
#' @import smoothr sp

# Test parameters
# x = pol; k = 12; N = 5L
ksmooth_polys <- function(x, k, N) {
  
  total <- length(x@polygons)
  #pb <- txtProgressBar(min = 1, max = total, style = 3)
  
  tp <- lapply(1:total, function(i) {
    #setTxtProgressBar(pb, i)
    
    for(j in 1:length(x@polygons[[i]]@Polygons)) {
      x@polygons[[i]]@Polygons[[j]]@coords <- na.omit(smoothr::smooth_ksmooth(x@polygons[[i]]@Polygons[[j]]@coords, smoothness = k, wrap = TRUE, n = N))
    }
    
    x@polygons[[i]]
    
  })
  
  out <- sp::SpatialPolygons(tp)
  sp::proj4string(out) <- sp::proj4string(x)
  
  sp::SpatialPolygonsDataFrame(out, x@data)
}


#' @title Standard error of mean
#' @param x numeric vector

se <- function(x) {
  sd(x, na.rm = T)/sqrt(sum(!is.na(x)))
}

