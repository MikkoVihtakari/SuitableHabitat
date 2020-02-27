#' @title Estimate habitat space using multiple constraining variables
#' @description The function estimates a multivariate habitat space for a species. The results of this function can be used in further plotting and suitable habitat estimation functions in the package.
#' @param data data.frame containing the data for habitat space estimation. Required except when all limits are set orthogonal.
#' @param cvars A named list defining the constraining variables for the habitat space. Each name refers to a variable name in \code{data} unless orthogonal limits are desired. In this case, the name refers to the variable name and the element must be a numeric vector of length two defining the minimum and maximum tolerance, respectively. If this is not the case, the variables are assumed simultaneously limiting and acquired from \code{data}. The variables will be plotted in the order given here with the first variable being understood as the x-axis, the second as the y-axis, the third as the z-axis, and so on. At the moment up to three list elements are allowed. This limit may be increased in future releases. 
#' @param log.transform A logical vector with the same length than \code{cvars} indicating whether the variables should be 10-based logarithm transformed before habitat space estimation. 
#' @param rvar a character argument specifying the column name for the response variable if not the same as the constraining variables (\code{cvars}). Set to \code{NULL} (default) to use the density of constraining variables as a response variable (i.e. presence-only data).
#' @param grid.size a numeric vector equally long as the number of simultaneously constraining variables (dimensions; \code{cvars}) defining the grid.size for binning which is required to estimate habitat spaces.
#' @param grid.limits a list of numeric vectors, each of length two defining the minimum and maximum limit for constraining variables (\code{cvars}). The list should be the same length length than the number of simultaneously constraining variables. Set to \code{NULL} to automatically estimate the limits. 
#' @param method a character argument specifying the method for probability estimation for the habitat space. Currently implemented methods:
#' \itemize{
#' \item \code{"kde.binary"} Multivariate kernel density estimation using a binary probability level to define suitable and not-suitable habitat in the multivariate space. The \code{non.suitable.level} argument sets the probability level.
#' \item \code{"kde.probability"} Multivariate kernel density estimation is used to calculate the probability for finding the species in a given region in the habitat space.
#' \item \code{"foo"} Frequencies of occurrence for each bin are used as probabilities for finding the species in a given region in the habitat space. Requires a response variable (\code{rvar}). If the response variable is not binary (1 or 0 / \code{TRUE} or \code{FALSE}, it will be converted to such.
#' \item \code{"quantile.regression"} Multivariate quantile regression. Not implemented yet. 
#' \item function of glm or gam family Multivariate glm or gam object. Not implemented yet. 
#' 
#' Note that setting all limits to orthogonal will lead to a rectangular habitat space and probabilities will not be calculated.
#' }
#' @param non.suitable.level a numeric argument defining the KDE probability or frequency of occurrence level for non-suitable habitat. The value can be set between 0 and 1. Defines the edge of the habitat space. Smaller the value, the larger the habitat space. 
#' @param chull.correction logical indicating whether convex hull (\link{chull}) should be applied to correct concave shapes along the edge of the habitat space.
#' @details The function retunrs only two dimensional habitat spaces at the moment.
#' @author Mikko Vihtakari
#' @import ks raster smoothr ggplot2
#' @importFrom spatialEco remove.holes
#' @export
#' 
#' 
# Test parameters
# cvars = list(temp = NA, logdepth = 2, sal = c(31, 37))
# cvars = list(temp = c(-1, 7), bdepth = c(300, 1300), sal = c(31, 37))
# data = k
# cvars = list("temp" = NA, "bdepth" = NA, "sal" = c(29, 37))
# log.transform = c(FALSE, TRUE, FALSE)
# rvar = NULL; grid.size = rep(30, length(cvars)); grid.limits = NULL; method = "kde.binary"; non.suitable.level = 0.001; chull.correction = TRUE
# cvars = c("temp", "depth")
habitat.space <- function(data = NULL, cvars, log.transform = rep(FALSE, length(cvars)), rvar = NULL, grid.size = rep(30, length(cvars)), grid.limits = NULL, method = "kde.binary", non.suitable.level = 0.001, chull.correction = TRUE) {
  
  ################################
  ## 1. Tests and definitions ####
  
  if(length(cvars) < 1) stop("cvars must be at least length of 1")
  
  ovars <- unlist(lapply(seq_along(cvars), function(i) {
    if(is.numeric(cvars[[i]]) & length(cvars[[i]]) == 2) {
      names(cvars[i])
    }
  }))
  
  if(is.null(ovars)) {
    svars <- names(cvars)
  } else {
    svars <- names(cvars)[!names(cvars) %in% ovars]
  }
  
  if(length(svars) + length(ovars) != length(cvars)) stop("cvars is specified wrong (the number of orthogonal and simultaneous variables does not match.")
  
  if(length(svars) > 0) {
    if(is.null(data)) stop("data must be supplied for simultaneously limiting factors")
    if(!all(svars %in% names(data))) stop("specified simultaneously limiting factors are not valid column names in data")
  }
  
  
  if(!is.null(grid.limits)) {
    if(length(grid.limits) != length(svars)) stop("grid.limits has to be the same length than constraining variables (svars)")
    if(!all(sapply(grid.limits, function(k) is.numeric(k) & length(k) == 2))) stop("each element in grid.limits has to be a numeric vector of length 2")
  }
  
  if(length(log.transform) != length(cvars)) stop("log.transform must have the same length than cvars")
  if(!is.logical(log.transform)) stop("log.transform must be a logical vector")
  
  ## Log-transform
  
  names(log.transform) <- names(cvars)
  
  if(any(log.transform)) {
    data[names(log.transform[log.transform])] <- lapply(data[names(log.transform[log.transform])], function(k) log10(k))
  }
  
  ###################################
  ## 2. Habitat space estimation ####
  
  
  ## All orthogonal habitat spaces ####
  
  if(length(svars) == 0) {
    
    stop("All orthogonal habitat spaces have not been implemented yet")
    
    ## KDE methods ####
    
  } else if(method %in% c("kde.binary", "kde.probability")) {
    
    ## Multivariate kernel density estimation
    
    grid.size <- grid.size[1:length(svars)]
    
    # i = 1
    svar.info <- lapply(seq_along(svars), function(i) {
      var.range <- range(data[[svars[i]]])
      var.diff <- diff(var.range)
      
      rounding.level <- ifelse(var.diff < 1, 0.01, ifelse(var.diff < 10, 0.1, ifelse(var.diff < 100, 1, 10)))
      
      if(!is.null(grid.limits)) {
        min.limit <- grid.limits[[i]][1]
        max.limit <- grid.limits[[i]][2]
      } else {
        min.limit <- round_any(min(data[[svars[i]]]), rounding.level, floor)
        max.limit <- round_any(max(data[[svars[i]]]), rounding.level, ceiling)
      }
      
      bin.range <- c(min.limit, max.limit)
      bin.diff <- diff(bin.range)
      
      bin.width <- bin.diff/grid.size[i]
      
      list(var.range = var.range, var.diff = var.diff, rounding.level = rounding.level, min.limit = min.limit, max.limit = max.limit, bin.range = bin.range, bin.diff = bin.diff, bin.width = bin.width, log.transform = unname(log.transform[names(log.transform) == svars[i]]))
    })
    
    names(svar.info) <- svars
    
    xmins <- unname(sapply(svar.info, function(k) k$min.limit))
    xmaxs <- unname(sapply(svar.info, function(k) k$max.limit))
    
    if(length(ovars) != 0) {
      
      ovar.info <- lapply(seq_along(ovars), function(i) {
        
        var.range <- range(cvars[[ovars[i]]])
        var.diff <- diff(var.range)
        
        rounding.level <- ifelse(var.diff < 1, 0.01, ifelse(var.diff < 10, 0.1, ifelse(var.diff < 100, 1, 10)))
        
        list(var.range = var.range, var.diff = var.diff, rounding.level = rounding.level, min.limit = min(var.range), max.limit = max(var.range), log.transform = unname(log.transform[names(log.transform) == ovars[i]]))  
      })
      
    } else {
      ovar.info <- NULL
    } 
    
    
    ## Kernel density
    
    kd <- ks::kde(data[svars], gridsize = grid.size, xmin = xmins, xmax = xmaxs)
    
  } else {
    stop(paste("method =", method, "has not been implemented"))
  }
  
  ######################################################################
  ## 3. Finding the boundaries of habitat space for KDE estimations ####
  
  if(method %in% c("kde.binary", "kde.probability")) {
    
    if (length(svars) == 1) {
      
      stop("1D methods have not been implemented yet.")
      
    } else if (length(svars) == 2) {
      
      ## Setting up the limit space
      
      kde.boundary.limit <- ks::contourLevels(kd, cont = 100*(1-non.suitable.level))
      
      m <- kd$estimate  >= kde.boundary.limit
      rownames(m) <- kd$eval.points[[1]]
      colnames(m) <- kd$eval.points[[2]]
      
      m <- t(m)
      
      ## Use GIS packages to approximate the habitat space
      
      hab.sp.r <- raster::raster(m[nrow(m):1,])
      raster::extent(hab.sp.r) <- c(range(kd$eval.points[[1]]), range(kd$eval.points[[2]]))
      hab.sp.r <- raster::clump(hab.sp.r, directions = 4)
      
      id.df <- stats::na.omit(as.data.frame(raster::freq(hab.sp.r)))
      
      hab.sp.r[hab.sp.r != id.df[which.max(id.df$count),"value"]] <- NA
      
      hab.sp.p <- raster::rasterToPolygons(hab.sp.r, dissolve = TRUE)
      hab.sp.p <- spatialEco::remove.holes(hab.sp.p)
      hab.sp.p <- smoothr::smooth(hab.sp.p, method = "ksmooth", smoothness = 2)
      
      hab.sp.dt <- suppressMessages(ggplot2::fortify(hab.sp.p))
      names(hab.sp.dt)[names(hab.sp.dt) %in% c("long", "lat")] <- svars
      
      ## Convex hull correction
      
      if(chull.correction) {
        hab.sp.ch.dt <- hab.sp.dt[grDevices::chull(hab.sp.dt[svars]),]
        hab.sp.ch.dt <- rbind(hab.sp.ch.dt[nrow(hab.sp.ch.dt),], hab.sp.ch.dt)
        rownames(hab.sp.ch.dt) <- 1:nrow(hab.sp.ch.dt)
        hab.sp.ch.p <- SpatialPolygonsDataFrame(
          Sr = SpatialPolygons(
            Srl = list(Polygons(
              srl = list(Polygon(hab.sp.ch.dt[svars]))
              , ID = 1)
            )
          )
          , data = data.frame(Value = 1))
        
      } else {
        hab.sp.ch.dt <- NULL
        hab.sp.ch.p <- NULL
      }
      
      
    } else {
      stop("The habitat space boundary estimation has not been implemented for higher than 2D spaces")
    }
    
  }
  
  ############################
  ## 4. Model fit to data ####
  
  if(length(svars) > 0) {
    sps <- sp::SpatialPoints(data[,svars])
    
    if(chull.correction) {
      tmp <- sp::over(sps, hab.sp.ch.p)
    } else {
      tmp <- sp::over(sps, hab.sp.p)
    }
    
    model.fit <- data.frame(n.in = sum(!is.na(tmp[[1]])), n.out = sum(is.na(tmp[[1]])))
    model.fit$pr.in <- 100*model.fit$n.in/length(sps)
    model.fit$pr.out <- 100*model.fit$n.out/length(sps)
    
  } else {
    model.fit <- NULL
  }
  
  ##############
  ## Return ####
  
  out <- list(method = method, svars = svars, ovars = ovars, cvars = cvars, dimensions = length(cvars), non.suitable.level = kde.boundary.limit, chull.correction = chull.correction, svar.info = svar.info, ovar.info = ovar.info, habitat.space.dt = hab.sp.dt, habitat.space.sp = hab.sp.p, habitat.space.chull.dt = hab.sp.ch.dt, habitat.space.chull.sp = hab.sp.ch.p, kde.object = kd, model.fit = model.fit)
  class(out) <- "habitatSpace"
  
  out
  
}
