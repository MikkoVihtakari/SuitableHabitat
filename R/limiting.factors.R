#' @title Define factors limiting suitable habitat
#' @description An internal function to calculate factors limiting suitable habitat used in the \code{\link{suitable.habitat}} function
#' @param habitat.space An object from the \code{\link{habitat.space}} function.
#' @param var.cols Character vector from the \code{\link{suitable.habitat}} function.
#' @param dt Data frame from the \code{\link{suitable.habitat}} function.
#' @param svars.dt Data frame from the \code{\link{suitable.habitat}} function.
#' @keywords internal
#' @seealso \code{\link{rasterize.nonsuitable.habitat}}
#' @export

limiting.factors <- function(habitat.space, var.cols, dt, svars.dt) {
  
  if(!all(habitat.space$svars == c("temp", "bdepth") | habitat.space$svars == c("temp", "depth"))) stop("find.lim.factors is only implemented for habitat spaceis with svars == c('temp', 'bdepth'). Set the parameter to FALSE to avoid this error.")
  
  var.cols <- c(var.cols, "lim.factor")
  
  if(habitat.space$chull.correction) {
    temp.lims <- unname(habitat.space$habitat.space.chull.sp@bbox[1,])
    depth.lims <- unname(habitat.space$habitat.space.chull.sp@bbox[2,])
  } else {
    temp.lims <- unname(habitat.space$habitat.space.sp@bbox[s1,])
    depth.lims <- unname(habitat.space$habitat.space.sp@bbox[2,])
  }
  
  
  dt$temp.lim <- svars.dt$temp < temp.lims[1] | svars.dt$temp > temp.lims[2]
  dt$temp.lim[dt$temp == -999]  <- NA
  
  dt$depth.lim <- svars.dt$depth < depth.lims[1] | svars.dt$depth > depth.lims[2]
  
  dt$lim.factor <- ifelse(is.na(dt$temp.lim), "land", # depths <1 got temp value -999. This was converted to NA for temp.lim
                          ifelse(!is.na(dt$habitat), "suitable", # non-NA habitats are TRUE, meaning they are suitable habitat
                                 ifelse(is.na(dt$habitat) & (!dt$temp.lim & dt$depth.lim) & !is.na(dt$temp.lim), "depth", # only depth limiting
                                        ifelse(is.na(dt$habitat) & (dt$temp.lim & !dt$depth.lim) & !is.na(dt$temp.lim), "temp", # only temp limiting
                                               ifelse(is.na(dt$habitat) & (!dt$temp.lim & !dt$depth.lim) & !is.na(dt$temp.lim), "temp&depth", # habitat space corners (compared to rectangle / orthogonal limits)
                                                      ifelse(is.na(dt$habitat) & (dt$temp.lim & dt$depth.lim) & !is.na(dt$temp.lim), "temp&&depth", # both temp and depth, not the corners
                                                             "error"))))))
  
  # Check the results
  # dt <- PlotSvalbard::transform_coord(dt, map.type = "panarctic", bind = TRUE)
  # PlotSvalbard::basemap("panarctic", limits = c("dt", "lon.utm", "lat.utm"), land.col = "grey80", base_size = 8) + geom_tile(data = dt, aes(x = lon.utm, y = lat.utm, fill = lim.factor), width = 1e4, height = 1e4)
  
  if(!is.null(habitat.space$ovars)) {
    
    if(habitat.space$ovars != "sal") stop("find.lim.factors is only implemented for habitat spaceis with ovars == 'sal'")
    
    sal.range <- range(habitat.space$cvars[[habitat.space$ovars]])
    
    dt$sal.lim <- dt$sal < sal.range[1] | dt$sal > sal.range[2]
    dt$sal.lim[dt$sal == -999]  <- NA
    
    dt$lim.factor <- ifelse(!is.na(dt$sal.lim) & dt$sal.lim, paste(dt$lim.factor, "sal", sep = "&"), dt$lim.factor)
    
    ## Clean up
    
    dt$lim.factor <- factor(dt$lim.factor)
    levels(dt$lim.factor)[levels(dt$lim.factor) == "land&sal"] <- "land"
    
    dt <- dt[names(dt) != "sal.lim"]
    
  } else {
    dt$lim.factor <- factor(dt$lim.factor)
  }
  
  ## Clean up
  
  dt <- dt[!names(dt) %in% c("temp.lim", "depth.lim")]
  levels(dt$lim.factor)[levels(dt$lim.factor) == "land"] <- NA
  
  fact.ord <- c("suitable", "depth", "depth&sal", "temp", "temp&depth", "temp&&depth", "temp&depth&sal", "temp&&depth&sal")
  fact.ord <- c(fact.ord, levels(dt$lim.factor)[!levels(dt$lim.factor) %in% fact.ord])
  dt$lim.factor <- factor(dt$lim.factor, levels = fact.ord)
  
  dt <- droplevels(dt)
  
  # levels(dt$lim.factor)[levels(dt$lim.factor) == "suitable"] <- NA
  
  ## Return
  
  dt
  
}