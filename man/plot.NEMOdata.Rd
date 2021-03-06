% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.NEMOdata.R
\name{plot.NEMOdata}
\alias{plot.NEMOdata}
\title{Plot a NEMO data on a map}
\usage{
\method{plot}{NEMOdata}(
  x,
  limits = 40,
  type = "all",
  midpoints = c(temp = 7, depth = 1500, sal = 34.5),
  ncol = 1,
  base_size = 8,
  land.col = "grey80",
  ...
)
}
\arguments{
\item{x}{\code{NEMOdata} object}

\item{limits}{Map limits. See \code{\link{basemap}}. The option "auto" limits the map using coordinate range of \code{x}. Alternatively use a numeric vector as described in \code{\link[ggOceanMaps]{basemap}} documentation.}

\item{type}{Character argument defining the types of model data to be plotted: 
\itemize{
\item \code{"depth"} Depth data as raster
\item \code{"temp"} Temperature data as raster
\item \code{"sal"} Salinity data as raster
\item \code{"all"} All of the above
}}

\item{midpoints}{Named vector defining the midpoints for color scales of plotted variables. See \code{\link[ggplot2]{scale_colour_gradient}}.}

\item{ncol}{Integer specifying the number of columns when all variables are plotted (\code{type = "all"}).}

\item{base_size}{Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.}

\item{land.col}{Character code specifying the color of land.}

\item{...}{Additional arguments passed to \code{\link[ggOceanMaps]{basemap}}.}
}
\description{
Plot method for \code{\link[=NEMOdata]{NEMOdata}} objects.
}
\author{
Mikko Vihtakari
}
