% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suitable.habitat.R
\name{suitable.habitat}
\alias{suitable.habitat}
\title{Estimate a suitable habitat for a species from NEMO output}
\usage{
suitable.habitat(
  habitat.space,
  oceangr.model = NEMOdata,
  proj4 = "+init=epsg:3995",
  lat.lim = 40,
  res = 350,
  drop.crumbs = 30000,
  buffer.width = 15000,
  hexbins = 100,
  find.lim.factors = TRUE
)
}
\arguments{
\item{habitat.space}{An object from the \code{\link{habitat.space}} function.}

\item{oceangr.model}{A list of oceanographic model data. The format should follow that exemplified in the \link{NEMOdata} object.}

\item{proj4}{Character argument specifying the \link[sp]{proj4string} (projection) for the smoothed polygon output.}

\item{lat.lim}{Single numeric value defining a latitude value which is used to cut the model data.}

\item{res}{Vertical and horizontal resolution of the smoothed polygon output. Setting this parameter makes the polygons more connected cutting corners in the raster based model.}

\item{drop.crumbs}{Single numeric value specifying a threshold (area in km2) for small disconnected polygons which should be removed from the polygonized version of the suitable habitat. Set to 0 (or \code{NA}) to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function.}

\item{buffer.width}{Single numeric value defining the \link[rgeos]{gBuffer} parameter for the smoothing of polygon output.}

\item{hexbins}{A number of (xbins) used for the hexagonization. See \code{\link[hexbin]{hexbin}}.}

\item{find.lim.factors}{Logical indicating whether the function should which factors (if any) make the NEMO grid cell non-suitable habitat.}
}
\value{
Returns a list containing suitable habitat estimates in following formats: 1) \strong{\code{$raw}:} oceanographic model (\code{oceangr.model}) grid cells, 2) \strong{\code{$raster}:} rasterized cells whose resolution is defined by the \code{res} with disconnected regions smaller than the \code{drop.crumbs} parameter removed (in km2), 3) \strong{\code{$polygon}:} polygonized raster data, and 4) \strong{\code{$hexagon}:} hexagonized raster data. See \code{\link[hexbin]{hexbin}}.
}
\description{
Estimates suitable habitat for a species from European Modeling of the Ocean (NEMO) physical oceanography model output based on limitations set by temperature, salinity and bottom depth.
}
\details{
The suitable habitat is estimated using the oceanographic model (\code{oceangr.model}) grid cells. The grid cell output are further manipulated using the \code{\link{rasterize.suitable.habitat}}, \code{\link{polygonize.suitable.habitat}}, and \code{\link{hexagonize.suitable.habitat}} functions.

The \code{\link[base]{dim}}ensions of NEMO input matrices must be equal.
}
