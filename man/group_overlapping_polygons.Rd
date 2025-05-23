% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{group_overlapping_polygons}
\alias{group_overlapping_polygons}
\title{Group Overlapping Polygons}
\usage{
group_overlapping_polygons(polygons_sf)
}
\arguments{
\item{polygons_sf}{sf object containing polygons to be grouped}
}
\value{
sf object with added group_id column
}
\description{
Assigns group IDs to overlapping polygons in a spatial dataset
}
