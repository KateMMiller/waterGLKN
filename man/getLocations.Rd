% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getLocations.R
\name{getLocations}
\alias{getLocations}
\title{getLocations: query GLKN sampling locations}
\usage{
getLocations(
  park = "all",
  site = "all",
  site_type = "all",
  active = TRUE,
  output = "short"
)
}
\arguments{
\item{park}{Combine data from all parks or one or more parks at a time. Valid inputs:
\describe{
\item{"all"}{Includes all parks in the network}
\item{"APIS"}{Apostle Islands National Lakeshore}
\item{"INDU"}{Indiana Dunes National Park}
\item{"ISRO"}{Isle Royale National Park}
\item{"MISS"}{Mississippi National River and Recreation Area}
\item{"PIRO"}{Pictured Rocks National Lakeshore}
\item{"SLBE"}{Sleeping Bear National Lakeshore}
\item{"SACN"}{St. Croix National Scenic Riverway}
\item{"VOYA"}{Voyageurs National Park}
}}

\item{site}{Filter on Location_ID. Easiest way to pick a site. Defaults to "all". Accepted sites are below.
If new sites are added, need to be added to this function as an accepted site.

Rivers: c('INDU_04', 'MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
          'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
          'SACN_SNKE_0.5', 'SACN_STCR_43.7', 'SACN_STCR_63.8', 'SACN_STCR_89.7' 'SACN_STCR_104.0', 'SACN_STCR_138.9')

Lakes: c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_05',
         'ISRO_01', 'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09',
         'ISRO_13', 'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32',
         'PIRO_01', 'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08',
         'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0',
         'SLBE_01', 'SLBE_02', 'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11',
         'VOYA_01', 'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
         'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
         'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')

Impoundments: c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')}

\item{site_type}{Quoted string to select either inland lake or stream sites. Options are below. Note that impounded sites are not actively
monitored sites, so active = F would need to be specified to return data for those sites.
\describe{
 \item{"all"}{Includes all location types}
 \item{"lake"}{Location_types that = "Lake"}
 \item{"river"}{Location_types that = "River/Stream"}
 \item{"impound"}{Location_types that = "Riverine Impoundment"}
 }}

\item{active}{Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.}

\item{output}{Specify if you want all fields returned (output = "verbose") or just the most important fields (output = "short"; default.)}
}
\value{
Data frame of site info
}
\description{
Queries GLKN water sampling locations by park and Location ID
}
\examples{
\dontrun{
# RUN IMPORT FIRST: import both lakes and rivers data as zip files
library(waterGLKN)
river_zip = ("../data/GLKN_water/records-2309369.zip")
lake_zip = ("../data/GLKN_water/records-2306516.zip")
importData(type = 'zip', filepath = c(river_zip, lake_zip))

# Select only SLBE sites
slbe <- getLocations(park = "SLBE")

# Get Lake St. Croix sites
lkstcr <- getLocations(site = c("SACN_STCR_20.0", "SACN_STCR_15.8", "SACN_STCR_2.0"))

# Get all lakes in SACN
sacn_lk <- getLocations(park = "SACN", site_type = "lake")

}
}
