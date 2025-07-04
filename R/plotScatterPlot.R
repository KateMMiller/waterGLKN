#' @include getResults.R
#' @include theme_WQ.R
#'
#' @title plotScatterPlot: Plot scatterplot of 2 variables
#'
#' @importFrom dplyr full_join group_by mutate summarize select
#' @import ggplot2
#'
#' @description This function produces points or loess smoothed lines of 2 variables, filtered on park, site, year,
#' month, and 2 parameters. If multiple sites are specified, they will be plotted on the same figure, unless facet_site = T.
#' Note that if you specify a site and parameter combination that doesn't exist (e.g., a stream site and a parameter
#' only collected in lakes), the function will return an error message instead of an empty plot. This function only works
#' with surface measurements. If more than one measurement occurs for a give site, day, parameter, and Activity_Depth_Group,
#' the median of the values will be used. Only plots measurements that are part of Vital Signs monitoring (i.e. no QC records),
#' and non-censored records.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APIS"}{Apostle Islands National Lakeshore}
#' \item{"INDU"}{Indiana Dunes National Park}
#' \item{"ISRO"}{Isle Royale National Park}
#' \item{"MISS"}{Mississippi National River and Recreation Area}
#' \item{"PIRO"}{Pictured Rocks National Lakeshore}
#' \item{"SLBE"}{Sleeping Bear National Lakeshore}
#' \item{"SACN"}{St. Croix National Scenic Riverway}
#' \item{"VOYA"}{Voyageurs National Park}
#' }
#' @param site Filter on Location_ID. Easiest way to pick a site. Defaults to "all". Accepted sites are below.
#' If new sites are added, need to be added to this function as an accepted site.
#'
#' Rivers: c('INDU_04', 'MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
#'           'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
#'           'SACN_SNKE_0.5', 'SACN_STCR_43.7', 'SACN_STCR_63.8', 'SACN_STCR_89.7' 'SACN_STCR_104.0', 'SACN_STCR_138.9')
#'
#' Lakes: c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_05',
#'           'ISRO_01', 'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09',
#'           'ISRO_13', 'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32',
#'           'PIRO_01', 'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08',
#'           'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0',
#'           'SLBE_01', 'SLBE_02', 'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11',
#'           'VOYA_01',
#'           'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
#'           'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
#'           'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')
#'
#' Impoundments: c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')
#'
#' @param site_type Quoted string to select either inland lake or stream sites.  Note that impounded sites are not actively
#' monitored sites, so active = F would need to be specified to return data for those sites. Options are:
#' \describe{
#'  \item{"all"}{Includes all location types}
#'  \item{"lake"}{Location_types that = "Lake"}
#'  \item{"river"}{Location_types that = "River/Stream"}
#'  \item{"impound"}{Location_types that = "Riverine Impoundment"}
#'  }
#'
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Note that most of the
#' events are between months 4 and 11, and these are set as the defaults.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameters Specify the two parameters to plot. The first parameter will be treated as the response (y).
#' The second parameter will be treated as the explanatory (x) variable. Current accepted values are:
#'     c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
#'       "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
#'       "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
#'       "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
#'       "WaveHt_cm", "WaveHt_m", "WindDir_Deg").
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns records that are either blank or "Surface" in the Activity_Relative_Depth column of the Results view.
#' If there are multiple surface samples for a given site, date, parameter, the median value will be used.
#'
#' @param layers Options are "points", "line", and "smooth". By default, both points and a smoothed line will plot.
#' If "smooth" specified, will plot a loess smoothed line. See span for more details. If only points specified, will
#' return a scatterplot. If 'line' is specified, a linear line will be plotted.
#'
#' @param palette Theme to plot points and lines. Options include 'viridis' (Default- ranges of blue,
#' green and yellow), magma (yellow, red, purple), plasma (brighter version of magma), turbo (rainbow),
#' or specify a vector of colors manually. If fewer colors than parameters are specified, they will be
#' ramped to generate enough colors.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing. Span can range from 0 to 1.
#'
#' @param facet_site Logical. If TRUE (default), will facet on site if multiple sites specified. If FALSE, will plot all sites
#' on the same figure. Only enabled when multiple sites specified.
#'
#' @param facet_scales Specify whether facet axes should be fixed (all the same; default) or "free_y", "free_x" or "free" (both).
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when facet_site = T. Default is 2.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' # RUN IMPORT FIRST: import both lakes and rivers data as zip files
#' library(waterGLKN)
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' # Plot Temp vs DO for VOYA all years on same figure
#' plotScatterPlot(park = "VOYA", parameters = c("DO_mgL", "TempWater_C"),
#'   palette = 'viridis', facet_site = F, legend_position = "bottom")
#'
#' # Plot Temp vs DO for VOYA all years on separate figures
#' plotScatterPlot(park = "VOYA", parameters = c("DO_mgL", "TempWater_C"),
#'   palette = 'viridis', facet_site = T, legend_position = "bottom")
#'
#' # Plot Secchi depth vs. surface ChlA in SLBE sites
#' plotScatterPlot(park = "SLBE", parameters = c("Secchi_m", "ChlA_ugL"),
#'   span = 0.9, facet_site = F, legend_position = 'bottom')
#'
#' # Plot all depths of ISRO_07 for temp vs ChlA using a linear instead of smoothed line
#' plotScatterPlot(site = "ISRO_07", sample_depth = "all", parameters = c("ChlA_ugL", "TempWater_C"),
#' layers = c("points", "line"))
#'
#' # Plot surface measurements of DO and temp for ISRO_07 and include smoothed and linear line.
#' plotScatterPlot(site = "ISRO_07", parameters = c("DOsat_pct", "TempWater_C"),
#'  layers = c("points", "line", 'smooth'))
#'
#'}
#'
#' @return Returns a ggplot scatterplot object
#'
#' @export
#'
plotScatterPlot <- function(park = "all",
                            site = "all",
                            site_type = "all",
                            sample_type = "VS",
                            years = 2007:format(Sys.Date(), "%Y"),
                            months = 4:11,
                            active = TRUE,
                            parameters = NA,
                            sample_depth = "surface",
                            #include_censored = FALSE,
                            layers = c("points", "smooth"),
                            palette = "viridis",
                            span = 0.3,
                            facet_site = TRUE,
                            facet_scales = "free_y",
                            legend_position = 'none',
                            numcol = 2,
                            gridlines = "none"){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #--- Error handling ---
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "INDU", "ISRO", "PIRO", "SACN", "SLBE", "VOYA"))

  Rivers <- c('INDU_04', 'MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
              'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
              'SACN_SNKE_0.5', 'SACN_STCR_43.7', 'SACN_STCR_63.8', 'SACN_STCR_89.7', 'SACN_STCR_104.0', 'SACN_STCR_138.9')

  Lakes <- c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_05',
            'ISRO_01', 'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09',
            'ISRO_13', 'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32',
            'PIRO_01', 'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08',
            'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_STCR_2.0', 'SACN_STCR_15.8', 'SACN_STCR_20.0',
            'SLBE_01', 'SLBE_02', 'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11',
            'VOYA_01', 'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
            'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
            'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')

  Impoundments <- c('SACN_CLAM_0.7', 'SACN_STCR_53.9', 'SACN_WILO_0.5')

  site <- match.arg(site, several.ok = TRUE, c("all", Rivers, Lakes, Impoundments))
  if(any(site == "all")){site = c(Rivers, Lakes, Impoundments)} else {site}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))
  sample_type <- match.arg(sample_type, c("all", "VS", "QC"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  stopifnot(length(parameters) == 2)
  sample_depth <- match.arg(sample_depth, c("all", "surface"))
  #stopifnot(class(include_censored) == "logical")
  layers <- match.arg(layers, c("points", "smooth", "line"), several.ok = TRUE)
  stopifnot(class(span) %in% "numeric")
  stopifnot(class(facet_site) == "logical")
  facet_scales <- match.arg(facet_scales, c("fixed", "free_y", "free_x", "free"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))

  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
              "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
              "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaveHt_cm", "WaveHt_m", "WindDir_Deg")

  typo <- parameters[(!parameters %in% params)]

  if(any(!parameters %in% params)){
    stop(paste0("The following parameters are not accepted values: ", paste0(typo, collapse = ","), "\n"),
         "Accepted values: ", paste0(params, collapse = ", "))}

  #-- Compile data for plotting --
  wdat_p1 <-
    getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
               months = months, active = active, parameter = parameters[1], sample_depth = sample_depth,
               include_censored = FALSE) |>
    group_by(Location_ID, year, month, sample_date, doy, Activity_Relative_Depth, param_name, censored) |>
    summarize(value = median(value, na.rm = T), .groups = 'keep')

  wdat_p2 <-
    getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
               months = months, active = active, parameter = parameters[2], sample_depth = sample_depth,
               include_censored = FALSE) |>
    group_by(Location_ID, year, month, sample_date, doy, Activity_Relative_Depth, param_name, censored) |>
    summarize(value = median(value, na.rm = T), .groups = 'keep')

  # When sample_depth = "surface", and there are multiple measurements for a given site, date and parameter, take the median
  wdat <- full_join(wdat_p1, wdat_p2, by = c("Location_ID", "year", "month", "doy", 'sample_date'),
                    suffix = c("_y", "_x"))

  # Drop NAs for params not sampled every month
  wdat <- wdat[!with(wdat, is.na(value_y) | is.na(value_x)),]

  if(nrow(wdat) == 0){stop("Combination of sites, years, and parameters returned a data frame with no records.")}

  y_lab <- ifelse(grepl("_", parameters[1]), paste0(gsub("_", " (", parameters[1]), ")"), paste0(parameters[1]))
  x_lab <- ifelse(grepl("_", parameters[2]), paste0(gsub("_", " (", parameters[2]), ")"), paste0(parameters[2]))

  #-- Set up plotting features --
  vir_pal = ifelse(palette %in%
                     c("viridis", "magma", "plasma", "turbo", "mako", "rocket", "cividis", "inferno"),
                   "viridis", "colbrew")

  pal <-
    pal <-
    if(any(vir_pal == "colbrew")){
      colorRampPalette(palette)(length(unique(wdat$Location_ID)))
      #rep(colorRampPalette(palette)(length(unique(parameter))), times = length(parameter) * length(unique(wdat2$SiteCode)))
    }

  facetsite <- ifelse(length(unique(wdat$Location_ID)) > 1 & facet_site == TRUE, TRUE, FALSE)

  #-- Create plot --
  wdat$censored <- ifelse(wdat$censored_x + wdat$censored_y > 0, TRUE, FALSE)

  scatplot <-
      ggplot(wdat, aes(x = value_x, y = value_y, group = Location_ID, color = Location_ID, fill = Location_ID)) +
      # layers
      {if(any(layers %in% "smooth")) geom_smooth(aes(text = paste0("Site: ", Location_ID, "<br>")),
                                      method = 'loess', formula = 'y ~ x', se = F, span = span) } +
      {if(any(layers %in% "line")) geom_smooth(aes(text = paste0("Site: ", Location_ID, "<br>")),
                                               method = 'lm', formula = 'y ~ x', se = F) } +
      geom_point(aes(text = paste0("Site: ", Location_ID, "<br>",
                                     "X Variable: ", x_lab, "<br>",
                                     "X: ", round(value_x, 1), "<br>",
                                     "Y Variable: ", y_lab, "<br>",
                                     "Y: ", round(value_y, 1), "<br>")),
                 alpha = 0.4, size = 2.5) +
      # geom_point(aes(shape = censored), alpha = 0.4, size = 2.5) +
      # scale_shape_manual(values = c(16, 8), name = "Censored") +
      # facets
      {if(facetsite == TRUE) facet_wrap(~Location_ID, scales = 'free_y', ncol = numcol)} +
      # themes
      theme_WQ() +
      theme(legend.position = legend_position,
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      {if(any(gridlines %in% c("grid_y", "both"))){
          theme(panel.grid.major.y = element_line(color = 'grey'))}} + #,
                #panel.grid.minor.y = element_line(color = 'grey'))}}+
      {if(any(gridlines %in% c("grid_x", "both"))){
          theme(panel.grid.major.x = element_line(color = 'grey'))}} + #,
                #panel.grid.minor.x = element_line(color = 'grey'))}}+
      # palettes
      {if(any(vir_pal == "viridis")) scale_color_viridis_d(option = palette)} +
      {if(any(vir_pal == "viridis")) scale_fill_viridis_d(option = palette)} +
      {if(any(vir_pal == "colbrew")) scale_fill_manual(values = pal)} +
      {if(any(vir_pal == "colbrew")) scale_color_manual(values = pal)} +
      #axis format
      scale_y_continuous(n.breaks = 8) +
      # labels
      #labs(x = "Year", y = ylab) +
      labs(x = x_lab, y = y_lab) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 1),
             shape = guide_legend(order = 1))


 return(#suppressWarnings(
   scatplot
   )
  #)
}


