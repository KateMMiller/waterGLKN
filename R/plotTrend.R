#' @include getResults.R
#' @include theme_WQ.R
#'
#' @title plotTrend: Plot WQ trends over time
#'
#' @importFrom dplyr mutate select
#' @import ggplot2
#'
#' @description This function produces a line or smoothed trend plot filtered on park, site, year, month, and parameter.
#' If multiple sites are specified, they will be plotted on the same figure. If multiple parameters are specified,
#' they will be plotted on separate figures. Note that if you specify a site and parameter combination that doesn't
#' exist (e.g., a stream site and a parameter only collected in lakes), the function will return an error message
#' instead of an empty plot.
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#' \describe{
#' \item{"all"}{Includes all parks in the network}
#' \item{"APIS"}{Apostle Islands National Lakeshore}
#' \item{"GRPO"}{Grand Portage National Monument}
#' \item{"INDU"}{Indiana Dunes National Park}
#' \item{"ISRO"}{Isle Royale National Park}
#' \item{"MISS"}{Mississippi National River and Recreation Area}
#' \item{"PIRO"}{Pictured Rocks National Lakeshore}
#' \item{"SLBE"}{Sleeping Bear National Lakeshore}
#' \item{"SACN"}{St. Croix National Scenic Riverway}
#' \item{"VOYA"}{Voyageurs National Park}
#'}
#' @param site Filter on Location_ID. Easiest way to pick a site. Defaults to "all". Accepted sites are below.
#' If new sites are added, need to be added to this function as an accepted site.
#'
#' Rivers: c('MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
#'         'SACN_CLAM_0.7', 'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
#'         'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_SNKE_0.5', 'SACN_STCR_104.0', 'SACN_STCR_138.9', 'SACN_STCR_15.8',
#'         'SACN_STCR_2.0', 'SACN_STCR_20.0', 'SACN_STCR_43.7', 'SACN_STCR_53.9', 'SACN_STCR_63.8', 'SACN_STCR_89.7',
#'         'SACN_WILO_0.5')
#'
#' Lakes: c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_04', 'INDU_05', 'ISRO_01',
#'          'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09', 'ISRO_13',
#'          'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32', 'PIRO_01',
#'          'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08', 'SLBE_01', 'SLBE_02',
#'          'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11', 'VOYA_01',
#'          'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
#'          'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
#'          'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')
#'
#' @param site_type Quoted string to select either inland lake or stream sites. Options are
#' \describe{
#'  \item{"all"}{Includes all location types}
#'  \item{"lake"}{Location_types that = "Lake"}
#'  \item{"river"}{Location_types that = "River/Stream"}
#'  \item{"impound"}{Location_types taht = "River Impoundment"}
#'  }
#'
#' @param sample_type Select the sample type. Options available are below Can only choose one option.
#' \describe{
#' \item{"all"}{All possible sampling events.}
#' \item{"VS"}{Default. GLKN Vital Signs monitoring events, which matches all non-QAQC Activity_Types.}
#' \item{"QC"}{Includes only include Quality Control sampling events listed in Activity_Type.}
#' }
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
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the Lower/Upper Quantification Limits values. Censored values are indicated by censored = TRUE, and
#' are defined as records where the Result_Detection_Condition = Present Above/Below Quantification Limit.
#' If FALSE (Default), only values with Result_Detection_Condition = "Detected and Quantified"
#' are returned in the value column.
#'
#' @param sample_depth Filter on sample depth. If "all" (Default), returns all sample depths. If "surface",
#' only returns records that are either blank or "Surface" in the Activity_Relative_Depth column of the Results view.
#' If there are multiple surface samples for a given site, date, parameter, the median value will be used.
#'
#' @param layers Options are "points" and "lines". By default, both will plot.
#'
#' @param palette Theme to plot points and lines. Options include 'viridis' (Default- ranges of blue,
#' green and yellow), magma (yellow, red, purple), plasma (brighter version of magma), turbo (rainbow),
#' or specify a vector of colors manually. If fewer colors than parameters are specified, they will be
#' ramped to generate enough colors.
#'
#' @param threshold Logical. If TRUE (Default), will plot a dashed (upper) or dotted (lower) line if a water
#' quality threshold exists for that parameter and site. If FALSE, no threshold line will be plotted.
#' *NOT CURRENTLY ENABLED*
#'
#' @param smooth Logical. If TRUE (Default), will plot a loess smoothed line. If FALSE, will plot actual line. Only
#' plots if layers argument includes 'lines'.
#'
#' @param span Numeric. Determines how smoothed the line will be for smooth = TRUE. Default is 0.3. Higher spans (up to 1)
#' cause more smoothing. Span can range from 0 to 1.
#'
#' @param legend_position Specify location of legend. To turn legend off, use legend_position = "none" (Default). Other
#' options are "top", "bottom", "left", "right".
#'
#' @param numcol Specify number of columns in the facet wrap, which is only enabled when either multiple years
#' are specified or multiple parks. Default is 2.
#'
#' @param gridlines Specify whether to add gridlines or not. Options are c("none" (Default), "grid_y", "grid_x", "both")
#'
#' @examples
#' \dontrun{
#'
#' #++++ UPDATE FOR GLKN ++++
#' # Plot smoothed surface pH for ISRO_01 for all years
#' plotTrend(site = "ISRO_01", parameter = "pH", palette = 'red')
#'
#' # Plot smoothed surface pH for Eagle Lake for all years, removing the legend and using span of 0.75.
#' plotTrend(site = "ACEAGL", parameter = "pH", span = 0.75)
#'
#' # Plot smoothed Secchi Depth in Jordan Pond for all years, including the legend,
#' # different color palette, and using span of 0.75.
#' plotTrend(site = "ACJORD", parameter = "SDepth_m", span = 0.75, palette = 'Set1')
#'
#' # Plot smoothed surface pH for active SARA streams over all years with 0.6 span.
#' plotTrend(park = "SARA", site = c("SARASA", "SARASC", "SARASD"), site_type = "stream",
#'           parameter = "pH", legend_position = "right", span = 0.6)
#'
#' # Plot smoothed surface SO4 for all MIMA streams over all years with 0.6 span
#' plotTrend(park = "MIMA", site_type = "stream",
#'           parameter = "SO4_ueqL", legend_position = "right", span = 0.6)
#'
#' # Plot non-smoothed surface of multiple Sonde parameters for all MIMA streams over all
#' # years with 0.6 span.
#' params <- c("Temp_F", "SpCond_uScm", "DOsat_pct", "pH")
#' plotTrend(park = "MIMA", site_type = "stream",
#'           parameter = params, legend_position = "right", span = 0.6)
#'
#'}
#'
#' @return Returns a ggplot object of specified parameter trends.
#'
#' @export
#'
plotTrend <- function(park = "all",
                      site = "all",
                      site_type = "all",
                      sample_type = "VS",
                      years = 2007:format(Sys.Date(), "%Y"),
                      months = 4:11,
                      active = TRUE,
                      parameters = NA,
                      sample_depth = "surface",
                      include_censored = FALSE,
                      layers = c("points", "lines"),
                      palette = "viridis",
                      span = 0.3,
                      facet_site = TRUE,
                      facet_scales = "free_y",
                      legend_position = 'none',
                      numcol = 2,
                      gridlines = "none"){

  # park = 'all'; site = 'all'; site_type = 'all'; years = 2013:2023;
  # parameter = c("ANC", "pH_Lab", "pH", "Temp_C"); ... = NULL

  #-- Error handling --
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "GRPO", "INDU", "ISRO", "PIRO", "SACN", "SLBE", "VOYA"))

  Rivers <- c('MISS_UM814', 'MISS_UM822', 'MISS_UM852', 'MISS_UM862', 'MISS_UM868', 'MISS_UM880', 'SACN_APLE_0.5',
              'SACN_CLAM_0.7', 'SACN_KINI_2.2', 'SACN_NAKA_4.8', 'SACN_NAKA_41.3', 'SACN_NAKA_74.5', 'SACN_NAKA_84.6',
              'SACN_PACQ_SP_01', 'SACN_PHIP_SP_01', 'SACN_SNKE_0.5', 'SACN_STCR_104.0', 'SACN_STCR_138.9', 'SACN_STCR_15.8',
              'SACN_STCR_2.0', 'SACN_STCR_20.0', 'SACN_STCR_43.7', 'SACN_STCR_53.9', 'SACN_STCR_63.8', 'SACN_STCR_89.7',
              'SACN_WILO_0.5')
  Lakes <- c('APIS_01', 'APIS_02', 'APIS_03', 'APIS_04', 'INDU_01', 'INDU_02', 'INDU_04', 'INDU_05', 'ISRO_01',
             'ISRO_02', 'ISRO_03', 'ISRO_04', 'ISRO_05', 'ISRO_06', 'ISRO_07', 'ISRO_08', 'ISRO_09', 'ISRO_13',
             'ISRO_14', 'ISRO_19', 'ISRO_20', 'ISRO_21', 'ISRO_22', 'ISRO_24', 'ISRO_30', 'ISRO_32', 'PIRO_01',
             'PIRO_02', 'PIRO_03', 'PIRO_04', 'PIRO_05', 'PIRO_06', 'PIRO_07', 'PIRO_08', 'SLBE_01', 'SLBE_02',
             'SLBE_03', 'SLBE_04', 'SLBE_05', 'SLBE_07', 'SLBE_08', 'SLBE_09', 'SLBE_10', 'SLBE_11', 'VOYA_01',
             'VOYA_02', 'VOYA_03', 'VOYA_04', 'VOYA_05', 'VOYA_06', 'VOYA_07', 'VOYA_08', 'VOYA_09', 'VOYA_10',
             'VOYA_11', 'VOYA_12', 'VOYA_13', 'VOYA_14', 'VOYA_15', 'VOYA_16', 'VOYA_17', 'VOYA_18', 'VOYA_19',
             'VOYA_20', 'VOYA_21', 'VOYA_22', 'VOYA_23', 'VOYA_24', 'VOYA_25')

  site <- match.arg(site, several.ok = TRUE, c("all", Rivers, Lakes))
  if(any(site == "all")){site = c(Rivers, Lakes)} else {site}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))
  sample_type <- match.arg(sample_type, c("all", "VS", "QC"))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))
  stopifnot(class(active) == "logical")
  sample_depth <- match.arg(sample_depth, c("all", "surface"))
  stopifnot(class(include_censored) == "logical")
  layers <- match.arg(layers, c("points", "lines"), several.ok = TRUE)
  stopifnot(class(span) %in% "numeric")
  stopifnot(class(facet_site) == "logical")
  facet_scales <- match.arg(facet_scales, c("fixed", "free_y", "free_x", "free"))
  legend_position <- match.arg(legend_position, c("none", "bottom", "top", "right", "left"))
  stopifnot(class(numcol) %in% c("numeric", "integer"))
  gridlines <- match.arg(gridlines, c("none", "grid_y", "grid_x", "both"))

  #-- Compile data for plotting --
  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL",
              "Na_mgL", "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "Secchi_m", "Si_mgL", "SO4_mgL", "SpecCond_uScm",
              "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaveHt_cm", "WaveHt_m", "WindDir_Deg")

  if(any(!parameters %in% params)){
    stop("At least one specified parameter is not an accepted value.")}

  wdat <-
    getResults(park = park, site = site, site_type = site_type, sample_type = "VS", years = years,
               months = months, active = active, parameter = parameters[1], sample_depth = sample_depth,
               include_censored = FALSE) |>
    group_by(Location_ID, year, month, sample_date, doy, Activity_Relative_Depth, param_name, censored) |>
    summarize(value = median(value, na.rm = T), .groups = 'drop')
  # Drop NAs (often from params that only have censored data and censored = F)

  wdat <- wdat[!is.na(wdat$value),]

  wdat$param_label <- ifelse(grepl("_", wdat$param_name),
                             paste0(gsub("_", " (", wdat$param_name), ")"),
                             paste0(wdat$param_name)
  )

  # join wdat with WQ thresholds, stored as a dataset in the package
  if(nrow(wdat) == 0){stop("Combination of sites, years, and parameters returned a data frame with no records.")}

  #-- Set up plotting features --
  ylabel <- ifelse(length(unique(wdat$param_label)) == 1, unique(wdat$param_label), "Value")
  wdat_cens <- wdat |> filter(censored == TRUE)

  head(wdat)

  wdat$sample_date <- as.Date(wdat$sample_date, format = c("%Y-%m-%d"))

  # Code below is to make an x-axis that can be generalized across a range of years/months
  # to have logical tick placement and labels, and to not include Nov-Apr, when when
  # samples aren't collected

  # May 1 doy = 122; Oct 31 doy = 305
  wdat$doy_norm <- (wdat$doy - 122)/(305-122)
  wdat$x_axis <- wdat$year - years[1] + wdat$doy_norm

  year_len <- length(unique(years))
  mon_len <- length(unique(months))

  wdat$mon <- factor(format(wdat$sample_date, "%b"), month.abb, ordered = TRUE)
  wdat$mon <- wdat$mon[,drop = T]

  # Expand year and months to include missed periods that make x-axis funky
  time_mat1 <- expand.grid(year = years, month = months) |> arrange(year, month)
  time_mat1$mon <- factor(time_mat1$month, levels = time_mat1$month, labels = month.abb[time_mat1$month], ordered = TRUE)
  time_mat1$mon <- time_mat1$mon[,drop = T]
  time_mat1$date <- as.Date(paste0(time_mat1$year, "-", time_mat1$month, "-", "01"), format = c("%Y-%m-%d"))
  time_mat1$doy <-  as.numeric(strftime(time_mat1$date, format = "%j"))
  time_mat1$doy_norm <- (time_mat1$doy - 122)/(305-122)
  time_mat1$x_axis <- time_mat1$year - years[1] + time_mat1$doy_norm

  time_mat <- time_mat1 |> filter(date <= max(wdat$sample_date)) # drop dates not included in wdat2
  time_mat$x_label <- if(year_len == 1){as.character(paste0(time_mat$mon, "-01"))
    } else if(year_len %in% c(2, 3, 4)){paste0(time_mat$mon, "-", time_mat$year)
    } else {(time_mat$year)}

  x_row_breaks <- if(year_len == 1){c(1:6)
  } else if(year_len %in% 2){seq(1, year_len * mon_len , 2)
  } else if(year_len %in% c(3:5)){seq(1, year_len * mon_len, 3)
  } else if(year_len %in% c(5:10)){seq(1, year_len * mon_len, mon_len)
          } else if(year_len > 10){seq(1, year_len * mon_len, mon_len*2)}

  xbreaks <- time_mat$x_axis[x_row_breaks]
  xlabs <- time_mat$x_label[x_row_breaks]

  vir_pal = ifelse(palette %in%
                     c("viridis", "magma", "plasma", "turbo", "mako", "rocket", "cividis", "inferno"),
                   "viridis", "colbrew")

  vir_pal = ifelse(palette %in%
                     c("viridis", "magma", "plasma", "turbo", "mako", "rocket", "cividis", "inferno"),
                   "viridis", "colbrew")

  pal <-
    if(any(vir_pal == "colbrew")){
      colorRampPalette(palette)(length(parameters) * length(unique(wdat$Location_ID)))
      #rep(colorRampPalette(palette)(length(unique(parameter))), times = length(parameter) * length(unique(wdat2$SiteCode)))
    }

  #-- Create plot --
  trendplot <-
    ggplot(wdat, aes(x = x_axis, y = value, group = {if(smooth == TRUE){Location_ID} else {year}},
                      color = Location_ID, fill = Location_ID)) +
    #layers
    {if(smooth == TRUE) geom_smooth(aes(text = paste0("Site: ", Location_ID, "<br>")),
                                    method = 'loess', formula = 'y ~ x', se = F, span = span) } +
    {if(smooth == FALSE & any(layers %in% "lines")) geom_line(aes(text = paste0("Site: ", Location_ID, "<br>")))} +
    {if(any(layers %in% "points") & include_censored == TRUE){
      geom_point(aes(text = paste0("Site: ", Location_ID, "<br>",
                                   "Parameter: ", param_label, "<br>",
                                   "Value: ", round(value, 1), "<br>"),
                     shape = censored),
                 alpha = 0.4, size = 2.5)}} +
    {if(any(layers %in% "points") & include_censored == TRUE){
      scale_shape_manual(values = c(16, 8), name = "Censored Values")}} +
    {if(any(layers %in% "points") & include_censored == FALSE){
      geom_point(aes(text = paste0("Site: ", Location_ID, "<br>",
                                   "Parameter: ", param_label, "<br>",
                                   "Value: ", round(value, 1), "<br>")),
                 alpha = 0.4, size = 2.5, shape = 16)}} +
    # {if(threshold == TRUE){geom_hline(aes(yintercept = UpperThreshold, linetype = "Upper WQ Threshold"), lwd = 0.7)}} +
    # {if(threshold == TRUE){geom_hline(aes(yintercept = LowerThreshold, linetype = "Lower WQ Threshold"), lwd = 0.7)}} +
    # {if(threshold == TRUE){scale_linetype_manual(values = c("dashed", "solid"))}} +
    # facets
    {if(length(unique(wdat$param_label))>1) facet_wrap(~param_label, scales = 'free_y', ncol = numcol)} +
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
    # color palettes
    {if(any(vir_pal == "viridis")) scale_color_viridis_d(option = palette)} +
    {if(any(vir_pal == "viridis")) scale_fill_viridis_d(option = palette)} +
    {if(any(vir_pal == "colbrew")) scale_fill_manual(values = pal)} +
    {if(any(vir_pal == "colbrew")) scale_color_manual(values = pal)} +
    #axis format
    scale_x_continuous(breaks = xbreaks,
                       labels = xlabs,
                       limits = c(0, max(wdat$x_axis))) +
    scale_y_continuous(n.breaks = 8) +
    # labels
    #labs(x = "Year", y = ylab) +
    labs(x = NULL, y = ylabel) +
    guides(fill = guide_legend(order = 1),
           color = guide_legend(order = 1),
           shape = guide_legend(order = 1))

  suppressWarnings(trendplot)

}


