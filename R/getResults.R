#' @include getLocations.R
#'
#' @title getResults: query GLKN water data
#'
#' @description Queries GLKN water data by site, year, month, parameter, etc.
#'
#' @importFrom dplyr filter
#'
#' @param park Combine data from all parks or one or more parks at a time. Valid inputs:
#'  \describe{
#'  \item{"all"}{Includes all parks in the network}
#'  \item{"APIS"}{Apostle Islands National Lakeshore}
#'  \item{"GRPO"}{Grand Portage National Monument}
#'  \item{"INDU"}{Indiana Dunes National Park}
#'  \item{"ISRO"}{Isle Royale National Park}
#'  \item{"MISS"}{Mississippi National River and Recreation Area}
#'  \item{"PIRO"}{Pictured Rocks National Lakeshore}
#'  \item{"SLBE"}{Sleeping Bear National Lakeshore}
#'  \item{"SACN"}{St. Croix National Scenic Riverway}
#'  \item{"VOYA"}{Voyageurs National Park}
#'  }
#'
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
#' \item{"VS"}{Default. GLKN Vital Signs monitoring events, which matches all Activity_Group_Type == "Field Set".}
#' \item{"QC"}{Activity_Group_Type == "QC Sample".}
#' \item{"rep"}{Activity_Group_Type == "Replicate"}
#' }
#'
#' @param years Numeric. Years to query. Accepted values start at 2007.
#'
#' @param months Numeric. Months to query by number. Accepted values range from 1:12. Default month range is
#' 4:10, as most non-QAQC sample events occur between the months of April and October.
#'
#' @param active Logical. If TRUE (Default), only returns actively monitored locations. If FALSE, returns all
#' locations that have been monitored at least once since 2007. Active sites are defined as sites that have at
#' least one sampling event between 2014 and 2024. See "./scripts/active_sites.R" for more details.
#'
#' @param parameter Specify the parameter(s) to return. Note if additional parameters are added to the Results view,
#' they need to be added as accepted values in this function. Current accepted values are:
#'    c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ppb", "ChlA_ugL", "Cl_mgL", "Depth_m",
#'      "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL", "NA_mgL",
#'      "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "SecchiCond", "SecchiDepth_m", "Si_mgL", "SO4_mgL",
#'      "SpecCond_uScm", "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
#'      "WaterLevelRef_m", "WaveHt_cm", "WaveHt_m",
#'      "WindCond", "WindDir_Deg")
#'
#' @param sample_depth Quoted string. Filters on sample depth. If "all" (default), returns all sample depths.
#' If "surface", only returns data from the uppermost sample.
#'
#' @param include_censored Logical. If TRUE, the value column includes non-censored and censored values
#' using the MDL/MRL/UQL values in the parameter flags. If the Flag column is not NA, that indicates
#' the value is a censored value. If FALSE (Default), only values with Result_Detection_Condition = "Detected and Quantified"
#' are returned in the value column.
#'
#' @param output Specify if you want all fields returned (output = "verbose") or just the most important fields
#' (output = "short"; default.)
#'
#' @return Data frame of GLKN water data
#'
#' @examples
#' \dontrun{
#' river_zip = ("../data/GLKN_water/records-2309369.zip")
#' lake_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = c(river_zip, lake_zip))
#'
#' #+++++ ADD EXAMPLES +++++
#'
#' }
#' @export

getResults <- function(park = "all", site = "all", site_type = "all",
                       sample_type = "VS",
                       years = 2007:format(Sys.Date(), "%Y"),
                       months = 4:10,
                       active = TRUE,
                       parameter = "all",
                       sample_depth = 'all',
                       include_censored = FALSE,
                       output = "short"){

  #---- Error handling ----
  park <- match.arg(park, several.ok = TRUE,
                    c("all", "APIS", "GRPO", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA"))
  if(any(park == "all")){park = c("APIS", "GRPO", "INDU", "ISRO", "MISS", "PIRO", "SLBE", "SACN", "VOYA")} else {park}

  site_type <- match.arg(site_type, several.ok = TRUE, c("all", "impound", "lake", "river"))

  sample_type <- match.arg(sample_type, several.ok = T, c("all", "VS", "QC", "rep"))

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

  stopifnot(is.logical(active))

  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2007)
  stopifnot(class(months) %in% c("numeric", "integer"), months %in% c(1:12))

  sample_depth <- match.arg(sample_depth, c("surface", "all"))

  params <- c("Alkalinity_mgL", "Ca_mgL", "ChlA_mgm3", "ChlA_Pheo_pct", "ChlA_ppb", "ChlA_ugL", "Cl_mgL", "Depth_m",
              "DO_mgL", "DOC_mgL", "DOsat_pct", "Hg_ngL", "HgMethyl_ngL", "K_mgL", "Mg_mgL", "N_ugL", "NA_mgL",
              "NH4_ugL", "NO2+NO3_ugL", "P_ugL", "pH", "SecchiCond", "SecchiDepth_m", "Si_mgL", "SO4_mgL",
              "SpecCond_uScm", "TempAir_C", "TempWater_C", "Transp_cm", "TSS_mgL", "Turbidity_NTU", "WaterLevel_m",
              "WaterLevelRef_m", "WaveHt_cm", "WaveHt_m", "WindCond", "WindDir_Deg")

  parameter <- match.arg(parameter, several.ok = T, c("all", params))
  if(any(parameter == "all")){parameter = params} else {parameter}

  sample_depth <- match.arg(sample_depth, c("all", "surface"))
  stopifnot(is.logical(include_censored))
  output <- match.arg(output, c("short", "verbose"))

  options(scipen = 10) # prevent scientific notation

  #---- compile data ----
  env <- if(exists("GLKN_WQ")){GLKN_WQ} else {.GlobalEnv}

  tryCatch({res <- get("Results", envir = env)},
           error = function(e){stop("GLKN water views not found. Please import data.")}
  )

  loc <- getLocations(park = park, site = site, site_type = site_type, active = active, output = "short")
  loc_evs <- loc$Location_ID

  res1 <- dplyr::filter(res, Location_ID %in% loc_evs)

  res1$Park_Code <- substr(res1$Location_ID, 1, 4)
  res1$sample_date <- format(as.Date(res1$Activity_Start_Date, format = "%Y-%m-%d"), "%Y-%m-%d")
  res1$year <- as.numeric(substr(res1$sample_date, 1, 4)) # faster than using format with date
  res1$month <- as.numeric(substr(res1$sample_date, 6, 7))
  res1$doy <- as.numeric(format(as.Date(res1$sample_date, format = "%Y-%m-%d"), "%j"))
  res1$samp_type <- NA_character_
  res1$samp_type[res1$Activity_Group_Type == "Field Set"] <- "VS"
  res1$samp_type[res1$Activity_Group_Type == "QC Sample"] <- "QC"
  res1$samp_type[res1$Activity_Group_Type == "Replicate"] <- "rep"

  # Handle censored values
  res1$value <- suppressWarnings(as.numeric(res1$Result_Text))
  res1$value_cen <- NA_real_
  res1$value_cen[res1$Result_Detection_Condition %in% "Detected and Quantified"] <-
    res1$value[res1$Result_Detection_Condition %in% "Detected and Quantified"]
  res1$value_cen[res1$Result_Detection_Condition %in% "Present Above Quantification Limit"] <-
    res1$Upper_Quantification_Limit[res1$Result_Detection_Condition %in% "Present Above Quantification Limit"]
  res1$value_cen[res1$Result_Detection_Condition %in% "Present Below Quantification Limit"] <-
    res1$Lower_Quantification_Limit[res1$Result_Detection_Condition %in% "Present Below Quantification Limit"]
  res1$censored <- ifelse(res1$Result_Detection_Condition %in% "Detected and Quantified", FALSE, TRUE)

  res2 <- if(any(park %in% "all")){res1
  } else {filter(res1, Park_Code %in% park)}

  res3 <- if(any(parameter %in% "all")){res2
  } else {filter(res2, param_name %in% parameter)}

  res4 <- filter(res3, year %in% years)
  res5 <- filter(res4, month %in% months)
  res6 <- if(any(sample_type %in% "all")){res5
  } else {filter(res5, samp_type %in% sample_type)}

  res7 <- if(any(sample_depth %in% "all")){res6
  } else {filter(res6, Activity_Relative_Depth %in% "surface")}

  res8 <- if(any(include_censored == TRUE)){res7
  } else {filter(res7, censored == FALSE)}

  res9 <- left_join(loc, res8, by = c("Org_Code", "Location_ID", "Park_Code"))

  res_final <- if(any(output == "verbose")){res9
  } else {res9[,c("Org_Code","Park_Code", "Project_ID", "Location_ID", "Location_Name", "site_type", "protocol",
                  "sample_date", "year", "month", "doy",
                  "Activity_Relative_Depth", "Activity_Depth", "Activity_Depth_Unit", "Activity_Group_Type", "samp_type",
                  "Characteristic_Name", "param_name", "Result_Detection_Condition",
                  "Result_Text", "value", "value_cen", "censored", "Result_Unit",
                  "Method_Detection_Limit", "Lower_Quantification_Limit", "Upper_Quantification_Limit", "Result_Comment")]}

  if(nrow(res_final) == 0){stop("Specified arguments returned an empty data frame.")}

  return(data.frame(res_final))

  } # end of fxn



