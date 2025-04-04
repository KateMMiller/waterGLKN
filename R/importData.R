#' @title importData: Import GLKN water data package
#'
#' @description This function imports views in GLKN water data packages. The easiest way to work with the data package is to
#' download the published data package from NPS DataStore manually (irma.nps.gov/datastore), or use csvs that match the format
#' of published data packages. The latter option being for importing pre-certified for tasks like performing QC checks. The other option
#' is to download a published data package using the NPSutils package. However, the NPSutils package has a lot of dependencies,
#' including the terra package, which typically fails to install from CRAN. Once NPSutils is installed, the process is less cumbersome.
#'
#' Each csv (view) in the data package is either added to a GLKN_rivers or GLKN_lakes environment in your workspace, or to your
#' global environment based on  whether new_env = TRUE or FALSE. If you want to import both rivers and lakes data, you need to run the
#' importData() function for each, and set new_env = TRUE.
#'
#' @param type Select how to import the data package.
#' \describe{
#' \item{"csv"}{Imports the csv version of the data package views from your local machine. If selected, must provide the
#' filepath for the csvs in the filepath argument.}
#' \item{"zip"}{Imports the csv version of the data package views from your local machine as a zipped file. If selected, must provide the
#' filepath and the name of the zip file.}
#' }
#'
#' @param filepath Quoted filepath where data package files live if type = "csv" or type = "zip".
#'
#' @param protocol Quoted string of the protocol being imported. For the big rivers protocol, specify "rivers". For the
#' inland lakes protocol, specify "lakes".
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in GLKN_rivers or GLKN_lakes environment. If \code{FALSE}, stores views in global environment, with
#' the protocol added as a suffix to the view name.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Download lakes and rivers data package from DataStore
#' devtools::install_github("nationalparkservice/NPSutils") # might take a few tries to install
#' NPSutils::get_data_packages(c("2306516", "2309369"))
#'
#' path_lake = "../data/GLKN_water/2306516"
#' importData(filepath = path_lake, protocol = "lakes")
#'
#' path_river = "../data/GLKN_water/2309369"
#' importData(filepath = path_river, protocol = "rivers")
#'
#' river_zip = ("../data/GLKN_water/records-2306516.zip")
#' importData(type = 'zip', filepath = river_zip, protocol = 'rivers')
#'
#' }
#'
#' @return Assigns water csvs to specified environment
#' @export

importData <- function(type = c("csv"), filepath = NA, protocol = "rivers", new_env = TRUE){

  #-- Error handling --
  type <- match.arg(type, c("zip", "csv"))
  protocol <- match.arg(protocol, c("lakes", "rivers"))
  stopifnot(class(new_env) == 'logical')

  # check that filepath was specified for non-DSN options
  if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when type = '",
                                  type, "' option."))}
  if(!grepl("/$", filepath)){filepath <- paste0(filepath, "/")}

  if(!file.exists(filepath)){
    stop(paste0("Specified file path does not exist. ",
                ifelse(grepl("sharepoint", filepath), " Note that file paths from Sharepoint or Teams are not accessible.",
                       "")))}

  # Check if type = 'csv' was specified, but .zip file is filepath
  if(type == 'csv' & grepl(".zip", filepath)){stop("Specified a zip file in filepath. Must use type = 'zip' instead of 'csv'.")}

  # Check if type = 'zip' was specified, check that filepath ends in .zip
  if(type == 'zip' & !grepl(".zip", filepath)){stop("File path for type = 'zip' requires the name of the zip file in the path.")}


  # Create new environment if new_env = T or set env as Global
  if(new_env == TRUE){
    if(protocol == "rivers"){GLKN_rivers <<- new.env()}
    if(protocol == "lakes"){GLKN_lakes <<- new.env()
    }

  env <- if(new_env == TRUE){
    if(protocol == 'rivers'){GLKN_rivers
    } else if(protocl == "lakes"){GLKN_lakes
    } else {.GlobalEnv}
  }
  }

  # Vector of file names in filepath that end in .csv (ie the data package views)
  wq_views <- c("Characteristics", "HUC", "Locations", "Projects", "Results")

  #-- Import from csvs --
  if(type == "csv"){
    # List csvs in filepath folder
    dp_list <- list.files(filepath, pattern = ".csv")
    # Drop csvs that don't matching names in the wq_views
    dp_list <- dp_list[grepl(paste0(wq_views, collapse = "|"), dp_list)]
    # Drop date stamp (if it exists) from file name if exists in 2 steps
    dp_list_names <- gsub("[[:digit:]]+|.csv", "", dp_list)
    dp_list_names <- gsub("_$","", dp_list_names)

    miss_vws <- setdiff(dp_list_names, wq_views)

    # Check for missing views
    if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                  paste0(miss_vws, collapse = ", "))}

    if(length(dp_list) > length(wq_views)){
      stop(
        "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

    # Setup progress bar
    pb <- txtProgressBar(min = 0, max = length(dp_list), style = 3)

    # Import the file names by applying read.csv to the dp_list of file names
    # This will return one list that includes all the datasets as individual elements
    # The na.string = NA converts "NA" in data to blanks. The check.names = F won't
    # replace invalid characters (eg "+") with "."
    dp_files <- lapply(seq_along(dp_list),
                       function(x){
                         fname = dp_list[[x]]
                         setTxtProgressBar(pb, x)
                         read.csv(paste0(filepath, fname),
                                  na.string = "NA",
                                  tryLogical = TRUE,
                                  check.names = FALSE)
                       })

    # Set the names of dp_files as the shorter dp_list2 names
    dp_files <- setNames(dp_files, dp_list_names)

    # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
    # environment as separate, named objects.
    list2env(dp_files, envir = env)

    # Close progress bar
    close(pb)
  }

  if(type == "zip"){
    # Check if can read files within the zip file
    tryCatch(
      {zfiles = utils::unzip(filepath, list = T)$Name
      },
      error = function(e){stop(paste0("Unable to import specified zip file."))})

    z_list = zfiles[grepl(paste0(wq_views, collapse = "|"), zfiles)]

    # Drop date stamp (if it exists) from file name if exists in 2 steps
    z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
    z_list_names <- gsub("./", "", z_list_names)
    z_list_names <- gsub("_$","", z_list_names)

    miss_vws <- setdiff(z_list_names, wq_views)

    # Check for missing views
    if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                  paste0(miss_vws, collapse = ", "))}

    if(length(z_list) > length(wq_views)){
      stop(
        "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
    }

    # Since the missing test passed, clean up files so only includes names in view_list, but
    # maintain order in files

    # Import views now that all tests passed
    pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

    wqviews <- unzip(filepath, junkpaths = TRUE, exdir = tempdir())

    view_import <-
      lapply(seq_along(wqviews), function(x){
        setTxtProgressBar(pb,x)
        read.csv(wqviews[x], na.string = "NA", check.names = FALSE)})

    view_import <- setNames(view_import, z_list_names)
    list2env(view_import, envir = env)
    # Close progress bar
    close(pb)
  }

    # Print message in console
  print(ifelse(new_env == TRUE,
               paste0(" Import complete. Views are located in GLKN_", protocol, " environment."),
               paste0(" Import complete. Views are located in global environment.")), quote = FALSE)
}
