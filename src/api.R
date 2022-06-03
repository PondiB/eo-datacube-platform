# Brian Pondi
# 20-04-2022
#

# Import relevant libs
library(rstac)
library(gdalcubes)
library(uuid)
library(magrittr)
library(bfast)
library(tidyverse)


# Additonal set ups
gdalcubes_options(parallel = 16)

#Surpress warnings
options(warn=-1)

# Import OpenEO implemented funcions
# source("./R/openeo-processes.R")

#set workdir
setwd(".")

# gdalcube global variable
stac_items <- NULL
data_cube <- NULL

#* @apiTitle EO Lightweight Platform
#* @apiDescription This service integrates STAC API, OpenEO and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* Discover available satellite imagery in your region of interest
#* @param xmin 6.1
#* @param ymin 46.8
#* @param xmax 6.2
#* @param ymax 46.3
#* @param datetime_range 2021-01-01/2021-06-31
#* @param collection_type sentinel-s2-l2a-cogs
#* @get /v1/stac/discover-data
function(xmin = "", ymin = "", xmax = "", ymax = "", datetime_range= "", collection_type = "") {
  #Convert bbox values to numeric
  min_x <- as.numeric(xmin)
  min_y <- as.numeric(ymin)
  max_x <- as.numeric(xmax)
  max_y <- as.numeric(ymax)
  #Connect to STAC API and get sentinel data
  stac_object = stac("https://earth-search.aws.element84.com/v0")
  items = stac_object %>%
    stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = c(6.1,46.2,6.2,46.3), 
              datetime = "2021-01-01/2021-03-31") %>%
    post_request() %>% items_fetch() 
  # Assign to global variable for stac_items
  stac_items <<- items          
}

#* Create datacube(gdalcubes) for your region of interest
#* @get /v1/processes/open-eo/load_collection
#* @serializer unboxedJSON
function() {
  # create image collection from stac items features
  img.col <- stac_image_collection(
    stac_items$features
  )
  
  # Define cube view with monthly aggregation, 100 Metres dimension
  v.overview = cube_view(srs="EPSG:3857", extent=img.col, dx=200, dy=200, dt = "P1M", resampling="average", aggregation="median")
  
  # gdalcubes creation
  cube.overview = raster_cube(img.col, v.overview)
  # Assign to a global variable
  data_cube <<- cube.overview
  
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes object created successfully")
}

#* Get list of OpenEO processes
#* @get /v1/processes/open-eo/list
function() {
  # TO DO, Implement some openeo processes that work on a raster-cube
  # https://processes.openeo.org
  processes_list <- list("apply","filter_bands", "filter_bbox", "resample_spatial","load_collection",
                         "merge_cubes","reduce_dimension","rename_dimension","rename_labels","run_udf", "save_result","trim_cube")
}

#* Select bands from gdalcube
#* @param bands B04,B08
#* @post /v1/processes/open-eo/filter_bands
function(bands = "") {

  #split user input
  bands.split <- str_split(bands, ",")
  bands.unlist <- unlist(bands.split)
    
  # filter bands function
  filter_bands <- function(data = data, bands = bands){
    if(is.null(bands)){
      stop("The bands values should not be empty")
    }
    cube = select_bands(data, bands.unlist)
    return(cube)
  }
  #call the function
  data_cube.filt <- filter_bands(data = data_cube, bands = bands)
  # rewrite filtered cubes to the global variable
  data_cube <<- data_cube.filt
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes bands filtered successfully")
}

#* Limits the data cube to the specified bounding box.
#* @param west 6.1
#* @param south 46.8
#* @param east 6.2
#* @param north 46.3
#* @post /v1/processes/open-eo/filter_bbox
function(west="", south="", east="", north=""){
  #Convert to numeric
  west <- as.numeric(west)
  south <- as.numeric(south)
  east <- as.numeric(east)
  north <- as.numeric(north)
  
  # filter bbox function
  filter_bbox <- function(data = data, extent = extent){
    #TO DO
    
  }
  #call the function
  data_cube.filt <- filter_bbox(data = data_cube, extent = extent)
  # rewrite filtered cubes to the global variable
  data_cube <<- data_cube.filt
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes filtered by bounding box successfully")

}

#* Apply a process to each pixel
#* @param process (B05-B04)/(B05+B04)
#* @post /v1/processes/open-eo/apply
function(process =""){
  # apply function
  apply <- function(data = data, process = process){
    if(is.null(data) | is.null(process) |is.empty(data) | is.empty(process)){
      stop("The cubes or process cannot be null or empty")
    }
    cube <- apply_pixel(data, process)
    return(cube)
  }
  #call the function
  data_cube.applied <- filter_bbox(data = data_cube, process)
  # rewrite filtered cubes to the global variable
  data_cube <<- data_cube.applied
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="Process applied successfully")
  
}

#* Resampling datacubes(gdalcubes)
#* @post /v1/processes/open-eo/resample_spatial
function(){

}


#* Merge two data cubes **Experimental
#* @param other_cube
#* @post /v1/processes/open-eo/merge_cubes
function(other_cube = ""){
  merge_cubes <- function(datacube1 , datacube2){
    #check if they are not datacubes
    `%!in%` <- Negate(`%in%`)
    if("cube" %!in% class(datacube1) && "cube" %!in% class(datacube1)) {
      stop('Provided cubes are not of class "cube"')
    }
    #check if the datacubes have equal dimesions
    compare = compare.list(dimensions(datacube1), dimensions(datacube2))
    if(FALSE %in% compare) {
      stop("Dimensions of the datacubes provided are not equal")
    }
    cube = join_bands(c(cube1, cube2))
    return(cube)
  }
  merge_cubes(data_cube, other_cube)
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="Process applied successfully")
  
}


#* Run a user defined process on gdalcubes
#* @param user_defined_process User-defined function
#* @post /v1/processes/open-eo/run_udf
function(user_defined_process) {
  #focus on apply pixel for a start
    func_parse <- parse(text = user_defined_process)
    user_function <- eval(func_parse)
    results <- data_cube %>% reduce_time(names= c("test_1", "test_2"),FUN = user_function)
}

#* Save processed data
#* @param format TIFF or NetCDF
#* @post /v1/processes/open-eo/save_result
function(format=""){
  #save_result, Default format is tif
  save_result <- function(data,format){
    if(is.null(format)| tolower(format) =="tiff"  | format =="") {
      write_tif(data, tempfile(pattern = "cube", tmpdir = getwd(),fileext = ".tif"))
    }else if(tolower(format) =="netcdf"){
      write_ncdf(data, tempfile(pattern = "cube", tmpdir = getwd(),fileext = ".nc"))
    }else{
      stop("The format entered is not supported")
    }
  }
  #call the function
  save_result(data = data_cube, format = format)
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="Processed data saved successfully")
  
}

#* Plot processed datacube
#* @get /v1/plot/datacube
#* @serializer png
function() {
    # plot images
    data_cube.plot <- data_cube %>% plot
}


#* Delete all files
#* @delete /v1/files
function() {
    #Delete files
}
