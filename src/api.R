# Brian Pondi
# 20-04-2022
#

# Import relevant libs
library(rstac)
library(openeo)
library(gdalcubes)
library(uuid)
library(httr2)
library(magrittr)
library(bfast)


# Additonal set ups
gdalcubes_options(parallel = 16)

# Import OpenEO implemented funcions
source("./R/openeo-processes.R")

#con = connect(host = "https://openeo.cloud")

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
#* @get /v1/discover-data
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

#* Create gdalcubes for your region of interest
#* @get /v1/create-gdalcubes
#* @serializer unboxedJSON
function() {
  # create image collection from stac items features
  img.col <- stac_image_collection(
    stac_items$features
  )
  
  # Define cube view with monthly aggregation, 100 Metres dimension
  v.overview = cube_view(srs="EPSG:3857", extent=img.col, dx=100, dy=100, dt = "P1M", resampling="average", aggregation="median")
  
  # Gdalcubes creation
  cube.overview = raster_cube(img.col, v.overview)
  # Assign to a global variable
  data_cube <<- cube.overview
  
  # Response JSON to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes object created successfully")
}

#* Get list of OpenEO processes
#* @get /v1/processes/open-eo
function() {
  # TO DO, Implement some openeo processes that work on a raster-cube
  # https://processes.openeo.org/#filter_bbox
  processes_list <- list("filter_bands", "filter_bbox", "resample_spatial", "save_result")
}

#* Run OpenEO process
#* @post /v1/run/processes/open-eo
function() {
  #TO DO
  stac_items
}

#* Validate a user-defined process
#* @param user_defined_process User-defined function
#* @post /v1/validate/user-defined-process
function(user_defined_process) {
  func_parse <- parse(text = user_defined_process)
  user_function <- eval(func_parse)
}


#* Run a user defined process on gdalcubes
#* @param user_defined_process User-defined function
#* @post /v1/run/user-defined-process
function(user_defined_process) {
  #focus on apply pixel for a start
    func_parse <- parse(text = user_defined_process)
    user_function <- eval(func_parse)
    results <- data_cube %>% reduce_time(names= c("test_1", "test_2"),FUN = user_function)
}


#* Delete all files
#* @delete /v1/files
function() {
    #Delete files
}
