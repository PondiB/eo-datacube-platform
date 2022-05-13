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

# Additonal set ups
gdalcubes_options(parallel = 8)

#con = connect(host = "https://openeo.cloud")

# Gdalcube global variable
stac_data <- NULL
data_cube <- NULL

#* @apiTitle EO Lightweight Platform
#* @apiDescription This service integrates STAC API, OpenEO and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* Discover available satellite imagery in your region of interest
#* @param bbox  e.g. 45.0,-20.1,47.0, -19.8
#* @param date_time e.g. 2020-01-01/2020-06-31
#* @param collection_type e.g. s2_l2a or ls8_sr
#* @get /v1/discover-data
function(bbox = "", date_time= "", collection_type = "") {
  s_obj <- stac("https://explorer.digitalearth.africa/stac")
  it_obj <- s_obj %>% stac_search(collections = "ls8_sr",
                      datetime = "2021-01-01/2021-03-31",
                      bbox = c(45.0, -20.1, 47.0, -19.8)) %>% get_request()            
}

#* Create gdalcubes for your region of interest
#* @param bbox  e.g. 45.0,-20.1,47.0, -19.8
#* @param date_time e.g. 2020-01-01/2020-06-31
#* @param collection_type e.g. s2_l2a or ls8_sr
#* @get /v1/create-gdalcubes
#* @serializer unboxedJSON
function(bbox = "", date_time= "", collection_type = "") {
  # get data from stac
  s_obj <- s_obj <- stac("https://explorer.digitalearth.africa/stac")
  it_obj <- s_obj %>% stac_search(collections = "ls8_sr",
                      datetime = "2021-01-01/2021-03-31",
                      bbox = c(45.0, -20.1, 47.0, -19.8)) %>% get_request()
  # modify global variable to contain stac data
  #stac_data <<- it_obj 
  #--------------------------------------------------------------------------------#
  # stac collection url
  stac_url <- "https://explorer.digitalearth.africa/stac/search?collection=ls8_sr&time=2022-01-01/2022-03-31&bbox=[45.0,-20.1,47.0,-19.8]"
  
  # get response from stac collection
  req <- request(stac_url)
  resp <- req_perform(req)
  resp_json <- resp_body_json(resp)
  
  
  # create image collection from stac api response
  img.col <- stac_image_collection(
    resp_json$features
  )
  
  # Define cube view with onthly aggregation, 100 Metres dimension
  v.overview = cube_view(srs="EPSG:3857", extent=img.col, dx=100, dy=100, dt = "P1M", resampling="average", aggregation="median")
  
  # Gdalcubes creation
  cube.overview = raster_cube(img.col, v.overview)
  # Assign to a global variable
  stac_data <<- cube.overview
  
  # Response JSON to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes object created successfully")
}

#* Get list of OpenEO processes
#* @get /v1/processes/open-eo
function() {
  #TO DO
  processes_list <- list("filter_bands", "filter_spatial", "ndvi", "mask", "save_result")
}

#* Run OpenEO process
#* @post /v1/run/processes/open-eo
function() {
  #TO DO
  stac_data
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
    func_parse <- parse(text = user_defined_process)
    user_function <- eval(func_parse)
}


#* Delete all files
#* @delete /v1/files
function() {
    #Delete files
}
