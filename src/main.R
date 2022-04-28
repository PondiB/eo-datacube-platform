# Brian Pondi
# 20-04-2022
# https://www.rplumber.io/
#

library(plumber)
library(rstac)
library(openeo)
library(gdalcubes)
library(uuid)
library(httr2)
library(magrittr)

con = connect(host = "https://openeo.cloud")

#* @apiTitle EO Lightweight Platform
#* @apiDescription This service integrates STAC API, OpenEO and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* Discover available satellite imagery in your region of interest
#* @param bbox  e.g. 45.0,-20.1,47.0, -19.8
#* @param date_time e.g. 2020-01-01/2020-06-31
#* @param collection_type e.g. s2_l2a or ls8_sr
#* @get /discover-data
function(bbox = "", date_time= "", collection_type = "") {
  s_obj <- s_obj <- stac("https://explorer.digitalearth.africa/stac")
  it_obj <- s_obj %>% stac_search(collections = "ls8_sr",
                      datetime = "2021-01-01/2021-03-31",
                      bbox = c(45.0, -20.1, 47.0, -19.8)) %>% get_request()
}

#* Create gdalcubes for your region of interest
#* @param bbox  e.g. 45.0,-20.1,47.0, -19.8
#* @param date_time e.g. 2020-01-01/2020-06-31
#* @param collection_type e.g. s2_l2a or ls8_sr
#* @get /create-gdalcubes
function(bbox = "", date_time= "", collection_type = "") {
  # Download data from stac

  # Create gdalcubes from downloaded data

}

#* Run a user defined process on gdalcubes
#* @param user_defined_process User-defined function
#* @post /user-defined-process
function(user_defined_process) {
    func_parse <- parse(text = user_defined_process)
    user_function <- eval(func_parse)
}


#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}
