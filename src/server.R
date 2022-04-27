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

#* @apiTitle EO Lightweight Platform
#* @apiDescription This service integrates STAC API, OpenEO and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* Discover available satellite imagery in your region of interest
#* @param bbox  e.g. 45.0,-20.1,  47.0, -19.8
#* @param date_time e.g. 2020-01-01/2020-06-31
#* @param collection_type e.g. s2_l2a or ls8_sr
#* @get /discover-data
function(bbox = "", date_time= "", collection_type = "") {
  s_obj <- s_obj <- stac("https://explorer.digitalearth.africa/stac")
  it_obj <- s_obj %>% stac_search(collections = "ls8_sr",
                      datetime = "2021-01-01/2021-12-31",
                      bbox = c(45.0,-20.1,  47.0, -19.8)) %>% get_request()
}

#* Create gdalcubes for your region of interest
#* @param msg The message to echo
#* @get /create-gdalcubes
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}


#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
