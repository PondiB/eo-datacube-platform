# TO DO - Implement a couple of OpenEO standardized process focusing on data cubes.

library(openeo)
connect(host="https://openeo.cloud")

# list collection and processes
#colls = list_collections()
#list_processes()
p = processes()

#' filter bands
filter_bands <- function(eo_datacube, bands){
  eo_datacube %>% select_bands(c("B04","B08"))
}

filter_bbox <- function(eo_datacube){
    
}

resample_spatial <- function(eo_datacube){
    
}

save_result <- function(eo_datacube){
    
}
