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

#* @apiTitle Lightweight Platform To Analyze Satellite Images
#* @apiDescription This service integrates STAC API, OpenEO standards and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* List of implemented OpenEO processes
#* @get /v1/processes/open-eo/list
function() {
  # TO DO, Implement some openeo processes that work on a raster-cube
  # https://processes.openeo.org
  processes_list <- list("apply","filter_bands", "filter_bbox", "resample_spatial","load_collection",
                         "merge_cubes","reduce_dimension","rename_dimension","rename_labels","run_udf", "save_result","trim_cube")
}

#* Discover available satellite imagery in your region of interest
#* @param xmin 6.1
#* @param ymin 46.8
#* @param xmax 6.2
#* @param ymax 46.3
#* @param time_range 2021-01-01/2021-06-31
#* @param collection  sentinel-s2-l2a-cogs
#* @get /v1/stac/discover-data
function(xmin = "6.1", ymin = "46.8", xmax = "6.2", ymax = "46.3", time_range = "2021-01-01/2021-06-30", collection = "sentinel-s2-l2a-cogs") {
  #Convert bbox values to numeric
  xmin <- as.numeric(xmin)
  ymin <- as.numeric(ymin)
  xmax <- as.numeric(xmax)
  ymax <- as.numeric(ymax)
  #Connect to STAC API and get sentinel data
  stac_object = stac("https://earth-search.aws.element84.com/v0")
  items = stac_object %>%
    stac_search(collections = collection,
              bbox = c(xmin,ymin,xmax,ymax), 
              datetime = time_range) %>%
    post_request() %>% items_fetch() 
  # Assign to global variable for stac_items
  stac_items <<- items          
}

#* Loads a collection and returns a processable data cube(gdalcube).
#* @param collection 
#* @param bbox 
#* @param time_range 
#* @param bands
#* @param spatial_resolution
#* @post /v1/processes/open-eo/load_collection
#* @serializer unboxedJSON
function(collection ="sentinel-s2-l2a-cogs", bbox ="6.1,46.2,6.2,46.3", 
         time_range ="2021-01-01/2021-06-30", bands = "B04,B08", spatial_resolution="250",
         temporal_resolution = "P1M") {
  
  load_collection <- function(id = collection, bbox = bbox,
                              temporal_extent = time_range, bands = bands,
                              spatial_resolution=spatial_resolution,
                              temporal_resolution =temporal_resolution){
    ##bbox to numeric
    bbox.split <- str_split(bbox, ",")
    bbox.unlist <- unlist(bbox.split)
    xmin <- as.numeric(bbox.unlist[1])
    ymin <- as.numeric(bbox.unlist[2])
    xmax <- as.numeric(bbox.unlist[3])
    ymax <- as.numeric(bbox.unlist[4])
    #Connect to STAC API and get sentinel data
    stac_object = stac("https://earth-search.aws.element84.com/v0")
    items = stac_object %>%
      stac_search(collections = collection,
                  bbox = c(xmin,ymin,xmax,ymax), 
                  datetime = temporal_extent) %>%
      post_request() %>% items_fetch() 
    # create image collection from stac items features
    img.col <- stac_image_collection(
       items$features
    )
    # Define cube view with monthly aggregation, 250 Metres dimension
    spatial_resolution <- as.numeric(spatial_resolution)
    v.overview = cube_view(srs="EPSG:3857", extent=img.col,
                           dx=spatial_resolution, dy=spatial_resolution, 
                           dt = temporal_resolution, resampling="average", aggregation="median")
    # gdalcubes creation
    cube = raster_cube(img.col, v.overview)
    if(bands != ""|| ! is.null(bands)){
      #split user input
      bands.split <- str_split(bands, ",")
      bands.unlist <- unlist(bands.split)
      # gdalcubes creation with band filtering
      cube = select_bands(cube, bands.unlist)
      return(cube)
    }
    return(cube)
  }

  # gdalcube main call
  cube.overview = load_collection(id = collection, bbox = bbox,
                                  temporal_extent = time_range, bands = bands,
                                  spatial_resolution=spatial_resolution,
                                  temporal_resolution =temporal_resolution)
  # Assign to a global variable
  data_cube <<- cube.overview
  
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="gdalcubes object created successfully")
}


#* Select bands from gdalcube
#* @param bands B04,B08
#* @post /v1/processes/open-eo/filter_bands
function(bands = "") {
  # filter bands function
  filter_bands <- function(data = data, bands = bands){
    if(is.null(bands)){
      stop("The bands values should not be empty")
    }
    #split user input
    bands.split <- str_split(bands, ",")
    bands.unlist <- unlist(bands.split)
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
#* @param bbox 6.1,46.8,6.2,46.3
#* @post /v1/processes/open-eo/filter_bbox
function(bbox = "6.1,46.8,6.2,46.3"){
  #Convert to numeric
  west <- as.numeric(west)
  south <- as.numeric(south)
  east <- as.numeric(east)
  north <- as.numeric(north)
  
  # filter bbox function
  filter_bbox <- function(data, bbox){
    #TO DO
    ##bbox to numeric
    bbox.split <- str_split(bbox, ",")
    bbox.unlist <- unlist(bbox.split)
    xmin <- as.numeric(bbox.unlist[1])
    ymin <- as.numeric(bbox.unlist[2])
    xmax <- as.numeric(bbox.unlist[3])
    ymax <- as.numeric(bbox.unlist[4])
    
    ##create sf points
    pt1 <- st_point(c(xmin,ymin))
    pt2 <- st_point(c(xmin,ymax))
    pt3 <- st_point(c(xmax,ymin))
    pt4 <- st_point(c(xmax,ymax))
    pt5 <- st_point(c(xmin,ymin))
    
    pts <- list(pt1, pt2, pt3, pt4, pt5)
    
    st_polygon(as.matrix(pts))
    
  }
  #call the function
  data_cube.filt <- filter_bbox(data = data_cube,bbox)
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
    if(is.empty(data) ||  is.empty(process)){
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

#* Reduce dimensions
#* @param reducer
#* @param dimension time or bands
#* @post /v1/processes/open-eo/reduce_dimension
function(reducer="", dimension=""){
  #reduce dimensions function
  reduce_dimension <-function(data,reducer, dimension){
    if(dimension == "time") {
      
      bands = bands(data)$name
      bandStr = c()
      
      for (i in 1:length(bands)) {
        bandStr = append(bandStr, sprintf("%s(%s)", reducer, bands[i]))
      }
      
      cube = reduce_time(data, bandStr)
      return(cube)
    }
    else if (dimension == "bands") {
      
      cube = apply_pixel(data, reducer, keep_bands = FALSE)
      return(cube)
    }
    else {
      stop('Kindly select "time" or "bands" as dimension')
    }
  }
  #call the function
  data_cube.reduced <- reduce_dimension(data = data_cube, reducer, dimension)
  # rewrite cubes to the global variable
  data_cube <<- data_cube.reduced
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="Dimensions reduced successfully")
  
}


#* Merge two data cubes **Experimental
#* @param other_cube
#* @post /v1/processes/open-eo/merge_cubes
function(other_cube = ""){
  #merge cubes function
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
  data_cube.merge <- merge_cubes(data_cube, other_cube)
  
  data_cube <<- data_cube.merge
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="Process applied successfully")
  
}


#* Run a user defined process on gdalcubes
#* @param udf User-defined function
#* @post /v1/processes/open-eo/run_udf
function(udf="") {
  #focus on apply pixel or reduce time for a start
  run_udf <- function(data, udf, runtime=NULL){
    #convert parsed string function to class function
    func_parse <- parse(text = udf)
    user_function <- eval(func_parse)
    #TO DO, how to identify a reduce or apply process
    if(process =="reduce"){
      results <- reduce_time(data,FUN = user_function)
    }else if (process = "apply"){
      results <- apply_pixel(data,FUN = user_function)
    }
    return(results)
  }
  data_cube.udf <- run_udf(data_cube, udf)
  
  data_cube <<- data_cube.udf
  # Response msg to user
  msg <- list(status = "SUCCESS", code = "200",message ="UDF  applied successfully")
    
}

#* Save processed data
#* @param format TIFF or NetCDF
#* @post /v1/processes/open-eo/save_result
function(format=""){
  #save_result, Default format is tif
  save_result <- function(data,format){
    if(is.null(format) || tolower(format) =="tiff" || format =="") {
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
