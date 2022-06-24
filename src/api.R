# Brian Pondi
# 20-04-2022
# API endpoints with some of the implemented openEo processes for cube objects

# Import relevant libs
library(rstac)
library(gdalcubes)
library(uuid)
library(magrittr)
library(bfast)
library(tidyverse)
library(botor)


# Additonal set ups
gdalcubes_options(parallel = 16)

# Surpress warnings
options(warn = -1)


# set workdir
setwd(".")
dir.create("data")
setwd("./data")

# gdalcube global variable
stac_items <-
  NULLdata_cube <- ""  ## OpenAPI complains of NULL objects

#* @apiTitle Lightweight Platform To Analyze Satellite Images
#* @apiDescription This service integrates STAC API, OpenEO standards and gdalcubes to be a lightweight platform to enable processing of time series satellite images.


#* List of implemented OpenEO processes
#* @get /v1/processes/open-eo/list
processes_list <- function() {
  # TO DO, Implement some openeo processes that work on a raster-cube
  # https://processes.openeo.org
  processes_list <-
    list(
      "filter_bands",
      "filter_bbox",
      "filter_spatial",
      "filter_temporal",
      "load_collection",
      "merge_cubes",
      "reduce_dimension",
      "rename_dimension",
      "rename_labels",
      "run_udf",
      "save_result"
    )
}

#* Discover available satellite imagery in your region of interest
#* @param xmin 7.1
#* @param ymin 51.8
#* @param xmax 7.2
#* @param ymax 52.8
#* @param time_range 2021-01-01/2021-06-30
#* @param collection  sentinel-s2-l2a-cogs
#* @get /v1/stac/discover_data
discover_data <-
  function(xmin = "7.1",
           ymin = "51.8",
           xmax = "7.2",
           ymax = "52.8",
           time_range = "2021-01-01/2021-06-30",
           collection = "sentinel-s2-l2a-cogs") {
    # Convert bbox values to numeric
    xmin <- as.numeric(xmin)
    ymin <- as.numeric(ymin)
    xmax <- as.numeric(xmax)
    ymax <- as.numeric(ymax)
    # Connect to STAC API and get sentinel data
    stac_object <- stac("https://earth-search.aws.element84.com/v0")
    items <- stac_object %>%
      stac_search(
        collections = collection,
        bbox = c(xmin, ymin, xmax, ymax),
        datetime = time_range
      ) %>%
      post_request() %>%
      items_fetch()
  }

#* Loads a collection and returns a processable data cube(gdalcube).
#* @param id collection Sentinel 2 COGS = sentinel-s2-l2a-cogs , Landsat 8 = landsat-8-l1-c1
#* @param spatial_extent bounding box of ROI
#* @param temporal_extent time range of interest e.g. 2021-01-01/2021-03-30
#* @param bands bands of interest e.g. B04,B08
#* @param spatial_resolution Resample value
#* @param temporal_resolution P1M = Monthly, P3M = Quarterly
#* @post /v1/processes/open-eo/load_collection
#* @serializer unboxedJSON
load_collection <-
  function(id = "sentinel-s2-l2a-cogs",
           spatial_extent = "7.1,51.8,7.2,52.8",
           temporal_extent = "2021-01-01/2021-06-30",
           bands = "B04,B08",
           spatial_resolution = "250",
           temporal_resolution = "P1M") {
    ## bbox to numeric
    spatial_extent.split <- str_split(spatial_extent, ",")
    spatial_extent.unlist <- unlist(spatial_extent.split)
    xmin <- as.numeric(spatial_extent.unlist[1])
    ymin <- as.numeric(spatial_extent.unlist[2])
    xmax <- as.numeric(spatial_extent.unlist[3])
    ymax <- as.numeric(spatial_extent.unlist[4])
    # Connect to STAC API and get sentinel data
    stac_object <- stac("https://earth-search.aws.element84.com/v0")
    items <- stac_object %>%
      stac_search(
        collections = id,
        bbox = c(xmin, ymin, xmax, ymax),
        datetime = temporal_extent
      ) %>%
      post_request() %>%
      items_fetch()
    # create image collection from stac items features
    img.col <- stac_image_collection(items$features)
    # Define cube view with monthly aggregation, 250 Metres dimension
    spatial_resolution <- as.numeric(spatial_resolution)
    v.overview <- cube_view(
      srs = "EPSG:3857",
      extent = img.col,
      dx = spatial_resolution,
      dy = spatial_resolution,
      dt = temporal_resolution,
      resampling = "average",
      aggregation = "median"
    )
    # gdalcubes creation
    cube <- raster_cube(img.col, v.overview)
    if (!is.null(bands)) {
      # split user input
      bands.split <- str_split(bands, ",")
      bands.unlist <- unlist(bands.split)
      # gdalcubes creation with band filtering
      cube <- select_bands(cube, bands.unlist)
    }

    # Assign to a global variable
    data_cube <<- cube

    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "gdalcubes object created successfully",
        cube = as_json(data_cube)
      )
  }


#* Select bands from gdalcube
#* @param bands B04,B08
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/filter_bands
filter_bands <- function(data = data_cube, bands = "B04,B08") {
  if (is.null(bands)) {
    stop("The bands values should not be empty")
  }
  # split user input
  bands.split <- str_split(bands, ",")
  bands.unlist <- unlist(bands.split)
  cube <- select_bands(data, bands.unlist)

  # Overwrite global data_cube variable
  data_cube <<- cube
  # Response msg to user
  msg <-
    list(status = "SUCCESS",
         code = "200",
         message = "gdalcubes bands filtered successfully")
}

#* Limits the data cube to the specified bounding box.
#* @param bbox 7.1,51.8,7.2,52.8
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/filter_bbox
filter_bbox <-
  function(data = data_cube, bbox = "7.1,51.8,7.2,52.8") {
    ## bbox to numeric
    bbox.split <- str_split(bbox, ",")
    bbox.unlist <- unlist(bbox.split)
    xmin <- as.numeric(bbox.unlist[1])
    ymin <- as.numeric(bbox.unlist[2])
    xmax <- as.numeric(bbox.unlist[3])
    ymax <- as.numeric(bbox.unlist[4])

    ## create sf points
    pt1 <- st_point(c(xmin, ymin))
    pt2 <- st_point(c(xmin, ymax))
    pt3 <- st_point(c(xmax, ymax))
    pt4 <- st_point(c(xmax, ymin))
    pt5 <- st_point(c(xmin, ymin))

    ## create polygon
    pts <- list(rbind(pt1, pt2, pt3, pt4, pt5))
    poly <- st_polygon(pts)
    poly <- st_sfc(poly, crs = 3857)

    # filter data cube
    cube <- filter_geom(data, poly)

    # Overwrite global data_cube variable
    data_cube <<- cube
    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "gdalcubes filtered by bounding box successfully",
        cube = as_json(data_cube)
      )
  }


#* Spatial filter using geometries.
#* @param geometries e.g. https:/.../california.geojson
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/filter_spatial
filter_spatial <-
  function(data = data_cube, geometries = "https:/.../california.geojson") {
    #read geojson url and convert to geometry
    geo.data = read_sf(geometries)
    geo.data = geo.data$geometry
    # TO DO check projections
    #filter
    cube <- filter_geom(data_cube, geo.data)

    # Overwrite global data_cube variable
    data_cube <<- cube

    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "gdalcubes filtered using geojson provided",
        cube = as_json(data_cube)
      )

  }



#* Temporal filter based on temporal intervals.
#* @param extent 2022-01-01,2022-03-30
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/filter_temporal
filter_temporal <-
  function(data = data_cube, extent = "2021-01-01,2021-03-30") {
    extent.split <- str_split(extent, ",")
    extent.unlist <- unlist(extent.split)
    cube <- select_time(data, c(extent.unlist[1], extent.unlist[2]))

    # Overwrite global data_cube variable
    data_cube <<- data_cube.time
    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "gdalcubes filtered by time interval successfully",
        cube = as_json(data_cube)
      )
  }

#* Renames a dimension in the data cube while preserving all other properties
#* @param dimension bands
#* @param target red
#* @param source B01
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/rename_dimension
rename_dimension <-
  function(data = data_cube,
           source = "B01",
           target = "red",
           dimension = "bands") {
    cube <- rename_bands(data, source = target)

    # Overwrite global data_cube variable
    data_cube <<- cube
    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "Renaming of dimension applied",
        cube = as_json(data_cube)
      )
  }

#* Rename dimension labels
#* @param dimension bands
#* @param target red,green,blue
#* @param source B01,B02,B03
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/rename_labels
rename_labels <-
  function(data = data_cube,
           dimension = "bands",
           target = "red,green,blue",
           source = "B01,B02,B03") {

  }


#* Reduce dimensions
#* @param reducer
#* @param dimension time or bands
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/reduce_dimension
reduce_dimension <-
  function(data = data_cube,
           reducer = "",
           dimension = "") {
    if (dimension == "time") {
      bands <- bands(data)$name
      bandStr <- c()

      for (i in 1:length(bands)) {
        bandStr <- append(bandStr, sprintf("%s(%s)", reducer, bands[i]))
      }
      cube <- reduce_time(data, bandStr)
    } else if (dimension == "bands") {
      cube <- apply_pixel(data, reducer, keep_bands = FALSE)
    } else {
      stop('Kindly select "time" or "bands" as dimension')
    }

    # Overwrite global data_cube variable
    data_cube <<- cube
    # Response msg to user
    msg <-
      list(
        status = "SUCCESS",
        code = "200",
        message = "Dimensions reduced successfully",
        cube = as_json(data_cube)
      )
  }


#* Merge two data cubes **Experimental
#* @param datacube2
#*  @param datacube1 **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/merge_cubes
merge_cubes <- function(datacube1 = data_cube,
                        datacube2 = "") {
  # check if they are not datacubes
  `%!in%` <- Negate(`%in%`)
  if ("cube" %!in% class(datacube1) &&
      "cube" %!in% class(datacube2)) {
    stop('Provided cubes are not of class "cube"')
  }
  # check if the datacubes have equal dimesions
  compare <-
    compare.list(dimensions(datacube1), dimensions(datacube2))
  if (FALSE %in% compare) {
    stop("Dimensions of the datacubes provided are not equal")
  }
  cube <- join_bands(c(cube1, cube2))
  # Overwrite global data_cube variable
  data_cube <<- cube
  # Response msg to user
  cube = as_json(data_cube)
  msg <-
    list(
      status = "SUCCESS",
      code = "200",
      message = "Process applied successfully",
      cube = as_json(data_cube)
    )
}


#* Run a user defined process on gdalcubes
#* @param udf User-defined function
#* @param runtime Leave it empty
#* @param data **Datacube is read in memory, Neglect param
#* @post /v1/processes/open-eo/run_udf
run_udf <- function(data = data_cube,
                    udf = "",
                    runtime = "") {
  runtime <- NULL
  # convert parsed string function to class function
  func_parse <- parse(text = udf)
  user_function <- eval(func_parse)
  # TO DO, how to identify a reduce or apply process
  if (process == "reduce") {
    results <- reduce_time(data, FUN = user_function)
  } else if (process == "apply") {
    results <- apply_pixel(data, FUN = user_function)
  }

  # Overwrite global data_cube variable
  data_cube <<- results
  # Response msg to user
  msg <-
    list(
      status = "SUCCESS",
      code = "200",
      message = "UDF  applied successfully",
      cube = as_json(data_cube)
    )
}

#* Save processed data
#* @param format GeoTIFF or NetCDF
#* @post /v1/processes/open-eo/save_result
save_result <- function(data = data_cube, format = "GeoTIFF") {
  setwd("./data")

  if (is.null(format) ||
      tolower(format) == "geotiff" || format == "") {
    write_tif(data,
              tempfile(
                pattern = "cube",
                tmpdir = getwd(),
                fileext = ".tif"
              ))
  } else if (tolower(format) == "netcdf") {
    write_ncdf(data,
               tempfile(
                 pattern = "cube",
                 tmpdir = getwd(),
                 fileext = ".nc"
               ))
  } else {
    stop("The format entered is not supported")
  }

  # Response msg to user
  msg <-
    list(status = "SUCCESS",
         code = "200",
         message = "Processed data saved successfully")
}



#* Export files to aws s3 bucket
#* @param aws_access_key_id AWS access key ID
#* @param aws_secret_access_key AWS secret access key
#* @param aws_session_token AWS temporary session token
#* @param region_name Default region when creating new connections
#* @param uri URI of an S3 object, should start with s3://, then bucket name and object key
#* @post /v1/export_to_s3
export_to_s3 <- function(aws_access_key_id = "",
                         aws_secret_access_key = "",
                         aws_session_token = "",
                         region_name = " ",
                         uri = "s3://") {
  setwd("./data")
  ## zip results
  files2zip <- dir('.', full.names = TRUE)
  zip(zipfile = 'resultzip', files = files2zip)

  ## export zipped file to s3 bucket
  botor(aws_access_key_id,
        aws_secret_access_key,
        aws_session_token,
        region_name)
  botor_client("S3", type = "s3")
  s3_upload_file(file = 'resultzip.zip', uri, content_type = mime_guess(file))

  # Response msg to user
  msg <-
    list(status = "SUCCESS",
         code = "200",
         message = "Files uploaded to S3 bucket")
}


#* Plot processed datacube
#* @get /v1/plot/datacube
#* @serializer png
function() {
  # plot images
  data_cube.plot <- data_cube %>% plot()
}

#* Delete all files
#* @delete /v1/files
delete_files <- function() {
  # delete a directory
  unlink("./data", recursive = TRUE)
  # Response msg to user
  msg <-
    list(status = "SUCCESS",
         code = "200",
         message = "All the processed files deleted")
}
