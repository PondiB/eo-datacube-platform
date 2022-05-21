source("dummy_processes.R")

library(rstac)
library(openeo)
library(gdalcubes)
library(uuid)
library(httr2)
library(magrittr)
library(bfast)

print("Brian")

add_up <- sum_up(10,8)

sub_up <-minus_up(10,8)

test_fun <- 'function(x) {
    knr <- exp(-((x["B08",]/10000)-(x["B04",]/10000))^2/(2))
    kndvi <- (1-knr) / (1+knr)   
    if (all(is.na(kndvi))) {
      return(c(NA,NA))
    }
    kndvi_ts = ts(kndvi, start = c(2016, 1), frequency = 12)
    library(bfast)
    tryCatch({
        result = bfastmonitor(kndvi_ts, start = c(2020,1), 
                              history = "all", level = 0.01)
        return(c(result$breakpoint, result$magnitude))
      }, error = function(x) {
        return(c(NA,NA))
      })
  }'

func_parse <- parse(text = test_fun)
class(func_parse)
user_function <- eval(func_parse)
class(user_function)
