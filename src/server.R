library(plumber)
r <- plumb("main.R")  
r$run(port=8000)