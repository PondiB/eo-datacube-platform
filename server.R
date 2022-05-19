
# Brian Pondi
# 20-04-2022
# https://www.rplumber.io/
#
library(plumber)
r <- plumb("./src/api.R")  
r$run(host = "0.0.0.0", port=8000)