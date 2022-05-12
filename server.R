
# Brian Pondi
# 20-04-2022
# https://www.rplumber.io/
#
library(plumber)
r <- plumb("./src/main.R")  
r$run(port=8000)