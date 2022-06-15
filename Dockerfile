FROM rocker/r-ver:4.1.0

RUN apt-get update -qq && apt-get install -y \
    libssl-dev \
    libcurl4-gnutls-dev\
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    sqlite3 

RUN install2.r --error \
    sf \
    gdalcubes \
    plumber \
    uuid \
    rstac \
    RColorBrewer \
    bfast \
    tidyverse \
    geojsonR \
    devtools \
    magrittr

COPY . .

# open port 8000 to traffic
EXPOSE 8000

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "server.R"]