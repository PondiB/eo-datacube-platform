FROM rocker/geospatial

RUN apt-get update 
RUN install2.r --error \
    gdalcubes \
    plumber \
    uuid \
    rstac \
    httr2 \
    magrittr

COPY ../ /

# open port 80 to traffic
EXPOSE 80

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "src/server.R"]