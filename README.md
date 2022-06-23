# Lightweight Platform for Processing Time Series Satellite Images

This service integrates STAC API, OpenEO standards and gdalcubes(Data Cubes) to be a  lightweight platform to enable processing of time series satellite images via RESTful APIs. It also supports users to run their custom R functions.


![](assets/lightweight-architecture.png)


There are about a quarter of the openEO standardized processes that deal with cube objects.
After processing the data , one can zip and export the final output to AWS S3 bucket where they can download and explore on open source tools like QGIS.
A User Interface that consumes the APIs can also be developed. Addittionally, one can extend the APIs into other services written in Java, Python, GoLang, JavaScript, etc.

![](assets/rest-api.png)


## Easy Deployment with Docker

Assuming you have Docker installed in your computing environment.
You first need to clone the repository via this command:

```bash
$ git clone https://github.com/PondiB/eo-datacube-platform.git
```

then you can change to that directory

```bash
$ cd eo-datacube-platform
```

Run it :

```bash
docker-compose up
```

Run in detached mode :

```bash
docker-compose up -d
```

Shutting it down:

```bash
docker-compose down
```


##### Docker-Compose  commands for Reference

Run it:

```bash
docker-compose up
```

Run in detached mode:

```bash
docker-compose up -d
```

Shutting it down:

```bash
docker-compose down
```


Force restart  and rebuild:

```bash
docker-compose up --build --force-recreate --no-deps -d
```
