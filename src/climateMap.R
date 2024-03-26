library(tidyverse)
library(ggmap)
library(geoknife)
library(rasterVis)
library(raster)
# define area of interest
#for a state from a web available dataset
stencil <- webgeom('state::Nebraska, Iowa')
fabric <- webdata(list(
  times = as.POSIXct(c('1980-01-01','2022-12-31')),
  url = 'https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1855/daymet_v4_prcp_monttl_na_2020.nc',
  variables = 'prcp'))
# create job to pull data
knife <- webprocess(algorithm = list('OPeNDAP Subset' = "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))
job <- geoknife(stencil, fabric, knife, email = 'jdavis132@huskers.unl.edu', show.progress = TRUE, OUTPUT_TYPE = "geotiff", wait=TRUE)

dest <- file.path('../', 'prcp_data.zip')

file <- download(job, destination = dest, overwrite = TRUE)

unzip(file, exdir=file.path(tempdir(),'prcp'))

tiff.dir <- file.path('../','prcp')

prcp <- raster(file.path(tiff.dir , dir(tiff.dir)))

plot <- ggplot(prcp, maxpixels = 5e5) + 
  geom_tile(aes(fill = value), alpha=1) +
  scale_fill_gradientn("Precipitation (mm)", colours=rev(heat.colors(5))) +
  coord_equal() + 
  theme_classic() + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) 
plot