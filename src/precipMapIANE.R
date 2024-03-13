library(tidyverse)
library(AOI)
library(climateR)
library(zonal)
library(sf)
library(googleway)
library(patchwork)
library(MoMAColors)
library(viridis)
library(scales)

# API_KEY <- 'AIzaSyBx0dJsUYphK3y144m_ldAOhc7loeHZUOo'

aoi <- aoi_get(state = c('NE', 'IA'), county = 'all')
gridmetPrecip <- getGridMET(AOI = aoi, varname = 'pr', startDate = '2021-11-01', endDate = '2023-10-31') %>%
  execute_zonal(geom = aoi, ID = 'fip_code') %>%
  rowwise() %>%
  mutate(accumPrecip = sum(across(starts_with('mean.pr')), na.rm = TRUE))

hipsLocations <- data.frame(state = rep(c('Nebraska', 'Iowa'), each = 3), 
                            city = c('Scottsbluff', 'North Platte', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'))
coords <- apply(hipsLocations, 1, function(x){google_geocode(address = paste(x['city'], x['state'], sep = ', '), key = API_KEY)})
hipsLocations <- cbind(hipsLocations, do.call(rbind, lapply(coords, geocode_coordinates)))

hipsLocations_sf <- st_as_sf(hipsLocations, coords = c('lng', 'lat'), remove = FALSE, crs = 4326, agr = 'constant')

nitrogenColors <- pal_brewer('seq', palette = 'YlOrRd')(3)
# show_col(nitrogenColors)
irrigationColors <- pal_brewer('seq', palette = 'Blues')(4)
# show_col(irrigationColors)

nitrogenDF3 <- tibble(val = rep(1), col = c('Low (75 lbs/acre)', 'Medium', 'High'))

map <- ggplot(data = gridmetPrecip) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  geom_sf(data = hipsLocations_sf) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Total Precipitation (mm), November 2021 - October 2023") +
  theme_void() +
  theme(legend.position = 'top')
map