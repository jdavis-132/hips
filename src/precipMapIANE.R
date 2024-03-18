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
library(grid)
library(gridExtra)
library(cowplot)

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

hipsLocations_sf <- st_as_sf(hipsLocations, coords = c('lng', 'lat'), remove = FALSE, crs = 4326, agr = 'constant') %>%
  rowwise() %>%
  mutate(city = case_when(city=='Scottsbluff' ~ 'Scottsbluff*', 
                          city=='North Platte' ~ 'North Platte*',
                          .default = city))

nitrogenColors <- pal_brewer('seq', palette = 'YlOrRd')(3)
# show_col(nitrogenColors)
irrigationColors <- pal_brewer('seq', palette = 'Greys')(3)
# show_col(irrigationColors)

nitrogenDF3 <- tibble(val = rep(1), 
                      col = c('75', '150-175', '225-250')) %>%
  mutate(col = factor(col, levels = c('75', '150-175', '225-250')))

sbPlot <- ggplot(nitrogenDF3, aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[3], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
sbPlot

np1Plot <- ggplot(nitrogenDF3, aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[3], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
np1Plot

np2Plot <- ggplot(nitrogenDF3, aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[2], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
np2Plot

np3LNKACPlot <- ggplot(nitrogenDF3, aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[1], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
np3LNKACPlot

mvPlot <- nitrogenDF3[2, ] %>%
  ggplot(aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[1], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors[2]) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
mvPlot

nitrogenLegend <- ggplot(nitrogenDF3, aes(col, fill = col)) +
  geom_bar() +
  scale_fill_manual(values = nitrogenColors) +
  labs(fill = str_wrap('Nitrogen Fertilizer (lbs/acre)', 19)) +
  theme_void() + 
  theme(legend.position = 'right', 
        legend.text = element_text(size = 10, color = 'black'),
        text = element_text(color = 'black', size = 10))
nitrogenLegend
nitrogenLegend <- get_legend(nitrogenLegend)

irrigationLevels <- tibble(label = c('0.0', '100-200', '>200')) %>%
  mutate(label = factor(label, level = c('0.0', '100-200', '>200')))
irrigationLegend <- ggplot(irrigationLevels, aes(label, fill = label)) +
  geom_bar() + 
  scale_fill_manual(values = irrigationColors) + 
  labs(fill = str_wrap('Irrigation Provided (mm)', 19)) + 
  theme_void() +
  theme(legend.position = 'right', 
        legend.text = element_text(color = 'black', size = 10),
        text = element_text(color = 'black', size = 10))
irrigationLegend
irrigationLegend <- get_legend(irrigationLegend)
# nitrogenLegend <- grid.draw(nitrogenLegend)
# subplotHeight = 0.1
# subPlotWidth = 0.025

map <- ggplot(data = gridmetPrecip) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  geom_sf(data = hipsLocations_sf, color = 'white') + 
  geom_sf_text(data = hipsLocations_sf, aes(label = city), position = position_nudge(x = 0.05, y = -0.15), 
               size.unit = 'pt', size = 10, color = 'white') +
  scale_fill_viridis_c(direction = -1) +
  guides(fill = guide_colourbar(barwidth = 10,
                                barheight = 1)) +
  labs(fill = str_wrap("Total Precipitation (mm), November 2021 - October 2023", 28)) +
  theme_void() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 10, color = 'black'),
        text = element_text(color = 'black', size = 10)) +
  inset_element(sbPlot, left = 0.055, bottom = 0.55, right = 0.08, top = 0.65) + 
  inset_element(np1Plot, left = 0.215, bottom = 0.37, right = 0.24, top = 0.47) +
  inset_element(np2Plot, left = 0.245, bottom = 0.37, right = 0.27, top = 0.47) +
  inset_element(np3LNKACPlot, left = 0.275, bottom = 0.37, right = 0.3, top = 0.47) +
  inset_element(np3LNKACPlot, left = 0.51, bottom = 0.28, right = 0.535, top = 0.38) + 
  inset_element(mvPlot, left = 0.565, bottom = 0.47, right = 0.59, top = 0.5) +
  inset_element(np3LNKACPlot, left = 0.715, bottom = 0.6, right = 0.74, top = 0.7) +
  inset_element(np3LNKACPlot, left = 0.85, bottom = 0.39, right = 0.875, top = 0.49) 
map

legends <- plot_grid(nitrogenLegend, irrigationLegend, ncol = 1)
experimentalDesign <- plot_grid(map, legends, nrow = 1, rel_widths = c(1, 0.2))
experimentalDesign

