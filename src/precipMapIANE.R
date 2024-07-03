library(tidyverse)
library(AOI)
library(climateR)
library(zonal)
library(sf)
library(patchwork)
# library(MoMAColors)
library(viridis)
library(scales)
# library(grid)
# library(gridExtra)
library(cowplot)

aoi <- aoi_get(state = c('NE', 'IA'), county = 'all')
gridmetPrecip <- getGridMET(AOI = aoi, varname = 'pr', startDate = '2021-11-01', endDate = '2023-10-31') %>%
  execute_zonal(geom = aoi, ID = 'fip_code') %>%
  rowwise() %>%
  mutate(accumPrecip = sum(across(starts_with('mean.pr')), na.rm = TRUE))

hipsLocations <- tibble(location = c('Scottsbluff*', 'North Platte*', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'),
                lat = c(41.85, 41.08808149, 40.8606363814325, 41.66986803903, 41.9857796124525, 41.19451639818),
                lon = c(-103.70, -100.775524839895, -96.5982886186525, -95.94593885585, -93.6916168881725, -91.479082779405))


hipsLocations_sf <- st_as_sf(hipsLocations, coords = c('lon', 'lat'), remove = FALSE, crs = 4326, agr = 'constant')

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
  labs(fill = str_wrap('Nitrogen Fertilizer (lbs/acre)', 9)) +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = 24, color = 'black'),
        text = element_text(color = 'black', size = 24))
nitrogenLegend
nitrogenLegend <- get_legend(nitrogenLegend)

irrigationLevels <- tibble(label = c('0.0', '100-200', '>200')) %>%
  mutate(label = factor(label, level = c('0.0', '100-200', '>200')))
irrigationLegend <- ggplot(irrigationLevels, aes(label, fill = label)) +
  geom_bar() + 
  scale_fill_manual(values = irrigationColors) + 
  labs(fill = str_wrap('Irrigation Provided (mm)', 11)) + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.text = element_text(color = 'black', size = 24),
        text = element_text(color = 'black', size = 24))
irrigationLegend
irrigationLegend <- get_legend(irrigationLegend)
# nitrogenLegend <- grid.draw(nitrogenLegend)
# subplotHeight = 0.1
# subPlotWidth = 0.025

map <- ggplot(data = gridmetPrecip) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  geom_sf(data = hipsLocations_sf, color = 'white', size = 0.5) + 
  geom_sf_text(data = hipsLocations_sf, aes(label = location), position = position_nudge(x = 0.05, y = -0.15), 
               size = 3, color = 'white') +
  scale_fill_viridis_c(direction = -1) +
  guides(fill = guide_colourbar(barwidth = 12,
                                barheight = 1)) +
  labs(fill = str_wrap("Total Precipitation (mm), November 2021 - October 2023", 28)) +
  theme_void() +
  theme(legend.position = 'top',
        legend.text = element_text(size = 24, color = 'black'),
        text = element_text(color = 'black', size = 24)) +
  inset_element(sbPlot, left = 0.055, bottom = 0.535, right = 0.08, top = 0.635) + 
  inset_element(np1Plot, left = 0.215, bottom = 0.345, right = 0.24, top = 0.445) +
  inset_element(np2Plot, left = 0.245, bottom = 0.345, right = 0.27, top = 0.445) +
  inset_element(np3LNKACPlot, left = 0.275, bottom = 0.345, right = 0.3, top = 0.445) +
  inset_element(np3LNKACPlot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.38) + 
  inset_element(mvPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533) +
  inset_element(np3LNKACPlot, left = 0.71, bottom = 0.575, right = 0.735, top = 0.675) +
  inset_element(np3LNKACPlot, left = 0.855, bottom = 0.37, right = 0.88, top = 0.47) 
map

legends <- plot_grid(nitrogenLegend, irrigationLegend, nrow = 1)
experimentalDesign <- plot_grid(map, legends, ncol = 1, rel_heights = c(1, 0.2))
experimentalDesign

