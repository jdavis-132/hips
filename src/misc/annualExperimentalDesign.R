library(tidyverse)
library(AOI)
library(climateR)
library(zonal)
library(sf)
library(patchwork)
# library(MoMAColors)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(cowplot)

aoi <- aoi_get(state = c('NE', 'IA'), county = 'all')

gridmet2022 <- getGridMET(AOI = aoi, varname = 'pr', startDate = '2021-11-01', endDate = '2022-10-31') %>%
  execute_zonal(geom = aoi, ID = 'fip_code') %>%
  rowwise() %>%
  mutate(accumPrecip = sum(across(starts_with('mean.pr')), na.rm = TRUE))
gridmet2023 <- getGridMET(AOI = aoi, varname = 'pr', startDate = '2022-11-01', endDate = '2023-10-31') %>%
  execute_zonal(geom = aoi, ID = 'fip_code') %>%
  rowwise() %>%
  mutate(accumPrecip = sum(across(starts_with('mean.pr')), na.rm = TRUE))
gridmet2024 <- getGridMET(AOI = aoi, varname = 'pr', startDate = '2023-11-01', endDate = '2024-10-31') %>%
  execute_zonal(geom = aoi, ID = 'fip_code') %>%
  rowwise() %>%
  mutate(accumPrecip = sum(across(starts_with('mean.pr')), na.rm = TRUE))

hipsLocations <- tibble(location = c('Scottsbluff', 'North Platte', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville', 'Clearwater'),
                lat = c(41.85, 41.08808149, 40.8606363814325, 41.66986803903, 41.9857796124525, 41.19451639818, 42.204116),
                lon = c(-103.70, -100.775524839895, -96.5982886186525, -95.94593885585, -93.6916168881725, -91.479082779405, -98.246518))


hipsLocations_sf <- st_as_sf(hipsLocations, coords = c('lon', 'lat'), remove = FALSE, crs = 4326, agr = 'constant')

nitrogenColors <- pal_brewer('seq', palette = 'YlOrRd')(3)
# show_col(nitrogenColors)
irrigationColors <- pal_brewer('seq', palette = 'Greys')(3)
# show_col(irrigationColors)

nitrogenDF3 <- tibble(val = rep(1), 
                      col = c('Low', 'Medium', 'High')) %>%
  mutate(col = factor(col, levels = c('Low', 'Medium', 'High')))

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

sbInbredsPlot <- nitrogenDF3[2, ] %>%
  ggplot(aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[3], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors[2]) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
sbInbredsPlot

npLI23Plot <- nitrogenDF3[2, ] %>%
  ggplot(aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[2], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors[2]) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
npLI23Plot

lnk24Plot <- ggplot(nitrogenDF3[1:2, ], aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[1], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
lnk24Plot

clearwaterPlot <- nitrogenDF3[3, ] %>%
  ggplot(aes(val, fill = col)) + 
  geom_bar(position = 'stack', color = irrigationColors[3], linewidth = 1) +
  scale_fill_manual(values = nitrogenColors[3]) + 
  labs(fill = '') +
  theme_void() +
  theme(legend.position = 'none')
clearwaterPlot

nitrogenLegend <- ggplot(nitrogenDF3, aes(col, fill = col)) +
  geom_bar() +
  scale_fill_manual(values = nitrogenColors) +
  labs(fill = str_wrap('Nitrogen Fertilizer', 9)) +
  theme_void() + 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(color = 'black', size = 9))
nitrogenLegend
nitrogenLegend <- get_plot_component(nitrogenLegend, 'guide-box')

irrigationLevels <- tibble(label = c('NI', 'LI', 'FI')) %>%
  mutate(label = factor(label, levels = c('NI', 'LI', 'FI')))
irrigationLegend <- ggplot(irrigationLevels, aes(label, fill = label)) +
  geom_bar() + 
  scale_fill_manual(values = irrigationColors) + 
  labs(fill = str_wrap('Irrigation Provided', 11)) + 
  theme_void() +
  theme(legend.position = 'bottom', 
        legend.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9))
irrigationLegend
irrigationLegend <- get_plot_component(irrigationLegend, 'guide-box')

legends <- plot_grid(nitrogenLegend, irrigationLegend, nrow = 1)
ggsave('src/images/experimentalDesignLegends.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')
# nitrogenLegend <- grid.draw(nitrogenLegend)
# subplotHeight = 0.1
# subPlotWidth = 0.025

map_2022 <- ggplot(data = gridmet2022) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = str_wrap("Total Precipitation (mm), November 2021 - October 2022", 10)) +
  theme_void() +
  theme(legend.position = 'right',
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(color = 'black', size = 9)) 
map_2022

map_2023 <- ggplot(data = gridmet2023) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = str_wrap("Total Precipitation (mm), November 2022 - October 2023", 10)) +
  theme_void() +
  theme(legend.position = 'right',
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(color = 'black', size = 9))
map_2023

map_2024 <- ggplot(data = gridmet2024) +
  geom_sf(aes(fill = accumPrecip), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = str_wrap("Total Precipitation (mm), November 2023 - October 2024", 10)) +
  theme_void() +
  theme(legend.position = 'right',
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(color = 'black', size = 9))
map_2024

hybrid22locs <- filter(hipsLocations_sf, location %in% c('Scottsbluff', 'North Platte', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'))

hybrids_2022 <- map_2022 + 
  geom_sf(data = hybrid22locs, color = 'white', size = 0.5) + 
  geom_sf_text(data = hybrid22locs, aes(label = location), position = position_nudge(x = 0.05, y = -0.15),
                size = 3, color = 'white') +
  inset_element(sbPlot, left = 0.055, bottom = 0.535, right = 0.08, top = 0.635) +
  inset_element(np1Plot, left = 0.215, bottom = 0.345, right = 0.24, top = 0.445) +
  inset_element(np2Plot, left = 0.245, bottom = 0.345, right = 0.27, top = 0.445) +
  inset_element(np3LNKACPlot, left = 0.275, bottom = 0.345, right = 0.3, top = 0.445) +
  inset_element(np3LNKACPlot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.38) +
  inset_element(mvPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533) +
  inset_element(np3LNKACPlot, left = 0.71, bottom = 0.575, right = 0.735, top = 0.675) +
  inset_element(np3LNKACPlot, left = 0.855, bottom = 0.37, right = 0.88, top = 0.47)
hybrids_2022
ggsave('src/images/experimentalDesign2022hybrids.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')

inbred22locs <- filter(hipsLocations_sf, location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'))

inbreds_2022 <- map_2022 + 
  geom_sf(data = inbred22locs, color = 'white', size = 0.5) + 
  geom_sf_text(data = inbred22locs, aes(label = location), position = position_nudge(x = 0.05, y = -0.15),
               size = 3, color = 'white') +
  inset_element(sbInbredsPlot, left = 0.055, bottom = 0.535, right = 0.08, top = 0.568) +
  inset_element(mvPlot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.313) +
  inset_element(mvPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533) +
  inset_element(np3LNKACPlot, left = 0.71, bottom = 0.575, right = 0.735, top = 0.675) +
  inset_element(np3LNKACPlot, left = 0.855, bottom = 0.37, right = 0.88, top = 0.47)
inbreds_2022
ggsave('src/images/experimentalDesign2022inbreds.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')

hybrid23locs <- filter(hipsLocations_sf, location %in% c('North Platte', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'))

hybrids_2023 <- map_2023 + 
  geom_sf(data = hybrid23locs, color = 'white', size = 0.5) + 
  geom_sf_text(data = hybrid23locs, aes(label = location), position = position_nudge(x = 0.05, y = -0.15),
               size = 3, color = 'white') +
  inset_element(npLI23Plot, left = 0.215, bottom = 0.345, right = 0.24, top = 0.378) +
  inset_element(mvPlot, left = 0.245, bottom = 0.345, right = 0.27, top = 0.378) +
  inset_element(np3LNKACPlot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.38) +
  inset_element(mvPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533) +
  inset_element(np3LNKACPlot, left = 0.71, bottom = 0.575, right = 0.735, top = 0.675) +
  inset_element(np3LNKACPlot, left = 0.855, bottom = 0.37, right = 0.88, top = 0.47)
hybrids_2023  
ggsave('src/images/experimentalDesign2023hybrids.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')

inbred23locs <- filter(hipsLocations_sf, location %in% c('Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'))

inbreds_2023 <- map_2023 + 
  geom_sf(data = inbred23locs, color = 'white', size = 0.5) + 
  geom_sf_text(data = inbred23locs, aes(label = location), position = position_nudge(x = 0.05, y = -0.15),
               size = 3, color = 'white') +
  inset_element(mvPlot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.313) +
  inset_element(mvPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533) +
  inset_element(np3LNKACPlot, left = 0.71, bottom = 0.575, right = 0.735, top = 0.675) +
  inset_element(np3LNKACPlot, left = 0.855, bottom = 0.37, right = 0.88, top = 0.47)
inbreds_2023  
ggsave('src/images/experimentalDesign2023inbreds.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')

hybrid24locs <- filter(hipsLocations_sf, location %in% c('Lincoln', 'Clearwater'))

hybrids_2024 <- map_2024 + 
  geom_sf(data = hybrid24locs, color = 'white', size = 0.5) + 
  geom_sf_text(data = hybrid24locs, aes(label = location), position = position_nudge(x = 0.05, y = -0.15),
               size = 3, color = 'white') +
  inset_element(lnk24Plot, left = 0.522, bottom = 0.28, right = 0.547, top = 0.346) + 
  inset_element(clearwaterPlot, left = 0.563, bottom = 0.5, right = 0.588, top = 0.533)
hybrids_2024
ggsave('src/images/experimentalDesign2024hybrids.svg', dpi = 1000, width = 12.8, height = 6.11, units = 'in')
