library(tidyverse)
library(lubridate)
library(readxl)
library(nasapower)
library(weathermetrics)
source('src/Functions.R')

lnk24 <- read_excel('data/2024/2024_Lincoln_EH_Data_11_20_24.xlsx', 
                    sheet = 'Data', 
                    col_names = c('plotNumber', 'range', 'row', 'genotype', 'poundsOfNitrogenPerAcre', 'rep', 'totalStandCount', 'anthesisDate',
                                  'silkDate', 'chlorophyllConcentration', 'percentLodging', 'earHeight', 'tasselTipHeight', 'combineYield',
                                  'combineMoisture', 'combineTestWeight', 'notes'),
                    col_types = c(rep('numeric', 3), 'text', 'skip', rep('numeric', 3), 'date', 'date', 'numeric', 'numeric', rep('skip', 6),
                                  'numeric', 'numeric', 'skip', 'skip', rep('numeric', 3), 'text'),
                    skip = 1) %>% 
  filter(!is.na(plotNumber)) %>% 
  mutate(location = 'Lincoln',
         sublocation = 'Lincoln', 
         plantingDate = '2024-05-15',
         plotLength = 18.5,
         harvestDate = '2024-09-26',
         irrigationProvided = 0) %>%
  group_by(poundsOfNitrogenPerAcre) %>% 
  mutate(nitrogenTreatment = case_when(poundsOfNitrogenPerAcre==0 ~ 'Low', .default = 'Medium')) %>% 
  group_by(rep) %>% 
  mutate(block = case_when(nitrogenTreatment=='Low' ~ rep, .default = rep + 2)) %>% 
  ungroup() %>%
  group_by(genotype) %>%
  mutate(genotype = str_to_upper(genotype)) %>% 
  select(!rep)
  
clearwater24 <- read_excel('data/2024/2024_Clearwater_EH_Data_11_20_24.xlsx', 
                           sheet = 'Data', 
                           col_names = c('plotNumber', 'range', 'row', 'genotype', 'poundsOfNitrogenPerAcre', 'block', 'plantDensity', 
                                         'percentLodging', 'anthesisDate', 'silkDate', 'combineMoisture', 'combineYield', 'notes'),
                           col_types = c(rep('numeric', 3), 'text', 'skip', 'text', 'numeric', 'numeric', 'skip', 'numeric', 'date', 'date', 
                                         'numeric', 'numeric', 'skip', 'text')) %>% 
  filter(!is.na(plotNumber)) %>%
  mutate(location = 'Clearwater',
         sublocation = 'Clearwater',
         plantingDate = '2024-05-03',
         plotLength = 17, 
         harvestDate = '2024-10-09', 
         irrigationProvided = 16,
         poundsOfNitrogenPerAcre = str_remove(poundsOfNitrogenPerAcre, 'N') %>% 
           as.numeric(), 
         nitrogenTreatment = 'High') %>% 
  rowwise() %>% 
  mutate(totalStandCount = plantDensity*2/1000)
# Remove plots filled due to poor germination, planting errors, or complete loss due to wildlife damage
hybrids2024 <- bind_rows(lnk24, clearwater24) %>% 
  filter((str_detect(notes, 'fill', negate = TRUE) & str_detect(notes, 'empty', negate = TRUE) & str_detect(notes, 'error', negate = TRUE)) | 
           is.na(notes)) %>%
  mutate(year = 2024) %>% 
  rowwise() %>% 
  mutate(daysToAnthesis = difftime(anthesisDate, ymd(plantingDate), units = 'days'), 
         daysToSilk = difftime(silkDate, ymd(plantingDate), units = 'days'), 
         anthesisSilkingInterval = difftime(silkDate, anthesisDate, units = 'days'), 
         earHeight = cm(12*earHeight), 
         tasselTipHeight = cm(12*tasselTipHeight), 
         plantDensity = totalStandCount/2*(17.5/plotLength)*1000,
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, plotLength),
         qrCode = str_c(year, location, poundsOfNitrogenPerAcre, 'range', range, 'row', row, plotNumber, sep = '$') %>% 
           str_to_upper(), 
         yieldPerAcre = case_when(str_detect(notes, 'yield discarded') ~ NA, .default = yieldPerAcre))
# Weather Data
lnk24weather <- read_excel('data/2024/2024_Lincoln_EH_Data_11_20_24.xlsx', 
                           sheet = 'Weather_Daily', 
                           col_names = c('date', 'maxTemp', 'minTemp'), 
                           col_types = c('date', 'numeric', 'skip', 'numeric', rep('skip', 8)), 
                           skip = 1) %>% 
  mutate(location = 'Lincoln')

clearwater24weather <- read_excel('data/2024/2024_Clearwater_EH_Data_11_20_24.xlsx', 
                                  sheet = 'Weather_Daily', 
                                  col_names = c('date', 'maxTemp', 'minTemp'),
                                  col_types = c('date', 'numeric', 'skip', 'numeric', rep('skip', 8)), 
                                  skip = 1) %>% 
  mutate(location = 'Clearwater')

fieldWeather <- bind_rows(lnk24weather, clearwater24weather) %>% 
  filter(!is.na(maxTemp) & !is.na(minTemp)) %>% 
  rowwise() %>% 
  mutate(GDD = getGDDs(minTemp, maxTemp))

sites <- tibble(location = c('Lincoln', 'Clearwater'),
                lat = c(40.8606363814325,  42.204116),
                lon = c(-96.5982886186525, -98.246518))
weather.pwr <- tibble(location = NULL)
for (i in 1:length(sites$location))
{
  siteWeather24 <- get_power(community = 'ag',
                             pars = c('T2M_MAX', 'T2M_MIN'),
                             temporal_api = 'daily', 
                             lonlat = c(sites$lon[i], sites$lat[i]), 
                             dates = c('2024-01-01', '2024-12-31')) %>%
    mutate(location = sites$location[i]) %>%
    rowwise() %>%
    mutate(tmin = celsius.to.fahrenheit(T2M_MIN), 
           tmax = celsius.to.fahrenheit(T2M_MAX)) %>%
    select(location, LAT, LON, YYYYMMDD, tmin, tmax) %>%
    mutate(GDD = getGDDs(tmin, tmax)) %>%
    rename(date = YYYYMMDD,
           lat = LAT,
           lon = LON)
  weather.pwr <- bind_rows(weather.pwr, siteWeather24)
}

lnkDays <- filter(fieldWeather, location=='Lincoln')$date
clearwaterDays <- filter(fieldWeather, location=='Clearwater')$date

pwr.impute <- filter(weather.pwr, !((location=='Lincoln' & date %in% lnkDays) | 
                                      (location=='Clearwater' & date %in% clearwaterDays)))

weather24Imputed <- bind_rows(fieldWeather, pwr.impute)
write.csv(weather24Imputed, 'outData/2024ImputedWeatherData.csv', quote = FALSE, row.names = FALSE)

hybrids2024 <- hybrids2024 %>% 
  rowwise() %>%
  mutate(GDDToAnthesis = getCumulativeGDDs(plantingDate, anthesisDate, weather24Imputed, location),
         GDDToSilk = getCumulativeGDDs(plantingDate, silkDate, weather24Imputed, location)) %>%
  mutate(anthesisSilkingIntervalGDD = GDDToSilk - GDDToAnthesis) %>%
  mutate(across(c(where(is.numeric), where(is.POSIXct)), ~case_when(.==-Inf ~ NA, .default = .)))

# Remove outliers
phenotypes <- c('chlorophyllConcentration', 'earHeight', 'tasselTipHeight', 'combineYield', 'combineMoisture', 
                'combineTestWeight', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'yieldPerAcre', 
                'GDDToAnthesis', 'GDDToSilk', 'anthesisSilkingIntervalGDD')
for(p in phenotypes)
{
  printHistogram(hybrids2024, p, title = p)
}


hybrids2024 <- hybrids2024 %>% 
  rowwise() %>% 
  mutate(anthesisSilkingIntervalGDD = case_when(anthesisSilkingIntervalGDD > 400 ~ NA, .default = anthesisSilkingIntervalGDD),
         GDDToSilk = case_when(GDDToSilk > 3200 ~ NA, .default = GDDToSilk), 
         GDDToAnthesis = case_when(GDDToAnthesis > 3000 ~ NA, .default = GDDToAnthesis), 
         anthesisSilkingInterval = case_when(anthesisSilkingInterval > 8 ~ NA, .default = anthesisSilkingInterval),
         combineMoisture = case_when(combineMoisture > 22 ~ NA, .default = combineMoisture),
         chlorophyllConcentration = case_when(chlorophyllConcentration < 200 ~ NA, .default = chlorophyllConcentration),
         environment = str_c(year, location, nitrogenTreatment, sep = ' ')) %>% 
  ungroup() %>% 
  mutate(plantingDate = as.Date(plantingDate),
         harvestDate = as.Date(harvestDate),
         daysToAnthesis = as.numeric(daysToAnthesis),
         daysToSilk = as.numeric(daysToSilk),
         anthesisSilkingInterval = as.numeric(anthesisSilkingInterval))
# Data integrity checks
plotRepCorr2(hybrids2024, phenotypes = phenotypes)

# Bind to big dataframe
hips2223 <- read_csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.3.csv')

# fix genotypes & blind commercial hybrids
hips <- bind_rows(hips2223, hybrids2024) %>% 
  group_by(genotype) %>% 
  mutate(genotype = str_to_upper(genotype)) %>% 
  mutate(genotype = case_when(genotype=="HOEGEMEYER 7089 AMXT" ~ 'COMMERCIAL HYBRID 1',
                              genotype=="HOEGEMEYER 8065RR" ~ 'COMMERCIAL HYBRID 2',
                              genotype=="PIONEER 1311 AMXT" ~ 'COMMERCIAL HYBRID 3',
                              genotype %in% c("PIONEER P0589 AMXT", "PIONEER PO589AMXT") ~ 'COMMERCIAL HYBRID 4',
                              genotype=="SYNGENTA NK0659-3120-EZ1" ~ 'COMMERCIAL HYBRID 5',
                              genotype=="SYNGENTA NK0760-3111" ~ 'COMMERCIAL HYBRID 6',
                              genotype=="WYFFELS W1782" ~ 'COMMERCIAL HYBRID 7',
                              genotype=="BREVANT B06Y18Q" ~ 'COMMERCIAL HYBRID 8',
                              genotype=="WYFFELS W2080" ~ 'COMMERCIAL HYBRID 9',
                              genotype=="P1185AM" ~ "COMMERCIAL HYBRID 10", 
                                                            .default = genotype)) %>% 
  rowwise() %>% 
  mutate(harvestDate = case_when((is.na(harvestDate) & year==2022 & location=='Missouri Valley') ~ as.Date('2022-10-11'), 
                                 (is.na(harvestDate) & year==2022 & location=='Scottsbluff') ~ as.Date('2022-11-09'),
                                 (is.na(harvestDate) & year==2022 & location=='North Platte1') ~ as.Date('2022-10-26'),
                                 .default = harvestDate)) %>% 
  select(!environment) %>% 
  arrange(year, location, sublocation, block, plotNumber) %>%
  relocate(qrCode, year, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, experiment, plotLength, totalStandCount, block, row, range, plotNumber, 
           genotype, pedigreeID, plantingDate, anthesisDate, silkDate, daysToAnthesis, daysToSilk, anthesisSilkingInterval, 
           GDDToAnthesis, GDDToSilk, anthesisSilkingIntervalGDD, earHeight, flagLeafHeight, plantDensity, combineYield, yieldPerAcre, combineMoisture, combineTestWeight, 
           earLength, earFillLength, earWidth, shelledCobWidth, kernelsPerRow, kernelRowNumber, kernelsPerEar, hundredKernelMass, kernelMassPerEar, shelledCobMass, 
           percentMoisture, percentStarch, percentProtein, percentOil, percentFiber, percentAsh, kernelColor, percentLodging, harvestDate, chlorophyllConcentration, tasselTipHeight, notes)

write_csv(hips, 'outData/HIPS_HYBRIDS_2022_TO_2024_V1.csv', quote = 'needed')
