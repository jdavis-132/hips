library(tidyverse)
library(lubridate)
library(readxl)
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
