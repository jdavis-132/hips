library(tidyverse)
source('src/Functions.R')

iaFieldDataHyb <- read_excel('data/2023/2023_yield_ICIA_v3.xlsx', 
                             sheet = '4-row plots', 
                             skip = 1,
                             col_types = c('numeric', rep('skip', 2), 'text', 'skip', 'text', 'numeric', 'skip', 'text', rep('skip', 2), 
                                           rep('date', 2), 'text', rep('skip', 2), 'numeric', rep('skip', 5), rep('numeric', 9), 'skip', 
                                           rep('text', 5), rep('numeric', 2), 'skip'),
                             col_names = c('pedigreeID', 'location', 'qrCode', 'poundsOfNitrogenPerAcre', 'genotype', 'plantingDate', 
                                           'harvestDate', 'experiment', 'rep', 'row', 'range', 'seedsPlanted', 'plantDensity', 'rootLodging',
                                           'stalkLodging', 'combineYield', 'combineTestWeight', 'combineMoisture', 'plotDiscarded', 
                                           'notes', 'tattooSensor', 'nitrogenSensor', 'solarPanel', 'flagLeafHeight', 'earHeight'))
iaFieldDataHyb <- iaFieldDataHyb %>%
  rowwise() %>%
  mutate(plotNumber = str_split_i(qrCode, '-', 3), 
         nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 100 & poundsOfNitrogenPerAcre < 200 ~ 'Medium', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High'),
         percentLodging = sum(rootLodging, stalkLodging, na.rm = TRUE)/seedsPlanted,
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5), 
         tattooSensor = case_when(!is.na(tattooSensor) ~ 'Tattoo sensor'),
         nitrogenSensor = case_when(!is.na(nitrogenSensor) ~ 'Nitrogen sensor'),
         solarPanel = case_when(!is.na(solarPanel) ~ 'Solar panel'), 
         combineYield = case_when(!is.na(solarPanel) ~ NA, .default = combineYield), 
         yieldPerAcre = case_when(!is.na(solarPanel) ~ NA, .default = yieldPerAcre),
         irrigationProvided = 0,
         plotLength = 17.5, 
         genotype = str_to_upper(genotype)) %>%
  filter(is.na(plotDiscarded)) %>%
  unite('notes', tattooSensor, nitrogenSensor, solarPanel, notes, sep = fixed(';'), na.rm = TRUE, remove = TRUE)

npFieldData <- read_excel('data/2023/hybrids/2023 Schnable hips_data_v2.xlsx',
                          sheet = 'Data', 
                          skip = 5, 
                          col_types = c('skip', 'numeric', rep('skip', 2), 'text', rep('skip', 2), 'text', rep('numeric', 2), rep('skip', 2),
                                        rep('numeric', 2), rep('date', 2), 'skip', rep('numeric', 5), 'text', rep('skip', 3), rep('numeric', 3),
                                        'skip'),
                          col_names = c('plotNumber', 'genotype', 'irrigationTreatment', 'range', 'row', 'totalStandCount', 'plantDensity',
                                        'anthesisDate', 'silkDate', 'earHeight', 'tasselHeight', 'stalkLodging', 'rootLodging', 'greenSnap',
                                        'harvestDate', 'combineYield', 'combineMoisture', 'combineTestWeight'))
npFieldData <- npFieldData %>%
  filter(genotype!='filler') %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype), 
         earHeight = earHeight*100,
         tasselHeight = tasselHeight*100, 
         percentLodging = sum(stalkLodging, rootLodging, greenSnap, na.rm = TRUE)/totalStandCount,
         harvestDate = dmy(harvestDate), 
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5), 
         plotLength = 17.5)

lnkYieldData <- read.csv('data/2023/hybrids/231127 Hybrid HIPS Lincoln - Combine Harvest Data - Turkus Curated.csv', 
                         col.names = c('plotNumber', 'row', 'range', 'combineYield', 'combineMoisture', 'combineTestWeight', 'harvestDate',
                                       'earsMissingFromCenterRows'))
lnkYieldData <- lnkYieldData %>%
  rowwise() %>%
  mutate(plotNumber = as.integer(plotNumber), 
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5),
         plotLength = 17.5) %>%
  mutate(yieldPerAcre = case_when(earsMissingFromCenterRows > 0 ~ NA, .default = yieldPerAcre),
         combineYield = case_when(earsMissingFromCenterRows > 0 ~ NA, .default = combineYield))

lnkHeight <- read.csv('data/2023/hybrids/2023HIBHIPS_plat_height.csv')
lnkHeight <- lnkHeight %>%
  rename(plotNumber = Plot.., 
         row = Row, 
         range = Range, 
         earHeight1 = Ear.Height.1,
         earHeight2 = Ear.Height2, 
         flagLeafHeight1 = Flaf.Leaf.1, 
         flagLeafHeight2 = Flaf.Leaf2) %>%
  rowwise() %>%
  mutate(location = 'Lincoln', 
         earHeight = mean(earHeight1, earHeight2, na.rm = TRUE)*12 %>%
           cm(),
         flagLeafHeight = mean(flagLeafHeight1, flagLeafHeight2, na.rm = TRUE)*12 %>%
           cm()) %>%
  select(plotNumber, row, range, earHeight, flagLeafHeight, location)

lnkData <- full_join(lnkYieldData, lnkHeight, join_by(plotNumber), suffix = c('', ''), keep = FALSE) %>%
  select(!earsMissingFromCenterRows)
