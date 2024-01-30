library(tidyverse)
library(readxl)
library(lubridate)
source('src/Functions.R')

iaFieldDataHyb <- read_excel('data/2023/2023_yield_ICIA_v3.xlsx', 
                             sheet = '4-row plots', 
                             skip = 1,
                             col_types = c('numeric', rep('skip', 2), 'text', 'skip', 'text', 'numeric', 'text', 'text', rep('skip', 2), 
                                           rep('date', 2), 'text', rep('skip', 2), 'numeric', rep('skip', 5), rep('numeric', 9), 'skip', 
                                           rep('text', 5), rep('numeric', 2), 'skip'),
                             col_names = c('pedigreeID', 'location', 'qrCode', 'poundsOfNitrogenPerAcre', 'pedigree', 'genotype',
                                           'plantingDate', 'harvestDate', 'experiment', 'rep', 'row', 'range', 'seedsPlanted', 
                                           'plantDensity', 'rootLodging', 'stalkLodging', 'combineYield', 'combineTestWeight',
                                           'combineMoisture', 'plotDiscarded', 'notes', 'tattooSensor', 'nitrogenSensor', 'solarPanel',
                                           'flagLeafHeight', 'earHeight'))
iaFieldDataHyb <- iaFieldDataHyb %>%
  rowwise() %>%
  mutate(plotNumber = str_split_i(qrCode, '-', 3) %>%
           as.numeric(), 
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
         genotype = case_when(genotype=='0' ~ pedigree, .default = genotype) %>%
           str_to_upper()) %>%
  filter(is.na(plotDiscarded)) %>%
  unite('notes', tattooSensor, nitrogenSensor, solarPanel, notes, sep = fixed(';'), na.rm = TRUE, remove = TRUE)

npFieldData <- read_excel('data/2023/hybrids/2023 Schnable hips_data_v2.xlsx',
                          sheet = 'Data', 
                          skip = 5, 
                          col_types = c('skip', 'numeric', 'skip', 'text', 'text', rep('skip', 2), 'text', rep('numeric', 2), rep('skip', 2),
                                        rep('numeric', 2), rep('date', 2), 'skip', rep('numeric', 5), 'text', rep('skip', 3), 
                                        rep('numeric', 3), 'skip'),
                          col_names = c('plotNumber', 'pedigree', 'genotype', 'irrigationTreatment', 'range', 'row', 'totalStandCount',
                                        'plantDensity', 'anthesisDate', 'silkDate', 'earHeight', 'tasselHeight', 'stalkLodging',
                                        'rootLodging', 'greenSnap', 'harvestDate', 'combineYield', 'combineMoisture', 'combineTestWeight'))
npFieldData <- npFieldData %>%
  filter(genotype!='filler') %>%
  rowwise() %>%
  mutate(genotype = case_when(genotype=='0' ~ pedigree, .default = genotype) %>%
           str_to_upper(), 
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
         combineYield = case_when(earsMissingFromCenterRows > 0 ~ NA, .default = combineYield), 
         harvestDate = str_split_i(harvestDate, ' ', 1) %>%
           mdy())

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

lnkFT <- read_excel('data/2023/hybrids/Hybrid Hips flowering NOTEs 2023  (1).xlsx', 
                    sheet = 'Sheet1', 
                    skip = 1, 
                    col_types = c(rep(c(rep('numeric', 3), 'date', 'skip', 'date', 'skip', 'text'), 2), 'skip'),
                    col_names = c('plotNumber_A', 'row_A', 'range_A', 'anthesisDate_A', 'silkDate_A', 'notes_A', 'plotNumber_B', 'row_B', 'range_B', 
                                  'anthesisDate_B', 'silkDate_B', 'notes_B'))
lnkFTA <- lnkFT %>% 
  select(contains('_A')) %>%
  rename_with(.fn = ~str_split_i(., '_', 1))

lnkFTB <- lnkFT %>% 
  select(contains('_B')) %>%
  rename_with(.fn = ~str_split_i(., '_', 1))

lnkFT <- bind_rows(lnkFTA, lnkFTB)

lnkFT <- lnkFT %>%
  rowwise() %>%
  mutate(notes = str_replace(notes, 'ST', 'thick stem') %>%
           str_replace('CC', 'compact canopy') %>%
           str_replace('LF', 'late tasseling') %>%
           str_replace('LS', 'late silking') %>%
           str_replace('T, ', 'Tall') %>%
           str_replace(', T', 'Tall') %>%
           str_replace_all(',', ';'),
         location = 'Lincoln')

lnkData <- full_join(lnkData, lnkFT, join_by(plotNumber), keep = FALSE, suffix = c('.yield', '')) %>%
  select(!ends_with('.yield')) %>%
  filter(!is.na(plotNumber))

lnkIndex <- read_excel('data/2023/Summary of HIPS 2023 Maps for Fields Visited by J Schanble Lab.xlsm',
                       sheet = 'Lincoln Hybrids - Index',
                       skip = 1,
                       col_types = c('text', 'skip', 'numeric', 'text', rep('skip', 2), rep('numeric', 2), 'text', 'numeric', rep('text', 3), rep('skip', 8)),
                       col_names = c('qrCode', 'plotNumber', 'location', 'row', 'range', 'poundsOfNitrogenPerAcre', 'rep', 'genotype', 'notes', 'ERNumber'))

lnkIndex <- lnkIndex %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         irrigationProvided = 0, 
         poundsOfNitrogenPerAcre = str_remove(poundsOfNitrogenPerAcre, 'N') %>%
           as.numeric(),
         nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 100 & poundsOfNitrogenPerAcre < 200 ~ 'Medium', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High'),
         block = case_when(nitrogenTreatment=='Low' & rep==1 ~ 1,
                           nitrogenTreatment=='Low' & rep==2 ~ 2,
                           nitrogenTreatment=='Medium' & rep==1 ~ 3,
                           nitrogenTreatment=='Medium' & rep==2 ~ 4,
                           nitrogenTreatment=='High' & rep==1 ~ 5,
                           nitrogenTreatment=='High' & rep==2 ~ 6), 
         genotype = str_to_upper(genotype))
lnk <- full_join(lnkData, lnkIndex, join_by(plotNumber), suffix = c('', '.idx'), keep = FALSE)

lnk <- lnk %>%
  rowwise() %>% 
  mutate(location = location.idx, 
         row = row.idx, 
         range = range.idx, 
         harvestDate = mdy(harvestDate)) %>% 
  unite('notes', notes, notes.idx, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  select(!c(rep, ends_with('.idx')))

npIndex <- read_excel('data/2023/Summary of HIPS 2023 Maps for Fields Visited by J Schanble Lab.xlsm', 
                      sheet = 'North Platte Hybrids - Index', 
                      skip = 1, 
                      col_types = c('text', rep('numeric', 3), 'text', 'numeric', rep('text', 2), rep('skip', 2), 'text', rep('skip', 8)), 
                      col_names = c('qrCode', 'plotNumber', 'row', 'range', 'genotype', 'rep', 'poundsOfNitrogenPerAcre',
                                    'irrigationTreatment', 'notes')) %>%
  filter(!is.na(plotNumber)) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         genotype = str_to_upper(genotype), 
         poundsOfNitrogenPerAcre = str_remove(poundsOfNitrogenPerAcre, 'N') %>%
           as.numeric(), 
         nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 100 & poundsOfNitrogenPerAcre < 200 ~ 'Medium', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High'))

np <- full_join(npIndex, npFieldData, join_by(plotNumber, genotype), suffix = c('', '.field'), keep = FALSE)

np <- np %>%
  mutate(location = 'North Platte') %>%
  select(!c(pedigree, ends_with('.field')))

mvHybFieldData <- filter(iaFieldDataHyb, location=='Missouri Valley')
acFieldData <- filter(iaFieldDataHyb, location!='Missouri Valley')

mvIndex <- read_excel('data/2023/Summary of HIPS 2023 Maps for Fields Visited by J Schanble Lab.xlsm', 
                      sheet = 'Missouri Valley Hybrids - Index', 
                      skip = 1, 
                      col_types = c(rep('skip', 2), 'text', rep('numeric', 3), 'skip', 'numeric', 'skip', 'skip', 'text', 'skip', 'text', 
                                    rep('skip', 4)), 
                      col_names = c('qrCode', 'plotNumber', 'row', 'range', 'block', 'location', 'experiment'))
mv <- full_join(mvHybFieldData, mvIndex, join_by(location, row, range, experiment), suffix = c('', '.idx'), keep = FALSE)

mv <- mv %>%
  rowwise() %>%
  mutate(qrCode = qrCode.idx, 
         plotNumber = plotNumber.idx) %>%
  select(pedigreeID, location, qrCode, poundsOfNitrogenPerAcre, genotype, plantingDate, harvestDate, experiment, row, range, seedsPlanted,
         plantDensity, rootLodging, stalkLodging, combineYield, combineTestWeight, combineMoisture, notes, flagLeafHeight, earHeight,
         plotNumber, nitrogenTreatment, percentLodging, yieldPerAcre, irrigationProvided, plotLength, block)

sbIndex <- read_excel('data/2023/Summary of HIPS 2023 Maps for Fields Visited by J Schanble Lab.xlsm', 
                      sheet = 'Scottsbluff Hybrids - Index', 
                      skip = 1, 
                      col_types = c(rep('skip', 2), 'text', rep('numeric', 3), 'text', 'numeric', 'text', 'skip', 'text', rep('skip', 3)), 
                      col_names = c('qrCode', 'plotNumber', 'row', 'range', 'genotype', 'rep', 'poundsOfNitrogenPerAcre', 'location')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         plotNumber = case_when(poundsOfNitrogenPerAcre=='150N' ~ plotNumber + 300,
                                poundsOfNitrogenPerAcre=='225N' ~ plotNumber + 600, 
                                .default = plotNumber), 
         genotype = str_to_upper(genotype), 
         poundsOfNitrogenPerAcre = str_remove(poundsOfNitrogenPerAcre, 'N') %>%
           as.numeric())

sbFieldData <- read_excel('data/2023/hybrids/Scottsbluff Hybrid HIPS 2023 - Data.xlsx', 
                          sheet = 'Data23', 
                          skip = 1,
                          col_types = rep('numeric', 18), 
                          col_names = c(paste0(c('plotNumber', 'plantHeight', 'earHeight', 'combineYield', 'combineMoisture',
                                                 'combineTestWeight'), '.Low'),
                                        paste0(c('plotNumber', 'plantHeight', 'earHeight', 'combineYield', 'combineMoisture',
                                                 'combineTestWeight'), '.Medium'), 
                                        paste0(c('plotNumber', 'plantHeight', 'earHeight', 'combineYield', 'combineMoisture',
                                                 'combineTestWeight'), '.High'))) %>%
  rowwise() %>%
  mutate(plotNumber.Medium = plotNumber.Medium + 300, 
         plotNumber.High = plotNumber.High + 600) 

sbLow <- select(sbFieldData, ends_with('.Low')) %>%
  rename_with(~str_remove(., '.Low')) %>%
  mutate(nitrogenTreatment = 'Low')

sbMedium <- select(sbFieldData, ends_with('.Medium')) %>%
  rename_with(~str_remove(., '.Medium')) %>%
  mutate(nitrogenTreatment = 'Medium')

sbHigh <- select(sbFieldData, ends_with('.High')) %>%
  rename_with(~str_remove(.,'.High')) %>%
  mutate(nitrogenTreatment = 'High')

sbFieldData <- bind_rows(sbLow, sbMedium, sbHigh) %>%
  filter(!is.na(plotNumber))

sb <- full_join(sbIndex, sbFieldData, join_by(plotNumber), keep = FALSE, suffix = c('', '')) %>%
  rowwise() %>%
  mutate(block = case_when(nitrogenTreatment=='Low' ~ rep, 
                           nitrogenTreatment=='Medium' & rep==1 ~ 3,
                           nitrogenTreatment=='Medium' & rep==2 ~ 4, 
                           nitrogenTreatment=='High' & rep==1 ~ 5,
                           nitrogenTreatment=='High' & rep==2 ~ 6), 
         plantHeight = cm(plantHeight), 
         earHeight = cm(earHeight), 
         combineMoisture = case_when(combineMoisture==0 ~ NA, .default = combineMoisture),
         combineTestWeight = case_when(combineTestWeight==0 ~ NA, .default = combineTestWeight), 
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5), 
         plotLength = 17.5, 
         plantingDate = mdy('05-24-2023'))

fieldData <- bind_rows(acFieldData, mv, lnk, np, sb)

pedigreeIDKey <- fieldData %>%
  select(pedigreeID, genotype) %>%
  group_by(pedigreeID) %>%
  summarise(genotype = max(genotype, na.rm = TRUE)) %>%
  filter(!is.na(pedigreeID) & !is.na(genotype))

fieldData <- fieldData %>%
  filter(!(genotype %in% c('FILLER', 'SOLAR PANEL', 'FILL', NA))) %>%
  rowwise() %>%
  mutate(plantingDate = case_when(location=='Lincoln' ~ mdy('05-16-2023'),
                                  location=='North Platte' ~ mdy('05-10-2023'),
                                  location=='Missouri Valley' ~ mdy('05-02-2023'),
                                  .default = plantingDate),
         harvestDate = case_when(location=='Missouri Valley' ~ mdy('09-25-2023'),
                                 location=='Lincoln' ~ mdy('10-23-2023'), 
                                 .default = harvestDate), 
         pedigreeID = case_when(is.na(pedigreeID) ~ pedigreeIDKey$pedigreeID[pedigreeIDKey$genotype==genotype], .default = pedigreeID), 
         sublocation = location, 
         poundsOfNitrogenPerAcre = case_when(location=='Missouri Valley' ~ 160, .default = poundsOfNitrogenPerAcre), 
         nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High', 
                                       .default = 'Medium'),
         irrigationProvided = case_when(location=='Missouri Valley' ~ 0,
                                        location=='North Platte' ~ 4.5,
                                        .default = irrigationProvided), 
         plotLength = case_when(is.na(plotLength) ~ 17.5, .default = plotLength),
         qrCode = str_to_upper(qrCode),
         rep = case_when(is.na(rep) ~ as.numeric(str_split_i(qrCode, fixed('$'), 6)), .default = rep),
         block = case_when(location=='North Platte' ~ 1,
                           nitrogenTreatment=='Low'| location=='Missouri Valley' ~ rep, 
                           nitrogenTreatment=='Medium' ~ rep + 2, 
                           nitrogenTreatment=='High' ~ rep + 4),
         daysToAnthesis = difftime(as.Date(anthesisDate), as.Date(plantingDate)) %>%
           as.integer(),
         daysToSilk = difftime(as.Date(silkDate), as.Date(plantingDate)) %>%
           as.integer(),
         anthesisSilkingInterval = difftime(as.Date(silkDate), as.Date(anthesisDate)) %>%
           as.integer(),
         flagLeafHeight = case_when(location=='North Platte' ~ tasselHeight, .default = flagLeafHeight)) %>%
  select(!c(pedigree, rep, seedsPlanted, rootLodging, stalkLodging, plotDiscarded, tasselHeight, greenSnap)) %>%
  filter(!str_detect(qrCode, 'INBRED'))

# Wrangle 2023 weather data to calculate GDDs

