library(tidyverse)
library(readxl)
library(lubridate)
library(daymetr)
library(cowplot)
library(MoMAColors)
library(lme4)
library(car)
library(nasapower)
library(weathermetrics)
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
           str_replace('DLO', 'different leaf orientation') %>%
           str_replace('BL', 'broad leaves') %>%
           str_replace('EL', 'elongated leaves') %>%
           str_replace('LY', 'low yielding') %>%
           str_replace_all(',', ';'),
         location = 'Lincoln')

lnkData <- full_join(lnkData, lnkFT, join_by(plotNumber), keep = FALSE, suffix = c('.yield', '')) %>%
  select(!ends_with('.yield')) %>%
  filter(!is.na(plotNumber))

lnkStandCt <- read.csv('data/2023/hybrids/2023HYB_HIPS_standing_count.csv')
colnames(lnkStandCt) <- c('field', 'range', 'plotNumber', 'totalStandCount')
lnkStandCt <- select(lnkStandCt, c(plotNumber, totalStandCount))
lnkData <- full_join(lnkData, lnkStandCt, join_by(plotNumber), keep = FALSE, suffix = c('', ''))

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
                                       poundsOfNitrogenPerAcre > 200 ~ 'High'),
         irrigationProvided = case_when(irrigationTreatment=='Full' ~ 4.5, 
                                        irrigationTreatment=='Dryland' ~ 0))

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
# ON HOLD: need DAYMET to impute weather data & not released for 2023 yet: 2022 data was released 2023-03-10
# weather.cf <- read_excel('data/2023/2023 Weather Stations/HIPS_Crawordsville_2023.xlsx', 
#                  sheet = 'Weather Data',
#                  skip = 2, 
#                  col_types = c('date', rep('skip', 6), 'numeric', rep('skip', 10)), 
#                  col_names = c('date', 'temp'))
# weather.cf <- weather.cf %>%
#   rowwise() %>%
#   mutate(date = floor_date(date, 'days')) %>%
#   group_by(date) %>%
#   summarise(maxTemp = max(temp, na.rm = TRUE), 
#             minTemp = min(temp, na.rm = TRUE), 
#             GDD = getGDDs(minTemp, maxTemp), 
#             location = 'Crawfordsville')
# season.cf <- tibble(date = seq(as.Date('2023-05-04'), as.Date('2023-10-02'), by = 'days'))
# impute.cf <- full_join(weather.cf, season.cf, join_by(date), keep = FALSE, suffix = c('', '')) %>%
#   filter(is.na(maxTemp))
# lat.cf <- mean(41.19504636, 41.19399869)
# lon.cf <- mean(-91.47757465, -91.48059795)
# daymet.cf <- download_daymet(site = 'Crawfordsville', lat = lat.cf, lon = lon.cf, start = 2023, end = 2023)
# daymet.cf <- daymet.cf$data %>%
#   filter(year==2023)

# QC traits
phenotypes <- c('plantDensity', 'combineYield', 'combineTestWeight', 'combineMoisture', 'flagLeafHeight', 'earHeight', 'percentLodging', 
                'yieldPerAcre', 'totalStandCount', 'plantHeight', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval')

plotRepCorr(fieldData, 'nitrogenTreatment', 'genotype', phenotypes, 'location')

# Remove outliers 
fieldData <- fieldData %>%
  rowwise() %>%
  mutate(anthesisDate = case_when(location=='North Platte' & plotNumber==146 ~ NA, .default = anthesisDate),
         daysToAnthesis = case_when(location=='North Platte' & plotNumber==146 ~ NA, 
                                    daysToAnthesis < 50 ~ NA, 
                                    .default = daysToAnthesis),
         silkDate = case_when(location=='North Platte' & plotNumber==349 ~ NA, .default = silkDate), 
         daysToSilk = case_when(location=='North Platte' & plotNumber==349 ~ NA, .default = daysToSilk), 
         anthesisSilkingInterval = case_when(location=='North Platte' & plotNumber %in% c(146, 349) ~ NA, 
                                             anthesisSilkingInterval > 15 ~ NA,
                                             .default = anthesisSilkingInterval), 
         plantHeight = case_when(plantHeight > 500 ~ NA, .default = plantHeight),
         totalStandCount = case_when(totalStandCount > 100 ~ NA, .default = totalStandCount),
         flagLeafHeight = case_when(location=='Lincoln' & flagLeafHeight < 100 ~ NA, 
                                    flagLeafHeight > 350 ~ NA,
                                    .default = flagLeafHeight),
         earHeight = case_when(earHeight > 225 ~ NA, 
                               location=='North Platte' & earHeight < 80 ~ NA,
                               .default = earHeight),
         combineTestWeight = case_when(combineTestWeight < 40 ~ NA, .default = combineTestWeight),
         combineMoisture = case_when(combineMoisture < 10 ~ NA, .default = combineMoisture))
plotRepCorr(fieldData, 'nitrogenTreatment', 'genotype', phenotypes, 'location')

# sbLong <- scottsbluff %>%
#   pivot_longer(any_of(sbPhenos), names_to = 'phenotype', values_to = 'val') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')))
# sbViolins <- ggplot(sbLong, aes(nitrogenTreatment, val, fill = nitrogenTreatment)) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#   facet_wrap(vars(phenotype), nrow = 2, scales = 'free')
# sbViolins

hybrids <- read.csv('outData/HIPS_2022_V3.5_HYBRIDS.csv')
scottsbluff <- filter(fieldData, location=='Scottsbluff')

nitrogenResponseViolins <- fieldData %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
  ggplot(aes(nitrogenTreatment, yieldPerAcre, fill = nitrogenTreatment)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
    facet_wrap(vars(location), nrow = 2) + 
    labs(title = '2023')
nitrogenResponseViolins

nitrogenResponseViolins <- hybrids %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
  ggplot(aes(nitrogenTreatment, yieldPerAcre, fill = nitrogenTreatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  facet_wrap(vars(location), nrow = 2) + 
  labs(title = '2022')
nitrogenResponseViolins


sbTestFieldData <- scottsbluff %>% 
  rowwise() %>%
  mutate(plotNumberSW = case_when(row==22 ~ plotNumber - 745,
                                  row==21 ~ plotNumber - 687,
                                  row==20 ~ plotNumber - 629, 
                                  row==19 ~ plotNumber - 571,
                                  row==18 ~ plotNumber - 513,
                                  row==17 ~ plotNumber - 455, 
                                  row==15 ~ plotNumber - 203, 
                                  row==14 ~ plotNumber - 145, 
                                  row==13 ~ plotNumber - 87,
                                  row==12 ~ plotNumber - 29, 
                                  row==11 ~ plotNumber + 29,
                                  row==10 ~ plotNumber + 87, 
                                  row==9 ~ plotNumber + 145, 
                                  row==8 ~ plotNumber + 203,
                                  row==6 ~ plotNumber + 455,
                                  row==5 ~ plotNumber + 513,
                                  row==4 ~ plotNumber + 571,
                                  row==3 ~ plotNumber + 629,
                                  row==2 ~ plotNumber + 687,
                                  row==1 ~ plotNumber + 745)) %>%
  select(c(plotNumberSW, plantHeight, earHeight, combineYield, combineMoisture, combineTestWeight, yieldPerAcre))

sbTestSW <- right_join(sbIndex, sbTestFieldData, join_by(plotNumber==plotNumberSW), keep = FALSE,
                      suffix = c('', '')) %>%
  rowwise() %>%
  mutate(nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High',
                                       .default = 'Medium')) %>%
  filter(location=='Scottsbluff')


# Probably not a SW plant start; this shows no noticeable improvement
sbPhenos <- c('earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 'yieldPerAcre')
plotRepCorr(sbTestSW, 'nitrogenTreatment', 'genotype', c(sbPhenos, 'plantHeight'), 'location')

# Let's check a NE plant start
sbTestNE <- scottsbluff %>%
  rowwise() %>%
  mutate(rangeNE = 30 - range)
sbTestNE <- right_join(sbIndex, sbTestNE, join_by(row, range==rangeNE), keep = FALSE, suffix = c('', '.ne')) %>%
  rowwise() %>%
  mutate(nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High',
                                       .default = 'Medium')) %>%
  filter(location=='Scottsbluff')


# Okay, probably not a NE plant start either
sbPhenos <- c('earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 'yieldPerAcre')
plotRepCorr(sbTestNE, 'nitrogenTreatment', 'genotype', c(sbPhenos, 'plantHeight'), 'location')

# Is it a NW plant start? 
sbTestFieldDataNW <- sbTestNE %>% 
  rowwise() %>%
  mutate(plotNumberNW = case_when(row==22 ~ plotNumber - 745,
                                  row==21 ~ plotNumber - 687,
                                  row==20 ~ plotNumber - 629, 
                                  row==19 ~ plotNumber - 571,
                                  row==18 ~ plotNumber - 513,
                                  row==17 ~ plotNumber - 455, 
                                  row==15 ~ plotNumber - 203, 
                                  row==14 ~ plotNumber - 145, 
                                  row==13 ~ plotNumber - 87,
                                  row==12 ~ plotNumber - 29, 
                                  row==11 ~ plotNumber + 29,
                                  row==10 ~ plotNumber + 87, 
                                  row==9 ~ plotNumber + 145, 
                                  row==8 ~ plotNumber + 203,
                                  row==6 ~ plotNumber + 455,
                                  row==5 ~ plotNumber + 513,
                                  row==4 ~ plotNumber + 571,
                                  row==3 ~ plotNumber + 629,
                                  row==2 ~ plotNumber + 687,
                                  row==1 ~ plotNumber + 745)) %>%
  select(c(plotNumberNW, plantHeight, earHeight, combineYield, combineMoisture, combineTestWeight, yieldPerAcre))

sbTestNW <- right_join(sbIndex, sbTestFieldDataNW, join_by(plotNumber==plotNumberNW), keep = FALSE,
                       suffix = c('', '')) %>%
  rowwise() %>%
  mutate(nitrogenTreatment = case_when(poundsOfNitrogenPerAcre < 100 ~ 'Low', 
                                       poundsOfNitrogenPerAcre > 200 ~ 'High',
                                       .default = 'Medium')) %>%
  filter(location=='Scottsbluff')


# Probably not a NW plant start; this shows no noticeable improvement
sbPhenos <- c('earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 'yieldPerAcre')
plotRepCorr(sbTestNW, 'nitrogenTreatment', 'genotype', c(sbPhenos, 'plantHeight'), 'location')

# Do observations from Scottsbluff correlate with observations from the 'same' genotypes at other locations?
# Probably not but let's check it anyway
fieldDataWide <- fieldData %>%
  group_by(genotype, location, nitrogenTreatment) %>%
  mutate(genotypeObsNum = 1:n()) %>%
  pivot_wider(id_cols = c(genotype, genotypeObsNum, nitrogenTreatment),
              names_from = c(location),
              values_from = c(earHeight, combineYield, combineMoisture, combineTestWeight, yieldPerAcre))
sbPhenos <- c('earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 'yieldPerAcre')

for(i in 1:length(sbPhenos))
{
  phenoSB <- paste0(sbPhenos[i], '_Scottsbluff')
  phenoLNK <- paste0(sbPhenos[i], '_Lincoln')
  phenoMV <- paste0(sbPhenos[i], '_Missouri Valley')
  phenoAmes <- paste0(sbPhenos[i], '_Ames')
  phenoCF <- paste0(sbPhenos[i], '_Crawfordsville')
  
  plotSBLNK <- ggplot(fieldDataWide, aes(.data[[phenoSB]], .data[[phenoLNK]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotSBMV <- ggplot(fieldDataWide, aes(.data[[phenoSB]], .data[[phenoMV]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotSBAmes <- ggplot(fieldDataWide, aes(.data[[phenoSB]], .data[[phenoAmes]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotSBCF <- ggplot(fieldDataWide, aes(.data[[phenoSB]], .data[[phenoCF]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotLNKMV <- ggplot(fieldDataWide, aes(.data[[phenoLNK]], .data[[phenoMV]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotLNKAmes <- ggplot(fieldDataWide, aes(.data[[phenoLNK]], .data[[phenoAmes]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotLNKCF <- ggplot(fieldDataWide, aes(.data[[phenoLNK]], .data[[phenoMV]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotMVAmes <- ggplot(fieldDataWide, aes(.data[[phenoMV]], .data[[phenoAmes]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotMVCF <- ggplot(fieldDataWide, aes(.data[[phenoMV]], .data[[phenoCF]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  plotAmesCF <- ggplot(fieldDataWide, aes(.data[[phenoAmes]], .data[[phenoCF]])) +
    geom_point() +
    theme(legend.position = 'none')
  
  all <- plot_grid(plotSBLNK, plotSBMV, plotSBAmes, plotSBCF, plotLNKMV, plotLNKAmes, plotLNKCF, plotMVAmes, plotMVCF,
                   plotAmesCF, nrow = 3)
  print(all)
}

# Wrangle NP + SB ear data
npSBEars <- read_excel('data/2023/hybrids/2023 HIPS Hybrid Ear Phenotyping - Worksheet 2.xlsx', 
                       sheet = 'DataGaps..Removed', 
                       skip = 1, 
                       col_types = c(rep('skip', 2), rep('text', 4), rep('skip', 3), 'text', rep('numeric', 10), 'text',
                                     rep('skip', 4)),
                       col_names = c('qrCode', 'topBox', 'bottomBox', 'rowBandNotes', 'earNotes', 'earWidth', 
                                     'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'shelledCobWidth', 
                                     'earLength', 'shelledCobMass', 'hundredKernelMass', 'kernelsPerEar', 'kernelColor')) %>%
  rowwise() %>%
  mutate(qrCode = case_when(str_detect(earNotes, 'Missing QR Code') ~ 'Missing QR Code', .default = qrCode)) %>%
  ungroup() %>%
  fill(qrCode, topBox, bottomBox, rowBandNotes, .direction = 'down') %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         kernelColor = str_remove(kernelColor, fixed('$')), 
         location = case_when(str_detect(qrCode, 'NORTH PLATTE') ~ 'North Platte',
                              str_detect(qrCode, 'SCOTTSBLUFF') ~ 'Scottsbluff'),
         plotNumber = str_split_i(qrCode, fixed('$'), 7) %>%
           str_split_i('ROW', 1) %>%
           str_remove('PLOT') %>%
           as.numeric(), 
         nitrogenTreatment = case_when(str_detect(qrCode, '150N') ~ 'Medium',
                                       str_detect(qrCode, '75N') ~ 'Low',
                                       str_detect(qrCode, '225N') ~ 'High'),
         kernelMassPerEar = earMass - shelledCobMass) %>%
  group_by(qrCode, topBox, bottomBox, rowBandNotes, plotNumber, location, nitrogenTreatment) %>%
  summarise(earNotes = str_flatten(earNotes, collapse = ';', na.rm = TRUE),
            earWidth = mean(earWidth, na.rm = TRUE)*0.1, 
            earFillLength = mean(earFillLength, na.rm = TRUE)*0.1,
            kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE),
            kernelsPerRow = mean(kernelsPerRow, na.rm = TRUE), 
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE), 
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE)*0.1,
            earLength = mean(earLength, na.rm = TRUE)*0.1,
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            hundredKernelMass = mean(hundredKernelMass, na.rm = TRUE), 
            kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE), 
            kernelColor = max(kernelColor, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(plotNumber = case_when(location=='Scottsbluff' & nitrogenTreatment=='Medium' ~ plotNumber + 300))

df <- full_join(fieldData, npSBEars, join_by(location, plotNumber), suffix = c('', '.ears'), keep = FALSE)
df <- filter(df, !is.na(qrCode))
plotRepCorr(df, 'nitrogenTreatment', 'genotype', c('earFillLength', 'earWidth', 'kernelRowNumber', 'kernelsPerRow', 'kernelMassPerEar', 'shelledCobWidth', 'earLength', 'shelledCobMass', 'hundredKernelMass', 'kernelsPerEar'), 'location')
# Okay, that didn't really work -- there seems to be issues with SB data and we don't have pairs of genotypes at NP since there's only 49 plots right now

# Let's get the Ames and Crawfordsville data wrangled
acKRN <- read_excel('data/2023/hybrids/2_KRN_trait_Hybrid_2023_Compiled.xlsx',
                    sheet = 'Sheet1', 
                    skip = 4, 
                    col_types = c('text', 'numeric', 'text', 'text', 'text', 'skip'),
                    col_names = c('qrCode', 'kernelRowNumber', 'notes', 'smoothCob', 'sweetcorn')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode) %>%
           str_split_i(' ', 1),
         smoothCob = case_when(!is.na(smoothCob) ~ 'smooth cob - ovule issue'),
         sweetcorn = case_when(!is.na(sweetcorn) ~ 'sweetcorn')) %>%
  unite('notes', c(notes, smoothCob, sweetcorn), sep = ';', remove = TRUE, na.rm = TRUE)

acEar <- read_excel('data/2023/hybrids/3_ear_trait_HYBRID_2023_compiled.xlsx',
                    sheet = 'Sheet1',
                    skip = 1, 
                    col_types = c('skip', 'text', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'text', 'skip'),
                    col_names = c('qrCode', 'earWidth', 'earMass', 'seedMissing')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode) %>%
           str_split_i(' ', 1), 
         seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on both sides at widest point of ear'))

acCob <- read_excel('data/2023/hybrids/5_cob_Traits_HYBRID_2023_compiled.xlsx', 
                    sheet = 'Sheet1', 
                    skip = 1,
                    col_types = c('skip', 'text', 'numeric', 'numeric', 'numeric', 'skip', 'text', 'text', 'skip', 'skip'),
                    col_names = c('qrCode', 'earLength', 'shelledCobWidth', 'shelledCobMass', 'cobBroke', 'string')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode) %>%
           str_split_i(' ', 1), 
         cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces during shelling'),
         string = case_when(!is.na(string) ~ 'Severe bend in ear. String used to measure cob length')) %>%
  unite('notes', c(cobBroke, string), sep = ';', remove = TRUE, na.rm = TRUE)

acSeed <- read_excel('data/2023/hybrids/6_seed_traits_HYBRID_2023_compiled.xlsx',
                     sheet = 'Sheet1',
                     skip = 4,
                     col_types = c('skip', 'text', 'numeric', 'numeric', 'skip', 'text', 'skip', 'skip', 'skip'),
                     col_names = c('qrCode', 'kernelsPerEar', 'kernelMassPerEar', 'notes')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode) %>%
           str_split_i(' ', 1), 
         kernelMassPerEar = case_when(!is.na(notes) ~ NA, .default = kernelMassPerEar))

acEarPhenotypes <- full_join(acKRN, acEar, join_by(qrCode), suffix = c('', '.ear'), keep = FALSE) %>%
  unite('notes', c(notes, seedMissing), sep = ';', remove = TRUE, na.rm = TRUE)

acEarPhenotypes <- full_join(acEarPhenotypes, acCob, join_by(qrCode), suffix = c('', '.cob'), keep = FALSE) %>%
  full_join(acSeed, join_by(qrCode), suffix = c('', '.seed'), keep = FALSE) %>%
  unite('notes', c(notes, notes.cob, notes.seed), sep = ';', remove = TRUE, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(kernelMassPerEar = case_when(is.na(kernelMassPerEar) ~ earMass - shelledCobMass, .default = kernelMassPerEar),
         qrCode = str_c(str_split_i(qrCode, '-', 1), str_split_i(qrCode, '-', 2), str_split_i(qrCode, '-', 3), 
                        sep = '-'), 
         hundredKernelMass = (kernelMassPerEar / kernelsPerEar) * 100) %>%
  select(!earMass) %>%
  group_by(qrCode) %>%
  summarise(kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE),
            notes = str_flatten(notes, collapse = ';', na.rm = TRUE),
            earWidth = mean(earWidth, na.rm = TRUE)*0.1,
            earLength = mean(earLength, na.rm = TRUE)*0.1,
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE)*0.1,
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE),
            kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE),
            hundredKernelMass = mean(hundredKernelMass, na.rm = TRUE))

neMVEars <- read_excel('data/2023/hybrids/2023_Hyb_HIPS_LNK_MV_NP_Final_KL_Curated.xlsx', 
                       sheet = 'Data Entry Sheet - NoEmptyRows', 
                       skip = 1, 
                       col_types = c(rep('skip', 2), rep('text', 4), rep('skip', 3), 'text', rep('numeric', 10), 'text',
                                     'text'),
                       col_names = c('qrCode', 'topBox', 'bottomBox', 'rowBandNotes', 'earNotes', 'earWidth', 
                                     'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'shelledCobWidth', 
                                     'earLength', 'shelledCobMass', 'hundredKernelMass', 'kernelsPerEar', 'kernelColor', 'adminNotes')) %>%
  rowwise() %>%
  mutate(qrCode = case_when(str_detect(earNotes, 'Missing QR Code') ~ 'Missing QR Code', .default = qrCode)) %>%
  ungroup() %>%
  fill(qrCode) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         kernelColor = str_remove(kernelColor, fixed('$')), 
         location = case_when(str_detect(qrCode, 'NORTH PLATTE') ~ 'North Platte',
                              str_detect(qrCode, 'SCOTTSBLUFF') ~ 'Scottsbluff',
                              str_detect(qrCode, 'LINCOLN') ~ 'Lincoln',
                              str_detect(qrCode, 'MISSOURI VALLEY') ~ 'Missouri Valley'),
         range = str_split_i(qrCode, fixed('$'), 8) %>%
           str_remove('RANGE') %>%
           as.numeric(),
         plotNumber = str_split_i(qrCode, fixed('$'), 7) %>%
           str_split_i('ROW', 1) %>%
           str_remove('PLOT') %>%
           as.numeric() %>%
           case_when(range > 17 & location=='Missouri Valley' ~ . + 200, .default = .), 
         nitrogenTreatment = case_when(str_detect(qrCode, '150N') ~ 'Medium',
                                       str_detect(qrCode, '75N') ~ 'Low',
                                       str_detect(qrCode, '225N') ~ 'High'),
         row = str_split_i(qrCode, fixed('$'), 7) %>%
           str_split_i('ROW', 2) %>%
           as.numeric(),
         genotype = str_split_i(qrCode, fixed('@'), 2),
         kernelMassPerEar = earMass - shelledCobMass,
         topBox = case_when(!is.na(topBox) ~ 'Plants not completely dried down at harvest'),
         bottomBox = case_when(!is.na(bottomBox) ~ 'Less than 4 ears available to harvest')) %>%
  unite('notes', topBox, bottomBox, rowBandNotes, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  unite('earNotes', earNotes, adminNotes, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  group_by(qrCode, plotNumber, location, nitrogenTreatment, range, row, genotype) %>%
  summarise(earNotes = str_flatten(earNotes, collapse = ';', na.rm = TRUE),
            notes = max(notes, na.rm = TRUE),
            earWidth = mean(earWidth, na.rm = TRUE)*0.1, 
            earFillLength = mean(earFillLength, na.rm = TRUE)*0.1,
            kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE),
            kernelsPerRow = mean(kernelsPerRow, na.rm = TRUE), 
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE), 
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE)*0.1,
            earLength = mean(earLength, na.rm = TRUE)*0.1,
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            hundredKernelMass = mean(hundredKernelMass, na.rm = TRUE), 
            kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE), 
            kernelColor = max(kernelColor, na.rm = TRUE)) %>%
  unite('notes', notes, earNotes, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  filter(!(genotype %in% c('FILLER', 'SOLAR 1', 'SOLAR 2', 'SOLAR 3', 'SOLAR 4')))
  
df2 <- full_join(fieldData, acEarPhenotypes, join_by(qrCode), suffix = c('', '.ears'), keep = FALSE) %>%
  full_join(neMVEars, join_by(location, range, row), suffix = c('', '.ne')) %>%
  filter(!is.na(location)) %>%
  # remove an MV plot that was discarded in the yield data for a planting error that we phenotyped ears for
  filter(!(location=='Missouri Valley' & row==19 & range==11)) %>% 
  rowwise() %>%
  mutate(qrCode = max(qrCode, qrCode.ne, na.rm = TRUE),
         plotNumber = case_when(location %in% c('North Platte', 'Lincoln', 'Scottsbluff', 'Missouri Valley') ~ plotNumber.ne,  
                                .default = plotNumber),
         nitrogenTreatment = max(nitrogenTreatment, nitrogenTreatment.ne, na.rm = TRUE),
         earWidth = max(earWidth, earWidth.ne, na.rm = TRUE),
         kernelRowNumber = max(kernelRowNumber, kernelRowNumber.ne, na.rm = TRUE), 
         kernelMassPerEar = max(kernelMassPerEar, kernelMassPerEar.ne, na.rm = TRUE), 
         shelledCobWidth = max(shelledCobWidth, shelledCobWidth.ne, na.rm = TRUE),
         earLength = max(earLength, earLength.ne, na.rm = TRUE), 
         shelledCobMass = max(shelledCobMass, shelledCobMass.ne, na.rm = TRUE),
         kernelsPerEar = max(kernelsPerEar, kernelsPerEar.ne, na.rm = TRUE), 
         hundredKernelMass = max(hundredKernelMass, hundredKernelMass.ne, na.rm = TRUE)) %>%
  unite('notes', notes, notes.ears, remove = TRUE, na.rm = TRUE, sep = ';') %>%
  # add height and meta data in for the MV plot that yield data was dropped due to harvest glitch
  mutate(genotype = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 'B73 x PHZ51', .default = genotype),
         earHeight = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 139, .default = earHeight),
         flagLeafHeight = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 264, .default = flagLeafHeight), 
         pedigreeID = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 1739604, .default = pedigreeID),
         notes = case_when(location=='Missouri Valley' & row==2 & range==10 ~ paste0(notes, '; Tattoo sensor; harvest glitch'), .default = notes),
         sublocation = case_when(location=='Missouri Valley' ~ 'Missouri Valley', .default = sublocation),
         irrigationProvided = case_when(location=='Missouri Valley' ~ 0, .default = irrigationProvided), 
         poundsOfNitrogenPerAcre = case_when(location=='Missouri Valley' ~ 160, .default = poundsOfNitrogenPerAcre),
         experiment = case_when(location=='Missouri Valley' & is.na(experiment) ~ 'LC2125', .default = experiment),
         plotLength = case_when(location=='Missouri Valley' & is.na(experiment) ~ 17.5, .default = plotLength),
         block = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 1, .default = block), 
         plantingDate = case_when(location=='Missouri Valley' ~ '2023-05-02'),
         plantDensity = case_when(location=='Missouri Valley' & row==2 & range==10 ~ 34000, .default = plantDensity),
         harvestDate = case_when(location=='Missouri Valley' & row==2 & range==10 ~ ymd('2023-09-25'), .default = harvestDate)) %>%
  select(!ends_with('.ne'))

# phenotypes <- c("plantDensity", "combineYield", "combineTestWeight", "combineMoisture", "flagLeafHeight", "earHeight", "percentLodging",
#                 "yieldPerAcre", 'totalStandCount', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'kernelRowNumber', 'earWidth',
#                 'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelMassPerEar', 'kernelsPerEar', 'hundredKernelMass', 'earFillLength', 
#                 'kernelsPerRow')
# 
# plotRepCorr(df2, 'nitrogenTreatment', 'genotype', phenotypes, 'location')

# fixHundredKernelMass <- full_join(acEar, acCob, join_by(qrCode), keep = FALSE) %>%
#   full_join(acSeed, join_by(qrCode), keep = FALSE) %>%
#   filter(str_detect(qrCode, '23-C-1746410')|
#            str_detect(qrCode, '23-C-1746525')| 
#            str_detect(qrCode, '23-C-1746706')|
#            str_detect(qrCode, '23-C-1746805')) %>%
#   select(qrCode, earMass, shelledCobMass, kernelMassPerEar, kernelsPerEar, starts_with('notes'))

# Remove outliers and correct when they are wrong due to a data entry error, seed spill, or calculation error (hundredKernelMass)
df2 <- df2 %>%
  rowwise() %>%
  mutate(plantDensity = case_when(plantDensity < 20000 | plantDensity > 50000 ~ NA, 
                                  location=='Lincoln' ~ (totalStandCount/2)*(plotLength/17.5)*1000,
                                  .default = plantDensity),
         flagLeafHeight = case_when(flagLeafHeight < 100 ~ NA, .default = flagLeafHeight),
         earLength = case_when(earLength > 22.5 ~ NA, 
                               location=='Crawfordsville' & earLength < 12.5 ~ NA,
                               location=='North Platte' & (earLength < 10 | earLength > 20) ~ NA,
                               .default = earLength), 
         shelledCobMass = case_when(location=='Ames' & shelledCobMass > 50 ~ NA, 
                                    location=='Crawfordsville' & shelledCobMass > 40 ~ NA,
                                    .default = shelledCobMass), 
         kernelMassPerEar = case_when(location=='Crawfordsville' & kernelMassPerEar > 275 ~ NA, .default = kernelMassPerEar),
         combineTestWeight = case_when(combineTestWeight < 45 ~ NA, .default = combineTestWeight), 
         hundredKernelMass = case_when(qrCode=='23-C-1746410' ~ mean(c((121/484)*100, (104.2/422)*100)), 
                                       qrCode=='23-C-1746525' ~ mean(c((137/475)*100, (148.1/470)*100)),
                                       qrCode=='23-C-1746706' ~ (200/655)*100,
                                       qrCode=='23-C-1746805' ~ mean(c((112.5/440)*100, (86.2/361)*100)),
                                       hundredKernelMass > 45 | hundredKernelMass < 15 ~ NA,
                                       
                                       .default = hundredKernelMass),
         kernelsPerEar = case_when(qrCode=='23-C-1746688' ~ 1617/3, .default = kernelsPerEar),
         kernelRowNumber = case_when(kernelRowNumber < 11.25 ~ NA, .default = kernelRowNumber),
         earWidth = case_when(earWidth < 3.5 ~ NA, .default = earWidth),
         shelledCobWidth = case_when((shelledCobWidth < 2)|(shelledCobWidth > 3.25) ~ NA, .default = shelledCobWidth),
         shelledCobMass = case_when(location=='Ames' & shelledCobMass > 45 ~ NA, 
                                    location=='Lincoln' & shelledCobMass > 35 ~ NA, 
                                    location %in% c('Missouri Valley', 'North Platte') & (shelledCobMass > 42.5 | shelledCobMass < 10) ~ NA,
                                    .default = shelledCobMass),
         kernelsPerRow = case_when(kernelsPerRow < 20 ~ NA, .default = kernelsPerRow),
         kernelsPerEar = case_when(kernelsPerEar < 250 ~ NA, 
                                   location=='North Platte' & kernelsPerEar > 750 ~ NA, 
                                   .default = kernelsPerEar),
         plotNumber = case_when(qrCode=='LINCOLN$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT4165ROW16$RANGE8$@2369 X PHP02' ~ 4165,
                                qrCode=='LINCOLN$HYBRID HIPS$225N$DRYLAND$REP$1$PLOT6054ROW5$RANGE26$@B73 X PHM49' ~ 6054,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$1$PLOT119ROW21$RANGE27$@ER-PAIR 17' ~ 319,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$1$PLOT131ROW21$RANGE33$@ER-PAIR 26' ~ 331,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT205ROW21$RANGE35$@ER-PAIR 14' ~ 405,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT216ROW19$RANGE40$@ER-PAIR 17' ~ 416,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT212ROW20$RANGE39$@ER-PAIR 19' ~ 412,
                                qrCode=='MISSOURI VALLEY$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT206ROW21$RANGE36$@ER-PAIR 26' ~ 406,
                                qrCode=='NORTH PLATTE$HYBRID HIPS$150N$FULL$REP$2$PLOT352ROW41$RANGE4$@PHK76 X LH82' ~ 352,
                                qrCode=='NORTH PLATTE$HYBRID HIPS$150N$FULL$REP$2$PLOT236ROW12$RANGE6$@PHK56 X W606S' ~ 236,
                                qrCode=='NORTH PLATTE$HYBRID HIPS$150N$DRYLAND$REP$2$PLOT305ROW29$RANGE3$@PHK56 X W606S' ~ 305,
                                qrCode=='NORTH PLATTE$HYBRID HIPS$150N$DRYLAND$REP$1$PLOT170ROW44$RANGE14$@B37 X OH43' ~ 170,
                                .default = plotNumber)) %>%
  ungroup() %>%
  mutate(across(c(where(is.numeric), where(is.POSIXct)), ~case_when(.==-Inf ~ NA, .default = .)))

# # Should there be a correlation for the same genotype, even if one is under full irrigation & the other isn't? Let's test it by looking at NP1 vs NP3 (suggested by Nikee)
# hybridsWideByLoc <- hybrids %>% 
#   group_by(location, genotype, nitrogenTreatment) %>% 
#   mutate(genotypeObsNum = 1:n()) %>% 
#   pivot_wider(id_cols = c(genotype, genotypeObsNum, nitrogenTreatment), 
#               names_from = location, 
#               values_from = c(earHeight, combineYield, combineMoisture, combineTestWeight, yieldPerAcre))
# 
# # Plot NP1 vs NP3
# for (i in sbPhenos)
# {
#   phenoNP1 <- paste0(i, '_North Platte1')
#   phenoNP3 <- paste0(i, '_North Platte3')
#   
#   plot <- ggplot(hybridsWideByLoc, aes(.data[[phenoNP1]], .data[[phenoNP3]], color = nitrogenTreatment)) +
#     geom_point()
#   print(plot)
#   
#   print(i)
#   print(cor(hybridsWideByLoc[[phenoNP1]], hybridsWideByLoc[[phenoNP3]], use = 'complete.obs'))
# }
# 
# sb21224 <- read_excel('data/2023/hybrids/240212 Scottsbluff_Corn Trial 2023_Data_2023_v2 from Coffey.xlsx', 
#                       sheet = 'Data23', 
#                       skip = 3)
# 
# sb21224Wide <- sb21224 %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(Genotype),
#          location = 'SB') %>%
#   plotRepCorr('N rate', 'genotype', c('Ear Ht. (cm)', 'Plot Weight (lb)', 'Moisture (%)', 'Test Weight (lb/bu)'), 'location')
# 
# sbPhenos <- c('plantHeight', sbPhenos)
# 
# for (i in sbPhenos)
# {
#   mapResponse(scottsbluff, i)
# }
# 
# scottsbluffSP <- scottsbluff
# for (i in sbPhenos)
# {
#   scottsbluffSP <- getSpatialCorrections(scottsbluff, i)
# }
# 
# plotRepCorr(scottsbluff, 'nitrogenTreatment', 'genotype', sbPhenos, 'location')
# 
# #  Does SB 2023 rep1 correlate with other locations? 
# df3 <- filter(df2, !(location=='Scottsbluff' & range > 14)) 
# df3Wide <- df3%>% 
#   group_by(genotype, location, nitrogenTreatment) %>%
#   mutate(genotypeObsNum = 1:n()) %>%
#   pivot_wider(id_cols = c(genotype, genotypeObsNum, nitrogenTreatment),
#               names_from = c(location),
#               values_from = c(earHeight, combineYield, combineMoisture, combineTestWeight, yieldPerAcre))
# sbPhenos <- c('earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 'yieldPerAcre')
# for(i in 1:length(sbPhenos))
# {
#   phenoSB <- paste0(sbPhenos[i], '_Scottsbluff')
#   phenoLNK <- paste0(sbPhenos[i], '_Lincoln')
#   phenoMV <- paste0(sbPhenos[i], '_Missouri Valley')
#   phenoAmes <- paste0(sbPhenos[i], '_Ames')
#   phenoCF <- paste0(sbPhenos[i], '_Crawfordsville')
#   print(sbPhenos[i])
#   
#   plotSBLNK <- ggplot(df3Wide, aes(.data[[phenoSB]], .data[[phenoLNK]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoSB]], df3Wide[[phenoLNK]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotSBMV <- ggplot(df3Wide, aes(.data[[phenoSB]], .data[[phenoMV]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoSB]], df3Wide[[phenoMV]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotSBAmes <- ggplot(df3Wide, aes(.data[[phenoSB]], .data[[phenoAmes]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoSB]], df3Wide[[phenoAmes]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotSBCF <- ggplot(df3Wide, aes(.data[[phenoSB]], .data[[phenoCF]])) +
#     geom_point() +
#     
#     labs(subtitle = cor(df3Wide[[phenoSB]], df3Wide[[phenoCF]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotLNKMV <- ggplot(df3Wide, aes(.data[[phenoLNK]], .data[[phenoMV]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoLNK]], df3Wide[[phenoMV]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotLNKAmes <- ggplot(df3Wide, aes(.data[[phenoLNK]], .data[[phenoAmes]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoLNK]], df3Wide[[phenoAmes]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotLNKCF <- ggplot(df3Wide, aes(.data[[phenoLNK]], .data[[phenoCF]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoLNK]], df3Wide[[phenoCF]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotMVAmes <- ggplot(df3Wide, aes(.data[[phenoMV]], .data[[phenoAmes]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoMV]], df3Wide[[phenoAmes]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotMVCF <- ggplot(df3Wide, aes(.data[[phenoMV]], .data[[phenoCF]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoMV]], df3Wide[[phenoCF]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   plotAmesCF <- ggplot(df3Wide, aes(.data[[phenoAmes]], .data[[phenoCF]])) +
#     geom_point() +
#     labs(subtitle = cor(df3Wide[[phenoAmes]], df3Wide[[phenoCF]], use = 'complete.obs')) +
#     theme(legend.position = 'none')
#   
#   all <- plot_grid(plotSBLNK, plotSBMV, plotSBAmes, plotSBCF, plotLNKMV, plotLNKAmes, plotLNKCF, plotMVAmes, plotMVCF,
#                    plotAmesCF, nrow = 3)
#   print(all)
# }
# #  Does SB 2023 rep 1 correlate across nitrogen treatments?
# sbR1 <- filter(df3, location=='Scottsbluff')
# sbR1Wide <- sbR1 %>%
#   pivot_wider(id_cols = genotype, names_from = nitrogenTreatment, values_from = all_of(sbPhenos))
# 
# for(i in sbPhenos)
# {
#   phenoLow <- paste0(i, '_Low')
#   phenoMed <- paste0(i, '_Medium')
#   phenoHigh <- paste0(i, '_High')
#   
#   plotLM <- ggplot(sbR1Wide, aes(.data[[phenoLow]], .data[[phenoMed]])) + 
#     geom_point() + 
#     labs(subtitle = cor(sbR1Wide[[phenoLow]], sbR1Wide[[phenoMed]], use = 'complete.obs'))
#   
#   plotLH <- ggplot(sbR1Wide, aes(.data[[phenoLow]], .data[[phenoHigh]])) + 
#     geom_point() +
#     labs(subtitle = cor(sbR1Wide[[phenoLow]], sbR1Wide[[phenoHigh]], use = 'complete.obs'))
#   
#   plotMH <- ggplot(sbR1Wide, aes(.data[[phenoMed]], .data[[phenoHigh]])) +
#     geom_point() +
#     labs(subtitle = cor(sbR1Wide[[phenoMed]], sbR1Wide[[phenoHigh]], use = 'complete.obs'))
#   
#   all <- plot_grid(plotLM, plotLH, plotMH, nrow=1)
#   print(all)
# }

hybridHIPS23 <- filter(df2, location!='Scottsbluff')

# nColors <- moma.colors('vonHeyl')
# nColors <- c(nColors[3], nColors[2], nColors[1])
# 
# nitrogenResponseViolins23 <- hybridHIPS23 %>%
#   filter(!(location %in% c('Missouri Valley', 'North Platte'))) %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
#   ggplot(aes(nitrogenTreatment, yieldPerAcre, fill = nitrogenTreatment)) +
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
#     facet_wrap(vars(location), nrow = 1) + 
#     scale_fill_manual(values = nColors) +
#     labs(title = '2023') +
#     theme(panel.background = element_blank())
# nitrogenResponseViolins23
# 
# nitrogenResponseViolins22 <- hybrids %>%
#   filter(location!='Missouri Valley') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')),
#          location = factor(location, 
#                            levels = c('Crawfordsville', 'Ames', 'Lincoln', 'North Platte1', 'North Platte2', 'North Platte3',
#                                       'Scottsbluff'))) %>%
#   ggplot(aes(nitrogenTreatment, yieldPerAcre, fill = nitrogenTreatment)) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
#   facet_wrap(vars(location), nrow = 2) + 
#   scale_fill_manual(values = nColors) +
#   labs(title = '2022') +
#   theme(panel.background = element_blank())
# nitrogenResponseViolins22
# 
# # Was there a statistically significant N effect on yield? 
# # 2022: statistically significant at all locations except NP1, but probably not practically significant in many sites 
# for(loc in c('Crawfordsville', 'Ames', 'Lincoln', 'North Platte1', 'North Platte2', 'North Platte3',
#             'Scottsbluff'))
# {
#   df <- filter(hybrids, location==loc)
#   print(loc)
#   model <- lmer(yieldPerAcre ~ nitrogenTreatment + (1|genotype) + (1|row) + (1|range), data = df)
#   print(Anova(model, type = 3))
# }
# 
# # 2023: Statistically significant at Ames and Crawfordsville, not significant at Lincoln
# for(loc in c('Crawfordsville', 'Ames', 'Lincoln'))
# {
#   df <- filter(hybridHIPS23, location==loc)
#   print(loc)
#   model <- lmer(yieldPerAcre ~ nitrogenTreatment + (1|genotype) + (1|row) + (1|range), data = df)
#   print(Anova(model, type = 3))
# }

hybridHIPS23 <- mutate(hybridHIPS23, year = '2023') %>%
  rowwise() %>%
  mutate(plantingDate = as.character(plantingDate), 
         anthesisDate = as.character(anthesisDate),
         silkDate = as.character(silkDate), 
         harvestDate = as.character(harvestDate) %>%
           str_split_i(' ', 1))
hybrids22 <- read.csv('outData/HIPS_2022_V3.7_HYBRIDS.csv') %>%
  mutate(year = '2022') %>%
  rowwise() %>%
  mutate(pedigreeID = list(case_when(genotype %in% pedigreeIDKey$genotype ~ pedigreeIDKey$pedigreeID[pedigreeIDKey$genotype==genotype])) %>%
           as.double(),
         harvestDate = case_when(harvestDate=='-Inf' ~ NA,
                                 .default = harvestDate) %>%
           str_split_i(' ', 1)) %>%
  mutate(harvestDate = case_when(str_detect(harvestDate, fixed('/')) ~ mdy(harvestDate), .default = ymd(harvestDate)) %>%
           as.character())

# 2023 HIPS weather
sites <- tibble(location = c('North Platte', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville'),
                lat = c(41.08808149, 40.8606363814325, 41.66986803903, 41.9857796124525, 41.19451639818),
                lon = c(-100.775524839895, -96.5982886186525, -95.94593885585, -93.6916168881725, -91.479082779405))
weather.pwr <- tibble(location = NULL)
for (i in 1:length(sites$location))
{
  siteWeather23 <- get_power(community = 'ag',
                             pars = c('T2M_MAX', 'T2M_MIN'),
                             temporal_api = 'daily', 
                             lonlat = c(sites$lon[i], sites$lat[i]), 
                             dates = c('2023-01-01', '2023-12-31')) %>%
    mutate(location = sites$location[i]) %>%
    rowwise() %>%
    mutate(tmin = celsius.to.fahrenheit(T2M_MIN), 
           tmax = celsius.to.fahrenheit(T2M_MAX)) %>%
    select(location, LAT, LON, YYYYMMDD, tmin, tmax) %>%
    mutate(GDD = getGDDs(tmin, tmax)) %>%
    rename(date = YYYYMMDD,
           lat = LAT,
           lon = LON)
  weather.pwr <- bind_rows(weather.pwr, siteWeather23)
}

npFieldWeather <- read_excel('data/2023/2023 Weather Stations/HIPS_North_Platte_2023.xlsx', skip = 2, col_names = FALSE)[, c(1, 8)]
colnames(npFieldWeather) <- c('datetime', 'temp')
npFieldWeather <- mutate(npFieldWeather, location='North Platte')

lnkFieldWeather <- read_excel('data/2023/2023 Weather Stations/HIPS_Lincoln_true_2023.xlsx', skip = 2, col_names = FALSE)[, c(1, 8)]
colnames(lnkFieldWeather) <- c('datetime', 'temp')
lnkFieldWeather <- mutate(lnkFieldWeather, location='Lincoln')

mvFieldWeather <- read_excel('data/2023/2023 Weather Stations/2023_HIPS_MO_Valley.xlsx', skip = 3, col_names = FALSE)[, c(1, 8)]
colnames(mvFieldWeather) <- c('datetime', 'temp')
mvFieldWeather <- mutate(mvFieldWeather, location='Missouri Valley')

amesFieldWeather <- read_excel('data/2023/2023 Weather Stations/HIPS_Ames_2023.xlsx', skip = 2, col_names = FALSE)[, c(1, 8)]
colnames(amesFieldWeather) <- c('datetime', 'temp')
amesFieldWeather <- mutate(amesFieldWeather, location='Ames')

cfFieldWeather <- read_excel('data/2023/2023 Weather Stations/HIPS_Crawordsville_2023.xlsx', skip = 2, col_names = FALSE)[, c(1, 8)]
colnames(cfFieldWeather) <- c('datetime', 'temp')
cfFieldWeather <- mutate(cfFieldWeather, location='Crawfordsville')

fieldWeather <- bind_rows(npFieldWeather, lnkFieldWeather, mvFieldWeather, amesFieldWeather, cfFieldWeather) %>%
  rowwise() %>%
  mutate(date = date(datetime)) %>%
  filter(!is.na(date)) %>%
  ungroup() %>%
  group_by(location, date) %>%
  summarise(tmax = max(temp, na.rm = TRUE),
            tmin = min(temp, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(GDD = getGDDs(tmin, tmax))

npDays <- filter(fieldWeather, location=='North Platte')$date
lnkDays <- filter(fieldWeather, location=='Lincoln')$date
mvDays <- filter(fieldWeather, location=='Missouri Valley')$date
amesDays <- filter(fieldWeather, location=='Ames')$date
cfDays <- filter(fieldWeather, location=='Crawfordsville')$date

pwr.impute <- filter(weather.pwr, !((location=='North Platte' & date %in% npDays) |
                                      (location=='Lincoln' & date %in% lnkDays) | 
                                      (location=='Missouri Valley' & date %in% mvDays) |
                                      (location=='Ames' & date %in% amesDays) |
                                      (location=='Crawfordsville' & date %in% cfDays)))

weather23Imputed <- bind_rows(fieldWeather, pwr.impute)
write.csv(weather23Imputed, 'outData/2023ImputedWeatherData.csv', quote = FALSE, row.names = FALSE)

hybridHIPS23 <- hybridHIPS23 %>%
  rowwise() %>%
  mutate(plantingDate = case_when(location=='Lincoln' ~ '2023-05-16',
                                  location=='North Platte' ~ '2023-05-10',
                                  .default = plantingDate)) %>%
  mutate(GDDToAnthesis = getCumulativeGDDs(plantingDate, anthesisDate, weather23Imputed, location),
         GDDToSilk = getCumulativeGDDs(plantingDate, silkDate, weather23Imputed, location)) %>%
  mutate(anthesisSilkingIntervalGDD = GDDToSilk - GDDToAnthesis) %>%
  mutate(across(c(where(is.numeric), where(is.POSIXct)), ~case_when(.==-Inf ~ NA, .default = .))) %>%
  # Remove outliers
  mutate(anthesisSilkingIntervalGDD = case_when(anthesisSilkingIntervalGDD > 200|anthesisSilkingIntervalGDD < -50 ~ NA, 
                                                .default = anthesisSilkingIntervalGDD),
         GDDToSilk = case_when(GDDToSilk < 1100 ~ NA, .default = GDDToSilk),
         GDDToAnthesis = case_when(GDDToAnthesis < 1100 ~ NA, .default = GDDToAnthesis))


hybridHIPS <- bind_rows(hybrids22, hybridHIPS23) %>%
  rowwise() %>%
  mutate(percentLodging = round(percentLodging, 2),
         earHeight = round(earHeight, 0),
         flagLeafHeight = round(flagLeafHeight, 0),
         plantDensity = round(plantDensity, 0), 
         combineYield = round(combineYield, 2),
         yieldPerAcre = round(yieldPerAcre, 2),
         combineMoisture = round(combineMoisture, 1),
         combineTestWeight = round(combineTestWeight, 1),
         earLength = round(earLength, 3),
         earFillLength = round(earFillLength, 3),
         earWidth = round(earWidth, 3),
         shelledCobWidth = round(shelledCobWidth, 3), 
         kernelsPerRow = round(kernelsPerRow, 1), 
         kernelRowNumber = round(kernelRowNumber, 1),
         kernelsPerEar = round(kernelsPerEar, 0),
         hundredKernelMass = round(hundredKernelMass, 3),
         kernelMassPerEar = round(kernelMassPerEar, 1),
         shelledCobMass = round(shelledCobMass, 3)) %>%
  arrange(year, location, sublocation, block, plotNumber) %>%
  relocate(qrCode, year, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, experiment, plotLength, totalStandCount, block, row, range, plotNumber, 
           genotype, pedigreeID, plantingDate, anthesisDate, silkDate, daysToAnthesis, daysToSilk, anthesisSilkingInterval, 
           GDDToAnthesis, GDDToSilk, anthesisSilkingIntervalGDD, earHeight, flagLeafHeight, plantDensity, combineYield, yieldPerAcre, combineMoisture, combineTestWeight, 
           earLength, earFillLength, earWidth, shelledCobWidth, kernelsPerRow, kernelRowNumber, kernelsPerEar, hundredKernelMass, kernelMassPerEar, shelledCobMass, 
           percentMoisture, percentStarch, percentProtein, percentOil, percentFiber, percentAsh, kernelColor, percentLodging, harvestDate, notes) %>%
  select(!c(ERNumber, irrigationTreatment, plantHeight))

write.csv(hybridHIPS, 'outData/HIPS_HYBRIDS_2022_AND_2023_V2.3.csv',  quote = FALSE, sep = ',', na = '', row.names = FALSE, col.names = TRUE)






