library(tidyverse)
library(readxl)
library(lubridate)
source('src/Functions.R')

df <- read.csv('../../../../../../../Downloads/2022_2023_inbred_HIPS_data_05_20_2024_v4.csv') %>% 
  mutate(earHeight = as.numeric(earHeight), 
         flagLeafHeight = as.numeric(flagLeafHeight), 
         plantDensity = as.numeric(plantDensity),
         experiment = case_when(experiment=='' ~ NA, .default = experiment),
         plantingDate = mdy(plantingDate),
         harvestDate = mdy(harvestDate),
         anthesisDate = mdy(anthesisDate),
         silkDate = mdy(silkDate)) %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .))) %>%
  mutate(plantingDate = as.character(plantingDate),
         harvestDate = as.character(harvestDate),
         anthesisDate = as.character(anthesisDate),
         silkDate = as.character(silkDate)) %>%
  arrange(year, location, sublocation, block, plotNumber) %>%
  relocate(qrCode, year, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, experiment, plotLength, totalStandCount, block, row, range, plotNumber, 
           genotype, pedigreeID, plantingDate, anthesisDate, silkDate, daysToAnthesis, daysToSilk, anthesisSilkingInterval, 
           GDDToAnthesis, GDDToSilk, anthesisSilkingIntervalGDD, earHeight, flagLeafHeight, plantDensity, 
           earLength, earFillLength, earWidth, shelledCobWidth, kernelsPerRow, kernelRowNumber, kernelsPerEar, hundredKernelMass, kernelMassPerEar, shelledCobMass, 
           kernelColor, harvestDate, notes) %>%
  select(!starts_with('percent'))

# Export combined inbred data 
write.csv(df, 'outData/INBREDS_2022_2023_v1.csv', quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)

phenotypes <- c('earHeight', 'flagLeafHeight', 'daysToAnthesis', 'daysToSilk', 'totalStandCount', 'anthesisSilkingInterval', 'GDDToAnthesis',
                'GDDToSilk', 'anthesisSilkingIntervalGDD', 'plantDensity', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 
                'kernelsPerRow', 'kernelRowNumber', 'kernelsPerEar', 'hundredKernelMass', 'kernelMassPerEar', 'shelledCobMass')

plotRepCorr(df, 'nitrogenTreatment', 'genotype', phenotypes, 'location')

# lnk <- filter(df, location=='Lincoln')
# plotRepCorr(lnk, 'nitrogenTreatment', 'genotype', phenotypes, 'location')
# 
# ft <- read.csv('data/2023/inbreds/UNL Inbreds flowering note 2023 - Ullagaddi Transcribed.csv')
# ft <- ft[, c(2, 3, 5, 7)]
# colnames(ft) <- c('plotNumber', 'anthesisDate', 'silkDate', 'notes')
# 
# ft <- ft %>%
#   rowwise() %>%
#   mutate(anthesisDay = str_split_i(anthesisDate, '-', 1) %>%
#            as.numeric(),
#          anthesisMonth = str_split_i(anthesisDate, '-', 2), 
#          silkDay = str_split_i(silkDate, '-', 1) %>%
#            as.numeric(),
#          silkMonth = str_split_i(silkDate, '-', 2)) %>%
#   mutate(anthesisDay = case_when(anthesisDate==' 7/20' ~ 20, .default = anthesisDay),
#          anthesisMonth = case_when(anthesisDate==' 7/20' ~ '07', 
#      '                              anthesisMonth=='Jul' ~ '07',
#                                    anthesisMonth=='Aug' ~ '08',
#                                    anthesisMonth=='Sep' ~ '09', 
#                                    anthesisMonth=='May' ~ '05'), 
#          silkDay = case_when(silkDate=='23/8' ~ 23,
#                              silkDay > 31 ~ NA, 
#                              .default = silkDay), 
#          silkMonth = case_when(silkDate=='23/8' ~ '08',
#                                silkMonth=='Jul' ~ '07',
#                                silkMonth=='Aug' ~ '08', 
#                                silkMonth=='Sep' ~ '09')) %>%
#   mutate(anthesisDate = case_when(!is.na(anthesisMonth) & !is.na(anthesisDay) ~ str_c('2023', anthesisMonth, anthesisDay, sep = '-')),
#          silkDate = case_when(!is.na(silkMonth) & !is.na(silkDay) ~ str_c('2023', silkMonth, silkDay, sep = '-'))) %>%
#   mutate(anthesisDate = ymd(anthesisDate),
#          silkDate = ymd(silkDate))
# 
# lnkIndex <- read_excel('data/2023/Summary of HIPS 2023 Maps for Fields Visited by J Schanble Lab.xlsm',
#                        sheet = 'Lincoln Inbreds - Index', 
#                        skip = 1,
#                        col_types = c(rep('skip', 2), 'numeric', 'text', rep('skip', 4), 'text', 'skip', 'text', rep('skip', 9)),
#                        col_names = c('plotNumber', 'location', 'nitrogenTreatment', 'genotype'))
# 
# lnkFT <- full_join(lnkIndex, ft, join_by(plotNumber), keep = FALSE, suffix = c('', '')) %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          plantingDate = ymd('2023-05-09')) %>%
#   mutate(daysToAnthesis = difftime(as.Date(anthesisDate), as.Date(plantingDate)) %>%
#            as.integer(),
#          daysToSilk = difftime(as.Date(silkDate), as.Date(plantingDate)) %>%
#            as.integer(),
#          anthesisSilkingInterval = difftime(as.Date(silkDate), as.Date(anthesisDate)) %>%
#            as.integer())
# 
# plotRepCorr(lnkFT, 'nitrogenTreatment', 'genotype', c('daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval'), 'location')


df <- df %>% 
  select(qrCode, year, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, experiment, plotLength, totalStandCount,
         block, row, range, plotNumber, genotype, pedigreeID, plantingDate, anthesisDate, silkDate, daysToAnthesis, daysToSilk, 
         anthesisSilkingInterval, earHeight, flagLeafHeight, notes) %>%
  rowwise() %>%
  mutate(across(where(is.character), ~case_when(.=='' ~ NA, .default = .)))

inbreds <- read.csv('outData/INBREDS_2022_2023_v1.csv')
nonMissingVals <- 0
totalPlots <- length(inbreds$qrCode)
vars <- phenotypes
inbreds <- inbreds %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
for(var in vars)
{
  numMissingVals <- as.numeric(sum(is.na(inbreds[[var]])))
  numNotMissing <- totalPlots - numMissingVals
  nonMissingVals <- nonMissingVals + numNotMissing
}
