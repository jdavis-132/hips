library(tidyverse)
library(MoMAColors)
library(readxl)
library(lubridate)
source('src/Functions.R')

nitrogenColors <- moma.colors('vonHeyl')
nitrogenColors <- c(nitrogenColors[3], nitrogenColors[2], nitrogenColors[1])

hybridsFinal <- read.csv('outData/HIPS_2022_V3.5_HYBRIDS.csv') %>%
  rowwise() %>%
  mutate(anonymizedLocation = case_when(location=='Scottsbluff' ~ 'Location 1',
                                        str_detect(location, 'North Platte') ~ 'Location 2',
                                        location=='Lincoln' ~ 'Location 3',
                                        location=='Missouri Valley' ~ 'Location 4',
                                        location=='Ames' ~ 'Location 5',
                                        location=='Crawfordsville' ~ 'Location 6'),
         nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')))

hybridsFinalWide <- hybridsFinal %>%
  group_by(genotype, location, nitrogenTreatment) %>%
  mutate(genotypeRepNum = 1:n()) %>%
  ungroup() %>%
  pivot_longer(c(kernelRowNumber, yieldPerAcre, earWidth), names_to = 'phenotype', values_to = 'value') %>%
  pivot_wider(id_cols = c(genotype, nitrogenTreatment, location, anonymizedLocation), names_from = c(phenotype, genotypeRepNum), values_from = value, names_sep = '.')
 
repCorrelationFixedSB <- hybridsFinalWide %>%
  filter(location %in% c('Scottsbluff', 'North Platte1', 'Lincoln')) %>%
  group_by(location) %>%
  mutate(correlation = cor(kernelRowNumber.1, kernelRowNumber.2, use = 'complete.obs') %>%
           round(digits = 3) %>%
           as.character()) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(kernelRowNumber.1, kernelRowNumber.2, color = nitrogenTreatment)) +
    geom_point() +
    facet_wrap(vars(anonymizedLocation, correlation)) + 
    scale_color_manual(values = nitrogenColors) +
    labs(x = 'Kernel Row Number, Replicate 1', y = 'Kernel Row Number, Replicate 2', color = 'Nitrogen Treatment') +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = 14),
          axis.text.x = element_text(color = 'black', size = 14, angle = 45),
          strip.text = element_text(color = 'black', size = 14),
          line = element_line(color = 'black', linewidth = 1),
          legend.position = 'top',
          plot.background = element_blank(),
          panel.background = element_blank())
repCorrelationFixedSB

repCorrelationFixedMVEar <- hybridsFinalWide %>%
  filter(location %in% c('North Platte1', 'Lincoln', 'Missouri Valley')) %>%
  group_by(location) %>%
  mutate(correlation = cor(kernelRowNumber.1, kernelRowNumber.2, use = 'complete.obs') %>%
           round(digits = 3) %>%
           as.character()) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(kernelRowNumber.1, kernelRowNumber.2, color = nitrogenTreatment)) +
  geom_point() +
  facet_wrap(vars(anonymizedLocation, correlation)) + 
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Kernel Row Number, Replicate 1', y = 'Kernel Row Number, Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
repCorrelationFixedMVEar

repCorrelationFixedMVYield <- hybridsFinalWide %>%
  filter(location %in% c('North Platte1', 'Lincoln', 'Missouri Valley')) %>%
  group_by(location) %>%
  mutate(correlation = cor(yieldPerAcre.1, yieldPerAcre.2, use = 'complete.obs') %>%
           round(digits = 3) %>%
           as.character()) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(yieldPerAcre.1, yieldPerAcre.2, color = nitrogenTreatment)) +
  geom_point() +
  facet_wrap(vars(anonymizedLocation, correlation)) + 
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Yield (Bushels/Acre), Replicate 1', y = 'Yield (Bushels/Acre), Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
repCorrelationFixedMVYield

perfectVariableCorrelation <- hybridsFinal %>%
  mutate(earFillLength = earLength) %>%
  ggplot(aes(earLength, earFillLength, color = nitrogenTreatment)) +
    geom_point() +
    scale_color_manual(values = nitrogenColors) + 
    labs(x = 'Ear Length (cm)', y = 'Ear Fill Length (cm)', color = 'Nitrogen Treatment') + 
    theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
perfectVariableCorrelation

variableCorrelationCorrected <- ggplot(hybridsFinal, aes(earLength, earFillLength, color = nitrogenTreatment)) +
  geom_point() +
  scale_color_manual(values = nitrogenColors) + 
  labs(x = 'Ear Length (cm)', y = 'Ear Fill Length (cm)', color = 'Nitrogen Treatment') + 
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
variableCorrelationCorrected


# Re-wrangle SB, NP, LNK, & MV hybrids to get the raw data together in one data frame to do the plots where issues existed
unlEars <- read.csv('data/plotleveleardata_v2.csv') %>%
  rename(qrCode = BarcodeAsScanned, 
         earLength = Cob.Length, 
         earFillLength = Kernel.Fill.Length,
         earWidth = Ear.Width, 
         shelledCobWidth = Cob.Width,
         kernelsPerEar = Kernel.Count,
         hundredKernelMass = X100.Kernel.weight,
         kernelsPerRow = Kernels.per.Row,
         kernelRowNumber = Kernel.Row.Number,
         kernelMassPerEar = KernelMass,
         kernelColor = KernelColor,
         shelledCobMass = Cob.Weight) %>%
  select(!c(Total.Moisture, KernelStriping)) %>%
  mutate(qrCode = str_to_upper(qrCode))

sbEars <- unlEars %>%
  filter(str_detect(qrCode, 'SCOTTSBLUFF') & str_detect(qrCode, 'HYBRID')) %>%
  rowwise() %>%
  mutate(location = 'Scottsbluff',
         anonymizedLocation = 'Location 1',
         nitrogenTreatment = case_when(str_detect(qrCode, 'HIGH') ~ 'Low',
                                       str_detect(qrCode, 'MEDIUM') ~ 'Medium',
                                       str_detect(qrCode, 'LOW') ~ 'High'),
         plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
           str_remove('PLOT') %>%
           as.numeric(),
         row = str_split_i(qrCode, fixed('$'), 5) %>%
           str_remove('ROW') %>%
           as.numeric(),
         range = str_split_i(qrCode, fixed('$'), 6) %>%
           str_remove('RANGE') %>%
           as.numeric(),
         genotype = str_split_i(qrCode, fixed('$'), 7))

npEars <- unlEars %>%
  filter(str_detect(qrCode, 'NORTH PLATTE')) %>%
  rowwise() %>%
  mutate(location = case_when(str_detect(qrCode, 'FULL') ~ 'North Platte1',
                              str_detect(qrCode, 'PARTIAL') ~ 'North Platte2',
                              str_detect(qrCode, 'NO IRRIGATION') ~ 'North Platte3'),
         anonymizedLocation = 'Location 2',
         nitrogenTreatment = case_when(str_detect(qrCode, 'HIGH') ~ 'High',
                                       str_detect(qrCode, 'MEDIUM') ~ 'Medium',
                                       str_detect(qrCode, 'LOW') ~ 'Low'),
         plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
           str_remove('PLOT') %>%
           as.numeric(),
         row = str_split_i(qrCode, fixed('$'), 5) %>%
           str_remove('ROW') %>%
           as.numeric(),
         range = str_split_i(qrCode, fixed('$'), 6) %>%
           str_remove('RANGE') %>% 
           as.numeric(),
         genotype = str_split_i(qrCode, fixed('$'), 7))

lnkEars <- unlEars %>%
  filter(!is.na(as.numeric(str_split_i(qrCode, fixed('$'), 1)))) %>%
  rowwise() %>%
  mutate(location = 'Lincoln',
         anonymizedLocation = 'Location 3',
         plotNumber = str_split_i(qrCode, fixed('$'), 1) %>%
           as.numeric(),
         row = str_split_i(qrCode, fixed('$'), 2) %>%
           str_remove('ROW') %>%
           as.numeric(),
         range = str_split_i(qrCode, fixed('$'), 3) %>%
           str_remove('RANGE') %>%
           as.numeric(),
         genotype = str_split_i(qrCode, fixed('$'), 4))

mvEars <- unlEars %>%
  filter(str_detect(qrCode, 'MV') & str_detect(qrCode, 'HYBRID')) %>%
  rowwise() %>%
  mutate(location = 'Missouri Valley',
         anonymizedLocation = 'Location 4', 
         nitrogenTreatment = 'Medium',
         plotNumber = case_when(str_detect(qrCode, 'REP1') ~ 
                                  str_split_i(qrCode, fixed('$'), 4) %>%
                                    str_remove('PLOT') %>%
                                    as.numeric() + 100,
                                str_detect(qrCode, 'REP2') ~ 
                                  str_split_i(qrCode, fixed('$'), 4) %>%
                                    str_remove('PLOT') %>%
                                    as.numeric() + 200),
         row = str_split_i(qrCode, fixed('$'), 5) %>%
           str_remove('ROW') %>%
           as.numeric(),
         range = str_split_i(qrCode, fixed('$'), 6) %>%
           str_remove('RANGE') %>%
           as.numeric(),
         genotype = str_split_i(qrCode, fixed('$'), 7))

err_genos719 <- paste0('PHW52 X PHM', 50:74)
unlEars <- bind_rows(sbEars, npEars, lnkEars, mvEars) %>%
  filter(!(genotype %in% c('', 'BORDER', 'SOLAR 4', 'SOLAR 3', 'SOLAR 2', 'SOLAR 1', 'FILL'))) %>%
  fixGenos(hips1.5_genoFixKey) %>%
  group_by(location, nitrogenTreatment, plotNumber) %>%
  filter(!(genotype %in% err_genos719) & 
           !(location=='North Platte3' & plotNumber==1478) & 
           !(location=='Lincoln' & plotNumber==5135) &
           !(location=='North Platte3' & plotNumber==1493) &
           !(location=='North Platte3' & plotNumber==1496)) %>%
  rowwise() %>%
  mutate(genotype = case_when(location=='North Platte3' & plotNumber==1426 ~ 'PHK76 X LH198',
                              location=='North Platte3' & plotNumber==1436 ~ '2369 X PHZ51',
                              location=='North Platte3' & plotNumber==1432 ~ 'PHB47 X 3IIH6',
                              location=='North Platte3' & plotNumber==1333 ~ 'PHP02 X LH198',
                              location=='North Platte3' & plotNumber==1331 ~ 'PHP02 X PHK76',
                              location=='North Platte3' & plotNumber==1328 ~ 'PHK76 X LH198',
                              location=='North Platte3' & plotNumber==1327 ~ 'PHK76 X LH82',
                              location=='North Platte3' & plotNumber==1326 ~ 'PHK76 X LH145',
                              location=='North Platte1' & plotNumber==214 ~ 'PHK56 X LH198',
                              .default = genotype)) %>%
  add_row(qrCode = '5135$ROW19$RANGE8$PHJ40 X LH82',
          earLength = mean(19, 15.5, 14, 14.5),
          earFillLength = mean(17, 15, 12.5, 13),
          earWidth = mean(4, 4, 3.5, 3.5),
          shelledCobWidth = mean(3, 3, 2.5, 2.5),
          shelledCobMass = mean(27.87, 24.25, 15.03, 15.24),
          kernelsPerEar = mean(597, 558, 433, 433),
          hundredKernelMass = mean(25.49, 20.87, 16.41, 18.13),
          kernelMassPerEar = mean(187.15 - 27.87, 137.29 - 24.25, 83.44 - 15.03, 96.84 - 15.24),
          kernelsPerRow = mean(36, 36, 28, 29),
          kernelRowNumber = 16,
          kernelColor = 'yellow',
          plotNumber = 5135,
          location = 'Lincoln',
          range = 8,
          row = 19,
          genotype = 'PHJ40 X LH82') %>%
  add_row(qrCode = 'NORTH PLATTE$HIPS - NO IRRIGATION - MEDIUM NITROGEN$REP2$PLOT1493$ROW19$RANGE24$PHP02 X PHJ89',
          earLength = mean(16.5, 17, 16, 17.5),
          earFillLength = mean(12, 13.5, 13, 13.5),
          earWidth = mean(4, 3.5, 4, 4),
          shelledCobWidth = mean(3, 3, 3, 3),
          shelledCobMass = mean(14.2, 15.2, 15.4, 16.3),
          kernelsPerEar = mean(345, 414, 432, 421),
          hundredKernelMass = mean(20.6, 17.2, 16.9, 15.7),
          kernelMassPerEar = mean(85.1 - 14.2, 87.5 - 15.2, 90.2 - 15.4, 79.6 - 16.3),
          kernelsPerRow = mean(27, 31, 32, 29),
          kernelRowNumber = mean(14, 14, 14, 16),
          kernelColor = 'yellow', 
          plotNumber = 1493,
          location = 'North Platte3',
          range = 24,
          row = 19, 
          genotype = 'PHP02 X PHJ89') %>%
  add_row(qrCode = 'NORTH PLATTE$HIPS - NO IRRIGATION - MEDIUM NITROGEN$REP2$PLOT1496$ROW22$RANGE24$4N506 X 3IIH6',
          earLength = mean(16, 16, 17, 18),
          earFillLength = mean(13.5, 14.5, 15, 15.5),
          earWidth = mean(4, 4, 4, 4),
          shelledCobWidth = mean(2.5, 2.5, 2.5, 3),
          shelledCobMass = mean(16, 16.8, 16.5, 19.9),
          kernelsPerEar = mean(485, 522, 416, 561),
          hundredKernelMass = mean(18.9, 19.7, 24.5, 22.8),
          kernelMassPerEar = mean(109.1 - 16, 117.6 - 16.8, 115.6 - 16.5, 148 - 19.9),
          kernelsPerRow = mean(33, 33, 31, 35),
          kernelRowNumber = mean(16, 18, 14, 16),
          kernelColor = 'yellow', 
          plotNumber = 1496,
          location = 'North Platte3',
          range = 24,
          row = 22, 
          genotype = '4N506 X 3IIH6') %>%
  ungroup() %>%
  group_by(location, plotNumber, genotype, range, row) %>%
  summarise(qrCode = max(qrCode),
            nitrogenTreatment = max(nitrogenTreatment),
            earLength = mean(earLength),
            earFillLength = mean(earFillLength),
            earWidth = mean(earWidth),
            shelledCobWidth = mean(shelledCobWidth),
            shelledCobMass = mean(shelledCobMass),
            kernelsPerEar = mean(kernelsPerEar),
            hundredKernelMass = mean(hundredKernelMass),
            kernelMassPerEar = mean(kernelMassPerEar),
            kernelColor = max(kernelColor),
            kernelsPerRow = mean(kernelsPerRow),
            kernelRowNumber = mean(kernelRowNumber))

nir <- read.csv('data/HybridHIPS_plotlevelNIR_v2.6.csv', 
                header = FALSE, 
                col.names = c('qrCode', 'location', 'nitrogenTreatment', 'irrigation', 'rep', 'row', 'range', 'plotNumber', 'genotype', 
                              'percentStarch', 'percentProtein', 'percentOil', 'percentFiber', 'percentAsh', 'percentMoisture')) %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode),
         plotNumber = case_when(location=='Missouri Valley' & rep==1 ~ as.numeric(plotNumber) + 100,
                                location=='Missouri Valley' & rep==2 ~ as.numeric(plotNumber) + 200,
                                .default = as.numeric(plotNumber))) %>%
  select(qrCode, location, row, range, plotNumber, nitrogenTreatment, starts_with('percent'))

hybridsRaw <- full_join(unlEars, nir, join_by(location, plotNumber, row, range), suffix = c('', '.nir'), keep = FALSE)

hybridsRaw <- hybridsRaw %>%
  rowwise() %>%
  mutate(nitrogenTreatment = max(nitrogenTreatment, nitrogenTreatment.nir, na.rm = TRUE)) %>%
  select(!ends_with('.nir'))

sbCombineData <- read_excel('data/Dipak Corn22_HM.xlsx',
                            skip = 1,
                            col_types = c(rep('skip', 3), rep('numeric', 4), 'text', 'skip'),
                            col_names = c('plotNumber', 'combineYield', 'combineMoisture', 'combineTestWeight', 'notes')) %>%
  filter(is.na(notes)) %>%
  rowwise() %>%
  mutate(yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 22.5)) %>%
  select(plotNumber, combineYield, combineMoisture, combineTestWeight, yieldPerAcre)

sbHeight <- read_excel('data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx', 
                       sheet = 'Hybrid_height data',
                       skip = 2,
                       col_types = c('numeric', 'skip', 'numeric', 'numeric', 'skip'),
                       col_names = c('plotNumber', 'earHeight', 'flagLeafHeight')) %>%
  mutate(plotNumber = plotNumber + 1000, 
         earHeight = cm(earHeight) %>%
           round(digits = 2),
         flagLeafHeight = cm(flagLeafHeight) %>%
           round(digits = 2))
sb <- full_join(sbCombineData, sbHeight, join_by(plotNumber), suffix = c('', ''), keep = FALSE) %>%
  mutate(location = 'Scottsbluff')

hybridsRaw <- full_join(hybridsRaw, sb, join_by(location, plotNumber), suffix = c('', ''), keep = FALSE)

np1FieldData <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx', 
                          sheet = 'Full Data',
                          skip = 4, 
                          col_types = c('skip', 'numeric', rep('skip', 8), 'numeric', rep('skip', 4), rep('numeric', 2), rep('skip', 8), rep('numeric', 4), 
                                        'skip', rep('text', 3), rep('skip', 6)),
                          col_names = c('plotNumber', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'combineYield', 'combineMoisture', 
                                        'combineTestWeight', 'plotLength', 'combineNotes', 'notesLC', 'notesHL')) %>%
  filter(is.na(combineNotes) & is.na(notesLC) & is.na(notesHL)) %>%
  rowwise() %>%
  mutate(location = 'North Platte1',
         flagLeafHeight = flagLeafHeight*100,
         earHeight = earHeight*100,
         plotLength = case_when(is.na(plotLength) ~ 17.5, .default = plotLength), 
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, plotLength)) %>%
  select(!contains('otes'))

np2FieldData <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx',
                           sheet = 'Reduced Irr Data', 
                           skip = 4,
                           col_types = c('skip', 'numeric', rep('skip', 8), 'numeric', rep('skip', 4), rep('numeric', 2), rep('skip', 8), rep('numeric', 4),
                                         'skip', rep('text', 2), rep('skip', 6)),
                           col_names = c('plotNumber', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight',
                                         'plotLength', 'combineNotes', 'notesLC')) %>%
  filter(is.na(combineNotes) & is.na(notesLC)) %>%
  rowwise() %>%
  mutate(location = 'North Platte2',
         flagLeafHeight = flagLeafHeight*100,
         earHeight = earHeight*100,
         plotLength = case_when(is.na(plotLength) ~ 17.5, .default = plotLength),
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, plotLength)) %>%
  select(!contains('otes'))

np3FieldData <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx',
                           sheet = 'No Irr Data',
                           skip = 4,
                           col_types = c('skip', 'numeric', rep('skip', 8), 'numeric', rep('skip', 4), rep('numeric', 2), rep('skip', 8), rep('numeric', 3), 
                                         'skip', rep('text', 2), rep('skip', 6)),
                           col_names = c('plotNumber', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'combineYield', 'combineMoisture', 'combineTestWeight', 
                                         'combineNotes', 'notesLC')) %>%
  filter(is.na(combineNotes) & is.na(notesLC)) %>%
  rowwise() %>%
  mutate(location = 'North Platte3',
         flagLeafHeight = flagLeafHeight*100,
         earHeight = earHeight*100,
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17)) %>%
  select(!contains('otes'))

npFieldData <- bind_rows(np1FieldData, np2FieldData, np3FieldData)
hybridsRaw <- filter(hybridsRaw, !is.na(qrCode))

hybridsRaw <- full_join(hybridsRaw, npFieldData, join_by(location, plotNumber), suffix = c('', '.np'), keep = FALSE)
hybridsRaw <- hybridsRaw %>%
  rowwise() %>%
  mutate(flagLeafHeight = max(flagLeafHeight, flagLeafHeight.np, na.rm = TRUE),
         earHeight = max(earHeight, earHeight.np, na.rm = TRUE),
         combineYield = max(combineYield, combineYield.np, na.rm = TRUE),
         combineMoisture = max(combineMoisture, combineMoisture.np, na.rm = TRUE),
         combineTestWeight = max(combineTestWeight, combineTestWeight.np, na.rm = TRUE),
         yieldPerAcre = max(yieldPerAcre, yieldPerAcre.np, na.rm = TRUE)) %>%
  select(!ends_with('.np'))

lnkFieldData <- read_excel('data/Summary of Lincoln Hybrid HIPS 2022 Data.xlsx', 
                           sheet = 'Combined Dataset', 
                           skip = 1, 
                           col_types = c('skip', 'numeric', 'text', 'skip', 'text', rep('skip', 14), rep('numeric', 2), 'skip', rep('numeric', 2), 
                                         rep('skip', 4), rep('numeric', 3), 'skip'),
                           col_names = c('plotNumber', 'location', 'nitrogenTreatment', 'earHeight1', 'flagLeafHeight1', 'earHeight2', 'flagLeafHeight2', 
                                         'combineYield', 'combineMoisture', 'combineTestWeight')) %>%
  rowwise() %>%
  mutate(flagLeafHeight = mean(c(flagLeafHeight1, flagLeafHeight2), na.rm = TRUE),
         earHeight = mean(c(earHeight1, earHeight2), na.rm = TRUE),
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5)) %>%
  select(!c(ends_with('1'), ends_with('2')))

hybridsRaw <- full_join(hybridsRaw, lnkFieldData, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE)

hybridsRaw <- hybridsRaw %>%
  rowwise() %>%
  mutate(nitrogenTreatment = max(nitrogenTreatment, nitrogenTreatment.lnk, na.rm = TRUE),
         combineYield = max(combineYield, combineYield.lnk, na.rm = TRUE),
         combineMoisture = max(combineMoisture, combineMoisture.lnk, na.rm = TRUE),
         combineTestWeight = max(combineTestWeight, combineTestWeight.lnk, na.rm = TRUE),
         flagLeafHeight = max(flagLeafHeight, flagLeafHeight.lnk, na.rm = TRUE),
         earHeight = max(earHeight, earHeight.lnk, na.rm = TRUE), 
         yieldPerAcre = max(yieldPerAcre, yieldPerAcre.lnk, na.rm = TRUE)) %>%
  select(!ends_with('.lnk'))

mvFieldData <- read_excel('data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx', 
                          sheet = 'RawData (4-Row)',
                          skip = 1, 
                          col_types = c(rep('skip', 3), 'text', rep('skip', 8), 'numeric', 'skip', 'skip', rep('numeric', 2), 'skip', 'text', 
                                        rep('numeric', 2), 'skip', 'numeric', 
                                        rep('skip', 15)), 
                          col_names = c('location', 'plotNumber', 'row', 'range', 'plotDiscarded', 'combineYield', 'combineMoisture', 'combineTestWeight')) %>%
  filter(str_detect(location, 'MOValley') & is.na(plotDiscarded)) %>%
  rowwise() %>%
  mutate(location = 'Missouri Valley',
         yieldPerAcre = buPerAc15.5(combineYield, combineMoisture, 17.5))

hybridsRaw <- full_join(hybridsRaw, mvFieldData, join_by(location, plotNumber), suffix = c('', '.mv'), keep = FALSE)

hybridsRaw <- hybridsRaw %>%
  rowwise() %>%
  mutate(combineYield = max(combineYield, combineYield.mv, na.rm = TRUE),
         combineMoisture = max(combineMoisture.mv, na.rm = TRUE),
         combineTestWeight = max(combineTestWeight, combineTestWeight.mv, na.rm = TRUE),
         yieldPerAcre = max(yieldPerAcre, yieldPerAcre.mv, na.rm = TRUE)) %>%
  select(!c(plotDiscarded, ends_with('.mv')))

mvHeightData <- read_excel('data/Plant_data_MO_Valley_2022.xlsx',
                           sheet = '4211', 
                           skip = 1, 
                           col_types = c('skip', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'numeric', 'numeric', rep('skip', 4)),
                           col_names = c('row', 'range', 'flagLeafHeight', 'earHeight')) %>%
  rowwise() %>%
  mutate(location = 'Missouri Valley')

hybridsRaw <- full_join(hybridsRaw, mvHeightData, join_by(location, range, row), suffix = c('', '.mv'), keep = FALSE)

hybridsRaw <- hybridsRaw %>%
  rowwise() %>%
  mutate(flagLeafHeight = max(flagLeafHeight, flagLeafHeight.mv, na.rm = TRUE),
         earHeight = max(earHeight, earHeight.mv, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~case_when(. <= 0 ~ NA, .default = .)))

hybridsRaw <- mutate(hybridsRaw, 
                     nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')),
                     anonymizedLocation = case_when(location=='Scottsbluff' ~ 'Location 1',
                                                    str_detect(location, 'North Platte') ~ 'Location 2',
                                                    location=='Lincoln' ~ 'Location 3',
                                                    location=='Missouri Valley' ~ 'Location 4')) %>%
  filter(!(is.na(qrCode) | str_detect(qrCode, 'BORDER')))

extremeOutlierHistogram <- ggplot(hybridsRaw, aes(earWidth)) +
  geom_histogram(fill = moma.colors('VanGogh', 1), binwidth = 0.1) + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(vars(anonymizedLocation), scales = 'free_y') +
  labs(x = 'Ear Width (cm)', y = 'Number of Observations') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        panel.background = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank())
extremeOutlierHistogram

hybridsRawWide <- hybridsRaw %>%
  group_by(genotype, location, nitrogenTreatment) %>%
  mutate(genotypeRepNum = 1:n()) %>%
  ungroup() %>%
  pivot_longer(c(kernelRowNumber, earWidth, yieldPerAcre), names_to = 'phenotype', values_to = 'value') %>%
  pivot_wider(id_cols = c(genotype, nitrogenTreatment, location, anonymizedLocation), names_from = c(phenotype, genotypeRepNum), values_from = value, names_sep = '.')

extremeOutlierRepCorrelation <- ggplot(hybridsRawWide, aes(earWidth.1, earWidth.2, color = nitrogenTreatment)) +
  geom_point() +
  scale_color_manual(values = nitrogenColors) + 
  labs(x = 'Ear Width (cm), Replicate 1', y = 'EarWidth (cm), Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
extremeOutlierRepCorrelation

earWidthCorrelationOutlierRemoved <- ggplot(hybridsFinalWide, aes(earWidth.1, earWidth.2, color = nitrogenTreatment)) +
  geom_point() +
  scale_color_manual(values = nitrogenColors) + 
  labs(x = 'Ear Width (cm), Replicate 1', y = 'EarWidth (cm), Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
earWidthCorrelationOutlierRemoved

sbViolins <- hybridsRaw %>%
  filter(location=='Scottsbluff') %>%
  pivot_longer(c(yieldPerAcre, percentProtein, earFillLength, flagLeafHeight), names_to = 'phenotype',
               values_to = 'value') %>%
  rowwise() %>%
  mutate(source = case_when(phenotype=='flagLeafHeight' ~ 'Source 1',
                            phenotype=='yieldPerAcre' ~ 'Source 2',
                            .default = 'Source 3'),
         phenotype = case_when(phenotype=='flagLeafHeight' ~ 'Flag Leaf Height (cm)',
                               phenotype=='yieldPerAcre' ~ 'Yield (Bushels/Acre)',
                               phenotype=='percentProtein' ~ 'Protein (%)',
                               phenotype=='earFillLength' ~ 'Ear Fill Length (cm)')) %>%
  ggplot(aes(nitrogenTreatment, value, fill = source)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    facet_wrap(vars(phenotype), scales = 'free_y') +
    scale_fill_manual(values = moma.colors('VanGogh', 3)) + 
    labs(x = 'Nitrogen Treatment', y = '', fill = 'Data Source') +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = 14),
          strip.text = element_text(color = 'black', size = 14),
          line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          legend.position = 'top')
sbViolins

repCorrelationRawSB <- hybridsRawWide %>%
  filter(location %in% c('Scottsbluff', 'North Platte1', 'Lincoln') & kernelRowNumber.1 < 40) %>%
  group_by(location) %>%
  mutate(correlation = cor(kernelRowNumber.1, kernelRowNumber.2, use = 'complete.obs') %>%
           round(digits = 3)) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(kernelRowNumber.1, kernelRowNumber.2, color = nitrogenTreatment)) + 
    geom_point() +
    facet_grid(cols = vars(anonymizedLocation, correlation)) +
    scale_color_manual(values = nitrogenColors) +
    labs(x = 'Kernel Row Number, Replicate 1', y = 'Kernel Row Number, Replicate 2', color = 'Nitrogen Treatment') +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = 14),
          axis.text.x = element_text(color = 'black', size = 14, angle = 45),
          strip.text = element_text(color = 'black', size = 14),
          line = element_line(color = 'black', linewidth = 1),
          legend.position = 'top',
          plot.background = element_blank(),
          panel.background = element_blank())
repCorrelationRawSB



repCorrelationRawMVEar <- hybridsRawWide %>%
  filter(location %in% c('North Platte1', 'Lincoln', 'Missouri Valley')) %>%
  group_by(location) %>%
  mutate(correlation = cor(kernelRowNumber.1, kernelRowNumber.2, use = 'complete.obs') %>%
           round(digits = 3)) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(kernelRowNumber.1, kernelRowNumber.2, color = nitrogenTreatment)) + 
  geom_point() +
  facet_grid(cols = vars(anonymizedLocation, correlation)) +
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Kernel Row Number, Replicate 1', y = 'Kernel Row Number, Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
repCorrelationRawMVEar



repCorrelationRawMVYield <- hybridsRawWide %>%
  filter(location %in% c('North Platte1', 'Lincoln', 'Missouri Valley')) %>%
  group_by(location) %>%
  mutate(correlation = cor(yieldPerAcre.1, yieldPerAcre.2, use = 'complete.obs') %>%
           round(digits = 3)) %>%
  mutate(correlation = paste0('R = ', correlation)) %>%
  ungroup() %>%
  ggplot(aes(yieldPerAcre.1, yieldPerAcre.2, color = nitrogenTreatment)) + 
  geom_point() +
  facet_grid(cols = vars(anonymizedLocation, correlation)) +
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Yield (Bushels/Acre), Replicate 1', y = 'Yield (Bushels/Acre), Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
repCorrelationRawMVYield

sbInbreds <- read_excel('data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx', 
                        sheet = 'Inbred_flowerind data', 
                        skip = 2, 
                        col_types = c('numeric', 'skip', 'date', 'date'),
                        col_names = c('plotNumber', 'anthesisDate', 'silkDate')) %>%
  rowwise() %>%
  mutate(location = 'Scottsbluff',
         daysToAnthesis = difftime(anthesisDate, ymd('2022-05-19'), units = 'days') %>%
           as.integer(),
         daysToSilk = difftime(silkDate, ymd('2022-05-19'), units = 'days') %>%
           as.integer())
sbHeight <- read_excel('data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx',
                       sheet = 'Inbred_height data',
                       skip = 2,
                       col_types = c('numeric', 'skip', 'numeric', 'numeric', 'skip'),
                       col_names = c('plotNumber', 'earHeight', 'flagLeafHeight')) %>%
  rowwise() %>%
  mutate(location = 'Scottsbluff', 
         earHeight = cm(earHeight) %>%
           round(digits = 2),
         flagLeafHeight = cm(flagLeafHeight) %>%
           round(digits = 2))
sbInbreds <- full_join(sbInbreds, sbHeight, join_by(location, plotNumber), suffix = c('', ''), keep = FALSE)

inbreds <- read.csv('outData/HIPS_2022_V4.8_INBREDS.csv')
inbredsWithSB <- full_join(inbreds, sbInbreds, join_by(location, plotNumber), suffix = c('', '.sb'), keep = FALSE)
inbredsWithSB <- inbredsWithSB %>%
  rowwise() %>%
  mutate(daysToAnthesis = max(daysToAnthesis, daysToAnthesis.sb, na.rm = TRUE),
         daysToSilk = max(daysToSilk, daysToSilk.sb, na.rm = TRUE), 
         earHeight = max(earHeight, earHeight.sb, na.rm = TRUE), 
         flagLeafHeight = max(flagLeafHeight, flagLeafHeight.sb, na.rm = TRUE),
         anonymizedLocation = case_when(location=='Scottsbluff' ~ 'Location 1',
                                        location=='Lincoln' ~ 'Location 3',
                                        location=='Missouri Valley' ~ 'Location 4',
                                        location=='Ames' ~ 'Location 5',
                                        location=='Crawfordsville' ~ 'Location 6')) %>%
  select(!ends_with('.sb'))

inbredsWithSBWide <- inbredsWithSB %>%
  group_by(genotype, location, nitrogenTreatment) %>%
  mutate(genotypeRepNum = 1:n()) %>%
  ungroup() %>%
  pivot_longer(c(earHeight, daysToAnthesis, kernelRowNumber), names_to = 'phenotype', values_to = 'value') %>%
  pivot_wider(id_cols = c(genotype, nitrogenTreatment, location, anonymizedLocation), names_from = c(phenotype, genotypeRepNum), values_from = value, names_sep = '.')

sbHeightCorrelations <- inbredsWithSBWide %>%
  filter(location %in% c('Scottsbluff', 'Lincoln', 'Ames')) %>%
  ggplot(aes(earHeight.1, earHeight.2, color = nitrogenTreatment)) + 
  geom_point() +
  facet_grid(cols = vars(anonymizedLocation)) +
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Ear Height (cm), Replicate 1', y = 'Ear Height (cm), Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
sbHeightCorrelations

sbKRNCorrelations <- inbredsWithSBWide %>%
  filter(location %in% c('Scottsbluff', 'Lincoln', 'Ames')) %>%
  ggplot(aes(kernelRowNumber.1, kernelRowNumber.2, color = nitrogenTreatment)) + 
  geom_point() +
  facet_grid(cols = vars(anonymizedLocation)) +
  scale_color_manual(values = nitrogenColors) +
  labs(x = 'Kernel Row Number, Replicate 1', y = 'Kernel Row Number, Replicate 2', color = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14, angle = 45),
        strip.text = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
sbKRNCorrelations


np2Violins <- hybridsFinal %>%
  filter(location=='North Platte2') %>%
  group_by(nitrogenTreatment, genotype, plotNumber) %>%
  pivot_longer(c(percentProtein, yieldPerAcre), names_to = 'phenotype', values_to = 'value') %>%
  rowwise() %>%
  mutate(label = case_when(phenotype=='percentProtein' ~ 'Protein (%)',
                           phenotype=='yieldPerAcre' ~ 'Yield (Bushels/Acre)')) %>%
  ggplot(aes(nitrogenTreatment, value, fill = nitrogenTreatment)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'black') +
    facet_wrap(vars(label), scales = 'free_y') +
    scale_fill_manual(values = nitrogenColors) +
    labs(x = 'Nitrogen Treatment', y = '', fill = 'Nitrogen Treatment') +
    theme(text = element_text(color = 'black', size = 14), 
          axis.text = element_text(color = 'black', size = 14),
          axis.text.x = element_text(color = 'black', size = 14),
          strip.text = element_text(color = 'black', size = 14), 
          line = element_line(color = 'black', linewidth = 1),
          legend.position = 'top',
          plot.background = element_blank(),
          panel.background = element_blank())
np2Violins

sbViolinsFinal <- hybridsFinal %>%
  filter(location=='Scottsbluff') %>%
  group_by(nitrogenTreatment, genotype, plotNumber) %>%
  pivot_longer(c(percentProtein, yieldPerAcre), names_to = 'phenotype', values_to = 'value') %>%
  rowwise() %>%
  mutate(label = case_when(phenotype=='percentProtein' ~ 'Protein (%)',
                           phenotype=='yieldPerAcre' ~ 'Yield (Bushels/Acre)')) %>%
  ggplot(aes(nitrogenTreatment, value, fill = nitrogenTreatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'black') +
  facet_wrap(vars(label), scales = 'free_y') +
  scale_fill_manual(values = nitrogenColors) +
  labs(x = 'Nitrogen Treatment', y = '', fill = 'Nitrogen Treatment') +
  theme(text = element_text(color = 'black', size = 14), 
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14),
        strip.text = element_text(color = 'black', size = 14), 
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
sbViolinsFinal

bigPhyllotaxyData <- read_excel('../../../Downloads/angles_three_days_6cm.xlsx', skip = 1)

bigPhyllotaxyData <- bigPhyllotaxyData %>%
  filter((`Accuracy reconstruction` > 0.7) & as.logical(`Topology skeleton`)) %>%
  rename(phi0 = phi...7,
         phi1 = phi...10, 
         phi2 = phi...13,
         phi3 = phi...16) %>%
  mutate(dataset = 'Dataset 1') %>%
  pivot_longer(c(phi0, phi1, phi2, phi3), names_to = 'angle', values_to = 'value') %>%
  select(angle, value, dataset)

validationPhyllotaxy <- read_excel('../../../Downloads/validation_plants_theta.xlsx', skip = 1)
validationPhyllotaxy1 <- validationPhyllotaxy %>%
  rename(phi0 = Phi...6,
         phi1 = Phi...9,
         phi2 = Phi...12,
         phi3 = Phi...15) %>%
  mutate(dataset = 'Dataset 2') %>%
  pivot_longer(c(phi0, phi1, phi2, phi3), names_to = 'angle', values_to = 'value') %>%
  select(angle, value, dataset)

phyllotaxyData1 <- bind_rows(bigPhyllotaxyData, validationPhyllotaxy1)

phyllotaxyHist <- ggplot(phyllotaxyData1, aes(dataset, value, fill = dataset)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'black') + 
  scale_fill_manual(values = moma.colors('VanGogh', 2)) + 
  labs(x = '', y = expression(phi), fill = '') + 
  theme(text = element_text(color = 'black', size = 14), 
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
phyllotaxyHist

validationPhyllotaxy2 <- validationPhyllotaxy %>%
  rename(phi0 = Theta...7,
         phi1 = Theta...10,
         phi2 = Theta...13,
         phi3 = Theta...16) %>%
  mutate(dataset = 'Dataset 2') %>%
  pivot_longer(c(phi0, phi1, phi2, phi3), names_to = 'angle', values_to = 'value') %>%
  select(angle, value, dataset)

phyllotaxyData2 <- bind_rows(bigPhyllotaxyData, validationPhyllotaxy2)

phyllotaxyHist2 <- ggplot(phyllotaxyData2, aes(dataset, value, fill = dataset)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'black') + 
  scale_fill_manual(values = moma.colors('VanGogh', 2)) + 
  labs(x = '', y = expression(phi), fill = '') + 
  theme(text = element_text(color = 'black', size = 14), 
        axis.text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = 14),
        line = element_line(color = 'black', linewidth = 1),
        legend.position = 'top',
        plot.background = element_blank(),
        panel.background = element_blank())
phyllotaxyHist2