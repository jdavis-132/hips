library(tidyverse)
library(MoMAColors)

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
  pivot_longer(c(kernelRowNumber, yieldPerAcre), names_to = 'phenotype', values_to = 'value') %>%
  pivot_wider(id_cols = c(genotype, nitrogenTreatment, location, anonymizedLocation), names_from = c(phenotype, genotypeRepNum), values_from = value, names_sep = '.')
 
repCorrelationFixedSB <- hybridsFinalWide %>%
  filter(location %in% c('Scottsbluff', 'Lincoln', 'Ames')) %>%
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
  filter(location %in% c('North Platte1', 'Missouri Valley', 'Crawfordsville')) %>%
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
  filter(location %in% c('North Platte1', 'Missouri Valley', 'Crawfordsville')) %>%
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


# Re-wrangle hybrids to get the raw data together in one data frame
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

unlEars <- bind_rows(sbEars, npEars, lnkEars, mvEars) %>%
  filter(!(genotype %in% c('', 'BORDER', 'SOLAR 4', 'SOLAR 3', 'SOLAR 2', 'SOLAR 1', 'FILL'))) %>%
  fixGenos(hips1.5_genoFixKey)

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
