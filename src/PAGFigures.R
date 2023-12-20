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
