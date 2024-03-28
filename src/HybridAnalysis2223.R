# setwd('davisjensina@gmail.com - Google Drive/My Drive/Schnable-Lab/HIPS/hips') # For running on Mac
# setwd('HIPS/hips') # For running on lab computer

library(tidyverse)
library(readxl)
library(lubridate)
library(daymetr)
library(cowplot)
library(MoMAColors)
library(lme4)
library(car)
library(jpeg)
library(scales)
source('src/Functions.R')

# hybrids <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.2.csv') %>%
#   filter(location!='') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'High', 'Medium'))) %>%
#   rowwise() %>%
#   # Since we are making an environment variable based on year, location, irrigationProvided, and nitrogenTreatment,
#   # let's have an option to keep 2022 NP as one location
#   mutate(semanticLocation = case_when(location %in% c('North Platte1', 'North Platte2', 'North Platte3') ~ 'North Platte', .default = location),
#          environment = str_c(year, semanticLocation, irrigationProvided, nitrogenTreatment, sep = ':'))

# Now we need to spatially correct within an environment
phenotypes <- c("plantDensity", "combineTestWeight", "combineMoisture", "flagLeafHeight", "earHeight", "yieldPerAcre", 
                'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'kernelRowNumber', 'earWidth',
                'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelMassPerEar', 'kernelsPerEar', 'hundredKernelMass',
                'earFillLength', 
                'kernelsPerRow')
phenotypeLabels <- c('Plant Density (plants/acre)', 'Test Weight (lbs/bushel)', 'Harvest Moisture (%)', 'Flag Leaf Height (cm)',
                     'Ear Height (cm)', 'Yield (bushels/acre)', 'Days to Anthesis', 'Days To Silk', 
                     'Anthesis Silking Interval (days)', 'Kernel Row Number', 'Ear Width (cm)', 'Ear Length (cm)',
                     'Shelled Cob Width (cm)', 'Shelled Cob Mass (cm)', 'Kernel Mass Per Ear (g)', 'Kernels Per Ear', 
                     'Hundred Kernel Mass (g)', 'Ear Fill Length (cm)', 'Kernels Per Row')
phenotypeLabelsVP <-  c('Plant Density (1)', 'Test Weight (2)', 'Harvest Moisture (2)', 'Flag Leaf Height (1)',
                        'Ear Height (1)', 'Yield (2)', 'Days to Anthesis (1)', 'Days to Silk (1)', 
                        'Anthesis Silking Interval (1)', 'Kernel Row Number (3)', 'Ear Width (3)', 'Ear Length (3)',
                        'Shelled Cob Width (3)', 'Shelled Cob Mass (3)', 'Kernel Mass Per Ear (3)', 
                        'Kernels Per Ear (3)', 
                        'Hundred Kernel Mass (3)', 'Ear Fill Length (3)', 'Kernels Per Row (3)')


yieldComponents <- c('earFillLength', 'earWidth', 'shelledCobWidth', 'earLength', 'kernelsPerEar', 'yieldPerAcre', 
                     'kernelsPerRow', 'kernelRowNumber', 'kernelMassPerEar', 'hundredKernelMass')
yieldComponentsLabels <- c('Ear Fill Length (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)', 'Ear Length (cm)',
                           'Kernels Per Ear', 'Yield (Bushels / Acre)', 'Kernels Per Row', 'Kernel Row Number', 
                           'Kernel Mass Per Ear (g)', 'Hundred Kernel Mass (g)')

# # # # testSPATS <- getSpatialCorrectionsEnvironment(hybrids, 'yieldPerAcre', 'environment')
# # # # testJoin <- full_join(hybrids, testSPATS, join_by(environment, plotNumber), suffix = c('', '.sp'), keep = FALSE)
# for(i in phenotypes)
# {
#   hybrids <- full_join(hybrids,
#                        getSpatialCorrectionsEnvironment(hybrids, i, 'environment'),
#                        by = join_by(environment, plotNumber),
#                        suffix = c('', '.sp'),
#                        keep = FALSE)
# }
# 
# hybrids <- hybrids %>%
#   rowwise() %>%
#   mutate(across(everything(), ~case_when(is.null(.) ~ NA, .default = .)))
# 
# # # Is there shrinkage toward the mean of a treatment ?
# for(i in 1:length(phenotypes))
# {
#   sp.correction.plot <- ggplot(hybrids, (aes(.data[[phenotypes[i]]], .data[[paste0(phenotypes[i], '.sp')]], color = environment))) +
#     geom_point() +
#     geom_abline(slope = 1) +
#     facet_wrap(vars(location, year)) +
#     theme(legend.position = 'none')
#   print(sp.correction.plot)
# }
# 
# # Don't use spatially corrected values if there is shrinkage towards the mean of a treatment
# hybrids <- hybrids %>%
#   rowwise() %>%
#   mutate(anthesisSilkingInterval.sp = case_when(location %in% c('North Platte1', 'North Platte2') & year=='2022' ~ anthesisSilkingInterval,
#                                                 .default = anthesisSilkingInterval.sp),
#          combineMoisture.sp = case_when(location=='Ames' & year=='2022' ~ combineMoisture, .default = combineMoisture.sp))
# 
# # Export spatial corrections so we don't have to run it again
# write.csv(hybrids, 'analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv')
hybrids <- read.csv('analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv') %>%
  filter(location!='') %>% 
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))

# # Location or irrigationProvided, which is mostly location, is most important for 15/19 traits when we don't count residual
# vc_all <- tibble(grp = NULL, responseVar = NULL, vcov = NULL, pctVar = NULL)
# for(i in 1:length(phenotypes))
# {
#   var <- paste0(phenotypes[i], '.sp')
#   vc_all <- bind_rows(vc_all, partitionVariance3(hybrids, var, phenotypes[i], '~ (1|year) + (1|semanticLocation/nitrogenTreatment) + (1|genotype) + (1|irrigationProvided) + (1|semanticLocation:genotype) + (1|nitrogenTreatment:genotype)'))
# }

# So let's fit a simpler model
vc_all <- tibble(grp = NULL, responseVar = NULL, vcov = NULL, pctVar = NULL)
for(i in 1:length(phenotypes))
{
  var <- paste0(phenotypes[i], '.sp')
  vc_all <- bind_rows(vc_all, partitionVariance3(hybrids, var, phenotypeLabelsVP[i], '~ (1|environment) + (1|genotype) + (1|genotype:environment)'))
}


# vc_all <- vc_all %>%
#   rowwise() %>%
#   mutate(grp = case_when(grp=='genotype' ~ 'Genotype',
#                          grp=='environment' ~ 'Environment',
#                          grp=='genotype:environment' ~ 'Genotype x Environment',
#                          .default = grp) %>%
#            factor(levels = c('Genotype', 'Environment', 'Genotype x Environment', 'Residual')),
#          label = factor(label, levels = c('Yield (Bushels / Acre)', 'Kernel Mass Per Ear (g)', 'Hundred Kernel Mass (g)',
#                                           'Kernels Per Ear', 'Kernel Row Number', 'Kernels Per Row', 'Ear Length (cm)', 
#                                           'Ear Fill Length (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)')))

vc_all <- vc_all %>%
  rowwise() %>%
  mutate(grp = case_when(grp=='genotype' ~ 'Genotype',
                         grp=='environment' ~ 'Environment',
                         grp=='genotype:environment' ~ 'Genotype x Environment',
                         .default = grp) %>%
           factor(levels = c('Environment', 'Genotype', 'Genotype x Environment', 'Residual')))
envImportanceOrder <- vc_all %>%
  filter(grp=='Environment') %>%
  arrange(desc(pctVar))
envImportanceOrder <- envImportanceOrder$label

vc_all <- mutate(vc_all, label = factor(label, levels = envImportanceOrder))

vp.plot <- ggplot(vc_all, aes(label, pctVar, fill = grp)) +
  geom_col(position = 'stack') + 
  scale_fill_viridis(discrete = TRUE, labels = label_wrap(11)) +
  scale_x_discrete(labels = label_wrap(8)) +
  labs(x = 'Phenotype', y = 'Percent Variance', fill = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, color = 'black'),
        axis.text.y = element_text(size = 10, color = 'black'),
        legend.text = element_text(size = 10, color = 'black'),
        text = element_text(size = 10, color = 'black'),
        legend.position = 'bottom',
        line = element_line(color = 'black', linewidth = 1),
        panel.grid = element_blank())
vp.plot
ggsave('analysis/variancePartitioning_20240224.png', width = 7.1, height = (7.1/12.57)*8.92, dpi=1000)
# Estimate FW plasticity across all environments where phenotype was observed
hybrids.pl <- estimatePlasticity2(hybrids, trait = paste0(yieldComponents[1], '.sp'), environment = 'environment', genotype = 'genotype')

for(i in 2:length(yieldComponents))
{
  hybrids.pl <- full_join(hybrids.pl, 
                          estimatePlasticity2(hybrids, trait = paste0(yieldComponents[i], '.sp'), environment = 'environment', genotype = 'genotype'),
                          join_by(genotype),
                          suffix = c('', ''), 
                          keep = FALSE)
  # Print mean of the b column so we know we are getting reasonable values
  print(mean(hybrids.pl[[paste0(yieldComponents[i], '.sp.b')]], na.rm = TRUE))
}

hybrids.pl <- hybrids.pl %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))

hybrids.pl <- hybrids.pl %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         earParent = str_split_i(genotype, ' X ', 1),
         pollenParent = str_split_i(genotype, ' X ', 2))

parentInfo <- read_excel('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.xlsx',
                         range = 'A2:D358', 
                         col_names = c('genotype', 'accession', 'additionalAccessionNames', 'releaseYear'),
                         col_types = rep('text', 4)) %>%
  filter(genotype!='NA') %>%
  mutate(releaseYear = case_when(releaseYear=='NEAR 1946' ~ 1946,
                                 releaseYear=='PRE 1956' ~ 1956,
                                 releaseYear=='PRE 1976' ~ 1976,
                                 releaseYear=='PRE 1941' ~ 1941,
                                 releaseYear=='PRE 1964' ~ 1964, 
                                 .default = as.numeric(releaseYear))) %>%
  select(genotype, releaseYear)

missingParents <- c('I159', '66', 'A344', 'B7', 'CI 3A', 'K201', 'KYS', 'C.I. 540', 'L 289', 'W606S', 'N209', 'A12', 'WF9')
missingParentReleaseYears <- c(1998, 2002, 1996, 1968, 1945, 1963, 1963, 1948, 1934, 1963, 1997,  1965, 1943)
missingParents <- tibble(genotype = missingParents, releaseYear = missingParentReleaseYears)
parentInfo <- parentInfo %>%
  filter(!(genotype %in% missingParents$genotype)) %>%
  bind_rows(missingParents)

hybrids.pl <- left_join(hybrids.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(earParentAge = releaseYear) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = releaseYear) %>%
  rowwise() %>%
  mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
         youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
         meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))

for(i in c(8, 10))
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitB <- paste0(yieldComponents[i], '.sp.b')
  meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
  plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')

  oldestParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Oldest Parent Release Year', width = 1)) +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))

  youngestParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Youngest Parent Release Year', width = 1)) +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  meanParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = meanParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Mean Parent Release Year', width = 1)) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  print(oldestParentPlot)
  print(youngestParentPlot)
  print(meanParentPlot)
  # ggsave(paste0('analysis/', yieldComponents[i], 'LinearPlasticityVsMean.png'), dpi = 1000)
}

orderedEnvironments <- hybrids %>%
  group_by(environment) %>%
  summarise(yieldMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  arrange(yieldMean)

orderedViolins <- hybrids %>%
  mutate(environment = factor(environment, levels = orderedEnvironments$environment)) %>%
  ggplot(aes(environment, yieldPerAcre.sp)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
    labs(x = 'Environment', y = 'Yield (Bushels/Acre)') + 
    theme(text = element_text(color = 'black', size = 14),
        axis.text = element_text(color = 'black', size = rel(1), angle = 90),
        axis.line = element_line(color = 'black', size = 1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'right',
        legend.background = element_rect(color = 'black'))
orderedViolins

# Okay, let's run the plasticity without LNK 2022
# Estimate FW plasticity across all environments where phenotype was observed
hybridsNOLNK22 <- filter(hybrids, !(location=='Lincoln' & year=='2022'))
hybridsNOLNK22.pl <- estimatePlasticity2(hybridsNOLNK22, trait = paste0(yieldComponents[1], '.sp'), environment = 'environment', genotype = 'genotype')

for(i in 2:length(yieldComponents))
{
  hybridsNOLNK22.pl <- full_join(hybridsNOLNK22.pl, 
                          estimatePlasticity2(hybridsNOLNK22, trait = paste0(yieldComponents[i], '.sp'), environment = 'environment', genotype = 'genotype'),
                          join_by(genotype),
                          suffix = c('', ''), 
                          keep = FALSE)
  # Print mean of the b column so we know we are getting reasonable values
  print(mean(hybridsNOLNK22.pl[[paste0(yieldComponents[i], '.sp.b')]], na.rm = TRUE))
}

hybridsNOLNK22.pl <- hybridsNOLNK22.pl %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))

hybridsNOLNK22.pl <- hybridsNOLNK22.pl %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         earParent = str_split_i(genotype, ' X ', 1),
         pollenParent = str_split_i(genotype, ' X ', 2))

# parentInfo <- read_excel('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.xlsx',
#                          range = 'A2:D357', 
#                          col_names = c('genotype', 'accession', 'additionalAccessionNames', 'releaseYear'),
#                          col_types = rep('text', 4)) %>%
#   filter(genotype!='NA') %>%
#   mutate(releaseYear = case_when(releaseYear=='NEAR 1946' ~ 1946,
#                                  releaseYear=='PRE 1956' ~ 1956,
#                                  releaseYear=='PRE 1976' ~ 1976,
#                                  releaseYear=='PRE 1941' ~ 1941,
#                                  releaseYear=='PRE 1964' ~ 1964, 
#                                  .default = as.numeric(releaseYear))) %>%
#   select(genotype, releaseYear)

hybridsNOLNK22.pl <- left_join(hybridsNOLNK22.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(earParentAge = releaseYear) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = releaseYear) %>%
  rowwise() %>%
  mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
         youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
         meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))

for(i in 1:length(yieldComponents))
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitB <- paste0(yieldComponents[i], '.sp.b')
  meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
  plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')
  
  oldestParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Oldest Parent Release Year', width = 1)) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  youngestParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Youngest Parent Release Year', width = 1)) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = meanParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Mean Parent Release Year', width = 1)) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  print(oldestParentPlot)
  print(youngestParentPlot)
  print(meanParentPlot)
} 

# Subset to the locations where we got a significant, classical N response on a population level and estimate plasticity --> does N plasticity correlate across location years?
nResponse <- filter(hybrids, str_detect(environment, '2023:Ames')|str_detect(environment, '2023:Crawfordsville')|location %in% c('North Platte2', 'Scottsbluff'))

nResponse.pl <- getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[1], '.sp'), 'nitrogenTreatment', 'genotype')

for(i in 2:length(yieldComponents))
{
  nResponse.pl <- full_join(nResponse.pl, 
                            getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[i], '.sp'), 'nitrogenTreatment', 'genotype'),
                            join_by(genotype, locationYear),
                            suffix = c('', ''),
                            keep = FALSE)
}

nResponse.pl <- nResponse.pl %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .))) %>% 
  mutate(locationYear = case_when(locationYear=='2022:North Platte' ~ '2022:North Platte:4.3', .default = locationYear))

nResponse.plWide <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = yieldPerAcre.sp.b)

corData <- cor(nResponse.plWide[, 2:5], use = 'complete.obs') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorYield <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c() + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1)) + 
  theme(text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_line(color = 'black', size = 1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'right',
        legend.background = element_blank())
nPlasticityCorYield

for(i in 1:length(yieldComponents))
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitBestEnv <- paste0(yieldComponents[i], '.sp.FWB')
  traitWorstEnv <- paste0(yieldComponents[i], '.sp.FWW')
  numHybrids <- length(hybrids.pl$genotype)
  
  sortedHybrids <- hybrids.pl %>%
    arrange(.data[[traitMu]]) %>%
    mutate(rankOrder = 1:numHybrids)
  
  cxMatrix <- matrix(0, numHybrids, numHybrids)
  
  for(x in 1:numHybrids)
  {
    xBest <- sortedHybrids[[traitBestEnv]][sortedHybrids$rankOrder==x]
    xWorst <- sortedHybrids[[traitWorstEnv]][sortedHybrids$rankOrder==x]
    
    if(is.na(xBest)|is.na(xWorst))
    {
      next
    }
    
    for(y in 1:numHybrids)
    {
      yBest <- sortedHybrids[[traitBestEnv]][sortedHybrids$rankOrder==y]
      yWorst <- sortedHybrids[[traitWorstEnv]][sortedHybrids$rankOrder==y]
      
      if(is.na(yBest)|is.na(yWorst))
      {
        next
      }
      
      if((xBest - yBest)*(xWorst - yWorst) < 0)
      {
        cxMatrix[x, y] <- 1
      }
    }
  }
  
  cxData <- as.data.frame(cxMatrix)
  cols <- colnames(cxData)
  cxData <- mutate(cxData, genotypeRank1 = 1:numHybrids) %>%
    pivot_longer(starts_with('V'), names_to = 'genotypeRank2', values_to = 'crossover', names_prefix = 'V')
  
  cxHeatmap <- ggplot(cxData, aes(genotypeRank1, as.numeric(genotypeRank2), fill = factor(crossover))) + 
    geom_tile(color = 'white') +
    scale_fill_manual(values = c('white', moma.colors('VanGogh', 1))) +
    labs(x = 'Genotype Mean Rank', y = 'Genotype Mean Rank', fill = str_wrap('Finlay-Wilkinson Predicted Crossover Interaction', 1), title = yieldComponentsLabels[i]) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text.x = element_text(color = 'black', size = rel(1)),
          axis.text.y = element_text(color = 'black', size = rel(1)),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'none',
          legend.background = element_blank())

  print(cxHeatmap)
}

# Time to overlay violins on the map
nColors <- moma.colors('vonHeyl')
nColors <- c(nColors[3], nColors[2], nColors[1])
hybridsByPlasticity <- hybrids.pl %>%
  arrange(yieldPerAcre.sp.b)


yieldViolinData <- hybrids %>%
  mutate(environment = factor(environment, levels = c("2022:Scottsbluff:16.9:Low", "2022:Scottsbluff:16.9:Medium", "2022:Scottsbluff:16.9:High", "2022:North Platte:0:Low", "2022:North Platte:0:Medium", 
                                                      "2022:North Platte:0:High", "2022:North Platte:4.3:Low", "2022:North Platte:4.3:Medium", "2022:North Platte:4.3:High","2022:North Platte:8.6:Low", 
                                                      "2022:North Platte:8.6:Medium", "2022:North Platte:8.6:High", "2023:North Platte:0:Medium", "2023:North Platte:4.5:Medium", 
                                                      "2022:Lincoln:0:Low", "2022:Lincoln:0:Medium", "2022:Lincoln:0:High", "2023:Lincoln:0:Low", "2023:Lincoln:0:Medium", "2023:Lincoln:0:High", 
                                                      "2022:Missouri Valley:0:Medium", "2023:Missouri Valley:0:Medium", "2022:Ames:0:Low", "2022:Ames:0:Medium", "2022:Ames:0:High", "2023:Ames:0:Low",
                                                      "2023:Ames:0:Medium", "2023:Ames:0:High", "2022:Crawfordsville:0:Low", "2022:Crawfordsville:0:Medium", "2022:Crawfordsville:0:High", 
                                                      "2023:Crawfordsville:0:Low", "2023:Crawfordsville:0:Medium", "2023:Crawfordsville:0:High"))) 
yieldViolins <- ggplot(yieldViolinData, aes(environment, yieldPerAcre.sp, fill = nitrogenTreatment, color = irrigationProvided, group = environment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1.1) + 
  geom_line(data = yieldViolinData, aes(group = genotype), alpha = 0.1, color = 'lightgrey') +  
  scale_fill_manual(values = nColors) + 
  labs(x = 'Environment', y = 'Yield (Bushels / Acre)', fill = 'Nitrogen Treatment', color = 'Irrigation Provided (in)') +
  theme(text = element_text(color = 'black', size = 14),
        axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, color = 'black'),
      axis.text.y = element_text(color = 'black', size = rel(1)),
      axis.text = element_text(color = 'black', size = rel(1)),
      axis.line = element_line(color = 'black', size = 1),
      line = element_line(size = 1),
      panel.background = element_rect(fill = 'transparent', color = NA),
      panel.border = element_blank(),
      panel.grid = element_blank(), 
      plot.background = element_rect(fill = 'transparent', color = NA), 
      legend.position = 'bottom',
      legend.background = element_blank())
yieldViolins

# How many plots and non-missing values do we have?
nonMissingVals <- 0
totalPlots <- length(hybrids$qrCode)
vars <- c('anthesisDate', 'silkDate', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk', 
          'anthesisSilkingIntervalSilkingGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'combineYield', 'yieldPerAcre', 
          'combineMoisture', 'combineTestWeight', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow', 
          'kernelRowNumber', 'kernelsPerEar', 'shelledCobMass', 'percentMoisture', 'percentStarch', 'percentProtein', 'percentOil', 
          'percentFiber', 'percentAsh', 'kernelColor', 'percentLodging', 'totalStandCount')
hybrids <- hybrids %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
for(var in vars)
{
  numMissingVals <- as.numeric(sum(is.na(hybrids[[var]])))
  numNotMissing <- totalPlots - numMissingVals
  nonMissingVals <- nonMissingVals + numNotMissing
}

# Are some of the patterns I'm seeing only due to the addition of hybrids in 2023? Let's subset to hybrids present in both years
hybrids2022 <- filter(hybrids, year=='2022')
hybrids2022 <- unique(hybrids2022$genotype)
hybrids2023 <- filter(hybrids, year=='2023')
hybrids2023 <- unique(hybrids2023$genotype)
hybridsCommon <- intersect(hybrids2022, hybrids2023)
hybridsCommon <- filter(hybrids, genotype %in% hybridsCommon)

# Estimate plasticity and plot vs mean performance
# Estimate FW plasticity across all environments where phenotype was observed
hybridsCommon.pl <- estimatePlasticity2(hybridsCommon, trait = paste0(yieldComponents[1], '.sp'), environment = 'environment', genotype = 'genotype')

for(i in 2:length(yieldComponents))
{
  hybridsCommon.pl <- full_join(hybridsCommon.pl, 
                          estimatePlasticity2(hybridsCommon, trait = paste0(yieldComponents[i], '.sp'), environment = 'environment', genotype = 'genotype'),
                          join_by(genotype),
                          suffix = c('', ''), 
                          keep = FALSE)
  # Print mean of the b column so we know we are getting reasonable values
  print(mean(hybridsCommon.pl[[paste0(yieldComponents[i], '.sp.b')]], na.rm = TRUE))
}

hybridsCommon.pl <- hybridsCommon.pl %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))

hybridsCommon.pl <- hybridsCommon.pl %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         earParent = str_split_i(genotype, ' X ', 1),
         pollenParent = str_split_i(genotype, ' X ', 2))
# parentInfo <- read.csv('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.csv')
# parentInfo <- read_excel('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.xlsx',
#                          range = 'A2:D357', 
#                          col_names = c('genotype', 'accession', 'additionalAccessionNames', 'releaseYear'),
#                          col_types = rep('text', 4)) %>%
#   filter(genotype!='NA') %>%
#   mutate(releaseYear = case_when(releaseYear=='NEAR 1946' ~ 1946,
#                                  releaseYear=='PRE 1956' ~ 1956,
#                                  releaseYear=='PRE 1976' ~ 1976,
#                                  releaseYear=='PRE 1941' ~ 1941,
#                                  releaseYear=='PRE 1964' ~ 1964, 
#                                  .default = as.numeric(releaseYear))) %>%
#   select(genotype, releaseYear)

hybridsCommon.pl <- left_join(hybridsCommon.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(earParentAge = releaseYear) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = releaseYear) %>%
  rowwise() %>%
  mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
         youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
         meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))

for(i in 1:length(yieldComponents))
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitB <- paste0(yieldComponents[i], '.sp.b')
  meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
  plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')

  oldestParentPlot <- ggplot(hybridsCommon.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Oldest Parent Release Year', width = 1)) +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))

  youngestParentPlot <- ggplot(hybridsCommon.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Youngest Parent Release Year', width = 1)) +
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  meanParentPlot <- ggplot(hybridsCommon.pl, aes(.data[[traitMu]], .data[[traitB]], color = meanParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c() +
    labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Mean Parent Release Year', width = 1)) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  
  print(oldestParentPlot)
  print(youngestParentPlot)
  print(meanParentPlot)
  # ggsave(paste0('analysis/', yieldComponents[i], 'LinearPlasticityVsMean.png'), dpi = 1000)
}

# Subset to the locations where we got a significant, classical N response on a population level and estimate plasticity --> does N plasticity correlate across location years?
nResponse <- filter(hybrids, str_detect(environment, '2023:Ames')|str_detect(environment, '2023:Crawfordsville')|location %in% c('North Platte2', 'Scottsbluff'))

nResponse.pl <- getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[1], '.sp'), 'nitrogenTreatment', 'genotype')

for(i in 2:length(yieldComponents))
{
  nResponse.pl <- full_join(nResponse.pl, 
                            getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[i], '.sp'), 'nitrogenTreatment', 'genotype'),
                            join_by(genotype, locationYear),
                            suffix = c('', ''),
                            keep = FALSE)
}

nResponse.pl <- nResponse.pl %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .))) %>% 
  mutate(locationYear = case_when(locationYear=='2022:North Platte' ~ '2022:North Platte:4.3', .default = locationYear)) %>% 
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         earParent = str_split_i(genotype, ' X ', 1),
         pollenParent = str_split_i(genotype, ' X ', 2)) %>%
  left_join(parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(earParentAge = releaseYear) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = releaseYear) %>%
  rowwise() %>%
  mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
         youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
         meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))
 # Yield
nResponse.plWide <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = yieldPerAcre.sp.b)

corData <- cor(nResponse.plWide[, 2:5], use = 'complete.obs') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorYield <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Yield (bushels/acre)') + 
  theme(text = element_text(color = 'black', size = 10),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        legend.background = element_blank())
nPlasticityCorYield
# Kernel Row Number
nResponse.plWideKRN <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = kernelRowNumber.sp.b)

corData <- cor(nResponse.plWideKRN[, 2:5], use = 'complete.obs') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorKRN <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Kernel Row Number') + 
  theme(text = element_text(color = 'black', size = 10),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        legend.background = element_blank())
nPlasticityCorKRN

# Hundred Kernel Mass
nResponse.plWideHKM <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = yieldPerAcre.sp.b)

corData <- cor(nResponse.plWideHKM[, 2:5], use = 'complete.obs') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorHKM <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Hundred Kernel Mass (g)') + 
  theme(text = element_text(color = 'black', size = 10),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        legend.background = element_blank())
nPlasticityCorHKM

nPlasticityCorLegend <- get_legend(nPlasticityCorHKM) 

# which variables, and locationYear should we show for scatter with plasticity and parentAge
# R2
locationYears <- unique(nResponse.pl$locationYear)
for(locationYear in locationYears)
{
  df <- filter(nResponse.pl, locationYear==locationYear)
  for (yieldComponent in yieldComponents)
  {
    print(paste0(locationYear, yieldComponent, 'meanParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'meanParentAge'), sep = ':'))
    print(paste0(locationYear, yieldComponent, 'youngestParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'youngestParentAge'), sep = ':'))
    print(paste0(locationYear, yieldComponent, 'oldestParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'oldestParentAge'), sep = ':'))
  }
}

#R
for(locationYear in locationYears)
{
  df <- filter(nResponse.pl, locationYear==locationYear)
  for (yieldComponent in yieldComponents)
  {
    print(paste0(locationYear, yieldComponent, 'meanParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'meanParentAge'), sep = ':'))
    print(paste0(locationYear, yieldComponent, 'youngestParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'youngestParentAge'), sep = ':'))
    print(paste0(locationYear, yieldComponent, 'oldestParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'oldestParentAge'), sep = ':'))
  }
}

sb22NResponse.pl <- filter(nResponse.pl, locationYear=='2022:Scottsbluff')

nPlasticitySBYield <- ggplot(sb22NResponse.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept = 1, color = 'black', linewidth = 1) +
  scale_color_viridis(direction = -1) +
  scale_y_continuous(limits = c(-2.5, 2.5)) + 
  labs(x = 'Mean Yield (bushels/acre)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
  theme(text = element_text(color = 'black', size = 10),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_line(color = 'black', size = 1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        legend.position = 'none',
        legend.background = element_blank())
 nPlasticitySBYield
 
 nPlasticityParentAgeLegend <- get_legend(nPlasticitySBYield)

 nPlasticitySBKRN <- ggplot(sb22NResponse.pl, aes(kernelRowNumber.sp.mu, kernelRowNumber.sp.b, color = meanParentAge)) +
   geom_point() +
   geom_hline(yintercept = 1, color = 'black', linewidth = 1) +
   scale_color_viridis(direction = -1) +
   scale_y_continuous(limits = c(-2.5, 2.5)) + 
   labs(x = 'Mean Kernel Row Number', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
   theme(text = element_text(color = 'black', size = 10),
         axis.text.x = element_text(color = 'black', size = rel(1)),
         axis.text = element_text(color = 'black', size = rel(1)),
         axis.line = element_line(color = 'black', size = 1),
         panel.background = element_blank(),
         panel.border = element_blank(),
         panel.grid = element_blank(),
         plot.background = element_blank(),
         legend.position = 'none',
         legend.background = element_blank())
 nPlasticitySBKRN

 nPlasticitySBHKM <- ggplot(sb22NResponse.pl, aes(hundredKernelMass.sp.mu, hundredKernelMass.sp.b, color = meanParentAge)) +
   geom_point() +
   geom_hline(yintercept = 1, color = 'black', linewidth = 1) +
   scale_color_viridis(direction = -1) +
   scale_y_continuous(limits = c(-2.5, 2.5)) + 
   labs(x = 'Mean Hundred Kernel Mass (g)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
   theme(text = element_text(color = 'black', size = 10),
         axis.text.x = element_text(color = 'black', size = rel(1)),
         axis.text = element_text(color = 'black', size = rel(1)),
         axis.line = element_line(color = 'black', size = 1),
         panel.background = element_blank(),
         panel.border = element_blank(),
         panel.grid = element_blank(),
         plot.background = element_blank(),
         legend.position = 'none',
         legend.background = element_blank())
 nPlasticitySBHKM

 # Which genotypes are 'most' and 'least' nitrogen stable across these envs for yield
 nResponseGenotypeLevel <- nResponse.pl %>%
   group_by(genotype) %>%
   summarise(maxNPlasticityYield = max(yieldPerAcre.sp.b, na.rm = TRUE), 
             minNPlasticityYield = min(yieldPerAcre.sp.b, na.rm = TRUE), 
             meanNPlasticityYield = mean(yieldPerAcre.sp.b, na.rm = TRUE)) %>%
   arrange(meanNPlasticityYield)
 
 lowPlasticityGenotype <- nResponseGenotypeLevel$genotype[2]
 avgPlasticityGenotype <- nResponseGenotypeLevel$genotype[47]
 highPlasticityGenotype <- nResponseGenotypeLevel$genotype[122]
 
nPlasticityLAH <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & (str_detect(environment, '2023:Ames')|str_detect(environment, '2023:Crawfordsville')|location %in% c('North Platte2', 'Scottsbluff')))  %>%
  group_by(genotype, environment, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')),
         genotype = factor(genotype, levels = lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype))

nPlasticityGenotypeLines <- ggplot(nPlasticityLAH, aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype)) + 
  geom_line()
nPlasticityGenotypeLines

# How well does N plasticity correlate between reps within a location
nResponseBlock.pl <- getNitrogenPlasticityByLocationYearBlock(nResponse, paste0(yieldComponents[1], '.sp'), 'nitrogenTreatment', 'genotype')

for(i in 2:length(yieldComponents))
{
  nResponseBlock.pl <- full_join(nResponseBlock.pl, 
                            getNitrogenPlasticityByLocationYearBlock(nResponse, paste0(yieldComponents[i], '.sp'), 'nitrogenTreatment', 'genotype'),
                            join_by(genotype, locationYear, blockSet),
                            suffix = c('', ''),
                            keep = FALSE)
}

for(i in 1:length(yieldComponents))
{
  traitNPlasticity <- paste0(yieldComponents[i], '.sp.b')
  
  dfWide <- nResponseBlock.pl %>%
    pivot_wider(id_cols = c(genotype, locationYear), 
                names_from = blockSet, 
                values_from = all_of(traitNPlasticity), 
                names_prefix = 'blockSet')
  
  nPlasticityBlockCorr <- ggplot(dfWide, aes(blockSet1, blockSet2)) + 
    geom_point() + 
    facet_wrap(vars(locationYear)) + 
    labs(title = yieldComponents[i])
  print(yieldComponents[i])
  print(cor(dfWide$blockSet1, dfWide$blockSet2, use = 'complete.obs'))
  print(nPlasticityBlockCorr)
}
  


for(i in 1:length(yieldComponents))
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitBestEnv <- paste0(yieldComponents[i], '.sp.FWB')
  traitWorstEnv <- paste0(yieldComponents[i], '.sp.FWW')
  numhybridsCommon <- length(hybridsCommon.pl$genotype)
  
  sortedhybridsCommon <- hybridsCommon.pl %>%
    arrange(.data[[traitMu]]) %>%
    mutate(rankOrder = 1:numhybridsCommon)
  
  cxMatrix <- matrix(0, numhybridsCommon, numhybridsCommon)
  
  for(x in 1:numhybridsCommon)
  {
    xBest <- sortedhybridsCommon[[traitBestEnv]][sortedhybridsCommon$rankOrder==x]
    xWorst <- sortedhybridsCommon[[traitWorstEnv]][sortedhybridsCommon$rankOrder==x]
    
    if(is.na(xBest)|is.na(xWorst))
    {
      next
    }
    
    for(y in 1:numhybridsCommon)
    {
      yBest <- sortedhybridsCommon[[traitBestEnv]][sortedhybridsCommon$rankOrder==y]
      yWorst <- sortedhybridsCommon[[traitWorstEnv]][sortedhybridsCommon$rankOrder==y]
      
      if(is.na(yBest)|is.na(yWorst))
      {
        next
      }
      
      if((xBest - yBest)*(xWorst - yWorst) < 0)
      {
        cxMatrix[x, y] <- 1
      }
    }
  }
  
  cxData <- as.data.frame(cxMatrix)
  cols <- colnames(cxData)
  cxData <- mutate(cxData, genotypeRank1 = 1:numhybridsCommon) %>%
    pivot_longer(starts_with('V'), names_to = 'genotypeRank2', values_to = 'crossover', names_prefix = 'V')
  
  cxHeatmap <- ggplot(cxData, aes(genotypeRank1, as.numeric(genotypeRank2), fill = factor(crossover))) + 
    geom_tile(color = 'white') +
    scale_fill_manual(values = c('white', moma.colors('VanGogh', 1))) +
    labs(x = 'Genotype Mean Rank', y = 'Genotype Mean Rank', fill = str_wrap('Finlay-Wilkinson Predicted Crossover Interaction', 1), title = yieldComponentsLabels[i]) + 
    theme(text = element_text(color = 'black', size = 14),
          axis.text.x = element_text(color = 'black', size = rel(1)),
          axis.text.y = element_text(color = 'black', size = rel(1)),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'none',
          legend.background = element_blank())
  
  print(cxHeatmap)
}

# How often are interactions between a pair of hybrids crossover interactions AND represent significant differences in the phenotype?
genotypePairs <- tibble(genotype1 = NULL, genotype2 = NULL)
hybridEnvs <- hybrids %>%
  group_by(environment) %>%
  summarise(environmentCode = cur_group_id())
hybrids <- full_join(hybrids, hybridEnvs, join_by(environment), keep = FALSE, suffix = c('', ''))

hybridsSigCrossovers <- hybrids %>%
  rowwise() %>%
  mutate(genotype = str_replace_all(genotype, '-', ' '))

allGenotypes <- unique(hybridsSigCrossovers$genotype)
totalGenotypes <- length(allGenotypes)

for(i in 1:totalGenotypes)
{
  df <- tibble(genotype1 = allGenotypes[i], genotype2 = allGenotypes[(i + 1):totalGenotypes])
  genotypePairs <- bind_rows(genotypePairs, df)
}

environments <- unique(hybrids$environmentCode)
totalEnvironments <- length(environments)

# Debug with yield
getSignificantCrossovers(hybridsSigCrossovers, 'yieldPerAcre', environments)

write.csv(genotypePairs, 'analysis/significantCrossovers.csv')















rfFeatures <- read.csv('analysis/featureImportances.csv')[,2:6]
colnames(rfFeatures) <- c('plantDensity', 'kernelRowNumber', 'kernelsPerRow', 'hundredKernelMass', 'environment')

rfFeaturesSummmary <- rfFeatures %>%
  summarise(plantDensity.mean = mean(plantDensity, na.rm = TRUE),
            plantDensity.sd = sd(plantDensity, na.rm = TRUE),
            kernelRowNumber.mean = mean(kernelRowNumber, na.rm = TRUE),
            kernelRowNumber.sd = sd(kernelRowNumber, na.rm = TRUE),
            kernelsPerRow.mean = mean(kernelsPerRow, na.rm = TRUE),
            kernelsPerRow.sd = sd(kernelsPerRow, na.rm = TRUE),
            hundredKernelMass.mean = mean(hundredKernelMass, na.rm = TRUE),
            hundredKernelMass.sd = sd(hundredKernelMass, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'val') %>%
  rowwise() %>%
  mutate(phenotype = str_split_i(type, fixed('.'), 1),
         statistic = str_split_i(type, fixed('.'), 2)) %>%
  select(phenotype, statistic, val) %>%
  pivot_wider(id_cols = phenotype, names_from = statistic, values_from = val) %>%
  mutate(phenotypeLabel = c('Plant Density (plants/acre)', 'Kernel Row Number', 'Kernels Per Row', 'Hundred Kernel Mass (g)')) %>%
  mutate(phenotypeLabel = factor(phenotypeLabel,
                                 levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Hundred Kernel Mass (g)', 
                                            'Kernels Per Row'), ordered = TRUE))

rfFeatures <- rfFeatures %>%
  pivot_longer(!c(environment), names_to = 'phenotype', values_to = 'val') %>%
  rowwise() %>%
  mutate(phenotypeLabel = case_when(phenotype=='plantDensity' ~ 'Plant Density (plants/acre)',
                                    phenotype=='kernelRowNumber' ~ 'Kernel Row Number',
                                    phenotype=='kernelsPerRow' ~ 'Kernels Per Row', 
                                    phenotype=='hundredKernelMass' ~ 'Hundred Kernel Mass (g)')) %>%
  mutate(phenotypeLabel = factor(phenotypeLabel,
                                 levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Hundred Kernel Mass (g)', 
                                            'Kernels Per Row'), ordered = TRUE))

featureImportance <- ggplot(rfFeatures, aes(val, phenotypeLabel, fill = phenotypeLabel)) +
  geom_boxplot(color = 'black') +
  scale_y_discrete(labels = str_wrap(rfFeatures$phenotypeLabel, 9)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = 'Mean Feature Importance', y = '') +
  theme_minimal() + 
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10),
        text = element_text(color = 'black', size = 10),
        panel.grid = element_blank(),
        legend.position = 'none')
featureImportance
# Extension: https://crops.extension.iastate.edu/blog/meaghan-anderson/making-yield-estimates-corn-2022-edition
yieldPredictions <- read.csv('analysis/RFpredictions.csv') %>%
  select(environment, plotNumber, predictedYield) %>%
  rename(predictedYieldRF = predictedYield) %>%
  full_join(hybrids, join_by(environment, plotNumber), keep = FALSE, suffix = c('', '')) %>%
  filter(!is.na(predictedYieldRF)) %>%
  select(environment, plotNumber, yieldPerAcre, predictedYieldRF, plantDensity, kernelRowNumber, kernelsPerRow, hundredKernelMass) %>%
  mutate(predictedYieldExtension = (plantDensity*kernelRowNumber*kernelsPerRow)/((56*453.592*100)/hundredKernelMass))

rfRegressionModel <- lm(predictedYieldRF ~ yieldPerAcre, data = yieldPredictions)
rfIntercept <- rfRegressionModel$coefficients[1]
rfSlope <- rfRegressionModel$coefficients[2]
# Adjusted R2 from summary(model)
rfR2 <- 0.6365

extensionRegressionModel <- lm(predictedYieldExtension ~ yieldPerAcre, data = yieldPredictions)
extensionIntercept <- extensionRegressionModel$coefficients[1]
extensionSlope <- extensionRegressionModel$coefficients[2]
# Adjusted R2 from summary(model)
extensionR2 <- 0.4567

yieldPredictionsSubsample <- yieldPredictions %>%
  slice_sample(prop = 0.5) %>%
  pivot_longer(c(predictedYieldExtension, predictedYieldRF), 
               values_to = 'predictedYield', 
               names_to = 'method', 
               names_prefix = 'predictedYield') %>%
  rowwise() %>%
  mutate(method = case_when(method=='RF' ~ 'Random Forest', .default = method))

yieldPredictionsPlot <- ggplot(yieldPredictionsSubsample, aes(yieldPerAcre, predictedYield, color = method)) + 
  geom_point() + 
  geom_abline(slope = extensionSlope, intercept = extensionIntercept, color = viridis_pal()(4)[1], linewidth = 1) +
  geom_abline(slope = rfSlope, intercept = rfIntercept, color = viridis_pal()(4)[2], linewidth = 1) +
  scale_color_manual(values = viridis_pal()(4)[1:2]) + 
  scale_x_continuous(limits = c(0, 300)) +
  labs(x = 'Actual Yield (bushels/acre)', y = 'Predicted Yield (bushels/acre)', color = 'Method') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10),
        legend.text = element_text(color = 'black', size = 10),
        text = element_text(color = 'black', size = 10),
        legend.position = 'top',
        panel.grid = element_blank())
yieldPredictionsPlot
# Single model 
rfFeatures <- read.csv('analysis/featureImportances5CV.csv')[2:5]
colnames(rfFeatures) <- c('plantDensity', 'kernelRowNumber', 'kernelsPerRow', 'hundredKernelMass')

rfFeaturesSummmary <- rfFeatures %>%
  summarise(plantDensity.mean = mean(plantDensity, na.rm = TRUE),
            plantDensity.sd = sd(plantDensity, na.rm = TRUE),
            kernelRowNumber.mean = mean(kernelRowNumber, na.rm = TRUE),
            kernelRowNumber.sd = sd(kernelRowNumber, na.rm = TRUE),
            kernelsPerRow.mean = mean(kernelsPerRow, na.rm = TRUE),
            kernelsPerRow.sd = sd(kernelsPerRow, na.rm = TRUE),
            hundredKernelMass.mean = mean(hundredKernelMass, na.rm = TRUE),
            hundredKernelMass.sd = sd(hundredKernelMass, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'val') %>%
  rowwise() %>%
  mutate(phenotype = str_split_i(type, fixed('.'), 1),
         statistic = str_split_i(type, fixed('.'), 2)) %>%
  select(phenotype, statistic, val) %>%
  pivot_wider(id_cols = phenotype, names_from = statistic, values_from = val) %>%
  mutate(phenotypeLabel = c('Plant Density (plants/acre)', 'Kernel Row Number', 'Kernels Per Row', 'Hundred Kernel Mass (g)')) %>%
  mutate(phenotypeLabel = factor(phenotypeLabel,
                                 levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Hundred Kernel Mass (g)', 
                                            'Kernels Per Row'), ordered = TRUE))

rfFeatures <- rfFeatures %>%
  pivot_longer(everything(), names_to = 'phenotype', values_to = 'val') %>%
  rowwise() %>%
  mutate(phenotypeLabel = case_when(phenotype=='plantDensity' ~ 'Plant Density (plants/acre)',
                                    phenotype=='kernelRowNumber' ~ 'Kernel Row Number',
                                    phenotype=='kernelsPerRow' ~ 'Kernels Per Row',
                                    phenotype=='hundredKernelMass' ~ 'Hundred Kernel Mass (g)')) %>%
  mutate(phenotypeLabel = factor(phenotypeLabel, 
                                 levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Kernels Per Row', 'Hundred Kernel Mass (g)'), 
                                 ordered = TRUE))

featureImportance <- ggplot(rfFeatures, aes(val, phenotypeLabel, fill = phenotypeLabel)) +
  geom_boxplot(color = 'black') +
  scale_y_discrete(labels = str_wrap(levels(rfFeatures$phenotypeLabel), 9)) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = 'Mean Feature Importance', y = '') +
  theme_minimal() + 
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10),
        text = element_text(color = 'black', size = 10),
        panel.grid = element_blank(),
        legend.position = 'none')
featureImportance
# Extension: https://crops.extension.iastate.edu/blog/meaghan-anderson/making-yield-estimates-corn-2022-edition
yieldPredictions <- read.csv('analysis/RFpredictions5CV.csv') %>%
  select(environment, plotNumber, predictedYield) %>%
  rename(predictedYieldRF = predictedYield) %>%
  full_join(hybrids, join_by(environment, plotNumber), keep = FALSE, suffix = c('', '')) %>%
  filter(!is.na(predictedYieldRF)) %>%
  select(environment, plotNumber, yieldPerAcre, predictedYieldRF, plantDensity, kernelRowNumber, kernelsPerRow, hundredKernelMass) %>%
  mutate(predictedYieldExtension = (plantDensity*kernelRowNumber*kernelsPerRow)/((56*453.592*100)/hundredKernelMass))

rfRegressionModel <- lm(predictedYieldRF ~ yieldPerAcre, data = yieldPredictions)
rfIntercept <- rfRegressionModel$coefficients[1]
rfSlope <- rfRegressionModel$coefficients[2]
# Adjusted R2 from summary(model)
rfR2 <-  0.113 

extensionRegressionModel <- lm(predictedYieldExtension ~ yieldPerAcre, data = yieldPredictions)
extensionIntercept <- extensionRegressionModel$coefficients[1]
extensionSlope <- extensionRegressionModel$coefficients[2]
# Adjusted R2 from summary(model)
extensionR2 <- 0.4567

yieldPredictionsSubsample <- yieldPredictions %>%
  slice_sample(prop = 0.5) %>%
  pivot_longer(c(predictedYieldExtension, predictedYieldRF), 
               values_to = 'predictedYield', 
               names_to = 'method', 
               names_prefix = 'predictedYield') %>%
  rowwise() %>%
  mutate(method = case_when(method=='RF' ~ 'Random Forest', .default = method))

yieldPredictionsPlot <- ggplot(yieldPredictionsSubsample, aes(yieldPerAcre, predictedYield, color = method)) + 
  geom_point() + 
  geom_abline(slope = extensionSlope, intercept = extensionIntercept, color = viridis_pal()(4)[1], linewidth = 1) +
  geom_abline(slope = rfSlope, intercept = rfIntercept, color = viridis_pal()(4)[2], linewidth = 1) +
  scale_color_manual(values = viridis_pal()(4)[1:2]) + 
  scale_x_continuous(limits = c(0, 300)) +
  labs(x = 'Actual Yield (bushels/acre)', y = 'Predicted Yield (bushels/acre)', color = 'Method') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10),
        legend.text = element_text(color = 'black', size = 10),
        text = element_text(color = 'black', size = 10),
        legend.position = 'top',
        panel.grid = element_blank())
yieldPredictionsPlot


workflow <- rasterGrob(readPNG('../workflow.png'))

fig1top <- plot_grid(experimentalDesign, workflow, labels = c('A', 'B'), rel_widths = c(1, 0.5))
fig1bottom <- plot_grid(vp.plot, yieldPredictionsPlot, featureImportance, nrow = 1, rel_widths = c(1, 0.4, 0.3), labels = c('C', 'D', 'E'))

fig1 <- plot_grid(fig1top, fig1bottom, ncol = 1)
fig1

# Variance partitioning for yield from yield components
vc_yield <- partitionVariance3(hybrids, 'yieldPerAcre.sp', 'Yield (bushels/acre)', '~ (1|plantDensity) + (1|kernelRowNumber) + (1|kernelsPerRow) + (1|hundredKernelMass)')

fig3topleft <- plot_grid(nPlasticityCorYield, nPlasticityCorKRN, nPlasticityCorHKM, nPlasticityCorLegend, labels = c('A', 'B', 'C', ''), nrow = 1, rel_widths = c(0.3, 0.3, 0.3, 0.1))
fig3topleft

fig3bottomleft <- plot_grid(nPlasticitySBYield, nPlasticitySBKRN, nPlasticitySBHKM, nPlasticityParentAgeLegend, labels = c('D', 'E', 'F', ''), nrow = 1, rel_widths = c(0.3, 0.3, 0.3, 0.1))
fig3bottomleft

fig3left <- plot_grid(fig3topleft, fig3bottomleft, ncol = 1)
fig3left
