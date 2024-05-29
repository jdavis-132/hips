# setwd('davisjensina@gmail.com - Google Drive/My Drive/Schnable-Lab/HIPS/hips') # For running on Mac
# setwd('HIPS/hips') # For running on lab computer

library(tidyverse)
library(readxl)
library(lubridate)
# library(daymetr)
library(cowplot)
library(patchwork)
library(MoMAColors)
library(lme4)
# library(car)
# library(jpeg)
library(scales)
library(grid)
library(png)
library(spFW)
source('src/Functions.R')

# hybrids <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.3.csv') %>%
#   filter(location!='') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'High', 'Medium'))) %>%
#   rowwise() %>%
#   # Since we are making an environment variable based on year, location, irrigationProvided, and nitrogenTreatment,
#   # let's have an option to keep 2022 NP as one location
#   mutate(semanticLocation = case_when(location %in% c('North Platte1', 'North Platte2', 'North Platte3') ~ 'North Platte', .default = location),
#          environment = str_c(year, semanticLocation, irrigationProvided, nitrogenTreatment, sep = ':'))
nitrogenColors <- pal_brewer('seq', palette = 'YlOrRd')(3)
# show_col(nitrogenColors)
irrigationColors <- pal_brewer('seq', palette = 'Greys')(3)
# show_col(irrigationColors)
# Now we need to spatially correct within an environment
phenotypes <- c("plantDensity", "combineTestWeight", "combineMoisture", "flagLeafHeight", "earHeight", "yieldPerAcre", 
                'GDDToAnthesis', 'GDDToSilk', 'anthesisSilkingIntervalGDD', 'kernelRowNumber', 'earWidth',
                'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelMassPerEar', 'kernelsPerEar', 'hundredKernelMass',
                'earFillLength', 'kernelsPerRow')
phenotypeLabels <- c('Plant Density (plants/acre)', 'Test Weight (lbs/bushel)', 'Harvest Moisture (%)', 'Flag Leaf Height (cm)',
                     'Ear Height (cm)', 'Yield (bushels/acre)', 'GDD to Anthesis*', 'GDD To Silk*', 
                     'Anthesis Silking Interval* (GDD)', 'Kernel Row Number', 'Ear Width (cm)', 'Ear Length (cm)',
                     'Shelled Cob Width (cm)', 'Shelled Cob Mass (cm)', 'Kernel Mass Per Ear (g)', 'Kernels Per Ear', 
                     'Hundred Kernel Mass (g)', 'Ear Fill Length* (cm)', 'Kernels Per Row*')
phenotypeLabelsVP <-  c('Plant Density (1)', 'Test Weight (2)', 'Harvest Moisture (2)', 'Flag Leaf Height (1)',
                        'Ear Height (1)', 'Yield (2)', 'GDD to Anthesis* (1)', 'GDD to Silk* (1)', 
                        'Anthesis Silking Interval* (1)', 'Kernel Row Number (3)', 'Ear Width (3)', 'Ear Length (3)',
                        'Shelled Cob Width (3)', 'Shelled Cob Mass (3)', 'Kernel Mass Per Ear (3)', 
                        'Kernels Per Ear (3)', 
                        'Hundred Kernel Mass (3)', 'Ear Fill Length* (3)', 'Kernels Per Row* (3)')


yieldComponents <- c('earFillLength', 'earWidth', 'shelledCobWidth', 'earLength', 'kernelsPerEar', 'yieldPerAcre', 
                     'kernelsPerRow', 'kernelRowNumber', 'kernelMassPerEar', 'hundredKernelMass')
yieldComponentsLabels <- c('Ear Fill Length* (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)', 'Ear Length (cm)',
                           'Kernels Per Ear', 'Yield (bushels/acre)', 'Kernels Per Row*', 'Kernel Row Number', 
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
# 
#   shrinkBoxesData <- hybrids %>%
#     pivot_longer(any_of(c(phenotypes[i], paste0(phenotypes[i], '.sp'))), names_to = 'col', values_to = 'val') %>%
#     rowwise() %>%
#     mutate(phenotype = str_remove(col, '.sp'),
#            form = case_when(str_detect(col, '.sp') ~ 'spatial', .default = 'raw')) %>%
#     pivot_wider(id_cols = c(environment, genotype, plotNumber, form, location, year, nitrogenTreatment),
#       names_from = phenotype,
#       values_from = val)
# 
#   shrinkBoxes <- ggplot(shrinkBoxesData, aes(nitrogenTreatment, .data[[phenotypes[i]]], fill = form)) +
#     geom_boxplot() +
#     facet_wrap(vars(location, year))
#   print(shrinkBoxes)
# }
# 
# # Don't use spatially corrected values if there is shrinkage towards the mean of a treatment
# hybrids <- hybrids %>%
#   rowwise() %>%
#   mutate(anthesisSilkingIntervalGDD.sp = case_when(location %in% c('North Platte1', 'North Platte2') & year=='2022' ~ anthesisSilkingInterval,
#                                                 .default = anthesisSilkingIntervalGDD.sp),
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
  scale_y_continuous(name = 'Variance', 
                     breaks = c(0, 25, 50, 75, 100), 
                     labels = c('0%', '25%', '50%', '75%', '100%')) +
  labs(x = 'Phenotype', y = 'Variance', fill = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11, color = 'black', angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 11, color = 'black'),
        text = element_text(size = 11, color = 'black'),
        legend.position = 'bottom',
        line = element_line(color = 'black', linewidth = 1),
        panel.grid = element_blank())
vp.plot

# ehvp <- vc_all %>%
#   filter(responseVar=='earHeight.sp') %>%
#   ggplot(aes(label, pctVar, fill = grp)) +
#   geom_col(position = 'stack') + 
#   scale_fill_viridis(discrete = TRUE, labels = label_wrap(11)) +
#   scale_x_discrete(labels = label_wrap(8)) +
#   labs(x = '', y = 'Percent Variance', fill = '') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 11, color = 'black'),
#         axis.text.y = element_text(size = 11, color = 'black'),
#         legend.text = element_text(size = 11, color = 'black'),
#         text = element_text(size = 11, color = 'black'),
#         legend.position = 'right',
#         line = element_line(color = 'black', linewidth = 1),
#         panel.grid = element_blank())
# ehvp
# ggsave('analysis/variancePartitioning_20240224.png', width = 7.1, height = (7.1/12.57)*8.92, dpi=1000)
# Estimate FW plasticity across all environments where phenotype was observed
# hybrids.pl <- estimatePlasticity3(hybrids, trait = paste0(yieldComponents[1], '.sp'), environment = 'environment', genotype = 'genotype')
# 
# for(i in 2:length(yieldComponents))
# {
#   hybrids.pl <- full_join(hybrids.pl, 
#                           estimatePlasticity3(hybrids, trait = paste0(yieldComponents[i], '.sp'), environment = 'environment', genotype = 'genotype'),
#                           join_by(genotype),
#                           suffix = c('', ''), 
#                           keep = FALSE)
#   # Print mean of the b column so we know we are getting reasonable values
#   print(mean(hybrids.pl[[paste0(yieldComponents[i], '.sp.b')]], na.rm = TRUE))
# }
# 
# hybrids.pl <- hybrids.pl %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))
# 
# hybrids.pl <- hybrids.pl %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          earParent = str_split_i(genotype, ' X ', 1),
#          pollenParent = str_split_i(genotype, ' X ', 2))

parentInfo <- read_excel('data/HIPS_Genotype_Information_2022_2023.xlsx') %>%
  rowwise() %>%
  mutate(age = `release year`) %>%
  mutate(age = case_when(age=="PRE 1952" ~ '1952',
                         age=="PRE 2003" ~ '2003', 
                         age=="PRE 2006" ~ '2006',
                         age=="PRE 1979" ~ '1979',
                         age=="PRE 1999" ~ '1999',
                         age=="PRE 1967" ~ '1967',
                         age=="PRE 1998" ~ '1998',
                         age=="PRE 1969" ~ '1969',
                         age=="NEAR 1946" ~ '1946',
                         age=="PRE 1956" ~ '1956',
                         age=="PRE 1963" ~ '1963',
                         age=="PRE 1932" ~ '1932',
                         age=="PRE 1976" ~ '1976',
                         age=="PRE 1992" ~ '1992',
                         age=="PRE 1951" ~ '1951', 
                         age=="PRE 1957" ~ '1957',
                         age=="PRE 1964" ~ '1964',
                         age=='PRE 1941' ~ '1941',
                         .default = age)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(age = case_when(is.na(age) & !is.na(`received/donation year`) ~ `received/donation year`, 
                         .default = age))

# missingParents <- c('I159', '66', 'A344', 'B7', 'CI 3A', 'K201', 'KYS', 'C.I. 540', 'L 289', 'W606S', 'N209', 'A12', 'WF9')
# missingParentages <- c(1998, 2002, 1996, 1968, 1945, 1963, 1963, 1948, 1934, 1963, 1997,  1965, 1943)
# missingParents <- tibble(genotype = missingParents, age = missingParentages)
# parentInfo <- parentInfo %>%
#   filter(!(genotype %in% missingParents$genotype)) %>%
#   bind_rows(missingParents)

# hybrids.pl <- left_join(hybrids.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(earParentAge = age) %>%
#   left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(pollenParentAge = age) %>%
#   rowwise() %>%
#   mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
#          youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
#          meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))

# for(i in c(8, 10))
# {
#   traitMu <- paste0(yieldComponents[i], '.sp.mu')
#   traitB <- paste0(yieldComponents[i], '.sp.b')
#   meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
#   plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')
# 
#   oldestParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c() +
#     labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Oldest Parent Release Year', width = 1)) +
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           plot.background = element_blank(),
#           legend.position = 'right',
#           legend.background = element_rect(color = 'black'))
# 
#   youngestParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c() +
#     labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Youngest Parent Release Year', width = 1)) +
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           plot.background = element_blank(),
#           legend.position = 'right',
#           legend.background = element_rect(color = 'black'))
#   
#   meanParentPlot <- ggplot(hybrids.pl, aes(.data[[traitMu]], .data[[traitB]], color = meanParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c() +
#     labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Mean Parent Release Year', width = 1)) + 
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(), 
#           plot.background = element_blank(), 
#           legend.position = 'right',
#           legend.background = element_rect(color = 'black'))
#   
#   print(oldestParentPlot)
#   print(youngestParentPlot)
#   print(meanParentPlot)
#   # ggsave(paste0('analysis/', yieldComponents[i], 'LinearPlasticityVsMean.png'), dpi = 1000)
# }
# 
orderedEnvironments <- hybrids %>%
  group_by(environment) %>%
  summarise(yieldMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  arrange(yieldMean)
envAnova <- aov(yieldPerAcre.sp ~ environment, data = hybrids)
envTukey <- TukeyHSD(envAnova)$environment

orderedViolins <- hybrids %>%
  rowwise() %>%
  mutate(environment = factor(environment, levels = orderedEnvironments$environment),
         irrigationLevel = case_when(irrigationProvided==0 ~ '0.0',
                                     irrigationProvided > 3.93 & irrigationProvided < 7.87 ~ '100-200',
                                     irrigationProvided > 7.87 ~ '>200'),
         nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
                                       nitrogenTreatment=='Medium' ~ '150-175',
                                       nitrogenTreatment=='High' ~ '225-250')) %>%
  mutate(irrigationLevel = factor(irrigationLevel, levels = c('0.0', '100-200', '>200')),
         nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250'))) %>%
  ggplot(aes(yieldPerAcre.sp, environment, fill = nitrogenTreatment, color = irrigationLevel)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_text(aes(275,'2023:Ames:0:High'), label = 'a', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:8.6:High'), label = 'ab', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:8.6:Medium'), label = 'ab', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:4.3:High'), label = 'bc', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:4.3:Medium'), label = 'cd', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:8.6:Low'), label = 'cde', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Crawfordsville:0:Medium'), label = 'de', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Crawfordsville:0:High'), label = 'ef', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Ames:0:Medium'), label = 'f', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Crawfordsville:0:High'), label = 'f', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Scottsbluff:16.9:High'), label = 'f', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Missouri Valley:0:Medium'), label = 'f', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Crawfordsville:0:Low'), label = 'fg', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:4.3:Low'), label = 'gh', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Missouri Valley:0:Medium'), label = 'gh', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Scottsbluff:16.9:Medium'), label = 'hi', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Ames:0:Low'), label = 'hij', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Ames:0:Low'), label = 'hij', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Crawfordsville:0:Low'), label = 'ij', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Crawfordsville:0:Medium'), label = 'j', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Ames:0:High'), label = 'k', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Ames:0:Medium'), label = 'k', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Scottsbluff:16.9:Low'), label = 'k', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:0:Low'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:North Platte:4.5:Medium'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Lincoln:0:High'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:0:High'), label = 'lm', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Lincoln:0:Low'), label = 'lm', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:Lincoln:0:Medium'), label = 'mn', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2023:North Platte:0:Medium'), label = 'n', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:North Platte:0:Medium'), label = 'n', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Lincoln:0:Medium'), label = 'o', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Lincoln:0:Low'), label = 'p', size = 3.88, color = 'black', hjust = 1) +
    geom_text(aes(275,'2022:Lincoln:0:High'), label = 'q', size = 3.88, color = 'black', hjust = 1) +
    scale_fill_manual(values = nitrogenColors) + 
    scale_color_manual(values = irrigationColors) +
    labs(x = 'Yield (Bushels/Acre)', y = 'Environment', fill = 'Nitrogen Fertilizer (lbs/acre)', color = 'Irrigation Provided') + 
    theme_minimal() + 
    theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        panel.grid = element_blank(),
        legend.position = 'right')
orderedViolins
ggsave('../orderedViolins.png', plot = orderedViolins, width = 6.5, height = 9, units = 'in', dpi=1000, bg = 'white')
# Okay, let's run the plasticity without LNK 2022
# Estimate FW plasticity across all environments where phenotype was observed
hybridsNOLNK22 <- filter(hybrids, !(location=='Lincoln' & year=='2022'))
hybridsNOLNK22.pl <- estimatePlasticity3(hybridsNOLNK22, trait = paste0(yieldComponents[1], '.sp'), environment = 'environment', genotype = 'genotype')

for(i in 2:length(yieldComponents))
{
  hybridsNOLNK22.pl <- full_join(hybridsNOLNK22.pl, 
                          estimatePlasticity3(hybridsNOLNK22, trait = paste0(yieldComponents[i], '.sp'), environment = 'environment', genotype = 'genotype'),
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
#                          col_names = c('genotype', 'accession', 'additionalAccessionNames', 'age'),
#                          col_types = rep('text', 4)) %>%
#   filter(genotype!='NA') %>%
#   mutate(age = case_when(age=='NEAR 1946' ~ 1946,
#                                  age=='PRE 1956' ~ 1956,
#                                  age=='PRE 1976' ~ 1976,
#                                  age=='PRE 1941' ~ 1941,
#                                  age=='PRE 1964' ~ 1964, 
#                                  .default = as.numeric(age))) %>%
#   select(genotype, age)

hybridsNOLNK22.pl <- left_join(hybridsNOLNK22.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(earParentAge = age) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = age) %>%
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
# 
# # Subset to the locations where we got a significant, classical N response on a population level and estimate plasticity --> does N plasticity correlate across location years?
# nResponse <- filter(hybrids, str_detect(environment, '2023:Ames')|str_detect(environment, '2023:Crawfordsville')|location %in% c('North Platte2', 'Scottsbluff'))
# 
# nResponse.pl <- getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[1], '.sp'), 'nitrogenTreatment', 'genotype')
# 
# for(i in 2:length(yieldComponents))
# {
#   nResponse.pl <- full_join(nResponse.pl,
#                             getNitrogenPlasticityByLocationYear(nResponse, paste0(yieldComponents[i], '.sp'), 'nitrogenTreatment', 'genotype'),
#                             join_by(genotype, locationYear),
#                             suffix = c('', ''),
#                             keep = FALSE)
# }
# 
# nResponse.pl <- nResponse.pl %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .))) %>%
#   mutate(locationYear = case_when(locationYear=='2022:North Platte' ~ '2022:North Platte:4.3', .default = locationYear))
# 
# nResponse.plWide <- nResponse.pl %>%
#   pivot_wider(id_cols = genotype,
#               names_from = locationYear,
#               values_from = yieldPerAcre.sp.b)
# 
# corData <- cor(nResponse.plWide[, 2:5], use = 'complete.obs',) %>%
#   as.table() %>%
#   as.data.frame()
# names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')
# 
# nPlasticityCorYield <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
#   geom_tile(color = 'white') +
#   scale_fill_viridis_c() +
#   scale_x_discrete(breaks = unique(corData$locationYear1),
#                    labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4),
#                               str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
#   scale_y_discrete(breaks = unique(corData$locationYear1),
#                    labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4),
#                               str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
#   labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1)) +
#   theme(text = element_text(color = 'black', size = 14),
#         axis.text.x = element_text(color = 'black', size = rel(1)),
#         axis.text = element_text(color = 'black', size = rel(1)),
#         axis.line = element_line(color = 'black', size = 1),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         plot.background = element_blank(),
#         legend.position = 'right',
#         legend.background = element_blank())
# nPlasticityCorYield
# 
# for(i in 1:length(yieldComponents))
# {
#   traitMu <- paste0(yieldComponents[i], '.sp.mu')
#   traitBestEnv <- paste0(yieldComponents[i], '.sp.FWB')
#   traitWorstEnv <- paste0(yieldComponents[i], '.sp.FWW')
#   numHybrids <- length(hybrids.pl$genotype)
# 
#   sortedHybrids <- hybrids.pl %>%
#     arrange(.data[[traitMu]]) %>%
#     mutate(rankOrder = 1:numHybrids)
# 
#   cxMatrix <- matrix(0, numHybrids, numHybrids)
# 
#   for(x in 1:numHybrids)
#   {
#     xBest <- sortedHybrids[[traitBestEnv]][sortedHybrids$rankOrder==x]
#     xWorst <- sortedHybrids[[traitWorstEnv]][sortedHybrids$rankOrder==x]
# 
#     if(is.na(xBest)|is.na(xWorst))
#     {
#       next
#     }
# 
#     for(y in 1:numHybrids)
#     {
#       yBest <- sortedHybrids[[traitBestEnv]][sortedHybrids$rankOrder==y]
#       yWorst <- sortedHybrids[[traitWorstEnv]][sortedHybrids$rankOrder==y]
# 
#       if(is.na(yBest)|is.na(yWorst))
#       {
#         next
#       }
# 
#       if((xBest - yBest)*(xWorst - yWorst) < 0)
#       {
#         cxMatrix[x, y] <- 1
#       }
#     }
#   }
# 
#   cxData <- as.data.frame(cxMatrix)
#   cols <- colnames(cxData)
#   cxData <- mutate(cxData, genotypeRank1 = 1:numHybrids) %>%
#     pivot_longer(starts_with('V'), names_to = 'genotypeRank2', values_to = 'crossover', names_prefix = 'V')
# 
#   cxHeatmap <- ggplot(cxData, aes(genotypeRank1, as.numeric(genotypeRank2), fill = factor(crossover))) +
#     geom_tile(color = 'white') +
#     scale_fill_manual(values = c('white', moma.colors('VanGogh', 1))) +
#     labs(x = 'Genotype Mean Rank', y = 'Genotype Mean Rank', fill = str_wrap('Finlay-Wilkinson Predicted Crossover Interaction', 1), title = yieldComponentsLabels[i]) +
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text.x = element_text(color = 'black', size = rel(1)),
#           axis.text.y = element_text(color = 'black', size = rel(1)),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           plot.background = element_blank(),
#           legend.position = 'none',
#           legend.background = element_blank())
# 
#   print(cxHeatmap)
# }

# # Time to overlay violins on the map
# nColors <- moma.colors('vonHeyl')
# nColors <- c(nColors[3], nColors[2], nColors[1])
# hybridsByPlasticity <- hybrids.pl %>%
#   arrange(yieldPerAcre.sp.b)
# 
# 
# yieldViolinData <- hybrids %>%
#   mutate(environment = factor(environment, levels = c("2022:Scottsbluff:16.9:Low", "2022:Scottsbluff:16.9:Medium", "2022:Scottsbluff:16.9:High", "2022:North Platte:0:Low", "2022:North Platte:0:Medium", 
#                                                       "2022:North Platte:0:High", "2022:North Platte:4.3:Low", "2022:North Platte:4.3:Medium", "2022:North Platte:4.3:High","2022:North Platte:8.6:Low", 
#                                                       "2022:North Platte:8.6:Medium", "2022:North Platte:8.6:High", "2023:North Platte:0:Medium", "2023:North Platte:4.5:Medium", 
#                                                       "2022:Lincoln:0:Low", "2022:Lincoln:0:Medium", "2022:Lincoln:0:High", "2023:Lincoln:0:Low", "2023:Lincoln:0:Medium", "2023:Lincoln:0:High", 
#                                                       "2022:Missouri Valley:0:Medium", "2023:Missouri Valley:0:Medium", "2022:Ames:0:Low", "2022:Ames:0:Medium", "2022:Ames:0:High", "2023:Ames:0:Low",
#                                                       "2023:Ames:0:Medium", "2023:Ames:0:High", "2022:Crawfordsville:0:Low", "2022:Crawfordsville:0:Medium", "2022:Crawfordsville:0:High", 
#                                                       "2023:Crawfordsville:0:Low", "2023:Crawfordsville:0:Medium", "2023:Crawfordsville:0:High"))) 
# yieldViolins <- ggplot(yieldViolinData, aes(environment, yieldPerAcre.sp, fill = nitrogenTreatment, color = irrigationProvided, group = environment)) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 1.1) + 
#   geom_line(data = yieldViolinData, aes(group = genotype), alpha = 0.1, color = 'lightgrey') +  
#   scale_fill_manual(values = nColors) + 
#   labs(x = 'Environment', y = 'Yield (Bushels / Acre)', fill = 'Nitrogen Treatment', color = 'Irrigation Provided (in)') +
#   theme(text = element_text(color = 'black', size = 14),
#         axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, color = 'black'),
#       axis.text.y = element_text(color = 'black', size = rel(1)),
#       axis.text = element_text(color = 'black', size = rel(1)),
#       axis.line = element_line(color = 'black', size = 1),
#       line = element_line(size = 1),
#       panel.background = element_rect(fill = 'transparent', color = NA),
#       panel.border = element_blank(),
#       panel.grid = element_blank(), 
#       plot.background = element_rect(fill = 'transparent', color = NA), 
#       legend.position = 'bottom',
#       legend.background = element_blank())
# yieldViolins
# 
# How many plots and non-missing values do we have?
nonMissingVals <- 0
totalPlots <- length(hybrids$qrCode)
# vars <- c('anthesisDate', 'silkDate', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk',
#           'anthesisSilkingIntervalSilkingGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'combineYield', 'yieldPerAcre',
#           'combineMoisture', 'combineTestWeight', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow',
#           'kernelRowNumber', 'kernelsPerEar', 'shelledCobMass', 'percentMoisture', 'percentStarch', 'percentProtein', 'percentOil',
#           'percentFiber', 'percentAsh', 'kernelColor', 'percentLodging', 'totalStandCount')
vars <- phenotypes
hybrids <- hybrids %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
for(var in vars)
{
  numMissingVals <- as.numeric(sum(is.na(hybrids[[var]])))
  numNotMissing <- totalPlots - numMissingVals
  nonMissingVals <- nonMissingVals + numNotMissing
}
# 
# # Are some of the patterns I'm seeing only due to the addition of hybrids in 2023? Let's subset to hybrids present in both years
# hybrids2022 <- filter(hybrids, year=='2022')
# hybrids2022 <- unique(hybrids2022$genotype)
# hybrids2023 <- filter(hybrids, year=='2023')
# hybrids2023 <- unique(hybrids2023$genotype)
# hybridsCommon <- intersect(hybrids2022, hybrids2023)
# hybridsCommon <- filter(hybrids, genotype %in% hybridsCommon)
# 
# # Estimate plasticity and plot vs mean performance
# # Estimate FW plasticity across all environments where phenotype was observed
# hybridsCommon.pl <- estimatePlasticity3(hybridsCommon, trait = paste0(phenotypes[1], '.sp'), environment = 'environment', genotype = 'genotype')
# 
# for(i in 2:length(phenotypes))
# {
#   hybridsCommon.pl <- full_join(hybridsCommon.pl, 
#                           estimatePlasticity3(hybridsCommon, trait = paste0(phenotypes[i], '.sp'), environment = 'environment', genotype = 'genotype'),
#                           join_by(genotype),
#                           suffix = c('', ''), 
#                           keep = FALSE)
#   # Print mean of the b column so we know we are getting reasonable values
#   print(mean(hybridsCommon.pl[[paste0(phenotypes[i], '.sp.b')]], na.rm = TRUE))
# }
# 
# hybridsCommon.pl <- hybridsCommon.pl %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))
# 
# hybridsCommon.pl <- hybridsCommon.pl %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          earParent = str_split_i(genotype, ' X ', 1),
#          pollenParent = str_split_i(genotype, ' X ', 2))
# # parentInfo <- read.csv('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.csv')
# # parentInfo <- read_excel('data/HIPS_Genotype_Information_2022 - HIPS_Genotype_Information_2022_2023.xlsx',
# #                          range = 'A2:D357', 
# #                          col_names = c('genotype', 'accession', 'additionalAccessionNames', 'age'),
# #                          col_types = rep('text', 4)) %>%
# #   filter(genotype!='NA') %>%
# #   mutate(age = case_when(age=='NEAR 1946' ~ 1946,
# #                                  age=='PRE 1956' ~ 1956,
# #                                  age=='PRE 1976' ~ 1976,
# #                                  age=='PRE 1941' ~ 1941,
# #                                  age=='PRE 1964' ~ 1964, 
# #                                  .default = as.numeric(age))) %>%
# #   select(genotype, age)
# 
# hybridsCommon.pl <- left_join(hybridsCommon.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(earParentAge = age) %>%
#   left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(pollenParentAge = age) %>%
#   rowwise() %>%
#   mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
#          youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
#          meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))

# for(i in 6)
# {
#   traitMu <- paste0(yieldComponents[i], '.sp.mu')
#   traitB <- paste0(yieldComponents[i], '.sp.b')
#   meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
#   plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')
# 
#   oldestParentPlot <- ggplot(hybridsCommon.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c() +
#     labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Oldest Parent Release Year', width = 1)) +
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           plot.background = element_blank(),
#           legend.position = 'right',
#           legend.background = element_rect(color = 'black'))
# 
#   youngestParentPlot <- ggplot(hybridsCommon.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c() +
#     labs(x = meanLabel, y = str_wrap(plasticityLabel, width = 20), color = str_wrap('Youngest Parent Release Year', width = 1)) +
#     theme(text = element_text(color = 'black', size = 14),
#           axis.text = element_text(color = 'black', size = rel(1)),
#           axis.line = element_line(color = 'black', size = 1),
#           panel.background = element_blank(),
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           plot.background = element_blank(),
#           legend.position = 'right',
#           legend.background = element_rect(color = 'black'))
#   
#   meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = meanParentAge)) +
#     geom_point() +
#     geom_hline(yintercept=1) +
#     scale_color_viridis_c(direction = -1) +
#     labs(x = meanLabel, y = plasticityLabel, color = str_wrap('Mean Parent Release Year', width = 1)) + 
#     theme_minimal() +
#     theme(axis.text.x = element_text(color = 'black', size = 11),
#           axis.text.y = element_text(color = 'black', size = 11),
#           legend.text = element_text(color = 'black', size = 11),
#           text = element_text(color = 'black', size = 11),
#           legend.position = 'none',
#           panel.grid = element_blank())
#   
#   print(oldestParentPlot)
#   print(youngestParentPlot)
#   print(meanParentPlot)
#   # ggsave(paste0('analysis/', yieldComponents[i], 'LinearPlasticityVsMean.png'), dpi = 1000)
# }

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
  rename(earParentAge = age) %>%
  left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
  rename(pollenParentAge = age) %>%
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

corData <- cor(nResponse.plWide[, 2:5], use = 'complete.obs', method = 'spearman') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorYield <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 10), str_wrap('2022 Scottsbluff', 10), 
                              str_wrap('2023 Ames', 10), str_wrap('2023 Crawfordsville', 8))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 10), str_wrap('2022 Scottsbluff', 10), 
                              str_wrap('2023 Ames', 10), str_wrap('2023 Crawfordsville', 10))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Yield (bushels/acre)') + 
  theme_minimal() +
  theme(text = element_text(color = 'black', size = 11),
        axis.text.x = element_text(color = 'black', size = rel(1), angle = 90),
        axis.text = element_text(color = 'black', size = rel(1), hjust = 1, margin = margin(0,0,0, 0)),
        plot.title = element_text(color = 'black', size = 11, hjust = 0.5),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        legend.background = element_blank())
nPlasticityCorYield
# # Kernel Row Number
# nResponse.plWideKRN <- nResponse.pl %>%
#   pivot_wider(id_cols = genotype, 
#               names_from = locationYear,
#               values_from = kernelRowNumber.sp.b)
# 
# corData <- cor(nResponse.plWideKRN[, 2:5], use = 'complete.obs') %>%
#   as.table() %>%
#   as.data.frame()
# names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')
# 
# nPlasticityCorKRN <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
#   geom_tile(color = 'white') +
#   scale_fill_viridis_c(direction = -1) + 
#   scale_x_discrete(breaks = unique(corData$locationYear1), 
#                    labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
#                               str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
#   scale_y_discrete(breaks = unique(corData$locationYear1), 
#                    labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
#                               str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
#   labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Kernel Row Number') + 
#   theme(text = element_text(color = 'black', size = 11),
#         axis.text.x = element_text(color = 'black', size = rel(1)),
#         axis.text = element_text(color = 'black', size = rel(1)),
#         axis.line = element_blank(),
#         panel.background = element_blank(),
#         panel.border = element_blank(),
#         panel.grid = element_blank(), 
#         plot.background = element_blank(), 
#         legend.position = 'none',
#         legend.background = element_blank())
# nPlasticityCorKRN

# Hundred Kernel Mass
nResponse.plWideHKM <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = yieldPerAcre.sp.b)

corData <- cor(nResponse.plWideHKM[, 2:5], use = 'complete.obs', method = 'spearman') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorHKM <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  guides(fill = guide_colourbar(barwidth = 12,
                                barheight = 1)) +
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 4), str_wrap('2022 Scottsbluff', 4), 
                              str_wrap('2023 Ames', 4), str_wrap('2023 Crawfordsville', 4))) +
  labs(x = '', y = '', fill = 'Nitrogen Plasticity Correlation', title = 'Hundred Kernel Mass (g)') + 
  theme(text = element_text(color = 'black', size = 11),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'bottom',
        legend.background = element_blank(), 
        legend.text = element_text(color = 'black', size = 11),
        legend.title = element_text(color = 'black', size = 11, vjust = 0.85))
nPlasticityCorHKM

nPlasticityCorLegend <- get_legend(nPlasticityCorHKM) 

nPlasticityCorHKM <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 10), str_wrap('2022 Scottsbluff', 10), 
                              str_wrap('2023 Ames', 10), str_wrap('2023 Crawfordsville', 10))) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(str_wrap('2022 North Platte:4.3', 10), str_wrap('2022 Scottsbluff', 10), 
                              str_wrap('2023 Ames', 10), str_wrap('2023 Crawfordsville', 10))) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Hundred Kernel Mass (g)') + 
  theme_minimal() +
  theme(text = element_text(color = 'black', size = 11),
        axis.text.x = element_text(color = 'black', size = rel(1), angle = 90),
        axis.text = element_text(color = 'black', size = rel(1), hjust = 1, margin = margin(0, 0, 0, 0)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        plot.title = element_text(color = 'black', size = 11, hjust = 0.5),
        legend.background = element_blank())
nPlasticityCorHKM


# which variables, and locationYear should we show for scatter with plasticity and parentAge
# R2
# locationYears <- unique(nResponse.pl$locationYear)
# for(locationYear in locationYears)
# {
#   df <- filter(nResponse.pl, locationYear==locationYear)
#   for (yieldComponent in yieldComponents)
#   {
#     print(paste0(locationYear, yieldComponent, 'meanParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'meanParentAge'), sep = ':'))
#     print(paste0(locationYear, yieldComponent, 'youngestParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'youngestParentAge'), sep = ':'))
#     print(paste0(locationYear, yieldComponent, 'oldestParentAge ', getR2(df, paste0(yieldComponent, '.sp.b'), 'oldestParentAge'), sep = ':'))
#   }
# }
# 
# #R
# for(locationYear in locationYears)
# {
#   df <- filter(nResponse.pl, locationYear==locationYear)
#   for (yieldComponent in yieldComponents)
#   {
#     print(paste0(locationYear, yieldComponent, 'meanParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'meanParentAge'), sep = ':'))
#     print(paste0(locationYear, yieldComponent, 'youngestParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'youngestParentAge'), sep = ':'))
#     print(paste0(locationYear, yieldComponent, 'oldestParentAge ', getR(df, paste0(yieldComponent, '.sp.b'), 'oldestParentAge'), sep = ':'))
#   }
# }

ames23NResponse.pl <- filter(nResponse.pl, locationYear=='2023:Ames')

nPlasticityAmesYield <- ggplot(ames23NResponse.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept = 1, color = 'black', linewidth = 1) +
  scale_color_viridis(direction = -1) +
  scale_y_continuous(limits = c(0, 1.5)) + 
  guides(color = guide_colourbar(barwidth = 12,
                         barheight = 1)) +
  labs(x = 'Mean Yield (bushels/acre)', y = 'Nitrogen Plasticity', color = 'Mean Parent Age') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'bottom',
        legend.title = element_text(color = 'black', size = 11, vjust = 0.75),
        panel.grid = element_blank())
 nPlasticityAmesYield
 
 nPlasticityParentAgeLegend <- get_legend(nPlasticityAmesYield)
 
 nPlasticityAmesYield <- ggplot(ames23NResponse.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
   geom_point() +
   geom_hline(yintercept = 1, color = 'black') +
   scale_color_viridis(direction = -1) +
   scale_y_continuous(limits = c(0.5, 1.5)) + 
   labs(x = 'Mean Yield (bushels/acre)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
   theme_minimal() +
   theme(axis.text.x = element_text(color = 'black', size = 11),
         axis.text.y = element_text(color = 'black', size = 11),
         legend.text = element_text(color = 'black', size = 11),
         text = element_text(color = 'black', size = 11),
         legend.position = 'none',
         panel.grid = element_blank())
 nPlasticityAmesYield

 # nPlasticitySBKRN <- ggplot(sb22NResponse.pl, aes(kernelRowNumber.sp.mu, kernelRowNumber.sp.b, color = meanParentAge)) +
 #   geom_point() +
 #   geom_hline(yintercept = 1, color = 'black') +
 #   scale_color_viridis(direction = -1) +
 #   scale_y_continuous(limits = c(-2.5, 2.5)) + 
 #   labs(x = 'Mean Kernel Row Number', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
 #   theme_minimal() +
 #   theme(axis.text.x = element_text(color = 'black', size = 11),
 #         axis.text.y = element_text(color = 'black', size = 11),
 #         legend.text = element_text(color = 'black', size = 11),
 #         text = element_text(color = 'black', size = 11),
 #         legend.position = 'none',
 #         panel.grid = element_blank())
 # nPlasticitySBKRN

 nPlasticityAmesHKM <- ggplot(ames23NResponse.pl, aes(hundredKernelMass.sp.mu, hundredKernelMass.sp.b, color = meanParentAge)) +
   geom_point() +
   geom_hline(yintercept = 1, color = 'black') +
   scale_color_viridis(direction = -1) +
   scale_y_continuous(limits = c(0.5, 1.5)) + 
   labs(x = 'Mean Hundred Kernel Mass (g)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
   theme_minimal() +
   theme(axis.text.x = element_text(color = 'black', size = 11),
         axis.text.y = element_text(color = 'black', size = 11),
         legend.text = element_text(color = 'black', size = 11),
         text = element_text(color = 'black', size = 11),
         legend.position = 'none',
         panel.grid = element_blank())
 nPlasticityAmesHKM

 # Which genotypes are 'most' and 'least' nitrogen stable across these envs for yield
ames23Genotypes <- ames23NResponse.pl$genotype
cf23NResponse.pl <- filter(nResponse.pl, locationYear=="2023:Crawfordsville")
cf23Genotypes <- cf23NResponse.pl$genotype
nResponse23Genotypes <- intersect(ames23Genotypes, cf23Genotypes)
sb22NResponse.pl <- filter(nResponse.pl, locationYear=='2022:Scottsbluff')
sb22Genotypes <- sb22NResponse.pl$genotype
np22NResponse.pl <- filter(nResponse.pl, locationYear=='2022:North Platte:4.3')
np22Genotypes <- np22NResponse.pl$genotype
nResponse22Genotypes <- intersect(sb22Genotypes, np22Genotypes)
nResponseGenotypes <- intersect(nResponse22Genotypes, nResponse23Genotypes)

nResponseGenotypeLevel <- nResponse.pl %>%
  filter(genotype %in% nResponseGenotypes) %>%
   group_by(genotype) %>%
   summarise(maxNPlasticityYield = max(yieldPerAcre.sp.b, na.rm = TRUE), 
             minNPlasticityYield = min(yieldPerAcre.sp.b, na.rm = TRUE), 
             meanNPlasticityYield = mean(yieldPerAcre.sp.b, na.rm = TRUE)) %>%
   arrange(meanNPlasticityYield)
 
lowPlasticityGenotype <- nResponseGenotypeLevel$genotype[1] 
avgPlasticityGenotype <- nResponseGenotypeLevel$genotype[round(length(nResponseGenotypeLevel$genotype)/2)]
highPlasticityGenotype <- nResponseGenotypeLevel$genotype[length(nResponseGenotypeLevel$genotype)]
 
nPlasticityLAHAmes <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & str_detect(environment, '2023:Ames'))  %>%
  group_by(genotype, environment, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
                                       nitrogenTreatment=='Medium' ~ '150-175',
                                       nitrogenTreatment=='High' ~ '225-250')) %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
         genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

nPlasticityLAHCF <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & str_detect(environment, '2023:Crawfordsville')) %>%
  group_by(genotype, environment, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
                                       nitrogenTreatment=='Medium' ~ '150-175',
                                       nitrogenTreatment=='High' ~ '225-250')) %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
         genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

nPlasticityLAHNP2 <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & location=='North Platte2') %>%
  group_by(genotype, environment, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
                                       nitrogenTreatment=='Medium' ~ '150-175',
                                       nitrogenTreatment=='High' ~ '225-250')) %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
         genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

nPlasticityLAHSB <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & location=='Scottsbluff') %>%
  group_by(genotype, environment, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
                                       nitrogenTreatment=='Medium' ~ '150-175',
                                       nitrogenTreatment=='High' ~ '225-250')) %>%
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
         genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

nPlasticityGenotypeLines <- ggplot() + 
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHNP2, linetype = 'solid') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHSB, linetype = 'dashed') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHAmes, linetype = 'dotted') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHCF, linetype = 'dotdash') +
  scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)],
                     label = label_wrap(20)) + 
  labs(x = 'Nitrogen Fertilizer (lbs/acre)', y = 'Mean Yield (bushels/acre)', color = 'Genotype') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'right',
        panel.grid = element_blank())
nPlasticityGenotypeLines

nPlasticityLinesGenotypeLegend <- get_legend(nPlasticityGenotypeLines)

linetypes <- tibble(environment = c('2022 North Platte:4.3', '2022 Scottsbluff', '2023 Ames', '2023 Crawfordsville'), x = 1, y = 1)
linetypePlot <- ggplot(linetypes, aes(x, y, linetype = environment)) +
  geom_line() + 
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash'),
                        labels = label_wrap(20)) +
  labs(linetype = 'Environment') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'right',
        panel.grid = element_blank())
linetypePlot

nPlasticityLinesLinetypeLegend <- get_legend(linetypePlot)

nPlasticityGenotypeLines <- ggplot() + 
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHAmes, linetype = 'solid') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHCF, linetype = 'dashed') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHNP2, linetype = 'dotted') +
  geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHSB, linetype = 'dotdash') +
  scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)]) + 
  labs(x = 'Nitrogen Fertilizer (lbs/acre)', y = 'Mean Yield (bushels/acre)', color = 'Genotype') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
nPlasticityGenotypeLines

nPlasticityGenotypeLinesLegends <- plot_grid(nPlasticityLinesGenotypeLegend, nPlasticityLinesLinetypeLegend, ncol = 1)
nPlasticityGenotypeLinesLegends

nPlasticityGenotypeLinesPlot <- plot_grid(nPlasticityGenotypeLines, nPlasticityGenotypeLinesLegends, ncol = 2, rel_widths = c(1, 0.4))
nPlasticityGenotypeLinesPlot
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

for(i in 6)
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
  print(cor(dfWide$blockSet1, dfWide$blockSet2, use = 'complete.obs', method = 'spearman'))
  print(nPlasticityBlockCorr)
}
  


for(i in 6)
{
  traitMu <- paste0(yieldComponents[i], '.sp.mu')
  traitBestEnv <- paste0(yieldComponents[i], '.sp.FWB')
  traitWorstEnv <- paste0(yieldComponents[i], '.sp.FWW')
  trait <- paste0(yieldComponents[i], '.sp')
  
  blups <- lmer(as.formula(paste0(trait, ' ~ environment + (1|genotype)')), data = hybridsNOLNK22) 
  blups <- ranef(blups)
  blups <- as_tibble(blups$genotype, rownames = 'genotype') %>%
    rename(blup = `(Intercept)`) %>%
    mutate(rankOrder = row_number(blup))
  
  sortedhybridsCommon <- hybridsNOLNK22.pl %>%
    full_join(blups, join_by(genotype), keep = FALSE, suffix = c('', '')) %>%
    arrange(blup) %>%
    filter(!is.na(.data[[traitBestEnv]]))
  numhybridsCommon <- length(sortedhybridsCommon$genotype)
  
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
        if(xBest > yBest)
        {
          cxMatrix[x, y] <- 1
        }
        else
        {
          cxMatrix[x, y] <- -1
        }
      }
    }
  }
  
  cxData <- as.data.frame(cxMatrix)
  cols <- colnames(cxData)
  cxData <- mutate(cxData, genotypeRank1 = 1:numhybridsCommon) %>%
    pivot_longer(starts_with('V'), names_to = 'genotypeRank2', values_to = 'crossover', names_prefix = 'V') %>%
    filter(crossover==-1 | crossover==1) %>%
    mutate(crossover = case_when(crossover==1 ~ 'X',
                                 crossover==-1 ~ 'Y'))
  
  cxHeatmap <- ggplot(cxData, aes(genotypeRank1, as.numeric(genotypeRank2), fill = factor(crossover))) + 
    geom_tile(color = 'white', alpha = 1) +
    scale_fill_manual(values = viridis_pal()(4)[1:2]) +
    labs(x = 'Hybrid Rank X', 
         y = 'Hybrid Rank Y', 
         fill = str_wrap('Superior Hybrid in Best Environment', 1), 
         title = yieldComponentsLabels[i]) + 
    theme_minimal() +
    theme(text = element_text(color = 'black', size = 11),
          title = element_text(color = 'black', size = 11),
          axis.text.x = element_text(color = 'black', size = rel(1)),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          plot.title = element_text(hjust = 0.5),
          legend.position = 'right',
          legend.background = element_blank())
  
  print(cxHeatmap)
}

# # How often are interactions between a pair of hybrids crossover interactions AND represent significant differences in the phenotype?
# genotypePairs <- tibble(genotype1 = NULL, genotype2 = NULL)
# hybridEnvs <- hybrids %>%
#   group_by(environment) %>%
#   summarise(environmentCode = cur_group_id())
# hybrids <- full_join(hybrids, hybridEnvs, join_by(environment), keep = FALSE, suffix = c('', ''))
# 
# hybridsSigCrossovers <- hybrids %>%
#   rowwise() %>%
#   mutate(genotype = str_replace_all(genotype, '-', ' '))
# 
# allGenotypes <- unique(hybridsSigCrossovers$genotype)
# totalGenotypes <- length(allGenotypes)
# 
# for(i in 1:totalGenotypes)
# {
#   df <- tibble(genotype1 = allGenotypes[i], genotype2 = allGenotypes[(i + 1):totalGenotypes])
#   genotypePairs <- bind_rows(genotypePairs, df)
# }
# 
# environments <- unique(hybrids$environmentCode)
# totalEnvironments <- length(environments)
# 
# for(i in phenotypes)
# {
#   genotypePairs <- full_join(genotypePairs, 
#                              getSignificantCrossovers(hybridsSigCrossovers, i, environments), 
#                              join_by(genotype1, genotype2), 
#                              keep = FALSE, 
#                              suffix = c('', ''))
# }
# 
# write.csv(genotypePairs, 'analysis/significantCrossovers.csv')
sigCrossovers <- read.csv('analysis/significantCrossovers.csv')

heatmap <- plotInteractionImportanceGrid(trait = 'yieldPerAcre', traitLabel = 'Yield (bushels/acre)', legendPosition = 'right')

highPlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
highPlasticityGenotype <- highPlasticityGenotype$genotype[length(highPlasticityGenotype$genotype)]

lowPlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
lowPlasticityGenotype <- lowPlasticityGenotype$genotype[1]

averagePlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
averagePlasticityGenotype <- averagePlasticityGenotype$genotype[round(length(averagePlasticityGenotype$genotype)/2)]

LAHPlasticities <- filter(hybridsNOLNK22.pl, genotype %in% c(lowPlasticityGenotype, averagePlasticityGenotype, highPlasticityGenotype)) %>% 
  select(genotype, yieldPerAcre.sp.b, yieldPerAcre.sp.FWW) %>%
  arrange(desc(yieldPerAcre.sp.b))

meanYield <- mean(hybridsNOLNK22$yieldPerAcre.sp, na.rm = TRUE)

LAHData <- hybridsNOLNK22 %>%
  group_by(environment) %>%
  mutate(envIndex = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  filter(genotype %in% c(lowPlasticityGenotype, averagePlasticityGenotype, highPlasticityGenotype)) %>%
  ungroup() %>%
  mutate(genotype = factor(genotype, levels = c(highPlasticityGenotype, averagePlasticityGenotype, lowPlasticityGenotype)))
worstEnvIndex <- min(LAHData$envIndex, na.rm = TRUE)

FWConceptualPlot <- ggplot(LAHData, aes(envIndex, yieldPerAcre.sp, color = genotype)) +
  geom_point() + 
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[1], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[1] - worstEnvIndex, color = viridis_pal()(4)[1]) +
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[2], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[2] - worstEnvIndex, color = viridis_pal()(4)[3]) +
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[3], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[3] - worstEnvIndex, color = viridis_pal()(4)[4]) +
  scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)]) + 
  labs(x = 'Mean Environment Yield (bushels/acre)', y = 'Yield (bushels/acre)', color = '') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'right',
        legend.background = element_blank(),
        panel.grid = element_blank())
FWConceptualPlotLegend <- get_legend(FWConceptualPlot)
FWConceptualPlot <- ggplot(LAHData, aes(envIndex, yieldPerAcre.sp, color = genotype)) +
  geom_point() + 
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[1], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[1] - worstEnvIndex, color = viridis_pal()(4)[1]) +
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[2], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[2] - worstEnvIndex, color = viridis_pal()(4)[3]) +
  geom_abline(slope = LAHPlasticities$yieldPerAcre.sp.b[3], intercept = LAHPlasticities$yieldPerAcre.sp.FWW[3] - worstEnvIndex, color = viridis_pal()(4)[4]) +
  scale_x_continuous(limits = c(50, 225)) + 
  scale_y_continuous(limits = c(50, 225)) +
  scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)]) + 
  labs(x = 'Mean Environment Yield (bushels/acre)', y = 'Yield (bushels/acre)', color = '') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank()) +
  inset_element(FWConceptualPlotLegend, left = 0.25, bottom = 0.6, right = 0.35, top = 1.05, on_top = FALSE)
FWConceptualPlot

phenotypeLabelsGCAVP <- c('Plant Density', 'Test Weight', 'Harvest Moisture', 'Flag Leaf Height',
                        'Ear Height', 'Yield', 'GDD To Anthesis*', 'GDD to Silk*', 
                        'Anthesis Silking Interval*', 'Kernel Row Number', 'Ear Width', 'Ear Length',
                        'Shelled Cob Width', 'Shelled Cob Mass', 'Kernel Mass Per Ear', 
                        'Kernels Per Ear', 
                        'Hundred Kernel Mass', 'Ear Fill Length*', 'Kernels Per Row*')

gca_vp <- tibble(grp = NULL, responseVar = NULL, vcov = NULL, pctVar = NULL)
for(i in 1:length(phenotypes))
{
  var <- paste0(phenotypes[i], '.sp.b')
  
  if(!(var %in% colnames(hybridsNOLNK22.pl))){next}
  
  gca_vp <- bind_rows(gca_vp, partitionVariance3(hybridsNOLNK22.pl, var, phenotypeLabelsGCAVP[i], '~ (1|earParent) + (1|pollenParent)'))
}

gca_vp <- gca_vp %>%
  rowwise() %>%
  mutate(grp = case_when(grp=='earParent' ~ 'Parent GCA',
                         grp=='pollenParent' ~ 'Parent GCA',
                         .default = grp) %>%
           factor(levels = c('Parent GCA', 'Residual'))) %>%
  group_by(grp, responseVar, label) %>%
  summarise(pctVar = sum(pctVar, na.rm = TRUE))

gcaImportanceOrder <- gca_vp %>%
  filter(grp=='Parent GCA') %>%
  arrange(desc(pctVar))
gcaImportanceOrder <-gcaImportanceOrder$label

gca_vp <- mutate(gca_vp, label = factor(label, levels = gcaImportanceOrder))

gca_vp.plot <- ggplot(gca_vp, aes(label, pctVar, fill = grp)) +
  geom_col(position = 'stack') + 
  scale_fill_manual(values = viridis_pal()(4)[1:2]) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     labels = c('0%', '25%', '50%', '75%', '100%')) +
  labs(x = 'Phenotype', y = 'Linear Plasticity Variance', fill = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11, color = 'black', angle = 90, margin = margin(0, 0, 0, 0), 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 11, color = 'black', vjust = 0, hjust = 0),
        legend.text = element_text(size = 11, color = 'black'),
        text = element_text(size = 11, color = 'black'),
        legend.position = 'right',
        line = element_line(color = 'black', linewidth = 1),
        panel.grid = element_blank())
gca_vp.plot



# rfFeatures <- read.csv('analysis/featureImportances.csv')[,2:6]
# colnames(rfFeatures) <- c('plantDensity', 'kernelRowNumber', 'kernelsPerRow', 'hundredKernelMass', 'environment')
# 
# rfFeaturesSummmary <- rfFeatures %>%
#   summarise(plantDensity.mean = mean(plantDensity, na.rm = TRUE),
#             plantDensity.sd = sd(plantDensity, na.rm = TRUE),
#             kernelRowNumber.mean = mean(kernelRowNumber, na.rm = TRUE),
#             kernelRowNumber.sd = sd(kernelRowNumber, na.rm = TRUE),
#             kernelsPerRow.mean = mean(kernelsPerRow, na.rm = TRUE),
#             kernelsPerRow.sd = sd(kernelsPerRow, na.rm = TRUE),
#             hundredKernelMass.mean = mean(hundredKernelMass, na.rm = TRUE),
#             hundredKernelMass.sd = sd(hundredKernelMass, na.rm = TRUE)) %>%
#   pivot_longer(everything(), names_to = 'type', values_to = 'val') %>%
#   rowwise() %>%
#   mutate(phenotype = str_split_i(type, fixed('.'), 1),
#          statistic = str_split_i(type, fixed('.'), 2)) %>%
#   select(phenotype, statistic, val) %>%
#   pivot_wider(id_cols = phenotype, names_from = statistic, values_from = val) %>%
#   mutate(phenotypeLabel = c('Plant Density (plants/acre)', 'Kernel Row Number', 'Kernels Per Row', 'Hundred Kernel Mass (g)')) %>%
#   mutate(phenotypeLabel = factor(phenotypeLabel,
#                                  levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Hundred Kernel Mass (g)', 
#                                             'Kernels Per Row'), ordered = TRUE))
# 
# rfFeatures <- rfFeatures %>%
#   pivot_longer(!c(environment), names_to = 'phenotype', values_to = 'val') %>%
#   rowwise() %>%
#   mutate(phenotypeLabel = case_when(phenotype=='plantDensity' ~ 'Plant Density (plants/acre)',
#                                     phenotype=='kernelRowNumber' ~ 'Kernel Row Number',
#                                     phenotype=='kernelsPerRow' ~ 'Kernels Per Row', 
#                                     phenotype=='hundredKernelMass' ~ 'Hundred Kernel Mass (g)')) %>%
#   mutate(phenotypeLabel = factor(phenotypeLabel,
#                                  levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Hundred Kernel Mass (g)', 
#                                             'Kernels Per Row'), ordered = TRUE))
# 
# featureImportance <- ggplot(rfFeatures, aes(val, phenotypeLabel, fill = phenotypeLabel)) +
#   geom_boxplot(color = 'black') +
#   scale_y_discrete(labels = str_wrap(rfFeatures$phenotypeLabel, 9)) +
#   scale_fill_viridis(discrete = TRUE) +
#   labs(x = 'Mean Feature Importance', y = '') +
#   theme_minimal() + 
#   theme(axis.text.x = element_text(color = 'black', size = 11),
#         axis.text.y = element_text(color = 'black', size = 11),
#         text = element_text(color = 'black', size = 11),
#         panel.grid = element_blank(),
#         legend.position = 'none')
# featureImportance
# # Extension: https://crops.extension.iastate.edu/blog/meaghan-anderson/making-yield-estimates-corn-2022-edition
# yieldPredictions <- read.csv('analysis/RFpredictions.csv') %>%
#   select(environment, plotNumber, predictedYield) %>%
#   rename(predictedYieldRF = predictedYield) %>%
#   full_join(hybrids, join_by(environment, plotNumber), keep = FALSE, suffix = c('', '')) %>%
#   filter(!is.na(predictedYieldRF)) %>%
#   select(environment, plotNumber, yieldPerAcre, predictedYieldRF, plantDensity, kernelRowNumber, kernelsPerRow, hundredKernelMass) %>%
#   mutate(predictedYieldExtension = (plantDensity*kernelRowNumber*kernelsPerRow)/((56*453.592*100)/hundredKernelMass))
# 
# rfRegressionModel <- lm(predictedYieldRF ~ yieldPerAcre, data = yieldPredictions)
# rfIntercept <- rfRegressionModel$coefficients[1]
# rfSlope <- rfRegressionModel$coefficients[2]
# # Adjusted R2 from summary(model)
# rfR2 <- 0.6365
# 
# extensionRegressionModel <- lm(predictedYieldExtension ~ yieldPerAcre, data = yieldPredictions)
# extensionIntercept <- extensionRegressionModel$coefficients[1]
# extensionSlope <- extensionRegressionModel$coefficients[2]
# # Adjusted R2 from summary(model)
# extensionR2 <- 0.4567
# 
# yieldPredictionsSubsample <- yieldPredictions %>%
#   slice_sample(prop = 0.5) %>%
#   pivot_longer(c(predictedYieldExtension, predictedYieldRF), 
#                values_to = 'predictedYield', 
#                names_to = 'method', 
#                names_prefix = 'predictedYield') %>%
#   rowwise() %>%
#   mutate(method = case_when(method=='RF' ~ 'Random Forest', .default = method))
# 
# yieldPredictionsPlot <- ggplot(yieldPredictionsSubsample, aes(yieldPerAcre, predictedYield, color = method)) + 
#   geom_point() + 
#   geom_abline(slope = extensionSlope, intercept = extensionIntercept, color = viridis_pal()(4)[1], linewidth = 1) +
#   geom_abline(slope = rfSlope, intercept = rfIntercept, color = viridis_pal()(4)[2], linewidth = 1) +
#   scale_color_manual(values = viridis_pal()(4)[1:2],
#                      labels = label_wrap(9)) + 
#   scale_x_continuous(limits = c(0, 300)) +
#   labs(x = 'Actual Yield (bushels/acre)', y = 'Predicted Yield (bushels/acre)', color = 'Method') + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 11),
#         axis.text.y = element_text(color = 'black', size = 11),
#         legend.text = element_text(color = 'black', size = 11),
#         text = element_text(color = 'black', size = 11),
#         legend.position = 'top',
#         panel.grid = element_blank())
# yieldPredictionsPlot
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
                                    phenotype=='kernelsPerRow' ~ 'Kernels Per Row*',
                                    phenotype=='hundredKernelMass' ~ 'Hundred Kernel Mass (g)')) %>%
  mutate(phenotypeLabel = factor(phenotypeLabel, 
                                 levels = c('Kernel Row Number', 'Plant Density (plants/acre)', 'Kernels Per Row*', 'Hundred Kernel Mass (g)'), 
                                 ordered = TRUE))

featureImportance <- ggplot(rfFeatures, aes(val, phenotypeLabel)) +
  geom_boxplot(color = 'black', fill = nitrogenColors[3]) +
  geom_text(aes(0.85, 'Hundred Kernel Mass (g)'), label = 'a', size = 3.88) +
  geom_text(aes(0.85, 'Kernels Per Row*'), label = 'b', size = 3.88) +
  geom_text(aes(0.85, 'Plant Density (plants/acre)'), label = 'b', size = 3.88) +
  geom_text(aes(0.85, 'Kernel Row Number'), label = 'c', size = 3.88) +
  scale_y_discrete(labels = str_wrap(levels(rfFeatures$phenotypeLabel), 9)) +
  labs(x = 'Feature Importance', y = '') +
  theme_minimal() + 
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
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
  scale_color_manual(values = viridis_pal()(4)[1:2],
                     labels = label_wrap(1)) + 
  scale_x_continuous(limits = c(0, 300)) +
  labs(x = 'Actual Yield (bushels/acre)', y = 'Predicted Yield (bushels/acre)', color = 'Method') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'top',
        panel.grid = element_blank())
yieldPredictionsPlot

meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c(direction = -1) +
    guides(color = guide_colourbar(barwidth = 6,
                                barheight = 1)) +
    labs(x = 'Hybrid Mean Yield (bushels/acre)', y = 'Yield Linear Plasticity', color = str_wrap('Mean Parent Release Year', 
                                                                                          width = 15)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(color = 'black', size = 11),
          axis.text.y = element_text(color = 'black', size = 11),
          legend.text = element_text(color = 'black', size = 11, margin = margin(0, 0, 0, 0)),
          text = element_text(color = 'black', size = 11),
          legend.position = 'top',
          panel.grid = element_blank())
# meanParentPlotLegend <- get_legend(meanParentPlot)
# meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
#   geom_point() +
#   geom_hline(yintercept=1) +
#   scale_color_viridis_c(direction = -1) +
#   labs(x = 'Mean Yield (bushels/acre)', y = 'Yield Linear Plasticity', color = str_wrap('Mean Parent Release Year', width = 1)) + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 11),
#         axis.text.y = element_text(color = 'black', size = 11),
#         legend.text = element_text(color = 'black', size = 11),
#         text = element_text(color = 'black', size = 11),
#         legend.position = 'none',
#         panel.grid = element_blank()) +
#   inset_element(meanParentPlotLegend, left = 0.125, bottom = 0.43, right = 0.225, top = 1, on_top = TRUE)
meanParentPlot

workflow <- rasterGrob(readPNG('../workflow.png'))

fig1left <- plot_grid(workflow, vp.plot, ncol = 1, labels = c('B', 'C'), rel_heights = c(0.4, 0.6))
fig1right <- plot_grid(yieldPredictionsPlot, featureImportance, ncol = 1, labels = c('D', 'E'))
fig1bottom <- plot_grid(fig1left, fig1right, nrow = 1, rel_widths = c(0.6, 0.4))
fig1 <- plot_grid(experimentalDesign, fig1bottom, ncol = 1, labels = c('A', '', ''), rel_heights = c(0.3, 0.7))
fig1
ggsave('../fig1HighRes.svg', plot = fig1, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

fig2left <- plot_grid(FWConceptualPlot, meanParentPlot, ncol = 1, labels = c('A', 'B'))
fig2right <- plot_grid(cxHeatmap, heatmap, ncol = 1, labels = c('C', 'D'))
fig2top <- plot_grid(fig2left, fig2right, nrow = 1, rel_widths = c(0.8, 1))
fig2 <- plot_grid(fig2top, gca_vp.plot, nrow = 2, labels = c('', 'E'), rel_heights = c(0.575, 0.425))
ggsave('../fig2HighRes.svg', plot = fig2, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

# Variance partitioning for yield from yield components
# vc_yield <- partitionVariance3(hybrids, 'yieldPerAcre.sp', 'Yield (bushels/acre)', '~ (1|plantDensity) + (1|kernelRowNumber) + (1|kernelsPerRow) + (1|hundredKernelMass)')

fig3top <- plot_grid(nPlasticityCorYield, nPlasticityCorHKM,labels = c('A', 'B'), 
                     nrow = 1, rel_widths = c(0.5, 0.5))
fig3top <- plot_grid(fig3top, nPlasticityCorLegend, ncol = 1, rel_heights = c(1, 0.075))
fig3middle <- plot_grid(nPlasticityAmesYield, nPlasticityAmesHKM, labels = c('C', 'D'), 
                        nrow = 1, rel_widths = c(0.5, 0.5))
fig3middle <- plot_grid(fig3middle, nPlasticityParentAgeLegend, ncol = 1, rel_heights = c(1, 0.15))

fig3 <- plot_grid(fig3top, fig3middle, nPlasticityGenotypeLinesPlot, nrow = 3, labels = c('', '', 'E'), rel_heights = c(0.55, 0.45, 0.45))
fig3
ggsave('../fig3HighRes.svg', plot = fig3, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')


# # Test spFW
# hybridsSummarized <- hybrids %>%
#   group_by(genotype, environment) %>%
#   summarise(yieldPerAcre.sp = mean(yieldPerAcre.sp, na.rm = TRUE))
# spFWModelSummarized <- HFWM_est(Y = hybridsSummarized$yieldPerAcre.sp, 
#                   VAR = hybridsSummarized$genotype,
#                   ENV = hybridsSummarized$environment,
#                   kin_info = FALSE,
#                   env_info = FALSE)
# 
# spFWModelBalanced <- HFWM_est(Y= hybridsCommon$yieldPerAcre.sp,
#                               VAR = hybridsCommon$genotype,
#                               ENV = hybridsCommon$environment, 
#                               kin_info = FALSE,
#                               env_info = FALSE)
# 
# # Testing nlsr
# model <- nls(yieldPerAcre.sp ~ genotype + environment + genotype*environment, data = hybrids, na.action = na.omit, start = c(genotype=1, environment=1))
block1 <- hybridsNOLNK22 %>% 
  filter((block %% 2)==0)
block1.pl <- estimatePlasticity3(block1, 'yieldPerAcre.sp', 'environment', 'genotype')
block2 <- hybridsNOLNK22 %>% 
  filter((block %% 2)!=0)
block2.pl <- estimatePlasticity3(block2, 'yieldPerAcre.sp', 'environment', 'genotype')
block.pl <- full_join(block1.pl, block2.pl, join_by(genotype), keep = FALSE, suffix = c('.1', '.2'))

blockRho <- cor(block.pl$yieldPerAcre.sp.b.1, block.pl$yieldPerAcre.sp.b.2, use = 'complete.obs', method = 'spearman')

blockCorr <- ggplot(block.pl, aes(yieldPerAcre.sp.b.1, yieldPerAcre.sp.b.2)) + 
  geom_point(color = viridis_pal()(4)[1]) +
  labs(x = 'Linear Plasticity - Block 1', y = 'Linear Plasticity - Block 2', title = paste0('Spearman Rank Correlation: ', round(blockRho, 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        plot.title = element_text(color = 'black', size = 11, hjust = 0.5),
        text = element_text(color = 'black', size = 11),
        panel.grid = element_blank())
blockCorr
ggsave('../figSBlockCorr.png', plot = blockCorr, width = 6.5, height = 6.5, units = 'in', dpi = 1000, bg = 'white')

nResponseBlockWide <- nResponseBlock.pl %>%
  pivot_wider(id_cols = c(genotype, locationYear), names_from = blockSet, values_from = yieldPerAcre.sp.b, names_prefix = 'b')

nResponseBlockRho <- cor(nResponseBlockWide$b1, nResponseBlockWide$b2, use = 'complete.obs', method = 'spearman')

nResponseCorr <- ggplot(nResponseBlockWide, aes(b1, b2)) +
  geom_point(color = viridis_pal()(4)[1]) + 
  labs(x = 'Nitrogen Plasticity - Block 1', y = 'Nitrogen Plasticity - Block 2', title = paste0('Spearman Rank Correlation: ', round(nResponseBlockRho, 4))) + 
  scale_x_continuous(limits = c(-1.25, 2)) +
  scale_y_continuous(limits = c(-1.25, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        plot.title = element_text(color = 'black', size = 11, hjust = 0.5),
        text = element_text(color = 'black', size = 11),
        panel.grid = element_blank())
nResponseCorr

ggsave('../figSNResponseBlockCorr.png', plot = nResponseCorr, width = 6.5, height = 6.5, units = 'in', dpi = 1000, bg = 'white')

meanParentPlotShelledCobWidth <- ggplot(hybridsNOLNK22.pl, aes(shelledCobWidth.sp.mu, shelledCobWidth.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = 'Hybrid Mean Shelled Cob Width (cm)', y = 'Shelled Cob Width Linear Plasticity', color = 'Mean Parent Release Year') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        legend.text = element_text(color = 'black', size = 11, margin = margin(0, 0, 0, 0)),
        text = element_text(color = 'black', size = 11),
        legend.position = 'top',
        panel.grid = element_blank())
meanParentPlotShelledCobWidth

meanParentPlotLegend <- get_legend(meanParentPlotShelledCobWidth)

meanParentPlotShelledCobWidth <- ggplot(hybridsNOLNK22.pl, aes(shelledCobWidth.sp.mu, shelledCobWidth.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Shelled Cob Width (cm)', 19), y = str_wrap('Shelled Cob Width Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                             width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotShelledCobWidth

meanParentPlotKRN <- ggplot(hybridsNOLNK22.pl, aes(kernelRowNumber.sp.mu, kernelRowNumber.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Kernel Row Number', 19), y = str_wrap('Kernel Row Number Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                               width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotKRN

meanParentPlotEarLength <- ggplot(hybridsNOLNK22.pl, aes(earLength.sp.mu, earLength.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Ear Length (cm)', 19), y = str_wrap('Ear Length Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotEarLength

meanParentPlotEarFillLength <- ggplot(hybridsNOLNK22.pl, aes(earFillLength.sp.mu, earFillLength.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Ear Fill Length* (cm)', 19), y = str_wrap('Ear Fill Length Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotEarFillLength

meanParentPlotKPR <- ggplot(hybridsNOLNK22.pl, aes(kernelsPerRow.sp.mu, kernelsPerRow.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Kernels Per Row*', 19), y = str_wrap('Kernels Per Row Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotKPR

meanParentPlotKPE <- ggplot(hybridsNOLNK22.pl, aes(kernelsPerEar.sp.mu, kernelsPerEar.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Kernels Per Ear', 19), y = str_wrap('Kernels Per Ear Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotKPE

meanParentPlotEarWidth <- ggplot(hybridsNOLNK22.pl, aes(earWidth.sp.mu, earWidth.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Ear Width (cm)', 19), y = str_wrap('Ear Width Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotEarWidth

meanParentPlotHKM <- ggplot(hybridsNOLNK22.pl, aes(hundredKernelMass.sp.mu, hundredKernelMass.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Hundred Kernel Mass (g)', 19), y = str_wrap('Hundred Kernel Mass Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotHKM

meanParentPlotKernelMass <- ggplot(hybridsNOLNK22.pl, aes(kernelMassPerEar.sp.mu, kernelMassPerEar.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Kernel Mass Per Ear (g)', 19), y = str_wrap('Kernel Mass Per Ear Linear Plasticity', 24), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotKernelMass

supplMeanParentPlots <- plot_grid(meanParentPlotShelledCobWidth, meanParentPlotKRN, meanParentPlotEarLength, 
                                  meanParentPlotEarFillLength, meanParentPlotKPR, meanParentPlotKPE,
                                  meanParentPlotEarWidth, meanParentPlotHKM, meanParentPlotKernelMass,
                                  nrow = 3, ncol = 3, labels = 'AUTO')
supplMeanParentPlots <- plot_grid(supplMeanParentPlots, meanParentPlotLegend, ncol = 1, rel_heights = c(1.5, 0.1))
ggsave('../figSMeanParentPlots.svg', plot = supplMeanParentPlots, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

# Model relationships: are they significant?
meanPerformanceByPlasticityYield <- lm(yieldPerAcre.sp.mu ~ yieldPerAcre.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityYield)

meanPerformanceByPlasticityShelledCobWidth <- lm(shelledCobWidth.sp.mu ~ shelledCobWidth.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityShelledCobWidth)

meanPerformanceByPlasticityKRN <- lm(kernelRowNumber.sp.mu ~ kernelRowNumber.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKRN)

meanPerformanceByPlasticityEarLength <- lm(earLength.sp.mu ~ earLength.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarLength)

meanPerformanceByPlasticityEarFillLength <- lm(earFillLength.sp.mu ~ earFillLength.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarFillLength)

meanPerformanceByPlasticityKPR <- lm(kernelsPerRow.sp.mu ~ kernelsPerRow.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKPR)

meanPerformanceByPlasticityKPE <- lm(kernelsPerEar.sp.mu ~ kernelsPerEar.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKPE)

meanPerformanceByPlasticityEarWidth <- lm(earWidth.sp.mu ~ earWidth.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarWidth)

meanPerformanceByPlasticityHKM <- lm(hundredKernelMass.sp.mu ~ hundredKernelMass.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityHKM)

meanPerformanceByPlasticityKernelMass <- lm(kernelMassPerEar.sp.mu ~ kernelMassPerEar.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKernelMass)

# How far apart in rank are the hybrids predicted to crossover by FW?
FWRankChangeRankDistance <- cxData %>%
  filter(crossover=='Y') %>%
  mutate(genotypeRank2 = as.numeric(genotypeRank2)) %>%
  rowwise() %>%
  mutate(rankDistance = abs(genotypeRank1 - genotypeRank2))

FWRankChangeRankDistanceHist <- FWRankChangeRankDistance %>%
  ggplot(aes(rankDistance)) +
    geom_histogram(binwidth = 1, fill = viridis_pal()(4)[1]) +
    labs(x = 'Hybrid Rank Difference', y = 'Predicted Rank Order Changes') + 
    theme_minimal() +
    theme(axis.text.x = element_text(color = 'black', size = 11),
        axis.text.y = element_text(color = 'black', size = 11),
        text = element_text(color = 'black', size = 11),
        legend.position = 'none',
        panel.grid = element_blank())
FWRankChangeRankDistanceHist
ggsave('../RankChangeDistanceHistogram.png', FWRankChangeRankDistanceHist, width = 6.5, height = 4, dpi = 1000, bg = 'white')

# How often does FW predict crossovers?
FWRRankChanges <- abs(cxMatrix) %>%
  as.data.frame() %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)))
numRankChanges <- rowSums(FWRRankChanges)/2
numHybrids <- length(hybridsNOLNK22.pl$genotype)
totalUniquePairs <- (numHybrids * (numHybrids - 1))/2
percentRankChanges <- numRankChanges/totalUniquePairs
percentRankChanges

# Model relationships with N plasticity in  Ames 2023: are they significant?
meanPerformanceByNPlasticityYield <- lm(yieldPerAcre.sp.mu ~ yieldPerAcre.sp.b + meanParentAge, data = ames23NResponse.pl)
anova(meanPerformanceByNPlasticityYield)

meanPerformanceByNPlasticityHKM <- lm(hundredKernelMass.sp.mu ~ hundredKernelMass.sp.b + meanParentAge, data = ames23NResponse.pl)
anova(meanPerformanceByNPlasticityHKM)

# Look at distribution of residuals for yield prediction models
yieldPredictions <- yieldPredictions %>%
  rowwise() %>%
  mutate(residualsRF = predictedYieldRF - yieldPerAcre,
         residualsExtension = predictedYieldExtension - yieldPerAcre)

extensionResidualsHistogram <- ggplot(yieldPredictions, aes(residualsExtension)) +
  geom_histogram() +
  labs(title = 'Extension Residuals')
extensionResidualsHistogram

rfResidualsHistogram <- ggplot(yieldPredictions, aes(residualsRF)) + 
  geom_histogram() + 
  labs(title = 'RF Residuals')
rfResidualsHistogram

# Are feature importances significantly different from each other?
anova <- aov(val ~ phenotype, data = rfFeatures)
tukey <- TukeyHSD(anova)

# interaction importance supplemental figs
interactionImportanceShelledCobWidth <- plotInteractionImportanceGrid(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width', 
                                                                      legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKRN <- plotInteractionImportanceGrid(trait = 'kernelRowNumber', traitLabel = 'Kernel Row Number', 
                                                          legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarLength <- plotInteractionImportanceGrid(trait = 'earLength', traitLabel = 'Ear Length', 
                                                                legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarFillLength <- plotInteractionImportanceGrid(trait = 'earFillLength', traitLabel = 'Ear Fill Length*', 
                                                                    legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKPR <- plotInteractionImportanceGrid(trait = 'kernelsPerRow', traitLabel = 'Kernels Per Row*', 
                                                          legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKPE <- plotInteractionImportanceGrid(trait = 'kernelsPerEar', traitLabel = 'Kernels Per Ear', 
                                                          legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarWidth <- plotInteractionImportanceGrid(trait = 'earWidth', traitLabel = 'Ear Width', 
                                                               legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceHKM <- plotInteractionImportanceGrid(trait = 'hundredKernelMass', traitLabel = 'Hundred Kernel Mass', 
                                                          legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKernelMass <- plotInteractionImportanceGrid(trait = 'kernelMassPerEar', traitLabel = 'Kernel Mass Per Ear', 
                                                                 legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceCombineTestWeight <- plotInteractionImportanceGrid(trait = 'combineTestWeight', traitLabel = 'Test Weight',
                                                                        legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceCombineMoisture <- plotInteractionImportanceGrid(trait = 'combineMoisture', traitLabel = 'Harvest Moisture',
                                                                      legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceFlagLeafHeight <- plotInteractionImportanceGrid(trait = 'flagLeafHeight', traitLabel = 'Flag Leaf Height',
                                                                      legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarHeight <- plotInteractionImportanceGrid(trait = 'earHeight', traitLabel = 'Ear Height',
                                                                     legendPosition = 'bottom', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)

supplInteractionImportancePlots <- plot_grid(interactionImportanceCombineTestWeight, interactionImportanceFlagLeafHeight, interactionImportanceShelledCobWidth, 
                                             interactionImportanceKRN, interactionImportanceEarLength,interactionImportanceKPE,
                                             interactionImportanceEarWidth, interactionImportanceHKM, interactionImportanceKernelMass,
                                             nrow = 3, ncol = 3, labels = 'AUTO')
ggsave('../figSInteractionImportancePlots.png', plot = supplInteractionImportancePlots, width = 8.125, height = 11.25, units = 'in', dpi = 1000, bg = 'white')

# N plasticity cor suppl figs
nPlasticityCorShelledCobWidth <- plotNPlasticityCor(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width',
                                                    legendPosition = 'bottom')
supplNPlasticityCorLegend <- get_legend(nPlasticityCorShelledCobWidth)
nPlasticityCorShelledCobWidth <- plotNPlasticityCor(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width',
                                                    legendPosition = 'none')
nPlasticityCorKRN <- plotNPlasticityCor(trait = 'kernelRowNumber', traitLabel = 'Kernel Row Number',
                                        legendPosition = 'none')
nPlasticityCorEarLength <- plotNPlasticityCor(trait = 'earLength', traitLabel = 'Ear Length',
                                              legendPosition = 'none')
# nPlasticityCorEarFillLength <- plotNPlasticityCor(trait = 'earFillLength', traitLabel = 'Ear Fill Length')
# nPlasticityCorKPR <- plotNPlasticityCor(trait = 'kernelsPerRow', traitLabel = 'Kernels Per Row')
nPlasticityCorKPE <- plotNPlasticityCor(trait = 'kernelsPerEar', traitLabel = 'Kernels Per Ear',
                                        legendPosition = 'none')
nPlasticityCorEarWidth <- plotNPlasticityCor(trait = 'earWidth', traitLabel = 'Ear Width',
                                             legendPosition = 'none')
nPlasticityCorKernelMass <- plotNPlasticityCor(trait = 'kernelMassPerEar', traitLabel = 'Kernel Mass Per Ear',
                                               legendPosition = 'none')

supplNPlasticityCorPlots <- plot_grid(nPlasticityCorShelledCobWidth, nPlasticityCorKRN, 
                                      nPlasticityCorEarLength, nPlasticityCorKPE,
                                      nPlasticityCorEarWidth, nPlasticityCorKernelMass,
                                      nrow = 3, ncol = 2, labels = 'AUTO')
supplNPlasticityCorPlots <- plot_grid(supplNPlasticityCorPlots, supplNPlasticityCorLegend, ncol = 1, rel_heights = c(1.2, 0.05))
ggsave('../figSNPlasticityCorPlots.png', plot = supplNPlasticityCorPlots, width = 5.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

nassYield <- read.csv('../NASSCornYield_2022_2023_IA_NE.csv') %>%
  rename(yield = Value,
         cv = `CV....`) %>%
  rowwise() %>% 
  mutate(sem = 0.01*cv*yield) %>%
  mutate(lower = yield - 1.96*sem,
         upper = yield + 1.96*sem)
min(nassYield$lower)
