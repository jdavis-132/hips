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
library(viridis)
source('src/Functions.R')
library(spFW)


# hybrids <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.3.csv') %>%
#   filter(location!='') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'High', 'Medium'))) %>%
#   rowwise() %>%
#   # Since we are making an environment variable based on year, location, irrigationProvided, and nitrogenTreatment,
#   # let's have an option to keep 2022 NP as one location
#   mutate(semanticLocation = case_when(location %in% c('North Platte1', 'North Platte2', 'North Platte3') ~ 'North Platte', .default = location),
#          environment = str_c(year, semanticLocation, irrigationProvided, nitrogenTreatment, sep = ':'),
#          genotype = case_when(genotype=="HOEGEMEYER 7089 AMXT" ~ 'COMMERCIAL HYBRID 1',
#                               genotype=="HOEGEMEYER 8065RR" ~ 'COMMERCIAL HYBRID 2', 
#                               genotype=="PIONEER 1311 AMXT" ~ 'COMMERCIAL HYBRID 3',
#                               genotype=="PIONEER P0589 AMXT" ~ 'COMMERCIAL HYBRID 4',
#                               genotype=="SYNGENTA NK0659-3120-EZ1" ~ 'COMMERCIAL HYBRID 5', 
#                               genotype=="SYNGENTA NK0760-3111" ~ 'COMMERCIAL HYBRID 6',
#                               genotype=="WYFFELS W1782" ~ 'COMMERCIAL HYBRID 7', 
#                               .default = genotype))
nitrogenColors <- pal_brewer('seq', palette = 'YlOrRd')(3)
# show_col(nitrogenColors)
irrigationColors <- pal_brewer('seq', palette = 'Greys')(3)
# show_col(irrigationColors)
# Now we need to spatially correct within an environment
phenotypes <- c(#"plantDensity", 
                "combineTestWeight", "combineMoisture", "flagLeafHeight", "earHeight", "yieldPerAcre", 
                'GDDToAnthesis', 'GDDToSilk', 'anthesisSilkingIntervalGDD', 'kernelRowNumber', 'earWidth',
                'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelMassPerEar', 'kernelsPerEar', 'hundredKernelMass',
                'earFillLength', 'kernelsPerRow')
phenotypeLabels <- c(#'Plant Density (plants/acre)', 
                     'Test Weight (lbs/bushel)', 'Harvest Moisture (%)', 'Flag Leaf Height (cm)',
                     'Ear Height (cm)', 'Yield (bushels/acre)', 'GDD to Anthesis*', 'GDD To Silk*', 
                     'Anthesis Silking Interval* (GDD)', 'Kernel Row Number', 'Ear Width (cm)', 'Ear Length (cm)',
                     'Shelled Cob Width (cm)', 'Shelled Cob Mass (cm)', 'Kernel Mass Per Ear (g)', 'Kernels Per Ear', 
                     'Hundred Kernel Mass (g)', 'Ear Fill Length* (cm)', 'Kernels Per Row*')
phenotypeLabelsVP <-  c(#'Plant Density (1)', 
                        'Test Weight', 'Harvest Moisture', 'Flag Leaf Height',
                        'Ear Height', 'Yield', 'GDD to Anthesis*', 'GDD to Silk*', 
                        'Anthesis Silking Interval*', 'Kernel Row Number', 'Ear Width', 'Ear Length',
                        'Shelled Cob Width', 'Shelled Cob Mass', 'Kernel Mass Per Ear', 
                        'Kernels Per Ear', 
                        'Hundred Kernel Mass', 'Ear Fill Length*', 'Kernels Per Row*')


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
# # 
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
# # Export spatial corrections so we don't have to run it again
# write.csv(hybrids, 'analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv', row.names = FALSE, quote = FALSE)

hybrids <- read.csv('analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv') %>%
  filter(location!='') %>% 
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
  rowwise() %>%
  mutate(genotype = case_when(genotype=="HOEGEMEYER 7089 AMXT" ~ 'COMMERCIAL HYBRID 1',
                              genotype=="HOEGEMEYER 8065RR" ~ 'COMMERCIAL HYBRID 2', 
                              genotype=="PIONEER 1311 AMXT" ~ 'COMMERCIAL HYBRID 3',
                              genotype=="PIONEER P0589 AMXT" ~ 'COMMERCIAL HYBRID 4',
                              genotype=="SYNGENTA NK0659-3120-EZ1" ~ 'COMMERCIAL HYBRID 5', 
                              genotype=="SYNGENTA NK0760-3111" ~ 'COMMERCIAL HYBRID 6',
                              genotype=="WYFFELS W1782" ~ 'COMMERCIAL HYBRID 7', 
                              .default = genotype)) %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))


envsPerHybrid <- tibble(hybrid = unique(hybrids$genotype), numEnvs = NULL)
for(i in 1:length(unique(envsPerHybrid$hybrid)))
{
  hybridData <- filter(hybrids, genotype==envsPerHybrid$hybrid[i])
  envsPerHybrid$numEnvs[i] <- length(unique(hybridData$environment))
}

singleEnvHybrids <- envsPerHybrid$hybrid[envsPerHybrid$numEnvs<4]
hybrids <- filter(hybrids, !(genotype %in% singleEnvHybrids)) %>%
  rowwise() %>%
  mutate(locationYear = str_c(year, semanticLocation, sep = ' ')) %>%
  mutate(locationYear = case_when(location=='North Platte1' ~ str_c(locationYear, ' FI'),
                                  location=='North Platte2' ~ str_c(locationYear, ' LI'),
                                  location=='North Platte3' ~ str_c(locationYear, ' NI'),
                                  locationYear=='2023 North Platte' & irrigationProvided==0 ~ str_c(locationYear, ' NI'),
                                  locationYear=='2023 North Platte' & irrigationProvided > 0 ~ str_c(locationYear, ' LI'),
                                  .default = locationYear)) %>%
  mutate(environment = str_c(locationYear, nitrogenTreatment, sep = ' '))
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

# # Compare broad-sense heritabilities with across/within env, locationYear spearman rhos
# rhoVsH2 <- tibble(trait = phenotypes, H2 = NULL, minAcrossEnvRho = NULL, maxAcrossEnvRho = NULL, 
#                   minAcrossLocationYearRho = NULL, maxAcrossLocationYearRho = NULL, minEnvRho = NULL, maxEnvRho = NULL, 
#                   minLocationYearRho = NULL, maxLocationYearRho = NULL, envRhoAgg = NULL, locationYearRhoAgg = NULL)
# # getCorrelations <- function(pheno) {
# for(pheno in phenotypes){
#   trait <- paste0(pheno, '.sp')
#   trait1 <- paste0(trait, '.1')
#   trait2 <- paste0(trait, '.2')
#   
#   envLevelPerformance <- hybrids %>%
#     group_by(genotype, environment) %>%
#     summarise('{trait}' := mean(.data[[trait]], na.rm = TRUE))
#   
#   locationYearLevelPerformance <- hybrids %>%
#     group_by(genotype, locationYear) %>%
#     summarise('{trait}' := mean(.data[[trait]], na.rm = TRUE))
#   
#   acrossEnvRhoData <- envLevelPerformance %>%
#     filter(!is.na(.data[[trait]])) %>%
#     pivot_wider(id_cols = genotype, names_from = environment, values_from = .data[[trait]]) %>%
#     ungroup() %>%
#     select(!genotype) %>%
#     cor(use = 'na.or.complete', method = 'spearman') %>%
#     as_tibble(rownames = 'environment1') %>%
#     mutate(across(where(is.numeric), ~case_when(.==1 ~ NA, .default = .))) %>%
#     pivot_longer(where(is.numeric), names_to = 'environment2', values_to = 'cor')%>%
#     mutate(across(where(is.numeric), ~case_when(.==-Inf | .==Inf ~ NA, .default = .)))
#   
#   acrossLocationYearRhoData <- locationYearLevelPerformance %>%
#     filter(!is.na(.data[[trait]])) %>%
#     pivot_wider(id_cols = genotype, names_from = locationYear, values_from = .data[[trait]]) %>%
#     ungroup() %>%
#     select(!genotype) %>%
#     cor(use = 'na.or.complete', method = 'spearman') %>%
#     as_tibble(rownames = 'locationYear1') %>%
#     mutate(across(where(is.numeric), ~case_when(.==1 ~ NA, .default = .))) %>%
#     pivot_longer(where(is.numeric), names_to = 'locationYear2', values_to = 'cor') %>%
#     mutate(across(where(is.numeric), ~case_when(.==-Inf | .==Inf ~ NA, .default = .)))
#   
#   envRhoData1 <- hybrids %>%
#     filter(block %% 2 != 0) %>% 
#     select(genotype, environment, all_of(trait), nitrogenTreatment)
#   envRhoData2 <- hybrids %>%
#     filter(block %% 2 == 0) %>% 
#     select(genotype, environment, all_of(trait), nitrogenTreatment)
#   envRhoData <- full_join(envRhoData1, envRhoData2, join_by(genotype, environment, nitrogenTreatment), suffix = c('.1', '.2'), keep = FALSE,
#                           relationship = 'many-to-many')
#   envRho <- envRhoData %>%  
#     filter(nitrogenTreatment %in% c('High')) %>%
#     group_by(environment) %>%
#     summarise(cor = cor(.data[[trait1]], .data[[trait2]], use = 'na.or.complete', method = 'spearman'))
#   
#   locationYearRhoData1 <- hybrids %>%
#     filter(block %% 2 != 0) %>%
#     select(genotype, locationYear, all_of(trait))
#   locationYearRhoData2 <- hybrids %>%
#     filter(block %% 2 == 0) %>%
#     select(genotype, locationYear, all_of(trait))
#   locationYearRhoData <- full_join(locationYearRhoData1, locationYearRhoData2, join_by(genotype, locationYear), suffix = c('.1', '.2'), keep = FALSE, 
#                                    relationship = 'many-to-many')
#   locationYearRho <- locationYearRhoData %>%
#     group_by(locationYear) %>%
#     summarise(cor = cor(.data[[trait1]], .data[[trait2]], use = 'na.or.complete', method = 'spearman'))
#   
#   rhoVsH2$H2[rhoVsH2$trait==pheno] <- vc_all$pctVar[vc_all$responseVar==trait]/100
#   rhoVsH2$minAcrossEnvRho[rhoVsH2$trait==pheno] <- min(acrossEnvRhoData$cor, na.rm = TRUE)
#   rhoVsH2$maxAcrossEnvRho[rhoVsH2$trait==pheno] <- max(acrossEnvRhoData$cor, na.rm = TRUE)
#   rhoVsH2$minAcrossLocationYearRho[rhoVsH2$trait==pheno] <- min(acrossLocationYearRhoData$cor, na.rm = TRUE)
#   rhoVsH2$maxcrossLocationYearRho[rhoVsH2$trait==pheno] <- max(acrossLocationYearRhoData$cor, na.rm = TRUE)
#   rhoVsH2$minEnvRho[rhoVsH2$trait==pheno] <- min(envRho$cor, na.rm = TRUE)
#   rhoVsH2$maxEnvRho[rhoVsH2$trait==pheno] <- max(envRho$cor, na.rm = TRUE)
#   rhoVsH2$minLocationYearRho[rhoVsH2$trait==pheno] <- min(locationYearRho$cor, na.rm = TRUE)
#   rhoVsH2$maxLocationYearRho[rhoVsH2$trait==pheno] <- min(locationYearRho$cor, na.rm = TRUE)
#   rhoVsH2$envRhoAgg[rhoVsH2$trait==pheno] <- cor(envRhoData[[trait1]], envRhoData[[trait2]], use = 'na.or.complete', method = 'spearman')
#   rhoVsH2$locationYearRhoAgg[rhoVsH2$trait==pheno] <- cor(locationYearRhoData[[trait1]], locationYearRhoData[[trait2]], use = 'na.or.complete', method = 'spearman')
# # }
# }

# for(i in phenotypes)
# {
#   getCorrelations(i)
# }
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
  labs(x = NULL, y = 'Variance', fill = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, color = 'black', angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(size = 9, color = 'black'),
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
#   theme(axis.text.x = element_text(size = 9, color = 'black'),
#         axis.text.y = element_text(size = 9, color = 'black'),
#         legend.text = element_text(size = 9, color = 'black'),
#         text = element_text(size = 9, color = 'black'),
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
                         .default = age)) %>% 
  select(genotype, age)

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
# envAnova <- aov(yieldPerAcre.sp ~ environment, data = hybrids)
# envTukey <- TukeyHSD(envAnova)$environment

orderedLocationYears <- hybrids %>%
  group_by(locationYear) %>%
  summarise(yieldMedian = median(yieldPerAcre.sp, na.rm = TRUE)) %>%
  arrange(desc(yieldMedian))

environmentMedians <- hybrids %>%
  group_by(locationYear, nitrogenTreatment) %>%
  summarise(yieldMedian = median(yieldPerAcre.sp, na.rm = TRUE))

orderedBoxplots <- hybrids %>%
  mutate(locationYear = factor(locationYear, levels = orderedLocationYears$locationYear)) %>%
  ggplot(aes(locationYear, yieldPerAcre.sp)) +
    geom_boxplot(color = 'black') +
    geom_point(aes(locationYear, yieldMedian, color = nitrogenTreatment), data = environmentMedians, size = 2) +
    geom_point(aes(locationYear, yieldMedian), data = environmentMedians, shape = 1, size = 2, color = "black") +
    scale_x_discrete(labels = label_wrap(10)) +
    scale_color_manual(values = nitrogenColors) + 
    labs(x = NULL, y = 'Yield (bushels/acre)') + 
    theme_minimal() + 
    theme(axis.text.x = element_text(color = 'black', size = 9, angle = 90, vjust = 0.5, hjust = 1, margin = margin(0, 0, 0, 0)),
          axis.text.y = element_text(color = 'black', size = 9),
          text = element_text(color = 'black', size = 9, vjust = 0.5),
          panel.grid = element_blank(),
          legend.position = 'none')
orderedBoxplots
# orderedViolins <- hybrids %>%
#   rowwise() %>%
#   mutate(environment = factor(environment, levels = orderedEnvironments$environment),
#          irrigationLevel = case_when(irrigationProvided==0 ~ '0.0',
#                                      irrigationProvided > 3.93 & irrigationProvided < 7.87 ~ '100-200',
#                                      irrigationProvided > 7.87 ~ '>200'),
#          nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
#                                        nitrogenTreatment=='Medium' ~ '150-175',
#                                        nitrogenTreatment=='High' ~ '225-250')) %>%
#   mutate(irrigationLevel = factor(irrigationLevel, levels = c('0.0', '100-200', '>200')),
#          nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250'))) %>%
#   ggplot(aes(yieldPerAcre.sp, environment, fill = nitrogenTreatment, color = irrigationLevel)) +
#     geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#     geom_text(aes(275,'2023:Ames:0:High'), label = 'a', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:8.6:High'), label = 'ab', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:8.6:Medium'), label = 'ab', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:4.3:High'), label = 'bc', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:4.3:Medium'), label = 'cd', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:8.6:Low'), label = 'cde', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Crawfordsville:0:Medium'), label = 'def', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Crawfordsville:0:High'), label = 'efg', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Ames:0:Medium'), label = 'fg', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Crawfordsville:0:High'), label = 'g', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Scottsbluff:16.9:High'), label = 'g', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Missouri Valley:0:Medium'), label = 'g', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Crawfordsville:0:Low'), label = 'gh', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:4.3:Low'), label = 'hi', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Missouri Valley:0:Medium'), label = 'hi', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Scottsbluff:16.9:Medium'), label = 'ij', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Ames:0:Low'), label = 'ijk', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Ames:0:Low'), label = 'ijk', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Crawfordsville:0:Low'), label = 'jk', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Crawfordsville:0:Medium'), label = 'k', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Ames:0:High'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Ames:0:Medium'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Scottsbluff:16.9:Low'), label = 'l', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:0:Low'), label = 'm', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:North Platte:4.5:Medium'), label = 'm', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Lincoln:0:High'), label = 'm', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:0:High'), label = 'mn', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Lincoln:0:Low'), label = 'mn', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:Lincoln:0:Medium'), label = 'no', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2023:North Platte:0:Medium'), label = 'o', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:North Platte:0:Medium'), label = 'o', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Lincoln:0:Medium'), label = 'p', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Lincoln:0:Low'), label = 'q', size = 3.88, color = 'black', hjust = 1) +
#     geom_text(aes(275,'2022:Lincoln:0:High'), label = 'r', size = 3.88, color = 'black', hjust = 1) +
#     scale_fill_manual(values = nitrogenColors) + 
#     scale_color_manual(values = irrigationColors) +
#     labs(x = 'Yield (bushels/acre)', y = 'Environment', fill = 'Nitrogen Fertilizer (lbs/acre)', color = 'Irrigation Provided') + 
#     theme_minimal() + 
#     theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         panel.grid = element_blank(),
#         legend.position = 'right')
# orderedViolins
# ggsave('../orderedViolins.png', plot = orderedViolins, width = 6.5, height = 9, units = 'in', dpi=1000, bg = 'white')

yieldBLUPS <- lmer(yieldPerAcre.sp ~ environment + (1|genotype), data = hybrids) 
yieldBLUPS <- ranef(yieldBLUPS)
yieldBLUPS <- as_tibble(yieldBLUPS$genotype, rownames = 'genotype') %>%
  rename(blup = `(Intercept)`) %>%
  mutate(rankOrder = row_number(blup))
top10Percent <- round(max(yieldBLUPS$rankOrder) * 0.9)
top10PercentOverallGenotypes <- yieldBLUPS$genotype[yieldBLUPS$rankOrder %in% top10Percent:max(yieldBLUPS$rankOrder)]
lower10PercentOverallGenotypes <- yieldBLUPS$genotype[yieldBLUPS$rankOrder %in% 1:round(max(yieldBLUPS$rankOrder)*0.1)]
top10PercentOverallPerformance <- hybrids %>%
  rowwise() %>%
  mutate(top10Percent = (genotype %in% top10PercentOverallGenotypes), 
         environment = factor(environment, levels = orderedEnvironments$environment))

top10PercentOverallAnova <- aov(yieldPerAcre.sp ~ factor(top10Percent)*environment, data = top10PercentOverallPerformance)
top10PercentOverallTukey <- TukeyHSD(top10PercentOverallAnova)
sigOverallGroupEnvs <- top10PercentOverallTukey[["factor(top10Percent):environment"]] %>%
  as_tibble(rownames = 'comp') %>%
  filter(`p adj` < 0.05) %>%
  rowwise() %>%
  mutate(grp1 = str_split_i(comp, '-', 1),
         grp2 = str_split_i(comp, '-', 2)) %>%
  mutate(overallRank1 = str_split_i(grp1, ':', 1),
         overallRank2 = str_split_i(grp2, ':', 1),
         env1 = str_split_fixed(grp1, ':', 2)[2],
         env2 = str_split_fixed(grp2, ':', 2)[2]) %>%
  filter(env1==env2)

sigAnnotations10PercentOverall <- tibble(environment = sigOverallGroupEnvs$env1)
                                           
top10PercentOverallPerformancePlot <- ggplot(top10PercentOverallPerformance, aes(top10Percent, yieldPerAcre.sp, fill = top10Percent)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'white') +
  geom_text(data = sigAnnotations10PercentOverall, mapping = aes(y = 0, x = FALSE, fill = FALSE), label = '*') +
  facet_wrap(~factor(environment, levels = orderedEnvironments$environment), strip.position = 'left', ncol = 9) + 
  scale_x_discrete(labels = c('', '')) +
  scale_fill_manual(values = viridis_pal()(4)[1:2], 
                    labels = c('Lower 90%', 'Upper 10%')) + 
  labs(x = '', y = 'Yield (bushels/acre)', fill = 'Overall Hybrid Rank') +
  theme_minimal() + 
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        strip.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        panel.grid = element_blank(),
        legend.position = 'top')
top10PercentOverallPerformancePlot
# ggsave('../top10PercentOverallPerformance.svg', plot = top10PercentOverallPerformancePlot,
       # width = 6.5, height = 9, units = 'in', dpi=1000, bg = 'white')

hybrids <- hybrids %>%
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

# # How important is mean parent age to explaining yield in each environment
# vc_meanParentAge <- tibble(grp = NULL, responseVar = NULL, vcov = NULL, pctVar = NULL, environment = NULL)
# for(env in orderedEnvironments$environment)
# {
#   envData <- filter(hybrids, environment==env)
#   plot <- ggplot(envData, aes(meanParentAge, yieldPerAcre.sp)) + 
#     geom_point() + 
#     labs(title = env)
#   print(plot)
#   print(env)
#   print(cor(envData$yieldPerAcre.sp, envData$meanParentAge, use = 'complete.obs', method = 'spearman'))
#   vc_env <- partitionVariance3(envData, 'yieldPerAcre.sp', 'Yield', '~ (1|earParentAge) + (1|pollenParentAge) + (1|earParentAge:pollenParentAge)') %>%
#     mutate(environment = env)
#   vc_meanParentAge <- bind_rows(vc_meanParentAge, vc_env)
# }
# 
# vc_meanParentAge <- mutate(vc_meanParentAge, 
#                            environment = factor(environment, levels = orderedEnvironments$environment))
# 
# meanParentAge_vp.plot <- ggplot(vc_meanParentAge, aes(environment, pctVar, fill = grp)) +
#   geom_col(position = 'stack') + 
#   scale_fill_viridis(discrete = TRUE, labels = label_wrap(11)) +
#   scale_y_continuous(name = 'Variance', 
#                      breaks = c(0, 25, 50, 75, 100), 
#                      labels = c('0%', '25%', '50%', '75%', '100%')) +
#   labs(x = 'Environment', y = 'Variance', fill = '') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(size = 9, color = 'black', angle = 90, hjust = 1, vjust = 0.5),
#         axis.text.y = element_text(size = 9, color = 'black'),
#         legend.text = element_text(size = 9, color = 'black'),
#         text = element_text(size = 9, color = 'black'),
#         legend.position = 'bottom',
#         line = element_line(color = 'black', linewidth = 1),
#         panel.grid = element_blank())
# meanParentAge_vp.plot
# Okay, let's run the plasticity without LNK 2022
# Estimate FW plasticity across all environments where phenotype was observed
hybridsNOLNK22 <- filter(hybrids, !(location=='Lincoln' & year=='2022'))
# hybridsNOLNK22.pl <- estimatePlasticity3(hybridsNOLNK22, trait = paste0(phenotypes[1], '.sp'), environment = 'environment', genotype = 'genotype')
# 
# for(i in 2:length(phenotypes))
# {
#   hybridsNOLNK22.pl <- full_join(hybridsNOLNK22.pl, 
#                           estimatePlasticity3(hybridsNOLNK22, trait = paste0(phenotypes[i], '.sp'), environment = 'environment', genotype = 'genotype'),
#                           join_by(genotype),
#                           suffix = c('', ''), 
#                           keep = FALSE)
#   # Print mean of the b column so we know we are getting reasonable values
#   print(mean(hybridsNOLNK22.pl[[paste0(phenotypes[i], '.sp.b')]], na.rm = TRUE))
# }
# 
# hybridsNOLNK22.pl <- hybridsNOLNK22.pl %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .)))
# 
# hybridsNOLNK22.pl <- hybridsNOLNK22.pl %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          earParent = str_split_i(genotype, ' X ', 1),
#          pollenParent = str_split_i(genotype, ' X ', 2))
# 
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
# hybridsNOLNK22.pl <- left_join(hybridsNOLNK22.pl, parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(earParentAge = age) %>%
#   left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(pollenParentAge = age) %>%
#   rowwise() %>%
#   mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
#          youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
#          meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))
# 
# write.csv(hybridsNOLNK22.pl, 'analysis/hybridsNOLNK22Plasticity.csv', row.names = FALSE, quote = FALSE)
hybridsNOLNK22.pl <- read_csv('analysis/hybridsNOLNK22Plasticity.csv')

for(i in 1:length(phenotypes))
{
  traitMu <- paste0(phenotypes[i], '.sp.mu')
  traitB <- paste0(phenotypes[i], '.sp.b')
  print(cor(hybridsNOLNK22.pl[[traitMu]], hybridsNOLNK22.pl[[traitB]], use = 'complete.obs'))
}
# for(i in 1:length(yieldComponents))
# {
#   traitMu <- paste0(yieldComponents[i], '.sp.mu')
#   traitB <- paste0(yieldComponents[i], '.sp.b')
#   meanLabel <- paste0('Mean ', yieldComponentsLabels[i])
#   plasticityLabel <- paste0(yieldComponentsLabels[i], ' Linear Plasticity')
# 
#   oldestParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = oldestParentAge)) +
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
#   youngestParentPlot <- ggplot(hybridsNOLNK22.pl, aes(.data[[traitMu]], .data[[traitB]], color = youngestParentAge)) +
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
# }
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
# nonMissingVals <- 0
# totalPlots <- length(hybrids$qrCode)
# vars <- c(#'anthesisDate', 'silkDate', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 
#           'GDDToAnthesis', 'GDDToSilk',
#           'anthesisSilkingIntervalSilkingGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'combineYield', 'yieldPerAcre',
#           'combineMoisture', 'combineTestWeight', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow',
#           'kernelRowNumber', 'kernelsPerEar', 'shelledCobMass', 'percentMoisture', 'percentStarch', 'percentProtein', 'percentOil',
#           'percentFiber', 'percentAsh', 'kernelColor', 'percentLodging', 'totalStandCount')
# vars <- phenotypes
# hybrids <- hybrids %>%
#   mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
# for(var in vars)
# {
#   numMissingVals <- as.numeric(sum(is.na(hybrids[[var]])))
#   numNotMissing <- totalPlots - numMissingVals
#   nonMissingVals <- nonMissingVals + numNotMissing
# }
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
#     theme(axis.text.x = element_text(color = 'black', size = 9),
#           axis.text.y = element_text(color = 'black', size = 9),
#           legend.text = element_text(color = 'black', size = 9),
#           text = element_text(color = 'black', size = 9),
#           legend.position = 'none',
#           panel.grid = element_blank())
#   
#   print(oldestParentPlot)
#   print(youngestParentPlot)
#   print(meanParentPlot)
#   # ggsave(paste0('analysis/', yieldComponents[i], 'LinearPlasticityVsMean.png'), dpi = 1000)
# }

nResponseLocationYears <- c('2023 Ames', '2022 Scottsbluff', '2022 North Platte LI')
# Subset to the locations where we got a significant, classical N response on a population level and estimate plasticity --> does N plasticity correlate across location years?
nResponse <- filter(hybrids, locationYear %in% nResponseLocationYears)

# nTreatmentsPerLocationYear <- nResponse %>%
#   group_by(locationYear, genotype) %>%
#   summarise(nitrogenTreatments = n_distinct(nitrogenTreatment, na.rm = TRUE)) %>%
#   filter(nitrogenTreatments < 3)
# singleNLevelHybrids <- unique(nTreatmentsPerLocationYear$genotype)
# 
# nResponse <- filter(nResponse, !(genotype %in% singleNLevelHybrids))
# 
# nResponse.pl <- getNitrogenPlasticityByLocationYear(nResponse, paste0(phenotypes[1], '.sp'), 'nitrogenTreatment', 'genotype')
# 
# for(i in 2:length(phenotypes))
# {
#   nResponse.pl <- full_join(nResponse.pl, 
#                             getNitrogenPlasticityByLocationYear(nResponse, paste0(phenotypes[i], '.sp'), 'nitrogenTreatment', 'genotype'),
#                             join_by(genotype, locationYear),
#                             suffix = c('', ''),
#                             keep = FALSE)
# }
# 
# nResponse.pl <- nResponse.pl %>%
#   rowwise() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .))) %>% 
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          earParent = str_split_i(genotype, ' X ', 1),
#          pollenParent = str_split_i(genotype, ' X ', 2)) %>%
#   left_join(parentInfo, join_by(earParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(earParentAge = age) %>%
#   left_join(parentInfo, join_by(pollenParent==genotype), suffix = c('', ''), keep = FALSE, relationship = 'many-to-one') %>%
#   rename(pollenParentAge = age) %>%
#   rowwise() %>%
#   mutate(oldestParentAge = min(earParentAge, pollenParentAge, na.rm = TRUE),
#          youngestParentAge = max(earParentAge, pollenParentAge, na.rm = TRUE),
#          meanParentAge = mean(c(earParentAge, pollenParentAge), na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .)))
# 
# write.csv(nResponse.pl, 'analysis/NitrogenResponsePlasticity.csv', quote = FALSE, row.names = FALSE)
nResponse.pl <- read_csv('analysis/NitrogenResponsePlasticity.csv')
 # Yield
nResponse.plWide <- nResponse.pl %>%
  pivot_wider(id_cols = genotype, 
              names_from = locationYear,
              values_from = yieldPerAcre.sp.b)

corData <- cor(nResponse.plWide[, 2:4], use = 'complete.obs', method = 'spearman') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

corDataYield <- corData %>% 
  filter(nPlasticityCor < 1)

nPlasticityCorYield <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1, limits = c(-0.1, 1)) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = label_wrap(10)) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = label_wrap(10)) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Yield (bushels/acre)') + 
  theme_minimal() +
  theme(text = element_text(color = 'black', size = 9),
        axis.text.x = element_text(color = 'black', size = 9, angle = 90),
        axis.text = element_text(color = 'black', size = 9, hjust = 1, margin = margin(0,0,0, 0)),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        legend.background = element_blank())
nPlasticityCorYield

# ggsave('../nPlasticityCorYield.png', dpi = 1000)
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
#   theme(text = element_text(color = 'black', size = 9),
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
              values_from = hundredKernelMass.sp.b)

corData <- cor(nResponse.plWideHKM[, 2:4], use = 'complete.obs', method = 'spearman') %>%
  as.table() %>%
  as.data.frame()
names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')

nPlasticityCorHKM <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1,
                       limits = c( -0.1, 1)) + 
  guides(fill = guide_colourbar(barwidth = 12,
                                barheight = 1)) +
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = label_wrap(10)) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = c(label_wrap(10))) +
  labs(x = '', y = '', fill = 'Nitrogen Plasticity Correlation', title = 'Hundred Kernel Mass (g)') + 
  theme(text = element_text(color = 'black', size = 9),
        axis.text.x = element_text(color = 'black', size = rel(1)),
        axis.text = element_text(color = 'black', size = rel(1)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'bottom',
        legend.background = element_blank(), 
        legend.text = element_text(color = 'black', size = 9),
        legend.title = element_text(color = 'black', size = 9, vjust = 0.85))
nPlasticityCorHKM

nPlasticityCorLegend <- get_plot_component(nPlasticityCorHKM, 'guide-box-bottom') 

nPlasticityCorHKM <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c(direction = -1) + 
  scale_x_discrete(breaks = unique(corData$locationYear1), 
                   labels = label_wrap(10)) +
  scale_y_discrete(breaks = unique(corData$locationYear1), 
                   labels = label_wrap(10)) +
  labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = 'Hundred Kernel Mass (g)') + 
  theme_minimal() +
  theme(text = element_text(color = 'black', size = 9),
        axis.text.x = element_text(color = 'black', size = rel(1), angle = 90),
        axis.text = element_text(color = 'black', size = rel(1), hjust = 1, margin = margin(0, 0, 0, 0)),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        legend.position = 'none',
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
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

ames23NResponse.pl <- filter(nResponse.pl, locationYear=='2023 Ames')
# ames23YieldByNResponse <- lm(yieldPerAcre.sp.mu~ yieldPerAcre.sp.b + meanParentAge, data = ames23NResponse.pl)
# anova(ames23YieldByNResponse)
# summary(ames23YieldByNResponse)
# 
# ames23HKMByNResponse <- lm(hundredKernelMass.sp.mu ~ hundredKernelMass.sp.b + meanParentAge, data = ames23NResponse.pl)
# anova(ames23HKMByNResponse)
# 
# nPlasticityAmesYield <- ggplot(ames23NResponse.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
#   geom_point() +
#   geom_hline(yintercept = 1, color = 'black', linewidth = 1) +
#   scale_color_viridis(direction = -1) +
#   scale_y_continuous(limits = c(0, 1.5)) + 
#   guides(color = guide_colourbar(barwidth = 12,
#                          barheight = 1)) +
#   labs(x = 'Mean Yield (bushels/acre)', y = 'Nitrogen Plasticity', color = 'Mean Parent Age') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'bottom',
#         legend.title = element_text(color = 'black', size = 9, vjust = 0.75),
#         panel.grid = element_blank())
#  nPlasticityAmesYield
#  
#  nPlasticityParentAgeLegend <- get_legend(nPlasticityAmesYield)
 
 # nPlasticityAmesYield <- ggplot(ames23NResponse.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
 #   geom_point() +
 #   geom_hline(yintercept = 1, color = 'black') +
 #   scale_color_viridis(direction = -1) +
 #   scale_y_continuous(limits = c(0.5, 1.5)) + 
 #   labs(x = 'Mean Yield (bushels/acre)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
 #   theme_minimal() +
 #   theme(axis.text.x = element_text(color = 'black', size = 9),
 #         axis.text.y = element_text(color = 'black', size = 9),
 #         legend.text = element_text(color = 'black', size = 9),
 #         text = element_text(color = 'black', size = 9),
 #         legend.position = 'none',
 #         panel.grid = element_blank())
 # nPlasticityAmesYield
 # 
 # # nPlasticitySBKRN <- ggplot(sb22NResponse.pl, aes(kernelRowNumber.sp.mu, kernelRowNumber.sp.b, color = meanParentAge)) +
 # #   geom_point() +
 # #   geom_hline(yintercept = 1, color = 'black') +
 # #   scale_color_viridis(direction = -1) +
 # #   scale_y_continuous(limits = c(-2.5, 2.5)) + 
 # #   labs(x = 'Mean Kernel Row Number', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
 # #   theme_minimal() +
 # #   theme(axis.text.x = element_text(color = 'black', size = 9),
 # #         axis.text.y = element_text(color = 'black', size = 9),
 # #         legend.text = element_text(color = 'black', size = 9),
 # #         text = element_text(color = 'black', size = 9),
 # #         legend.position = 'none',
 # #         panel.grid = element_blank())
 # # nPlasticitySBKRN
 # 
 # nPlasticityAmesHKM <- ggplot(ames23NResponse.pl, aes(hundredKernelMass.sp.mu, hundredKernelMass.sp.b, color = meanParentAge)) +
 #   geom_point() +
 #   geom_hline(yintercept = 1, color = 'black') +
 #   scale_color_viridis(direction = -1) +
 #   scale_y_continuous(limits = c(0.5, 1.5)) + 
 #   labs(x = 'Mean Hundred Kernel Mass (g)', y = 'Nitrogen Plasticity', color = str_wrap('Mean Parent Age', 6)) +
 #   theme_minimal() +
 #   theme(axis.text.x = element_text(color = 'black', size = 9),
 #         axis.text.y = element_text(color = 'black', size = 9),
 #         legend.text = element_text(color = 'black', size = 9),
 #         text = element_text(color = 'black', size = 9),
 #         legend.position = 'none',
 #         panel.grid = element_blank())
 # nPlasticityAmesHKM

 # Which genotypes are 'most' and 'least' nitrogen stable across these envs for yield
ames23Genotypes <- ames23NResponse.pl$genotype
# cf23NResponse.pl <- filter(nResponse.pl, locationYear=="2023 Crawfordsville")
# cf23Genotypes <- cf23NResponse.pl$genotype
# nResponse23Genotypes <- intersect(ames23Genotypes, cf23Genotypes)
sb22NResponse.pl <- filter(nResponse.pl, locationYear=='2022 Scottsbluff')
sb22Genotypes <- sb22NResponse.pl$genotype
np22NResponse.pl <- filter(nResponse.pl, locationYear=='2022 North Platte LI')
np22Genotypes <- np22NResponse.pl$genotype
nResponse22Genotypes <- intersect(sb22Genotypes, np22Genotypes)
nResponseGenotypes <- intersect(nResponse22Genotypes, ames23Genotypes)

nResponsePopulation <- nResponse %>%
  group_by(nitrogenTreatment, locationYear) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  mutate(genotype = 'Population')

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

nResponseLAH <- hybrids %>%
  filter(genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype)) %>%
  filter(locationYear %in% nResponseLocationYears) %>%
  group_by(genotype, locationYear, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>% 
  bind_rows(nResponsePopulation) %>%
  mutate(genotype = factor(genotype, levels = c('Population', highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

nPlasticityGenotypeLines <- ggplot(nResponseLAH, aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype, linetype = genotype)) +
  geom_line() +
  facet_wrap(vars(locationYear), nrow = 1) +
  scale_color_manual(values = viridis_pal()(4)[c(2, 1, 3, 4)]) +
  scale_linetype_manual(values = c('dashed', 'solid', 'solid', 'solid')) + 
  labs(x = 'Nitrogen Treatment', y = 'Yield (bushels/acre)', color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9),
        strip.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'bottom',
        panel.grid = element_blank())
nPlasticityGenotypeLines


  
  

# nPlasticityLAHAmes <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & str_detect(environment, '2023:Ames'))  %>%
#   group_by(genotype, environment, nitrogenTreatment) %>%
#   summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
#   mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
#                                        nitrogenTreatment=='Medium' ~ '150-175',
#                                        nitrogenTreatment=='High' ~ '225-250')) %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
#          genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))
# 
# nPlasticityLAHCF <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & str_detect(environment, '2023:Crawfordsville')) %>%
#   group_by(genotype, environment, nitrogenTreatment) %>%
#   summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
#   mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
#                                        nitrogenTreatment=='Medium' ~ '150-175',
#                                        nitrogenTreatment=='High' ~ '225-250')) %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
#          genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))
# 
# nPlasticityLAHNP2 <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & location=='North Platte2') %>%
#   group_by(genotype, environment, nitrogenTreatment) %>%
#   summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
#   mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
#                                        nitrogenTreatment=='Medium' ~ '150-175',
#                                        nitrogenTreatment=='High' ~ '225-250')) %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
#          genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))
# 
# nPlasticityLAHSB <- filter(hybrids, genotype %in% c(lowPlasticityGenotype, avgPlasticityGenotype, highPlasticityGenotype) & location=='Scottsbluff') %>%
#   group_by(genotype, environment, nitrogenTreatment) %>%
#   summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
#   mutate(nitrogenTreatment = case_when(nitrogenTreatment=='Low' ~ '75',
#                                        nitrogenTreatment=='Medium' ~ '150-175',
#                                        nitrogenTreatment=='High' ~ '225-250')) %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('75', '150-175', '225-250')),
#          genotype = factor(genotype, levels = c(highPlasticityGenotype, avgPlasticityGenotype, lowPlasticityGenotype)))

# nPlasticityGenotypeLines <- ggplot() + 
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHNP2, linetype = 'solid') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHSB, linetype = 'dashed') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHAmes, linetype = 'dotted') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHCF, linetype = 'dotdash') +
#   scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)],
#                      label = label_wrap(20)) + 
#   labs(x = 'Nitrogen Fertilizer (lbs/acre)', y = 'Mean Yield (bushels/acre)', color = 'Genotype') + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'right',
#         panel.grid = element_blank())
# nPlasticityGenotypeLines
# 
# nPlasticityLinesGenotypeLegend <- get_legend(nPlasticityGenotypeLines)
# 
# linetypes <- tibble(environment = c('2022 North Platte:4.3', '2022 Scottsbluff', '2023 Ames', '2023 Crawfordsville'), x = 1, y = 1)
# linetypePlot <- ggplot(linetypes, aes(x, y, linetype = environment)) +
#   geom_line() + 
#   scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'dotdash'),
#                         labels = label_wrap(20)) +
#   labs(linetype = 'Environment') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'right',
#         panel.grid = element_blank())
# linetypePlot
# 
# nPlasticityLinesLinetypeLegend <- get_legend(linetypePlot)
# 
# nPlasticityGenotypeLines <- ggplot() + 
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHNP2, linetype = 'solid') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHSB, linetype = 'dashed') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHAmes, linetype = 'dotted') +
#   geom_line(aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype), data = nPlasticityLAHCF, linetype = 'dotdash') +
#   scale_color_manual(values = viridis_pal()(4)[c(1, 3, 4)]) + 
#   labs(x = 'Nitrogen Fertilizer (lbs/acre)', y = 'Mean Yield (bushels/acre)', color = 'Genotype') + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'none',
#         panel.grid = element_blank())
# nPlasticityGenotypeLines

# nPlasticityGenotypeLinesLegends <- plot_grid(nPlasticityLinesGenotypeLegend, nPlasticityLinesLinetypeLegend, ncol = 1)
# nPlasticityGenotypeLinesLegends
# 
# nPlasticityGenotypeLinesPlot <- plot_grid(nPlasticityGenotypeLines, nPlasticityGenotypeLinesLegends, ncol = 2, rel_widths = c(1, 0.4))
# nPlasticityGenotypeLinesPlot
# How well does N plasticity correlate between reps within a location
# nResponseBlock.pl <- getNitrogenPlasticityByLocationYearBlock(nResponse, paste0(phenotypes[1], '.sp'), 'nitrogenTreatment', 'genotype')
# 
# for(i in 2:length(phenotypes))
# {
#   nResponseBlock.pl <- full_join(nResponseBlock.pl, 
#                             getNitrogenPlasticityByLocationYearBlock(nResponse, paste0(phenotypes[i], '.sp'), 'nitrogenTreatment', 'genotype'),
#                             join_by(genotype, locationYear, blockSet),
#                             suffix = c('', ''),
#                             keep = FALSE)
# }
# write.csv(nResponseBlock.pl, 'analysis/nitrogenResponseBlockPlasticity.csv', quote = FALSE, row.names = FALSE)
nResponseBlock.pl <- read.csv('analysis/nitrogenResponseBlockPlasticity.csv')

nResponseBlockCorrByLocYear <- nResponseBlock.pl %>%
  pivot_wider(id_cols = c(genotype, locationYear), names_from = blockSet, names_prefix = 'b', 
              values_from = yieldPerAcre.sp.b)%>%
  group_by(locationYear) %>%
  summarise(nPlasticityCor = cor(b1, b2, use = 'complete.obs', method = 'spearman')) %>%
  rowwise() %>%
  mutate(locationYear = str_replace(locationYear, ':', ' ')) %>%
  mutate(locationYear = case_when(str_detect(locationYear, 'North Platte') ~ str_c(locationYear, ' LI'), 
                                  .default = locationYear))

nResponseCorSummary <- corDataYield %>%
  left_join(nResponseBlockCorrByLocYear, join_by(locationYear1==locationYear), keep = FALSE, 
            suffix = c('', '.1'), relationship = 'many-to-one') %>%
  left_join(nResponseBlockCorrByLocYear, join_by(locationYear2==locationYear), keep = FALSE,
            suffix = c('.a', '.2'), relationship = 'many-to-one') %>%
  rowwise() %>%
  mutate(a1 = nPlasticityCor.a - nPlasticityCor.1,
         a2 = nPlasticityCor.a - nPlasticityCor.2) %>%
  arrange(nPlasticityCor.a)
nResponseCorSummary <- nResponseCorSummary[c(1, 3, 5, 7, 9, 11), ]


# for(i in 6)
# {
#   traitNPlasticity <- paste0(yieldComponents[i], '.sp.b')
#   
#   dfWide <- nResponseBlock.pl %>%
#     pivot_wider(id_cols = c(genotype, locationYear), 
#                 names_from = blockSet, 
#                 values_from = all_of(traitNPlasticity), 
#                 names_prefix = 'blockSet')
#   
#   nPlasticityBlockCorr <- ggplot(dfWide, aes(blockSet1, blockSet2)) + 
#     geom_point() + 
#     facet_wrap(vars(locationYear)) + 
#     labs(title = yieldComponents[i])
#   print(yieldComponents[i])
#   print(cor(dfWide$blockSet1, dfWide$blockSet2, use = 'complete.obs', method = 'spearman'))
#   print(nPlasticityBlockCorr)
# }

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
    scale_x_continuous(limits = c(0, 120)) +
    scale_y_continuous(limits = c(0, 120)) +
    guides(shape = guide_legend(override.aes = list(size = 0.5))) +
    labs(x = 'Hybrid Rank', 
         y = 'Hybrid Rank', 
         fill = str_wrap('Superior Hybrid in Best Environment', 20), 
         title = yieldComponentsLabels[i]) + 
    theme_minimal() +
    theme(text = element_text(color = 'black', size = 9),
          title = element_text(color = 'black', size = 9),
          axis.text.x = element_text(color = 'black', size = rel(1)),
          axis.text = element_text(color = 'black', size = rel(1)),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom',
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

heatmap <- plotInteractionImportanceGrid(trait = 'yieldPerAcre', 
                                         traitLabel = 'Yield (bushels/acre)', 
                                         legendPosition = 'bottom',
                                         legendTextAngle = 90)
heatmap

sigCrossoversGenotypeLevel <- sigCrossovers %>%
  group_by(genotype1) %>%
  summarise(yieldTotalNormalizedScore = sum(yieldPerAcreScoreNormalized, na.rm = TRUE)) %>%
  left_join(yieldBLUPS, join_by(genotype1==genotype), keep = FALSE, suffix = c('', '')) %>%
  select(genotype1, yieldTotalNormalizedScore, rankOrder)

freqCrossoverHybrids <- c('PHK76 X LH82', 'PHP02 X PHJ89', 'LH195 X PHM49', 'PHK56 X W606S', 'PHP02 X PHG47')

freqCrossoverHybridData1LNK <- sigCrossovers %>%
  filter((genotype1 %in% freqCrossoverHybrids)) %>%
  select(genotype1, yieldPerAcreRank.E7.G1, yieldPerAcreRank.E8.G1, yieldPerAcreRank.E9.G1) %>%
  rowwise() %>%
  mutate(lnkRank = mean(c_across(c(yieldPerAcreRank.E7.G1, yieldPerAcreRank.E8.G1, yieldPerAcreRank.E9.G1)), na.rm = TRUE)) %>%
  group_by(genotype1) %>%
  summarise(lnkRank = mean(lnkRank, na.rm = TRUE))

freqCrossoverHybridData2LNK <- sigCrossovers %>%
  filter((genotype2 %in% freqCrossoverHybrids)) %>%
  select(genotype2, yieldPerAcreRank.E7.G2, yieldPerAcreRank.E8.G2, yieldPerAcreRank.E9.G2) %>%
  rowwise() %>%
  mutate(lnkRank = mean(c_across(c(yieldPerAcreRank.E7.G2, yieldPerAcreRank.E8.G2, yieldPerAcreRank.E9.G2)), na.rm = TRUE)) %>%
  group_by(genotype2) %>%
  summarise(lnkRank = mean(lnkRank, na.rm = TRUE))

freqCrossoverHybridData1 <- sigCrossovers %>%
  filter((genotype1 %in% freqCrossoverHybrids)) %>%
  select(c(genotype1, contains('yieldPerAcreRank'))) %>%
  select(c(genotype1, contains('.G1'))) %>%
  select(!c(yieldPerAcreRank.E7.G1, yieldPerAcreRank.E8.G1, yieldPerAcreRank.E9.G1)) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(contains('yieldPerAcreRank')), na.rm = TRUE)) %>%
  group_by(genotype1) %>%
  summarise(mean = mean(mean, na.rm = TRUE)) %>%
  full_join(freqCrossoverHybridData1LNK, join_by(genotype1))

freqCrossoverHybridData2 <- sigCrossovers %>%
  filter((genotype2 %in% freqCrossoverHybrids)) %>%
  select(c(genotype2, contains('yieldPerAcreRank'))) %>%
  select(c(genotype2, contains('.G2'))) %>%
  select(!c(yieldPerAcreRank.E7.G2, yieldPerAcreRank.E8.G2, yieldPerAcreRank.E9.G2)) %>%
  rowwise() %>%
  mutate(mean = mean(c_across(contains('yieldPerAcreRank')), na.rm = TRUE)) %>%
  group_by(genotype2) %>%
  summarise(mean = mean(mean, na.rm = TRUE)) %>%
  full_join(freqCrossoverHybridData2LNK, join_by(genotype2))

top72Percent <- yieldBLUPS %>%
  arrange(rankOrder)
top72Percent <- top72Percent$genotype[33:117]

freqCrossoverHybridDF <- sigCrossovers %>%
  filter(((genotype1 %in% freqCrossoverHybrids) & (genotype2 %in% top72Percent))|
           ((genotype1 %in% top72Percent)|(genotype2 %in% freqCrossoverHybrids))) %>%
  select(!yieldPerAcreScoreNormalized) %>%
  rowwise() %>%
  mutate(across(contains('yieldPerAcreScore'), ~case_when(.>0~1))) %>%
  mutate(nonZeroEnvPairs = sum(c_across(contains('yieldPerAcreScore')), na.rm = TRUE)) %>%
  mutate(percentNonZeroEnvPairs = (nonZeroEnvPairs/(yieldPerAcreComparedEnvs*(yieldPerAcreComparedEnvs - 1)))*100,
         freqCrossoverHybrid1 = case_when(genotype1 %in% freqCrossoverHybrids ~ genotype1, 
                                          .default = genotype2),
         freqCrossoverHybrid2 = case_when((genotype1 %in%freqCrossoverHybrids) & (genotype2 %in% freqCrossoverHybrids) ~ genotype2)) %>%
  select(genotype1, genotype2, freqCrossoverHybrid1, freqCrossoverHybrid2, nonZeroEnvPairs, percentNonZeroEnvPairs) %>%
  arrange(freqCrossoverHybrid1, freqCrossoverHybrid2, nonZeroEnvPairs)

phk76lh82 <- filter(freqCrossoverHybridDF, freqCrossoverHybrid1=="PHK76 X LH82"|freqCrossoverHybrid2=="PHK76 X LH82")
php02phj89 <- filter(freqCrossoverHybridDF, freqCrossoverHybrid1=="PHP02 X PHJ89"|freqCrossoverHybrid2=="PHP02 X PHJ89")
lh195phm49 <- filter(freqCrossoverHybridDF, freqCrossoverHybrid1=="LH195 X PHM49"|freqCrossoverHybrid2=="LH195 X PHM49")
phk56w606s <- filter(freqCrossoverHybridDF, freqCrossoverHybrid1=="PHK56 X W606S"|freqCrossoverHybrid2=="PHK56 X W606S")
php02phg47 <- filter(freqCrossoverHybridDF, freqCrossoverHybrid1=="PHK56 X W606S"|freqCrossoverHybrid2=="PHK56 X W606S")
# ggsave('../interactionImportance.png', dpi = 1000)

highPlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
mostPlastic10PercentGenotypes <- highPlasticityGenotype$genotype[round(0.9*length(highPlasticityGenotype$genotype)):length(highPlasticityGenotype$genotype)]
highPlasticityGenotype <- highPlasticityGenotype$genotype[length(highPlasticityGenotype$genotype)]

lowPlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
leastPlastic10PercentGenotypes <- lowPlasticityGenotype$genotype[1:round(0.1*length(lowPlasticityGenotype$genotype))]
lowPlasticityGenotype <- lowPlasticityGenotype$genotype[1]

averagePlasticityGenotype <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
averagePlasticityGenotype <- averagePlasticityGenotype$genotype[ceiling(length(averagePlasticityGenotype$genotype)/2)]

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
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank()) +
  inset_element(FWConceptualPlotLegend, left = 0.25, bottom = 0.6, right = 0.35, top = 1.05, on_top = FALSE)
FWConceptualPlot

# ggsave('../FWConceptualPlot.png', dpi = 1000, width = 3*3.92, height = 3*3.52, units = 'in', bg = 'white')

hybrids <- hybrids %>%
  rowwise() %>%
  mutate(mostPlastic10Percent = (genotype %in% mostPlastic10PercentGenotypes),
         environment = factor(environment, levels = orderedEnvironments$environment))

mostPlasticAnova <- aov(yieldPerAcre.sp ~ factor(mostPlastic10Percent)*environment, data = hybrids)
mostPlasticTukey <- TukeyHSD(mostPlasticAnova)
sigPlasticityGroupEnvs <- mostPlasticTukey[["factor(mostPlastic10Percent):environment"]] %>%
  as_tibble(rownames = 'comp') %>%
  filter(`p adj` < 0.05) %>%
  rowwise() %>%
  mutate(grp1 = str_split_i(comp, '-', 1),
         grp2 = str_split_i(comp, '-', 2)) %>%
  mutate(plasticity1 = str_split_i(grp1, ':', 1),
         plasticity2 = str_split_i(grp2, ':', 1),
         env1 = str_split_fixed(grp1, ':', 2)[2],
         env2 = str_split_fixed(grp2, ':', 2)[2]) %>%
  filter(env1==env2)

sigAnnotations10PercentMostPlastic <- tibble(environment = factor(sigPlasticityGroupEnvs$env1, levels = orderedEnvironments$environment))
mostPlastic10PercentPerformance <- ggplot(hybrids, aes(mostPlastic10Percent, yieldPerAcre.sp, fill = mostPlastic10Percent)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = 'white') +
  geom_text(data = sigAnnotations10PercentMostPlastic, mapping = aes(y = 0, x = FALSE, fill = FALSE), label = '*') +
  facet_wrap(vars(environment), strip.position = 'left', ncol = 9) + 
  scale_x_discrete(labels = c('', '')) +
  scale_fill_manual(values = viridis_pal()(4)[1:2], 
                    labels = c('Lower 90%', 'Upper 10%')) + 
  labs(x = '', y = 'Yield (bushels/acre)', fill = 'Plasticity Rank') +
  theme_minimal() + 
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        strip.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9, hjust = 0.5),
        panel.grid = element_blank(),
        legend.position = 'top')
mostPlastic10PercentPerformance
# ggsave('../mostPlastic10PercentPerformance.svg', plot = mostPlastic10PercentPerformance,
#        width = 6.5, height = 9, units = 'in', dpi=1000, bg = 'white')


# How does yield as % of env mean change with env mean for plastic vs less plastic genotypes?
percentMeanData <- hybrids %>%
  rowwise() %>%
  mutate(leastPlastic10Percent = (genotype %in% leastPlastic10PercentGenotypes),
         best10PercentOverall = (genotype %in% top10PercentOverallGenotypes), 
         worst10PercentOverall = (genotype %in% lower10PercentOverallGenotypes), 
         environment = factor(environment, levels = orderedEnvironments$environment)) %>%
  group_by(environment, genotype) %>%
  mutate(hybridEnvMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(environment) %>%
  mutate(envMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  rowwise() %>% 
  mutate(hybridPercentEnvMean = (hybridEnvMean/envMean)*100) %>%
  ungroup()

percentMeanPlotExtremePlasticity <- percentMeanData %>%
  filter(mostPlastic10Percent|leastPlastic10Percent) %>%
  ggplot(aes(envMean, hybridPercentEnvMean, group = genotype,
                                               color = meanParentAge)) +
  geom_smooth(method = 'lm') + 
  geom_point() + 
  scale_color_viridis(direction = -1) + 
  guides(fill = guide_colourbar(barwidth = 1000,
                                barheight = 1)) +
  scale_y_continuous(breaks = c(50, 100, 150, 200),
                     labels = c('50%', '100%', '150%', '200%')) +
  labs(x = 'Environment Mean Yield (bushels/acre)', y = str_wrap('Hybrid Mean Yield As Percent of Environment Mean', 28), 
       color = str_wrap('Mean Parent Release Year', 1), title = '10% Most & Least Plastic Hybrids') +
  theme_minimal() +  
  theme(axis.text.x = element_text(color = 'black', size = 9, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9, hjust = 0.5),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        # legend.text = element_text(color = 'black', size = 9, vjust = 0.5, angle = 90),legend.text = element_text(color = 'black', size = 9, vjust = 0.5, angle = 90),
        # legend.position = 'bottom', 
        panel.grid = element_blank())
percentMeanPlotExtremePlasticity

# ggsave('../percentMeanExtremePlasticity.png', dpi = 1000)

percentMeanPlotOverallPerformance <- percentMeanData %>%
  filter(best10PercentOverall|worst10PercentOverall) %>%
ggplot(aes(envMean, hybridPercentEnvMean, group = genotype,
                            color = meanParentAge)) +
  geom_smooth(method = 'lm') + 
  geom_point() + 
  scale_color_viridis(direction = -1) + 
  scale_y_continuous(breaks = c(50, 100, 150, 200), 
                     labels = c('50%', '100%', '150%', '200%')) +
  labs(x = 'Environment Mean Yield (bushels/acre)', y = 'Hybrid Mean Yield As Percent of Environment Mean', 
       color = str_wrap('Mean Parent Release Year', 1), title = '10% Highest & Lowest Overall Yielding Hybrids') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9, hjust = 0.5),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        panel.grid = element_blank())
percentMeanPlotOverallPerformance

percentMeanPlots <- plot_grid(percentMeanPlotExtremePlasticity, percentMeanPlotOverallPerformance, 
                              ncol = 1, labels = 'AUTO')
# ggsave('../percentMeanPlots.png', plot = percentMeanPlots, width = 6.5, height = 9, units = 'in',
#        dpi = 1000, bg = 'white')

# Estimate plasticity using percent mean data
percentEnvMeanDataPlotLevel <- hybridsNOLNK22 %>%
  group_by(environment) %>%
  mutate(environmentMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(percentEnvironmentYield = (yieldPerAcre.sp/environmentMean)*100)
# Not really what we want - uses mean(percentEnvironmentMean) which is always 100 as x-axis in the regression
# percentEnvMean.pl <- estimatePlasticity3(percentEnvMeanDataPlotLevel, 'percentEnvironmentYield', 'environment', 'genotype')
hybridSummary <- hybridsNOLNK22 %>% 
  group_by(genotype) %>% 
  summarise(meanYield = mean(yieldPerAcre.sp, na.rm = TRUE))
# what if we try lm 
percentEnvMeanModel <- lm(percentEnvironmentYield ~ genotype + genotype*environmentMean, 
                          data = percentEnvMeanDataPlotLevel)
percentEnvMu <- percentEnvMeanModel$coefficients['(Intercept)']
environmentMeanEffect <- percentEnvMeanModel$coefficients['environmentMean']
percentEnvMean.pl <- percentEnvMeanModel$coefficients %>%
  as_tibble(rownames = 'genotype') %>%
  rowwise() %>%
  mutate(valueID = case_when(str_detect(genotype, ':environmentMean') ~ 'slope', .default = 'intercept'), 
         genotype = str_remove(genotype, 'genotype') %>%
           str_remove(':environmentMean')) %>%
  pivot_wider(id_cols = genotype, 
              names_from = valueID, 
              values_from = value) %>%
  filter(!(genotype %in% c('(Intercept)', 'environmentMean'))) %>%
  mutate(expectedValue = intercept + percentEnvMu, 
         b = slope + environmentMeanEffect + 1) %>%
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
  mutate(across(where(is.numeric), ~case_when(.==-Inf|.==Inf ~ NA, .default = .))) %>%
  left_join(hybridSummary)



percentEnvMeanPlotPlasticityVersusRelativePerformance <- ggplot(percentEnvMean.pl, aes(expectedValue, b, color = meanParentAge)) + 
  geom_point() + 
  scale_color_viridis(direction = -1) + 
  scale_x_continuous(breaks = c(50, 100, 150, 200),
                     labels = c('50%', '100%', '150%', '200%')) +
  # scale_y_continuous(breaks = c(50, 100, 150, 200), 
  #                    labels = c('50%', '100%', '150%', '200%')) +
  labs(x = 'Mean Hybrid Percent of Environment Mean', y = 'Linear Plasticity', 
       color = str_wrap('Mean Parent Release Year', 1), title = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9, hjust = 0.5),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        panel.grid = element_blank())
percentEnvMeanPlotPlasticityVersusRelativePerformance

percentEnvMeanPlotPlasticityVersusMeanYield <- ggplot(percentEnvMean.pl, aes(meanYield, b, color = meanParentAge)) + 
  geom_point() + 
  scale_color_viridis(direction = -1) + 
  # scale_x_continuous(breaks = c(50, 100, 150, 200),
  #                    labels = c('50%', '100%', '150%', '200%')) +
  # scale_y_continuous(breaks = c(50, 100, 150, 200), 
  #                    labels = c('50%', '100%', '150%', '200%')) +
  labs(x = 'Mean Hybrid Yield (bushels/acre)', y = 'Linear Plasticity', 
       color = str_wrap('Mean Parent Release Year', 1), title = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9, hjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9, hjust = 0.5),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        panel.grid = element_blank())
percentEnvMeanPlotPlasticityVersusMeanYield


phenotypeLabelsGCAVP <- c(#'Plant Density', 
                        'Test Weight', 'Harvest Moisture', 'Flag Leaf Height',
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
  guides(shape = guide_legend(override.aes = list(size = 0.5))) +
  labs(x = NULL, y = 'Linear Plasticity Variance', fill = '') +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9, color = 'black', angle = 90, margin = margin(0, 0, 0, 0), 
                                   vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 9, color = 'black', vjust = 0, hjust = 0),
        legend.text = element_text(size = 9, color = 'black'),
        text = element_text(size = 9, color = 'black'),
        legend.position = 'bottom',
        line = element_line(color = 'black', linewidth = 1),
        panel.grid = element_blank())
gca_vp.plot
# ggsave('../gca_vp.png', width = 3*6.22, height = 3*3, units = 'in', dpi = 1000)



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
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
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
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'top',
#         panel.grid = element_blank())
# yieldPredictionsPlot
# Single model 
# rfFeatures <- read.csv('analysis/featureImportances5CV.csv')[2:5]
# colnames(rfFeatures) <- c('plantDensity', 'kernelRowNumber', 'kernelsPerRow', 'hundredKernelMass')
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
#   pivot_longer(everything(), names_to = 'phenotype', values_to = 'val') %>%
#   rowwise() %>%
#   mutate(phenotypeLabel = case_when(phenotype=='plantDensity' ~ 'Plant Density',
#                                     phenotype=='kernelRowNumber' ~ 'Kernel Row Number',
#                                     phenotype=='kernelsPerRow' ~ 'Kernels Per Row*',
#                                     phenotype=='hundredKernelMass' ~ 'Hundred Kernel Mass')) %>%
#   mutate(phenotypeLabel = factor(phenotypeLabel, 
#                                  levels = c('Kernel Row Number', 'Plant Density', 'Kernels Per Row*', 'Hundred Kernel Mass'), 
#                                  ordered = TRUE))
# 
# featureImportance <- ggplot(rfFeatures, aes(val, phenotypeLabel)) +
#   geom_boxplot(color = 'black', fill = nitrogenColors[3]) +
#   # geom_text(aes(0.85, 'Hundred Kernel Mass (g)'), label = 'a', size = 3.88) +
#   # geom_text(aes(0.85, 'Kernels Per Row*'), label = 'b', size = 3.88) +
#   # geom_text(aes(0.85, 'Plant Density (plants/acre)'), label = 'c', size = 3.88) +
#   # geom_text(aes(0.85, 'Kernel Row Number'), label = 'd', size = 3.88) +
#   scale_y_discrete(labels = str_wrap(levels(rfFeatures$phenotypeLabel), 9)) +
#   labs(x = 'Feature Importance', y = '') +
#   theme_minimal() + 
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         panel.grid = element_blank(),
#         legend.position = 'none')
# featureImportance
# # Extension: https://crops.extension.iastate.edu/blog/meaghan-anderson/making-yield-estimates-corn-2022-edition
# yieldPredictions <- read.csv('analysis/RFpredictions5CV.csv') %>%
#   select(environment, plotNumber, predictedYield) %>%
#   rowwise() %>%
#   mutate(environment = str_replace(environment, ':0:', ' NI ') %>%
#            str_replace('8.6', 'FI') %>%
#            str_replace('4.3', 'LI') %>%
#            str_replace('4.5', 'LI') %>% 
#            str_replace_all(':', ' ')) %>%
#   mutate(environment = case_when(str_detect(environment, 'Lincoln')|str_detect(environment, 'Missouri Valley') ~ str_replace(environment, ' NI ', ' '), .default = environment)) %>%
#   rename(predictedYieldRF = predictedYield) %>%
#   full_join(hybrids, join_by(environment, plotNumber), keep = FALSE, suffix = c('', '')) %>%
#   filter(!is.na(predictedYieldRF)) %>%
#   select(environment, plotNumber, yieldPerAcre, predictedYieldRF, plantDensity, kernelRowNumber, kernelsPerRow, hundredKernelMass) %>%
#   mutate(predictedYieldExtension = (plantDensity*kernelRowNumber*kernelsPerRow)/((56*453.592*100)/hundredKernelMass))
# 
# rfRegressionModel <- lm(predictedYieldRF ~ yieldPerAcre, data = yieldPredictions)
# rfIntercept <- rfRegressionModel$coefficients[1]
# rfSlope <- rfRegressionModel$coefficients[2]
# summary(rfRegressionModel)
# # Adjusted R2 from summary(model)
# rfR2 <-  0.1111
# 
# extensionRegressionModel <- lm(predictedYieldExtension ~ yieldPerAcre, data = yieldPredictions)
# extensionIntercept <- extensionRegressionModel$coefficients[1]
# extensionSlope <- extensionRegressionModel$coefficients[2]
# summary(extensionRegressionModel)
# # Adjusted R2 from summary(model)
# extensionR2 <- 0.4564
# 
# yieldPredictionsSubsample <- yieldPredictions %>%
#   filter(!is.na(yieldPerAcre)) %>%
#   filter(!is.na(predictedYieldExtension)) %>%
#   filter(!is.na(predictedYieldRF)) %>%
#   ungroup() %>%
#   dplyr::slice_sample(prop = 0.10) %>%
#   pivot_longer(c(predictedYieldExtension, predictedYieldRF), 
#                values_to = 'predictedYield', 
#                names_to = 'method', 
#                names_prefix = 'predictedYield') %>%
#   rowwise() %>%
#   mutate(method = case_when(method=='RF' ~ 'Random Forest',
#                             method=='Extension' ~ 'Expert'))
# 
# yieldPredictionsPlot <- ggplot(yieldPredictionsSubsample, aes(yieldPerAcre, predictedYield, color = method)) + 
#   geom_point() + 
#   geom_abline(slope = extensionSlope, intercept = extensionIntercept, color = viridis_pal()(4)[1], linewidth = 1) +
#   geom_abline(slope = rfSlope, intercept = rfIntercept, color = viridis_pal()(4)[2], linewidth = 1) +
#   scale_color_manual(values = viridis_pal()(4)[1:2],
#                      labels = label_wrap(1)) + 
#   scale_x_continuous(limits = c(0, 300)) +
#   labs(x = 'Actual Yield (bushels/acre)', y = 'Predicted Yield (bushels/acre)', color = 'Method') + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'top',
#         panel.grid = element_blank())
# yieldPredictionsPlot

# rf <- plot_grid(featureImportance, yieldPredictionsPlot, nrow = 1)
# ggsave('../rf.png', dpi = 1000, units = 'in')

meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
    geom_point() +
    geom_hline(yintercept=1) +
    scale_color_viridis_c(direction = -1) +
    guides(color = guide_colourbar(barwidth = 8,
                                barheight = 1)) +
    labs(x = 'Hybrid Mean Yield (bushels/acre)', y = 'Yield Linear Plasticity', color = str_wrap('Mean Parent Release Year', 12)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(color = 'black', size = 9),
          axis.text.y = element_text(color = 'black', size = 9),
          legend.text = element_text(color = 'black', size = 9, margin = margin(0, 0, 0, 0)),
          text = element_text(color = 'black', size = 9),
          legend.position = 'top',
          panel.grid = element_blank())
meanParentPlot
# ggsave('../meanParentPlot.png', dpi = 1000, units = 'in')
# meanParentPlotLegend <- get_legend(meanParentPlot)
# meanParentPlot <- ggplot(hybridsNOLNK22.pl, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = meanParentAge)) +
#   geom_point() +
#   geom_hline(yintercept=1) +
#   scale_color_viridis_c(direction = -1) +
#   labs(x = 'Mean Yield (bushels/acre)', y = 'Yield Linear Plasticity', color = str_wrap('Mean Parent Release Year', width = 1)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         legend.text = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'none',
#         panel.grid = element_blank()) +
#   inset_element(meanParentPlotLegend, left = 0.125, bottom = 0.43, right = 0.225, top = 1, on_top = TRUE)


# workflow <- rasterGrob(readPNG('../workflow.png'))

# fig1left <- plot_grid(workflow, vp.plot, ncol = 1, labels = c('B', 'C'), rel_heights = c(0.4, 0.6))
# fig1right <- plot_grid(yieldPredictionsPlot, featureImportance, ncol = 1)
# fig1bottom <- plot_grid(fig1left, fig1right, nrow = 1, rel_widths = c(0.6, 0.4))
# fig1 <- plot_grid(experimentalDesign, fig1bottom, ncol = 1, labels = c('A', '', ''), rel_heights = c(0.3, 0.7))
# fig1

# fig1middle <- plot_grid(workflow, orderedBoxplots, nrow = 1, labels = c('B', 'C'), rel_widths = c(0.25, 0.75))
fig1 <- plot_grid(experimentalDesign, orderedBoxplots, vp.plot, ncol = 1, labels = 'AUTO', rel_heights = c(0.29, 0.33, 0.37))
# ggsave('../fig1HighRes.svg', plot = fig1, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

# fig2left <- plot_grid(FWConceptualPlot, meanParentPlot, ncol = 1, labels = c('A', 'B'))
# fig2right <- plot_grid(cxHeatmap, heatmap, ncol = 1, labels = c('C', 'D'))
# fig2top <- plot_grid(fig2left, fig2right, nrow = 1, rel_widths = c(0.8, 1))
# fig2 <- plot_grid(fig2top, gca_vp.plot, nrow = 2, labels = c('', 'E'), rel_heights = c(0.575, 0.425))
# ggsave('../fig2HighRes.svg', plot = fig2, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

fig2top <- plot_grid(FWConceptualPlot, meanParentPlot, nrow = 1, labels = 'AUTO')
# fig2bottom <- plot_grid(cxHeatmap, heatmap, nrow = 1, labels = c('D', 'E'))
fig2 <- plot_grid(fig2top, gca_vp.plot, nrow = 2, labels = c('', 'C'), rel_heights = c(0.425, 0.575))
# ggsave('../fig2HighRes.svg', plot = fig2, width = 6.5, height = 6.5, units = 'in', dpi = 1000, bg = 'white')

# Variance partitioning for yield from yield components
# vc_yield <- partitionVariance3(hybrids, 'yieldPerAcre.sp', 'Yield (bushels/acre)', '~ (1|plantDensity) + (1|kernelRowNumber) + (1|kernelsPerRow) + (1|hundredKernelMass)')

fig3top <- plot_grid(nPlasticityCorYield, nPlasticityCorHKM, labels = c('A', 'B'), 
                     nrow = 1, rel_widths = c(0.5, 0.5))
fig3top <- plot_grid(fig3top, nPlasticityCorLegend, ncol = 1, rel_heights = c(1, 0.075))
# fig3middle <- plot_grid(nPlasticityAmesYield, nPlasticityAmesHKM, labels = c('C', 'D'), 
#                         nrow = 1, rel_widths = c(0.5, 0.5))
# fig3middle <- plot_grid(fig3middle, nPlasticityParentAgeLegend, ncol = 1, rel_heights = c(1, 0.15))

fig3 <- plot_grid(fig3top, nPlasticityGenotypeLines, nrow = 2, labels = c('', 'C'), rel_heights = c(0.5, 0.5))
fig3
# ggsave('../fig3HighRes.png', plot = fig3, width = 6.5, height = 6.75, units = 'in', dpi = 1000, bg = 'white')


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
# # model <- nls(yieldPerAcre.sp ~ genotype + environment + genotype*environment, data = hybrids, na.action = na.omit, start = c(genotype=1, environment=1))
# block1 <- hybridsNOLNK22 %>% 
#   filter((block %% 2)==0)
# block1.pl <- estimatePlasticity3(block1, 'yieldPerAcre.sp', 'environment', 'genotype')
# block2 <- hybridsNOLNK22 %>% 
#   filter((block %% 2)!=0)
# block2.pl <- estimatePlasticity3(block2, 'yieldPerAcre.sp', 'environment', 'genotype')
# block.pl <- full_join(block1.pl, block2.pl, join_by(genotype), keep = FALSE, suffix = c('.1', '.2'))
# write.csv(block.pl, 'analysis/overallYieldBlockPlasticity.csv', quote = FALSE, row.names = FALSE)
block.pl <- read.csv('analysis/overallYieldBlockPlasticity.csv')

blockRho <- cor(block.pl$yieldPerAcre.sp.b.1, block.pl$yieldPerAcre.sp.b.2, use = 'complete.obs', method = 'spearman')

blockCorr <- ggplot(block.pl, aes(yieldPerAcre.sp.b.1, yieldPerAcre.sp.b.2)) + 
  geom_point(color = viridis_pal()(4)[1]) +
  labs(x = 'Linear Plasticity - Block 1', y = 'Linear Plasticity - Block 2', title = paste0('Spearman Rank Correlation: ', round(blockRho, 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        text = element_text(color = 'black', size = 9),
        panel.grid = element_blank())
blockCorr
# ggsave('../figSBlockCorr.png', plot = blockCorr, width = 6.5, height = 6.5, units = 'in', dpi = 1000, bg = 'white')

nResponseBlockWide <- nResponseBlock.pl %>%
  pivot_wider(id_cols = c(genotype, locationYear), names_from = blockSet, values_from = yieldPerAcre.sp.b, names_prefix = 'b')

nResponseBlockRho <- cor(nResponseBlockWide$b1, nResponseBlockWide$b2, use = 'complete.obs', method = 'spearman')

nResponseCorr <- ggplot(nResponseBlockWide, aes(b1, b2)) +
  geom_point(color = viridis_pal()(4)[1]) + 
  labs(x = 'Nitrogen Plasticity - Block 1', y = 'Nitrogen Plasticity - Block 2', title = paste0('Spearman Rank Correlation: ', round(nResponseBlockRho, 4))) + 
  scale_x_continuous(limits = c(0.6, 1.4)) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        text = element_text(color = 'black', size = 9),
        panel.grid = element_blank())
nResponseCorr

# ggsave('../figSNResponseBlockCorr.png', plot = nResponseCorr, width = 6.5, height = 6.5, units = 'in', dpi = 1000, bg = 'white')

for(i in 1:length(phenotypes))
{
  plotNitrogenPlasticityBlockCor(nResponseBlock.pl, phenotypes[i], phenotypeLabels[i])
}

meanParentPlotShelledCobWidth <- ggplot(hybridsNOLNK22.pl, aes(shelledCobWidth.sp.mu, shelledCobWidth.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_color_viridis_c(direction = -1) +
  # guides(color = guide_colourbar(barwidth = 6,
  #                                barheight = 1)) +
  labs(x = 'Hybrid Mean Shelled Cob Width (cm)', y = 'Shelled Cob Width Linear Plasticity', color = 'Mean Parent Release Year') + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9, margin = margin(0, 0, 0, 0)),
        text = element_text(color = 'black', size = 9),
        legend.position = 'right',
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
  labs(x = str_wrap('Hybrid Mean Shelled Cob Width (cm)', 19), y = str_wrap('Shelled Cob Width Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                             width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Kernel Row Number', 19), y = str_wrap('Kernel Row Number Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                               width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Ear Length (cm)', 19), y = str_wrap('Ear Length Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Ear Fill Length* (cm)', 19), y = str_wrap('Ear Fill Length Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Kernels Per Row*', 19), y = str_wrap('Kernels Per Row Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Kernels Per Ear', 19), y = str_wrap('Kernels Per Ear Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Ear Width (cm)', 19), y = str_wrap('Ear Width Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Hundred Kernel Mass (g)', 19), y = str_wrap('Hundred Kernel Mass Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
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
  labs(x = str_wrap('Hybrid Mean Kernel Mass Per Ear (g)', 19), y = str_wrap('Kernel Mass Per Ear Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                        width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())
meanParentPlotKernelMass

# meanParentPlotPlantDensity <- ggplot(hybridsNOLNK22.pl, aes(plantDensity.sp.mu/1000, plantDensity.sp.b, color = meanParentAge)) +
#   geom_point() +
#   geom_hline(yintercept=1) +
#   scale_y_continuous(limits = c(0.6, 1.4)) +
#   scale_color_viridis_c(direction = -1) +
#   guides(color = guide_colourbar(barwidth = 6,
#                                  barheight = 1)) +
#   labs(x = str_wrap('Hybrid Mean Plant Density (1000 plants/acre)', 19), y = str_wrap('Plant Density Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
#                                                                                                                                             width = 15)) + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(color = 'black', size = 9),
#         axis.text.y = element_text(color = 'black', size = 9),
#         text = element_text(color = 'black', size = 9),
#         legend.position = 'none',
#         panel.grid = element_blank())

meanParentPlotTestWt <- ggplot(hybridsNOLNK22.pl, aes(combineTestWeight.sp.mu, combineTestWeight.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Test Weight (lbs/bushel)', 19), y = str_wrap('Test Weight Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotHarvestMoisture <- ggplot(hybridsNOLNK22.pl, aes(combineMoisture.sp.mu, combineMoisture.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Harvest Moisture (%)', 19), y = str_wrap('Harvest Moisture Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotFlagLeafHeight <- ggplot(hybridsNOLNK22.pl, aes(flagLeafHeight.sp.mu, flagLeafHeight.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Flag Leaf Height (cm)', 19), y = str_wrap('Flag Leaf Height Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotEarHeight <- ggplot(hybridsNOLNK22.pl, aes(earHeight.sp.mu, earHeight.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Ear Height (cm)', 19), y = str_wrap('Ear Height Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotGDDToAnthesis <- ggplot(hybridsNOLNK22.pl, aes(GDDToAnthesis.sp.mu, GDDToAnthesis.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean GDD to Anthesis', 19), y = str_wrap('GDD to Anthesis Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9, angle = 90),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotGDDToSilk <- ggplot(hybridsNOLNK22.pl, aes(GDDToSilk.sp.mu, GDDToSilk.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean GDD to Silk', 19), y = str_wrap('GDD to Silk Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9, angle = 90),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotASI <- ggplot(hybridsNOLNK22.pl, aes(anthesisSilkingIntervalGDD.sp.mu, anthesisSilkingIntervalGDD.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Anthesis-Silking Interval (GDD)', 19), y = str_wrap('Anthesis-Silking Interval Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

meanParentPlotCobMass <- ggplot(hybridsNOLNK22.pl, aes(shelledCobMass.sp.mu, shelledCobMass.sp.b, color = meanParentAge)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_y_continuous(limits = c(0.6, 1.4)) +
  scale_color_viridis_c(direction = -1) +
  guides(color = guide_colourbar(barwidth = 6,
                                 barheight = 1)) +
  labs(x = str_wrap('Hybrid Mean Shelled Cob Mass (g)', 19), y = str_wrap('Shelled Cob Mass Linear Plasticity', 15), color = str_wrap('Mean Parent Release Year', 
                                                                                                                                          width = 15)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())

supplMeanParentPlots <- plot_grid(meanParentPlotLegend, meanParentPlotEarHeight, meanParentPlotFlagLeafHeight, 
                                  meanParentPlotGDDToAnthesis, 
                                  meanParentPlotGDDToSilk, meanParentPlotASI, meanParentPlotTestWt,
                                  meanParentPlotHarvestMoisture, meanParentPlotCobMass, meanParentPlotShelledCobWidth, 
                                  meanParentPlotKRN, meanParentPlotEarLength, meanParentPlotEarFillLength, 
                                  meanParentPlotKPR, meanParentPlotKPE, meanParentPlotEarWidth, 
                                  meanParentPlotHKM, meanParentPlotKernelMass, 
                                  nrow = 6, ncol = 3, labels = c('', 'A', 'B', 
                                                                 'C', 'D', 'E', 
                                                                 'F', 'G', 'H', 
                                                                 'I', 'J', 'K', 
                                                                 'L', 'M', 'N', 
                                                                 'O', 'P', 'Q'))
supplMeanParentPlots <- plot_grid(supplMeanParentPlots, , ncol = 1, rel_heights = c(1.5, 0.1))
# ggsave('../figSMeanParentPlots.svg', plot = supplMeanParentPlots, width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

# Model relationships: are they significant?
meanPerformanceByPlasticityOnlyYield <- lm(yieldPerAcre.sp.mu ~ yieldPerAcre.sp.b, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityOnlyYield)
summary(meanPerformanceByPlasticityOnlyYield)

meanPerformanceByPlasticityYield <- lm(yieldPerAcre.sp.mu ~ yieldPerAcre.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityYield)
summary(meanPerformanceByPlasticityYield)

meanPerformanceByPlasticityShelledCobWidth <- lm(shelledCobWidth.sp.mu ~ shelledCobWidth.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityShelledCobWidth)
summary(meanPerformanceByPlasticityShelledCobWidth)

meanPerformanceByPlasticityKRN <- lm(kernelRowNumber.sp.mu ~ kernelRowNumber.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKRN)
summary(meanPerformanceByPlasticityKRN)

meanPerformanceByPlasticityEarLength <- lm(earLength.sp.mu ~ earLength.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarLength)
summary(meanPerformanceByPlasticityEarLength)

meanPerformanceByPlasticityEarFillLength <- lm(earFillLength.sp.mu ~ earFillLength.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarFillLength)
summary(meanPerformanceByPlasticityEarFillLength)

meanPerformanceByPlasticityKPR <- lm(kernelsPerRow.sp.mu ~ kernelsPerRow.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKPR)
summary(meanPerformanceByPlasticityKPR)

meanPerformanceByPlasticityKPE <- lm(kernelsPerEar.sp.mu ~ kernelsPerEar.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKPE)
summary(meanPerformanceByPlasticityKPE)

meanPerformanceByPlasticityEarWidth <- lm(earWidth.sp.mu ~ earWidth.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarWidth)
summary(meanPerformanceByPlasticityEarWidth)

meanPerformanceByPlasticityHKM <- lm(hundredKernelMass.sp.mu ~ hundredKernelMass.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityHKM)
summary(meanPerformanceByPlasticityHKM)

meanPerformanceByPlasticityKernelMass <- lm(kernelMassPerEar.sp.mu ~ kernelMassPerEar.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityKernelMass)
summary(meanPerformanceByPlasticityKernelMass)

# meanPerformanceByPlasticityPlantDensity <- lm(plantDensity.sp.mu ~ plantDensity.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
# anova(meanPerformanceByPlasticityPlantDensity)
# summary(meanPerformanceByPlasticityPlantDensity)

meanPerformanceByPlasticityTestWt <- lm(combineTestWeight.sp.mu ~ combineTestWeight.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityTestWt)
summary(meanPerformanceByPlasticityTestWt)

meanPerformanceByPlasticityCombineMoisture <- lm(combineMoisture.sp.mu ~ combineMoisture.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityCombineMoisture)
summary(meanPerformanceByPlasticityCombineMoisture)

meanPerformanceByPlasticityFlagLeafHeight <- lm(flagLeafHeight.sp.mu ~ flagLeafHeight.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityFlagLeafHeight)
summary(meanPerformanceByPlasticityFlagLeafHeight)

meanPerformanceByPlasticityEarHeight <- lm(earHeight.sp.mu ~ earHeight.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityEarHeight)
summary(meanPerformanceByPlasticityEarHeight)

meanPerformanceByPlasticityGDDToAnthesis <- lm(GDDToAnthesis.sp.mu ~ GDDToAnthesis.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityGDDToAnthesis)
summary(meanPerformanceByPlasticityGDDToAnthesis)

meanPerformanceByPlasticityGDDToSilk <- lm(GDDToSilk.sp.mu ~ GDDToSilk.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityGDDToSilk)
summary(meanPerformanceByPlasticityGDDToSilk)

meanPerformanceByPlasticityAnthesisSilkingIntervalGDD <- lm(anthesisSilkingIntervalGDD.sp.mu ~ anthesisSilkingIntervalGDD.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityAnthesisSilkingIntervalGDD)
summary(meanPerformanceByPlasticityAnthesisSilkingIntervalGDD)

meanPerformanceByPlasticityCobMass  <- lm(shelledCobMass.sp.mu ~ shelledCobMass.sp.b + meanParentAge, data = hybridsNOLNK22.pl)
anova(meanPerformanceByPlasticityCobMass)
summary(meanPerformanceByPlasticityCobMass)
# How far apart in rank are the hybrids predicted to crossover by FW?
FWRankChangeRankDistance <- cxData %>%
  filter(crossover=='Y') %>%
  mutate(genotypeRank2 = as.numeric(genotypeRank2)) %>%
  rowwise() %>%
  mutate(rankDistance = abs(genotypeRank1 - genotypeRank2))

bestEnvYield <- filter(hybridsNOLNK22, environment=='2023:Ames:0:High') %>%
  group_by(genotype) %>%
  summarise(bestEnvMeanYield = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
  full_join(sortedhybridsCommon, join_by(genotype), keep = FALSE, suffix = c('', '')) %>%
  select(genotype, rankOrder, bestEnvMeanYield, yieldPerAcre.sp.FWB, yieldPerAcre.sp.mu)


FWRankChangeRankDistance <- FWRankChangeRankDistance %>%
  left_join(bestEnvYield, join_by(genotypeRank1==rankOrder), keep = FALSE, suffix = c('', '')) %>%
  rename(genotype1BestEnvYieldReal = bestEnvMeanYield, 
         genotype1FWB = yieldPerAcre.sp.FWB) %>%
  left_join(bestEnvYield, join_by(genotypeRank2==rankOrder), keep = FALSE, suffix = c('.1', '.2')) %>%
  rename(genotype2BestEnvYieldReal = bestEnvMeanYield,
         genotype2FWB = yieldPerAcre.sp.FWB) %>%
  rowwise() %>% 
  mutate(yieldDiffBestEnvironmentReal = abs(genotype1BestEnvYieldReal - genotype2BestEnvYieldReal),
         yieldDiffFWB = abs(genotype1FWB - genotype2FWB))


FWRankChangeRankDistanceHist <- FWRankChangeRankDistance %>%
  ggplot(aes(rankDistance)) +
    geom_histogram(binwidth = 1, fill = viridis_pal()(4)[1]) +
    labs(x = 'Hybrid Rank Difference', y = 'Predicted Rank Order Changes') + 
    theme_minimal() +
    theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'none',
        panel.grid = element_blank())
FWRankChangeRankDistanceHist
# ggsave('../RankChangeDistanceHistogram.png', FWRankChangeRankDistanceHist, width = 6.5, height = 4, dpi = 1000, bg = 'white')

# How often does FW predict crossovers?
FWRRankChanges <- abs(cxMatrix) %>%
  as.data.frame() %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)))
numRankChanges <- rowSums(FWRRankChanges)/2
numHybrids <- length(hybridsNOLNK22.pl$genotype)
totalUniquePairs <- (numHybrids * (numHybrids - 1))/2
percentRankChanges <- numRankChanges/totalUniquePairs
percentRankChanges

# # Model relationships with N plasticity in  Ames 2023: are they significant?
# meanPerformanceByNPlasticityYield <- lm(yieldPerAcre.sp.mu ~ yieldPerAcre.sp.b + meanParentAge, data = ames23NResponse.pl)
# anova(meanPerformanceByNPlasticityYield)
# 
# meanPerformanceByNPlasticityHKM <- lm(hundredKernelMass.sp.mu ~ hundredKernelMass.sp.b + meanParentAge, data = ames23NResponse.pl)
# anova(meanPerformanceByNPlasticityHKM)

# # Look at distribution of residuals for yield prediction models
# yieldPredictions <- yieldPredictions %>%
#   rowwise() %>%
#   mutate(residualsRF = predictedYieldRF - yieldPerAcre,
#          residualsExtension = predictedYieldExtension - yieldPerAcre)
# 
# extensionResidualsHistogram <- ggplot(yieldPredictions, aes(residualsExtension)) +
#   geom_histogram() +
#   labs(title = 'Extension Residuals')
# extensionResidualsHistogram
# 
# rfResidualsHistogram <- ggplot(yieldPredictions, aes(residualsRF)) + 
#   geom_histogram() + 
#   labs(title = 'RF Residuals')
# rfResidualsHistogram
# 
# # Are feature importances significantly different from each other?
# anova <- aov(val ~ phenotype, data = rfFeatures)
# tukey <- TukeyHSD(anova)

# interaction importance supplemental figs
# interactionImportancePlantDensity <- plotInteractionImportanceGrid(trait = 'plantDensity', traitLabel = 'Plant Density', 
                                                                      # legendPosition = 'right', legendTextAngle = 0, xAxisLabelAngle = 90,
                                                                      # legendTitle = str_wrap('Normalized Interaction Importance Score', 20))
# interactionImportanceSLegend <- get_legend(interactionImportancePlantDensity)
# interactionImportancePlantDensity <- plotInteractionImportanceGrid(trait = 'plantDensity', traitLabel = 'Plant Density', 
#                                                                    legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
# interactionImportancePlantDensity

interactionImportanceGDDToAnthesis <- plotInteractionImportanceGrid(trait = 'GDDToAnthesis', traitLabel = 'GDD to Anthesis*', 
                                                                      legendPosition = 'right', legendTextAngle = 0, 
                                                                      legendTitle = str_wrap('Normalized Interaction Importance Score', 20), 
                                                                      xAxisLabelAngle = 90)
interactionImportanceGDDToAnthesis
interactionImportanceSLegend <- get_plot_component(interactionImportanceGDDToAnthesis, 'guide-box-right')
interactionImportanceGDDToAnthesis <- plotInteractionImportanceGrid(trait = 'GDDToAnthesis', traitLabel = 'GDD to Anthesis*', 
                                                                    legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceGDDToAnthesis
interactionImportanceGDDToSilk <- plotInteractionImportanceGrid(trait = 'GDDToSilk', traitLabel = 'GDD to Silk*', 
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceGDDToSilk
interactionImportanceAnthesiSilkingIntervalGDD <- plotInteractionImportanceGrid(trait = 'anthesisSilkingIntervalGDD', traitLabel = ' Anthesis Silking Interval*', 
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceAnthesiSilkingIntervalGDD
interactionImportanceShelledCobWidth <- plotInteractionImportanceGrid(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width', 
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceShelledCobWidth
interactionImportanceKRN <- plotInteractionImportanceGrid(trait = 'kernelRowNumber', traitLabel = 'Kernel Row Number', 
                                                          legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKRN
interactionImportanceEarLength <- plotInteractionImportanceGrid(trait = 'earLength', traitLabel = 'Ear Length', 
                                                                legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarLength
interactionImportanceEarFillLength <- plotInteractionImportanceGrid(trait = 'earFillLength', traitLabel = 'Ear Fill Length*', 
                                                                    legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarFillLength
interactionImportanceKPR <- plotInteractionImportanceGrid(trait = 'kernelsPerRow', traitLabel = 'Kernels Per Row*', 
                                                          legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKPR
interactionImportanceKPE <- plotInteractionImportanceGrid(trait = 'kernelsPerEar', traitLabel = 'Kernels Per Ear', 
                                                          legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKPE
interactionImportanceEarWidth <- plotInteractionImportanceGrid(trait = 'earWidth', traitLabel = 'Ear Width', 
                                                               legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarWidth
interactionImportanceHKM <- plotInteractionImportanceGrid(trait = 'hundredKernelMass', traitLabel = 'Hundred Kernel Mass', 
                                                          legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceHKM
interactionImportanceKernelMass <- plotInteractionImportanceGrid(trait = 'kernelMassPerEar', traitLabel = 'Kernel Mass Per Ear', 
                                                                 legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceKernelMass
interactionImportanceCombineTestWeight <- plotInteractionImportanceGrid(trait = 'combineTestWeight', traitLabel = 'Test Weight',
                                                                        legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceCombineTestWeight
interactionImportanceCombineMoisture <- plotInteractionImportanceGrid(trait = 'combineMoisture', traitLabel = 'Harvest Moisture',
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceCombineMoisture
interactionImportanceFlagLeafHeight <- plotInteractionImportanceGrid(trait = 'flagLeafHeight', traitLabel = 'Flag Leaf Height',
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceFlagLeafHeight
interactionImportanceEarHeight <- plotInteractionImportanceGrid(trait = 'earHeight', traitLabel = 'Ear Height',
                                                                     legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceEarHeight

interactionImportanceShelledCobMass <- plotInteractionImportanceGrid(trait = 'shelledCobMass', traitLabel = 'Shelled Cob Mass', 
                                                                      legendPosition = 'none', legendTextAngle = 90, legendTitle = '', xAxisLabelAngle = 90)
interactionImportanceShelledCobMass


supplInteractionImportancePlots <- plot_grid(interactionImportanceSLegend, interactionImportanceEarHeight, interactionImportanceFlagLeafHeight, interactionImportanceGDDToAnthesis, 
                                             interactionImportanceGDDToSilk, interactionImportanceAnthesiSilkingIntervalGDD, interactionImportanceCombineTestWeight, 
                                             interactionImportanceCombineMoisture, interactionImportanceShelledCobMass, interactionImportanceShelledCobWidth, 
                                             interactionImportanceKRN, interactionImportanceEarLength, interactionImportanceEarFillLength, 
                                             interactionImportanceKPR, interactionImportanceKPE, interactionImportanceEarWidth, 
                                             interactionImportanceHKM, interactionImportanceKernelMass,
                                             nrow = 6, ncol = 3, labels = c('', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q'))
supplInteractionImportancePlots

# supplInteractionImportancePlots <- plot_grid(supplInteractionImportancePlots, ncol = 1, rel_heights = c(1.8, 0.15))
# ggsave('../figSInteractionImportancePlots.svg', plot = supplInteractionImportancePlots, width = 5.5, height = 11,
#        units = 'in', dpi = 1000, bg = 'white')

interactionImportanceDFWidespread <- sigCrossovers %>%
  filter(combineTestWeightScoreNormalized >= 0.10 | combineMoistureScoreNormalized >= 0.10) %>%
  select(genotype1, genotype2, combineTestWeightScoreNormalized, combineMoistureScoreNormalized)

for(i in 1:length(phenotypes))
{
  print(plotNPlasticityCor(trait = phenotypes[i], traitLabel = phenotypeLabels[i], legendPosition = 'bottom'))
}


# N plasticity cor suppl figs
nPlasticityCorShelledCobWidth <- plotNPlasticityCor(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width',
                                                    legendPosition = 'right')
supplNPlasticityCorLegend <- get_legend(nPlasticityCorShelledCobWidth)
nPlasticityCorShelledCobWidth <- plotNPlasticityCor(trait = 'shelledCobWidth', traitLabel = 'Shelled Cob Width',
                                                    legendPosition = 'none')
nPlasticityCorShelledCobWidth
nPlasticityCorKRN <- plotNPlasticityCor(trait = 'kernelRowNumber', traitLabel = 'Kernel Row Number',
                                        legendPosition = 'none')
nPlasticityCorKRN
nPlasticityCorEarLength <- plotNPlasticityCor(trait = 'earLength', traitLabel = 'Ear Length',
                                              legendPosition = 'none')
nPlasticityCorEarLength
# nPlasticityCorEarFillLength <- plotNPlasticityCor(trait = 'earFillLength', traitLabel = 'Ear Fill Length*', 
#                                                   legendPosition = 'none')
# nPlasticityCorEarFillLength
# nPlasticityCorKPR <- plotNPlasticityCor(trait = 'kernelsPerRow', traitLabel = 'Kernels Per Row*',
#                                         legendPosition = 'none')
# nPlasticityCorKPR
nPlasticityCorKPE <- plotNPlasticityCor(trait = 'kernelsPerEar', traitLabel = 'Kernels Per Ear',
                                        legendPosition = 'none')
nPlasticityCorKPE
nPlasticityCorEarWidth <- plotNPlasticityCor(trait = 'earWidth', traitLabel = 'Ear Width',
                                             legendPosition = 'none')
nPlasticityCorEarWidth
nPlasticityCorKernelMass <- plotNPlasticityCor(trait = 'kernelMassPerEar', traitLabel = 'Kernel Mass Per Ear',
                                               legendPosition = 'none')
nPlasticityCorKernelMass
# nPlasticityCorPlantDensity <- plotNPlasticityCor(trait = 'plantDensity', traitLabel = 'Plant Density',
#                                                legendPosition = 'none')
# nPlasticityCorPlantDensity
nPlasticityCorTestWt <- plotNPlasticityCor(trait = 'combineTestWeight', traitLabel = 'Test Weight',
                                               legendPosition = 'none')
nPlasticityCorTestWt

nPlasticityCorHarvestMoisture <- plotNPlasticityCor(trait = 'combineMoisture', traitLabel = 'Harvest Moisture',
                                           legendPosition = 'none')
nPlasticityCorHarvestMoisture

nPlasticityCorFlagLeafHeight <- plotNPlasticityCor(trait = 'flagLeafHeight', traitLabel = 'Flag Leaf Height',
                                           legendPosition = 'none')
nPlasticityCorFlagLeafHeight

nPlasticityCorEarHeight <- plotNPlasticityCor(trait = 'earHeight', traitLabel = 'Ear Height',
                                           legendPosition = 'none')
nPlasticityCorEarHeight

# nPlasticityCorGDDToAnthesis <- plotNPlasticityCor(trait = 'GDDToAnthesis', traitLabel = 'GDD to Anthesis',
#                                            legendPosition = 'none')
# nPlasticityCorGDDToAnthesis
# 
# nPlasticityCorGDDToSilk <- plotNPlasticityCor(trait = 'GDDToSilk', traitLabel = 'GDD to Silk',
#                                            legendPosition = 'none')
# nPlasticityCorGDDToSilk
# 
# nPlasticityCorASI <- plotNPlasticityCor(trait = 'anthesisSilkingIntervalGDD', traitLabel = 'Anthesis-Silking Interval',
#                                            legendPosition = 'none')
# nPlasticityCorASI

nPlasticityCorCobMass <- plotNPlasticityCor(trait = 'shelledCobMass', traitLabel = 'Shelled Cob Mass',
                                           legendPosition = 'none')
nPlasticityCorCobMass


supplNPlasticityCorPlots <- plot_grid(supplNPlasticityCorLegend, nPlasticityCorEarHeight, nPlasticityCorFlagLeafHeight, 
                                      nPlasticityCorTestWt, nPlasticityCorHarvestMoisture, nPlasticityCorCobMass, 
                                      nPlasticityCorShelledCobWidth, nPlasticityCorKRN, nPlasticityCorEarLength, 
                                      nPlasticityCorKPE, nPlasticityCorEarWidth, nPlasticityCorKernelMass, 
                                      nrow = 4, ncol = 3, labels = c('', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'))
# ggsave('../figSNPlasticityCorPlots.png', plot = supplNPlasticityCorPlots, width = 8.25, height = 11, units = 'in', dpi = 1000, bg = 'white')

nassYield <- read.csv('../NASSCornYield_2022_2023_IA_NE.csv') %>%
  rename(yield = Value,
         cv = `CV....`) %>%
  rowwise() %>% 
  mutate(sem = 0.01*cv*yield) %>%
  mutate(lower = yield - 1.96*sem,
         upper = yield + 1.96*sem)
min(nassYield$lower)

g2f2014 <- read.csv('../g2f_2014_hybrid_data_clean.csv')
g2f2015 <- read.csv('../g2f_2015_hybrid_data_clean.csv')
g2f2016 <- read.csv('../g2f_2016_hybrid_data_clean.csv')
g2f2017 <- read.csv('../g2f_2017_hybrid_data_clean.csv')
g2f2018 <- read.csv('../g2f_2018_hybrid_data_clean.csv')
g2f2019 <- read.csv('../g2f_2019_phenotypic_clean_data.csv')
g2f2020 <- read.csv('../g2f_2020_phenotypic_clean_data.csv') %>%
  mutate(Pass = as.integer(Pass))
g2f2021 <- read.csv('../g2f_2021_phenotypic_clean_data.csv') %>%
  mutate(Plot = as.integer(Plot))
g2f2022 <- read.csv('../g2f_2022_phenotypic_clean_data.csv') %>%
  mutate(Plot_ID = as.integer(Plot_ID))

g2f <- bind_rows(g2f2014, g2f2015, g2f2016, g2f2017, g2f2018, g2f2019, g2f2020, g2f2021, g2f2022)
g2fInbreds <- c("", "W22-UNIFORM MU STRAIN", "TX714", "PHZ51", "PHW65", "PI197", "PHTD5", "PHT69", "PHRE1", "PHR03", "PHP02", 
                "PHK76", "PHJ89", "PHJ40", "PH207", "PHAJ0", "PHB47", "MO17", "LH244", "LH82", "LH185", "LH195", "LH145",
                "B84", "B73")
g2fHybrids <- filter(g2f, !(Pedigree %in% g2fInbreds)) %>%
  rowwise() %>%
  mutate(locationYear = str_c(Year, Field.Location, sep = ':'),
         standCount = case_when(!is.na(Stand.Count..plants.) ~ Stand.Count..plants.,
                                !is.na(Stand....) ~ Stand....,
                                !is.na(Stand.Count....of.plants.) ~ Stand.Count....of.plants.),
         anthesisDate = case_when(!is.na(Anthesis..date.) ~ Anthesis..date.,
                                  !is.na(Anthesis..MM.DD.YY.) ~ Anthesis..MM.DD.YY.),
         silkDate = case_when(!is.na(Silking..date.) ~ Silking..date.,
                              !is.na(Silking..MM.DD.YY.) ~ Silking..MM.DD.YY.),
         daysToAnthesis = case_when(!is.na(Pollen.DAP..days.) ~ Pollen.DAP..days.,
                                    !is.na(Anthesis..days.) ~ Anthesis..days.),
         daysToSilk = case_when(!is.na(Silk.DAP..days.) ~ Silk.DAP..days.,
                                !is.na(Silking..days.) ~ Silking..days.),
         rootLodging = case_when(!is.na(Root.Lodging..plants.) ~ Root.Lodging..plants.,
                                 !is.na(Root.Lodging....of.plants.) ~ Root.Lodging....of.plants.),
         stalkLodging = case_when(!is.na(Stalk.Lodging..plants.) ~ Stalk.Lodging..plants.,
                                  !is.na(Stalk.Lodging....of.plants.) ~ Stalk.Lodging....of.plants.),
         testWeight = case_when(!is.na(Test.Weight..lbs.bu.) ~ Test.Weight..lbs.bu.,
                                !is.na(Test.Weight..lbs.) ~ Test.Weight..lbs.)) %>%
  select(!c(Stand.Count..plants., Stand...., Stand.Count....of.plants., Anthesis..date., Anthesis..MM.DD.YY., Silking..date., 
            Silking..MM.DD.YY., Pollen.DAP..days., Anthesis..days., Silk.DAP..days., Silking..days., Root.Lodging..plants.,
            Root.Lodging....of.plants., Stalk.Lodging..plants., Stalk.Lodging....of.plants., Test.Weight..lbs.bu., Test.Weight..lbs.))
g2fLocationYears <- unique(g2fHybrids$locationYear)
locationYearData <- filter(g2fHybrids, locationYear==g2fLocationYears[1])
g2fCommonHybrids <- unique(locationYearData$Pedigree)

for(i in 2:length(g2fLocationYears))
{
  locationYearData <- filter(g2fHybrids, locationYear==g2fLocationYears[i])
  locationYearHybrids <- unique(locationYearData$Pedigree)
  g2fCommonHybrids <- intersect(g2fCommonHybrids, locationYearHybrids)
}

g2fPhenotypes <- colnames(g2fHybrids)[c(22:26, 50:53, 57, 59:66)]
locationYearsPerPhenotype <- tibble(phenotype = g2fPhenotypes, numLocationYears = NULL)
for(i in 1:length(g2fPhenotypes))
{
  phenotype <- filter(g2fHybrids, !is.na(.data[[g2fPhenotypes[i]]]))
  locationYearsPerPhenotype$numLocationYears[i] <- length(unique(phenotype$locationYear))
}

locationYearsPerHybrid <- tibble(hybrid = unique(g2fHybrids$Pedigree), numLocationYears = NULL)
for(i in 1:length(unique(locationYearsPerHybrid$hybrid)))
{
  hybridData <- filter(g2fHybrids, Pedigree==locationYearsPerHybrid$hybrid[i])
  locationYearsPerHybrid$numLocationYears[i] <- length(unique(hybridData$locationYear))
}

locationYearsPerHybridPlot <- ggplot(locationYearsPerHybrid, aes(numLocationYears)) + 
  geom_histogram(binwidth = 5) +
  labs(x = 'Location Years', y = 'Number of Hybrids') +
  theme_minimal()
locationYearsPerHybridPlot

hybridsPerLocationYear <- g2fHybrids %>%
  group_by(locationYear, Pedigree) %>% 
  summarise(n = n()) %>% 
  summarise(n = n())

# which phenotypes are N-responsive?
nResponseViolinData <- filter(hybrids, !(str_detect(environment, 'Missouri Valley')|str_detect(environment, '2023:North Platte'))) %>%
  rowwise() %>%
  mutate(locationYear = str_c(year, location, sep = ':'))

plotPhenotypeNitrogenResponseViolins <- function(data = nResponseViolinData, phenotype, phenotypeLabel)
{
  AOV <- aov(as.formula(paste0(phenotype, ' ~ locationYear*nitrogenTreatment')), data = nResponseViolinData)
  Tukey <- TukeyHSD(AOV)
  sigComp <- Tukey[["locationYear:nitrogenTreatment"]] %>%
    as_tibble(rownames = 'comp', .name_repair = ~str_remove_all(., ' '))
  sigComp <- filter(sigComp, padj < 0.05) %>% 
    rowwise() %>%
    mutate(env1 = str_split_i(comp, '-', 1),
           env2 = str_split_i(comp, '-', 2)) %>%
    mutate(locationYear1 = str_split_fixed(env1, ':', 3) %>%
             str_flatten(':'),
           locationYear2 = str_split_fixed(env2, ':', 3) %>%
             str_flatten(':'),
           nitrogenTreatment1 = str_split_i(env1, ':', 3),
           nitrogenTreatment2 = str_split_i(env2, ':', 3)) %>%
    filter(locationYear1==locationYear2)
  
  # if(length(sigComp$locationYear1 > 0))
  # {
    p <- ggplot(nResponseViolinData, aes(nitrogenTreatment, .data[[phenotype]], fill = nitrogenTreatment)) + 
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
      # geom_text(data = sigComp, mapping = aes(x = 'Low', y = 0), label = nitrogenTreatment1) +
      # geom_text(data = sigComp, mapping = aes(x = 'High', y = 0), label = nitrogenTreatment2) +
      scale_fill_manual(values = nitrogenColors) + 
      facet_wrap(vars(locationYear)) + 
      labs(x = 'Nitrogen Level', y = phenotypeLabel, fill = '') + 
      theme_minimal() +
      theme(axis.text.x = element_text(color = 'black', size = 9),
            axis.text.y = element_text(color = 'black', size = 9),
            text = element_text(color = 'black', size = 9),
            legend.position = 'none',
            panel.grid = element_blank())
    print(p)
  # }
}

for(i in 1:length(phenotypes))
{
  trait <- paste0(phenotypes[i], '.sp')
  plotPhenotypeNitrogenResponseViolins(phenotype = trait, phenotypeLabel = phenotypeLabels[i])
}

hybridsExtremeValues <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.3_noExtremeValueRemoval.csv') %>% 
  filter(!str_detect(qrCode, 'Block ID:')) %>%
  filter(!str_detect(qrCode, 'AKW'))
hybrids122 <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.3.csv') %>%
    filter(location!='') %>%
    mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'High', 'Medium'))) %>%
    rowwise() %>%
    # Since we are making an environment variable based on year, location, irrigationProvided, and nitrogenTreatment,
    # let's have an option to keep 2022 NP as one location
    mutate(semanticLocation = case_when(location %in% c('North Platte1', 'North Platte2', 'North Platte3') ~ 'North Platte', .default = location),
           environment = str_c(year, semanticLocation, irrigationProvided, nitrogenTreatment, sep = ':'))

nonMissingVals122 <- 0
totalPlots122 <- length(hybrids122$qrCode)
# vars <- c('anthesisDate', 'silkDate', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk',
#           'anthesisSilkingIntervalSilkingGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'combineYield', 'yieldPerAcre',
#           'combineMoisture', 'combineTestWeight', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow',
#           'kernelRowNumber', 'kernelsPerEar', 'shelledCobMass', 'percentMoisture', 'percentStarch', 'percentProtein', 'percentOil',
#           'percentFiber', 'percentAsh', 'kernelColor', 'percentLodging', 'totalStandCount')
vars <- phenotypes
hybrids122 <- hybrids122 %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
print('dataset')
for(var in vars)
{
  numMissingVals <- as.numeric(sum(is.na(hybrids122[[var]])))
  numNotMissing <- totalPlots122 - numMissingVals
  nonMissingVals122 <- nonMissingVals122 + numNotMissing
  print(var)
  print(numNotMissing)
}

nonMissingValsExtreme <- 0
totalPlotsExtreme <- length(hybridsExtremeValues$qrCode)
# vars <- c('anthesisDate', 'silkDate', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk',
#           'anthesisSilkingIntervalSilkingGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'combineYield', 'yieldPerAcre',
#           'combineMoisture', 'combineTestWeight', 'earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow',
#           'kernelRowNumber', 'kernelsPerEar', 'shelledCobMass', 'percentMoisture', 'percentStarch', 'percentProtein', 'percentOil',
#           'percentFiber', 'percentAsh', 'kernelColor', 'percentLodging', 'totalStandCount')
vars <- phenotypes
hybridsExtremeValues <- hybridsExtremeValues %>%
  mutate(across(is.character, ~case_when(.=='' ~ NA, .default = .)))
print('with extreme values')
for(var in vars)
{
  numMissingVals <- as.numeric(sum(is.na(hybridsExtremeValues[[var]])))
  numNotMissing <- totalPlotsExtreme - numMissingVals
  nonMissingValsExtreme <- nonMissingValsExtreme + numNotMissing
  print(var)
  print(numNotMissing)
}

interactionImportanceConceptualScores <- sigCrossovers %>%
  filter(if_any(contains('yieldPerAcreScore.'), ~ .x == 2)) %>%
  # select(c(genotype1, genotype2, contains('yieldPerAcreScore'))) %>%
  filter(if_any(contains('yieldPerAcreScore.'), ~ .x == 1)) %>%
  filter(genotype1=='PHK56 X 3IIH6' & genotype2=='PHP02 X PHJ89')%>%
  # select(contains('yieldPerAcreScore.E11'))
  select(genotype1, genotype2, yieldPerAcreSigDiff.E7, yieldPerAcreSigDiff.E15, yieldPerAcreSigDiff.E27, 
         yieldPerAcreSigDiff.E1, yieldPerAcreRank.E7.G1, yieldPerAcreRank.E7.G2, yieldPerAcreRank.E27.G1,
         yieldPerAcreRank.E27.G2, yieldPerAcreRank.E15.G1, yieldPerAcreRank.E15.G2, yieldPerAcreRank.E1.G1, 
         yieldPerAcreRank.E1.G2, yieldPerAcreScore.E7.27, yieldPerAcreScore.E15.27, yieldPerAcreScore.E1.27)
View(interactionImportanceConceptualScores)

scoreZeroData <- filter(hybrids, (genotype %in% c('PHK56 X 3IIH6', 'PHP02 X PHJ89')) & 
                      (environment %in% c('2023 Crawfordsville Low', '2022 Ames High'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 Ames High')))
scoreZeroEnvs <- filter(hybrids, (environment %in% c('2023 Crawfordsville Low', '2022 Ames High'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 Ames High')))

scoreZeroPlot <- ggplot(scoreZeroData, aes(environment, yieldPerAcre.sp, 
                                           color = genotype, 
                                           group = genotype)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  geom_boxplot(aes(environment, yieldPerAcre.sp, group = environment), 
              data = scoreZeroEnvs, 
              color = 'black', fill = NA) + 
  annotate('text', x = '2023 Crawfordsville Low', y = 200, label = '*', size = 9, color = 'black') +
  scale_color_manual(values = viridis_pal()(4)[1:2]) +
  scale_x_discrete(breaks = c('2023 Crawfordsville Low', '2022 Ames High'),
                   labels = c(str_wrap('2023 Crawfordsville Low', 16), str_wrap('2022 Ames High', 16))) +
  scale_y_continuous(limits = c(10, 200)) +
  labs(x = 'Environment', y = 'Yield (bushels/acre)', color = 'Genotype', 
       title = 'Interaction Importance Score = 0') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        legend.position = 'none',
        panel.grid = element_blank())
scoreZeroPlot

scoreOneData <- filter(hybrids, (genotype %in% c('PHK56 X 3IIH6', 'PHP02 X PHJ89')) & 
                          (environment %in% c('2023 Crawfordsville Low', '2022 North Platte LI Low'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 North Platte LI Low')))
scoreOneEnvs <- filter(hybrids, (environment %in% c('2023 Crawfordsville Low', '2022 North Platte LI Low'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 North Platte LI Low')))

scoreOnePlot <- ggplot(scoreOneData, aes(environment, yieldPerAcre.sp, color = genotype, group = genotype)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  geom_boxplot(aes(environment, yieldPerAcre.sp, group = environment), 
              data = scoreOneEnvs, 
              color = 'black', fill = NA) + 
  annotate('text', x = '2023 Crawfordsville Low', y = 200, label = '*', size = 9, color = 'black') +
  scale_color_manual(values = viridis_pal()(4)[1:2]) +
  scale_x_discrete(breaks = c('2023 Crawfordsville Low', '2022 North Platte LI Low'),
                   labels = c(str_wrap('2023 Crawfordsville Low', 16), str_wrap('2022 North Platte LI Low', 16))) +
  scale_y_continuous(limits = c(10, 200)) +
  labs(x = 'Environment', y = 'Yield (bushels/acre)', color = 'Genotype', 
       title = 'Interaction Importance Score = 1') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        legend.position = 'none',
        panel.grid = element_blank())
scoreOnePlot

scoreTwoData <- filter(hybrids, (genotype %in% c('PHK56 X 3IIH6', 'PHP02 X PHJ89')) & 
                         (environment %in% c('2023 Crawfordsville Low', '2022 Lincoln High'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 Lincoln High')))
scoreTwoEnvs <- filter(hybrids, (environment %in% c('2023 Crawfordsville Low', '2022 Lincoln High'))) %>%
  mutate(environment = factor(environment, levels = c('2023 Crawfordsville Low', '2022 Lincoln High')))

scoreTwoPlot <- ggplot(scoreTwoData, aes(environment, yieldPerAcre.sp, color = genotype, group = genotype)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  geom_boxplot(aes(environment, yieldPerAcre.sp, group = environment), 
              data = scoreTwoEnvs, 
              color = 'black', fill = NA) + 
  annotate('text', x = '2023 Crawfordsville Low', y = 200, label = '*', size = 9, color = 'black') +
  annotate('text', x = '2022 Lincoln High', y = 200, label = '*', size = 9, color = 'black') +
  scale_color_manual(values = viridis_pal()(4)[1:2]) +
  scale_x_discrete(breaks = c('2023 Crawfordsville Low', '2022 Lincoln High'),
                   labels = c(str_wrap('2023 Crawfordsville Low', 16), str_wrap('2022 Lincoln High', 16))) +
  scale_y_continuous(limits = c(10, 200)) +
  labs(x = 'Environment', y = 'Yield (bushels/acre)', color = 'Genotype', 
       title = 'Interaction Importance Score = 2') +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
        legend.position = 'bottom',
        panel.grid = element_blank())
scoreTwoPlot

interactionImportanceConceptualFigure <- plot_grid(scoreZeroPlot, scoreOnePlot, scoreTwoPlot, ncol = 1)
# ggsave('../interactionImportanceConceptual.png', width = 6.5, height = 9, units = 'in', dpi = 1000, bg = 'white')

fig4left <- plot_grid(cxHeatmap, heatmap, ncol = 1, labels = c('A', 'C'))
fig4 <- plot_grid(fig4left, interactionImportanceConceptualFigure, nrow = 1, labels = c('', 'B'))
fig4
ggsave('../fig4HighRes.svg', width = 6.5, height = 8, units = 'in', dpi = 1000, bg = 'white')

maxInteractionImportanceScore <- sigCrossovers %>%
  filter(genotype1=="2369 X LH123HT" & genotype2=="K201 X OS426") %>%
  select(c(genotype1, genotype2, contains('yieldPerAcreRC')))

noImportantInteractions <- sigCrossovers %>%
  filter(!if_any(contains('yieldPerAcreScore.'), ~ .x > 0))

lowInteractionImportanceScores <- sigCrossovers %>%
  filter(yieldPerAcreScoreNormalized <= 0.05)

# mlcasbottom <- plot_grid(meanParentPlot, featureImportance, 
#                          yieldPredictionsPlot, percentMeanPlotExtremePlasticity, 
#                          nrow = 2, labels = c('B', 'C', 'D', 'E'), rel_heights = c(0.4, 0.6))
# mlcas <- plot_grid(experimentalDesign, mlcasbottom, nrow = 2, labels = c('A', ''), rel_heights = c(0.4, 0.6))
# mlcas
# ggsave('../mlcas.svg', width = 6.5, height = 7.25, units = 'in', dpi = 1000, bg = 'white')

amesNResponseLAH <- hybrids %>%
  filter(genotype %in% c('LH195 X LH185', 'PHB47 X PHJ89', 'F42 X MO17')) %>%
  filter(locationYear == '2023 Ames') %>%
  group_by(genotype, locationYear, nitrogenTreatment) %>%
  summarise(yieldPerAcreMean = mean(yieldPerAcre.sp, na.rm = TRUE)) %>% 
  bind_rows(nResponsePopulation) %>%
  filter(locationYear=='2023 Ames') %>%
  mutate(genotype = factor(genotype, levels = c('Population', 'F42 X MO17', 'PHB47 X PHJ89', 'LH195 X LH185')))

ames23 <- filter(hybrids, locationYear=='2023 Ames')

amesNPlasticityGenotypeLines <- ggplot(amesNResponseLAH, aes(nitrogenTreatment, yieldPerAcreMean, color = genotype, group = genotype, linetype = genotype)) +
  geom_line(size = 1.5) + 
  geom_boxplot(aes(nitrogenTreatment, yieldPerAcre.sp), data = ames23,  color = 'black', fill = 'transparent', inherit.aes = FALSE) +
  scale_color_manual(values = moma.colors('VanGogh', 4, direction = -1)[c(3, 1, 2, 4)]) +
  scale_linetype_manual(values = c('dashed', 'solid', 'solid', 'solid')) + 
  labs(x = 'Nitrogen Treatment', y = 'Yield (bushels/acre)', color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9),
        text = element_text(color = 'black', size = 9),
        legend.position = 'top',
        panel.grid = element_blank())
amesNPlasticityGenotypeLines 

proposalMPPData <- hybridsNOLNK22.pl %>%
  arrange(yieldPerAcre.sp.b)
proposalMPPData$relPlasticity <- c(rep('Less Responsive Hybrids', 29), rep('Other Hybrids', 59), 
                                   rep('Most Responsive Hybrids', 29))
proposalMPPData <- mutate(proposalMPPData, relPlasticity = factor(relPlasticity, levels = c('Most Responsive Hybrids', 'Other Hybrids', 'Less Responsive Hybrids')))

proposalMPPlot <- ggplot(proposalMPPData, aes(yieldPerAcre.sp.mu, yieldPerAcre.sp.b, color = relPlasticity)) +
  geom_point() +
  geom_hline(yintercept=1) +
  scale_color_manual(values = moma.colors('VanGogh', 3, direction = -1)) +
  labs(x = 'Hybrid Mean Yield (bushels/acre)', y = 'Yield Linear Plasticity', color = NULL) + 
  theme_minimal() +
  theme(axis.text.x = element_text(color = 'black', size = 9),
        axis.text.y = element_text(color = 'black', size = 9),
        legend.text = element_text(color = 'black', size = 9, margin = margin(0, 0, 0, 0)),
        text = element_text(color = 'black', size = 9),
        legend.position = 'top',
        panel.grid = element_blank())
proposalMPPlot


# proposal <- plot_grid(amesNPlasticityGenotypeLines, proposalMPPlot, ncol = 1, labels = 'AUTO')
# ggsave('../proposal.svg', width = 3.5, height = 7, units = 'in', dpi = 1000, bg = 'white')
