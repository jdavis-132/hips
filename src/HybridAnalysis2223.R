library(tidyverse)
library(SpATS)
library(lme4)
library(viridis)
library(scales)
library(MoMAColors)
library(wesanderson)
library(ggh4x)
source('src/Functions.R')

# Read in data, order N treatment, create environment variable
# Not sure why, but the input file has the correct number of lines but reading it in we get 377 rows with notes as qrcodes and no data
hybrids <- read.csv('outData/HIPS_HYBRIDS_2022_AND_2023_V2.csv') %>%
  filter(location!='') %>% 
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'High', 'Medium'))) %>%
  rowwise() %>%
  # Since we are making an environment variable based on year, location, irrigationProvided, and nitrogenTreatment, 
  # let's have an option to keep 2022 NP as one location
  mutate(semanticLocation = case_when(location %in% c('North Platte1', 'North Platte2', 'North Platte3') ~ 'North Platte', .default = location),
         environment = str_c(year, semanticLocation, irrigationProvided, nitrogenTreatment, sep = ':'))

# Now we need to spatially correct within an environment
phenotypes <- c("plantDensity", "combineYield", "combineTestWeight", "combineMoisture", "flagLeafHeight", "earHeight", "yieldPerAcre", 
                'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'kernelRowNumber', 'earWidth',
                'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelMassPerEar', 'kernelsPerEar', 'hundredKernelMass', 'earFillLength', 
                'kernelsPerRow')

for(i in phenotypes)
{
  hybrids <- full_join(hybrids, 
                       getSpatialCorrectionsEnvironment(hybrids, i, 'environment'), 
                       by = join_by(environment, plotNumber), 
                       suffix = c('', '.sp'), 
                       keep = FALSE)
}

hybrids <- hybrids %>%
  rowwise() %>%
  mutate(across(everything(), ~case_when(is.null(.) ~ NA, .default = .)))

# # Is there shrinkage toward the mean of a treatment ?
for(i in 1:length(phenotypes))
{
  sp.correction.plot <- ggplot(hybrids, (aes(.data[[phenotypes[i]]], .data[[paste0(phenotypes[i], '.sp')]], color = environment))) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(vars(location, year))
  print(sp.correction.plot)
}

