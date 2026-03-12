library(tidyverse)
final_earleveldata <- read_csv('finalData/HIPS_INBREDS_2022_2023_EARLEVEL_v3.csv')
traits <- c('earLength', 'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow', 'kernelRowNumber', 'kernelsPerEar',
            'hundredKernelMass', 'kernelMassPerEar', 'shelledCobMass', 'earMass')
missing <- final_earleveldata %>% 
  group_by(environment, genotype) %>% 
  mutate(across(all_of(traits), 
                ~sum(!is.na(.x)), 
                .names = 'nonMissingVals_{.col}')) %>%
  filter(if_any(all_of(str_c('nonMissingVals_', traits)), ~ . < 1)) %>% 
  group_by(plotNumber) %>% 
  mutate(earsPerPlot = n())

environments <- unique(final_earleveldata$environment)

for(e in environments)
{
  print(e)
  total_env_genotypes <- n_distinct(final_earleveldata$genotype[final_earleveldata$environment==e])
  print(str_c('Total Genotypes: ', total_env_genotypes))
  for(t in traits)
  {
    nonMissingCol <- str_c('nonMissingVals_', t)
    missing_genotypes <- n_distinct(missing$genotype[missing$environment==e & missing[[nonMissingCol]]==0])
    print(str_c('Genotypes Missing ', t, ': ', missing_genotypes, ' (', round((missing_genotypes/total_env_genotypes)*100, 2), '%)'))
  }
  print('\n')
}
