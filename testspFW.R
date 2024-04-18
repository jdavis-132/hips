library(spFW)
library(tidyverse)

earHeight <- read.csv('../../../S24/CSCE879/project/earHeight.csv')

y <- earHeight$earHeight
geno <- earHeight$genotype
environment <- earHeight$sourceEnvironment

spFWModel <- HFWM_est(Y=y, VAR=geno, ENV=environment)

hybrids <- read.csv('analysis/HYBRIDS_2022_2023_SPATIALLYCORRECTED.csv') %>%
  filter(location!='') %>% 
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'))) %>%
  rowwise() %>%
  mutate(across(where(is.numeric), ~case_when(.==-Inf ~ NA, .default = .))) %>%
  filter(!is.na(yieldPerAcre.sp)) %>%
  filter(!str_detect(environment, '2022:Lincoln'))
y <- hybrids$yieldPerAcre.sp
geno <- hybrids$genotype
environment <- hybrids$environment

# Fix: filter for NAs before fitting model!!

spFWModel <- HFWM_est(Y=y, VAR=geno, ENV=environment)
plasticity <- as_tibble(spFWModel$b, rownames = 'genotype')
genotypeRank <- as_tibble(spFWModel$g, rownames = 'genotype')
plasticity <- full_join(plasticity, genotypeRank, join_by(genotype), keep = FALSE, suffix = c('.b', '.g'))
p <- ggplot(plasticity, aes(value.g, value.b)) + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  labs(x = 'BLUP', y = 'plasticity')
p