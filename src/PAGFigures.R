library(tidyverse)

hybridsFinal <- read.csv('outData/HIPS_2022_V3.5_HYBRIDS.csv') %>%
  rowwise() %>%
  mutate(anonymizedLocation = case_when(location=='Scottsbluff' ~ 'Location 1',
                                        str_detect(location, 'North Platte') ~ 'Location 2',
                                        location=='Lincoln' ~ 'Location 3',
                                        location=='Missouri Valley' ~ 'Location 4',
                                        location=='Ames' ~ 'Location 5',
                                        location=='Crawfordsville' ~ 'Location 6'))

sbLNKAmes <- filter(hybridsFinal, location %in% c('Scottsbluff', 'Lincoln', 'Ames'))
sbLNKAmesWide <- sbLNKAmes %>%
  pivot_longer()
  