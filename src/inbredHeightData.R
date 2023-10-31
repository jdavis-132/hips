library(tidyverse)

df2022Inbreds <- read.csv('/Users/jensinadavis/Downloads/SAMS_2022_V3.2_INBREDS.csv')
response_vars <- c('earHeight', 'flagLeafHeight')
inbreds2022.wide <- plotRepCorr(df2022Inbreds, 'nitrogenTreatment', 'genotype', response_vars, 'location')
sb <- filter(inbreds2022.wide, location=='Scottsbluff')
cor(sb$earHeight.1, sb$earHeight.2, use = 'complete.obs')
acl <- filter(inbreds2022.wide, location %in% c('Ames', 'Crawfordsville', 'Lincoln'))
cor(acl$earHeight.1, acl$earHeight.2, use = 'complete.obs')

