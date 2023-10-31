library(tidyverse)
library(readxl)
library(grDevices)
source('src/HybridHIPSAnalysis.R')
df2022Inbreds <- read.csv('/Users/jensinadavis/Downloads/SAMS_2022_V3.2_INBREDS.csv')
response_vars <- c('earHeight', 'flagLeafHeight')
inbreds2022.wide <- plotRepCorr(df2022Inbreds, 'nitrogenTreatment', 'genotype', response_vars, 'location')
sb <- filter(inbreds2022.wide, location=='Scottsbluff')
cor(sb$earHeight.1, sb$earHeight.2, use = 'complete.obs')
acl <- filter(inbreds2022.wide, location %in% c('Ames', 'Crawfordsville', 'Lincoln'))
cor(acl$earHeight.1, acl$earHeight.2, use = 'complete.obs')

lnk2023InbredHeights <- read_excel('data/2023/inbreds/231018 Finalized Height Data from Lopez-Corona - Additional info added by Turkus.xlsx', sheet = 'Combined File', col_names = c('experiment', 'plotNumber', 'genotype', 'pi', 'row', 'range', 'earHeight1', 'flagLeafHeight1', 'tasselTipHeight1', 'earHeight2', 'flagLeafHeight2', 'tasselTiplHeight2', 'notes', 'person'), skip = 1)
lnk2023InbredHeights <- lnk2023InbredHeights %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         earHeight = mean(c(as.numeric(earHeight1), as.numeric(earHeight2)), na.rm = TRUE) * 12 %>%
           cm(),
         flagLeafHeight = mean(c(as.numeric(flagLeafHeight1), as.numeric(flagLeafHeight2)), na.rm = TRUE) * 12 %>%
           cm(),
         tasselTipHeight = mean(c(as.numeric(tasselTipHeight1), as.numeric(tasselTiplHeight2)), na.rm = TRUE) * 12 %>%
           cm(), 
         location = 'Lincoln', 
         nitrogenTreatment = 'Medium') %>%
  select(!c(ends_with('1'), ends_with('2'), 'experiment', 'pi', 'person')) %>%
  relocate(notes, .after = nitrogenTreatment)
lnk2023.wide <- plotRepCorr(lnk2023InbredHeights, 'nitrogenTreatment', 'genotype', c('earHeight', 'flagLeafHeight', 'tasselTipHeight'), 'location')

#Export to csv
write.csv(lnk2023InbredHeights, file = 'outData/2023_Inbred_Height_Data_Lincoln.csv', quote = FALSE, sep = ',', na = '', row.names = FALSE)
