library(tidyverse)
library(readxl)
library(grDevices)
source('src/HybridHIPSAnalysis.R')
df2022Inbreds <- read.csv('/Users/jensinadavis/Downloads/SAMS_2022_V4.0_INBREDS.csv')
response_vars <- c('earHeight', 'flagLeafHeight')
locationsExceptScottsbluff <- c('Lincoln', 'Missouri Valley', 'Ames', 'CrawfordsVille')
# Do Scottsbluff height obs correlate with height obs of the same genotype in other locations
df2022InbredsWide <- df2022Inbreds %>% 
  filter(earHeight < 400) %>%
  group_by(genotype, location) %>%
  mutate(genotypeObsNum = 1:n()) %>%
  pivot_wider(id_cols = c(genotype, genotypeObsNum), 
              names_from = c(location), 
              values_from = c(earHeight, flagLeafHeight))
for (i in locationsExceptScottsbluff)
{
  sbEarHeight <- 'earHeight_Scottsbluff'
  sbFlagLeafHeight <- 'flagLeafHeight_Scottsbluff'
  loc2EarHeight <- paste0('earHeight_', i)
  loc2FlagLeafHeight <- paste0('flagLeafHeight_', i)
  
  earHeightPlot <- ggplot(df2022InbredsWide, aes(.data[[loc2EarHeight]], .data[[sbEarHeight]])) +
    geom_point() +
    labs(title = i)
  
  flagLeafHeightPlot <- ggplot(df2022InbredsWide, aes(.data[[loc2FlagLeafHeight]], .data[[sbFlagLeafHeight]])) +
    geom_point() + 
    labs(title = i)
  
  print(paste0('Ear Height, ', i, ': ', cor(df2022InbredsWide[[loc2EarHeight]], df2022InbredsWide[[sbEarHeight]], use = 'complete.obs')))
  print(paste0('Flag Leaf Height, ', i, ': ', cor(df2022InbredsWide[[loc2FlagLeafHeight]], df2022InbredsWide[[sbFlagLeafHeight]], use = 'complete.obs')))
  print(earHeightPlot)
  print(flagLeafHeightPlot)
}


inbreds2022.wide <- plotRepCorr(df2022Inbreds, 'nitrogenTreatment', 'genotype', response_vars, 'location')
sb <- filter(df2022Inbreds, location=='Scottsbluff') %>%
  filter(earHeight < 400)
cor(sb$earHeight.1, sb$earHeight.2, use = 'complete.obs')
aclmv <- filter(df2022Inbreds, location %in% c('Ames', 'Crawfordsville', 'Lincoln', 'Missouri Valley'))
aclmv.wide <- plotRepCorr(aclmv, 'nitrogenTreatment', 'genotype', response_vars, 'location')
cor(aclmv$earHeight.1, aclmv$earHeight.2, use = 'complete.obs')
cor(aclmv$flagLeafHeight.1, aclmv$flagLeafHeight.2, use = 'complete.obs')
sbWide <- plotRepCorr(sb, 'nitrogenTreatment', 'genotype', response_vars, 'location')

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


# Check scottsbluff fixed data
sbFixed <- read.csv('/Users/jensinadavis/Downloads/Scottsbluff_Modified_v3 (2).csv')
sbFixed <- sbFixed %>%
  filter(earHeight < 400) %>%
  mutate(genotype = str_to_upper(genotype))
sbFixedWide <- plotRepCorr(sbFixed, 'nitrogenTreatment', 'genotype', response_vars, 'location')

sbEvenRows <- sbFixed %>%
  filter((row %% 2)==0) %>%
  mutate(genotype = str_to_upper(genotype))
sbEvenRowsWide <- plotRepCorr(sbEvenRows, 'nitrogenTreatment', 'genotype', response_vars, 'location')

sbOddRows <- sbFixed  %>%
  filter((row %% 2)!=0) %>%
  mutate(genotype = str_to_upper(genotype))
sbOddRowsWide <- plotRepCorr(sbOddRows, 'nitrogenTreatment', 'genotype', response_vars, 'location')


sbIndex <- read_excel("data/Scottsbluff Inbred HIPS - Summary.xlsx",
                      sheet = "Index (Modified)", col_names = c('row', 'range', 'plotNumber', 'genotype', 'block'),
                      col_types = c(rep('numeric', 3), 'text', 'text'),
                      skip = 1) %>%
  mutate(genotype = str_to_upper(genotype),
         plotNumberNew = case_when(
           row==3 & range < 19 ~ plotNumber - 682,
                                   row==5 ~ plotNumber - 434,
                                   row==7 ~ plotNumber - 186,
                                   row==9 ~ plotNumber + 62,
                                   row==11 ~ plotNumber + 310,
                                   row==13 ~ plotNumber + 558,
                                   row==4 ~ plotNumber - 619 + 2*(range - 3),
                                   row==6 ~ plotNumber - 371 + 2*(range - 3),
                                   row==8 ~ plotNumber - 123 + 2*(range - 3),
                                   row==10 ~ plotNumber + 125 + 2*(range - 3),
                                   row==12 ~ plotNumber + 373 + 2*(range - 3),
                                   row==14 & range < 19 ~ plotNumber + 621 + 2*(range - 3)))
sbIndexOrig <- sbIndex %>%
  select(plotNumber, genotype)
sbIndex$genotypeNew <- rep(NA, length(sbIndex$plotNumberNew))
for(i in 1:length(sbIndex$plotNumberNew))
{
  sbIndex$genotypeNew[i] <- sbIndexOrig$genotype[which(sbIndexOrig$plotNumber==sbIndex$plotNumberNew[i])][1]
}

sbHeightData <- read_excel('data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx', 
    sheet = "Inbred_height data", 
    skip = 2, 
    col_types = c('numeric', 'skip', rep('numeric', 2), 'skip'),
    col_names = c('plotNumber', 'earHeight', 'flagLeafHeight'))
sb2.5 <- left_join(sbIndex, sbHeightData, join_by(plotNumberNew == plotNumber)) %>%
  mutate(nitrogenTreatment = 'High',
         location = 'Scottsbluff',
         plotNumber = plotNumberNew)
sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')


# 
# sbOrig <- df2022Inbreds %>%
#   filter(location=='Scottsbluff') %>%
#   mutate(plotNumber = as.numeric(plotNumber))
# 
# sb2.5 <- left_join(sbIndex, sbOrig, join_by(plotNumberNew==plotNumber), suffix = c('.index', '.data'), keep = FALSE)
# sb2.5 <- sb2.5 %>%
#   mutate(row = row.index,
#          range = range.index, 
#          genotype = genotype.index,
#          plotNumber = plotNumberNew) %>%
#   select(!ends_with('.index'), !ends_with('.data')) %>%
#   filter(!is.na(qrCode))
# sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotype', response_vars, 'location')

sb2 <- sbFixed

sb2Index <- sb2 %>%
  select(range, row, plotNumber, genotype)

sb2.5 <- sb2 %>%
  rowwise() %>%
  mutate(genotypeNew = rep(NA, length(genotype)))

for(i in 1:length(sb2.5$plotNumber))
{
  currRow <- sb2.5$row[i]
  if(currRow < 3)
  {
    break
  }
  if((currRow %% 2)==0)
  {
    sb2.5$genotypeNew[i] <- sb2.5$genotype[i]
    next
  }
  currRange <- sb2.5$range[i]
  offset <- abs(33 - currRange)*2 + 1
  index <- NA
  if(currRange < 34)
  {
    index <- which(sb2Index$row==currRow & sb2Index$range==(currRange + offset))
  }
  else
  {
    index <- which(sb2Index$row==currRow & sb2Index$range==(currRange - offset))
  }
  if(length(index)>=1)
  {
    sb2.5$genotypeNew[i] <- sb2Index$genotype[index]
    print(paste0("i= ",i, ' ', "index= ",index))
  }
}

sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')

sb2.5Odd <- sb2.5 %>%
  filter((row %% 2)!=0)
sb2.5OddWide <- plotRepCorr(sb2.5Odd, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')

sb2.5Even <- sb2.5 %>%
  filter((row %%2)==0|row==13)
sb2.5EvenWide <- plotRepCorr(sb2.5Even, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')
