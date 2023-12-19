library(tidyverse)
library(readxl)
library(cowplot)
library(lubridate)
source('src/Functions.R')
source('src/WrangleWeatherData.R')
# df2022Inbreds <- read.csv('/Users/jensinadavis/Downloads/SAMS_2022_V4.0_INBREDS.csv')
# response_vars <- c('earHeight', 'flagLeafHeight')
# locationsExceptScottsbluff <- c('Lincoln', 'Missouri Valley', 'Ames', 'CrawfordsVille')
# # Do Scottsbluff height obs correlate with height obs of the same genotype in other locations
# df2022InbredsWide <- df2022Inbreds %>% 
#   filter(earHeight < 400) %>%
#   group_by(genotype, location) %>%
#   mutate(genotypeObsNum = 1:n()) %>%
#   pivot_wider(id_cols = c(genotype, genotypeObsNum), 
#               names_from = c(location), 
#               values_from = c(earHeight, flagLeafHeight))
# for (i in locationsExceptScottsbluff)
# {
#   sbEarHeight <- 'earHeight_Scottsbluff'
#   sbFlagLeafHeight <- 'flagLeafHeight_Scottsbluff'
#   loc2EarHeight <- paste0('earHeight_', i)
#   loc2FlagLeafHeight <- paste0('flagLeafHeight_', i)
#   
#   earHeightPlot <- ggplot(df2022InbredsWide, aes(.data[[loc2EarHeight]], .data[[sbEarHeight]])) +
#     geom_point() +
#     labs(title = i)
#   
#   flagLeafHeightPlot <- ggplot(df2022InbredsWide, aes(.data[[loc2FlagLeafHeight]], .data[[sbFlagLeafHeight]])) +
#     geom_point() + 
#     labs(title = i)
#   
#   print(paste0('Ear Height, ', i, ': ', cor(df2022InbredsWide[[loc2EarHeight]], df2022InbredsWide[[sbEarHeight]], use = 'complete.obs')))
#   print(paste0('Flag Leaf Height, ', i, ': ', cor(df2022InbredsWide[[loc2FlagLeafHeight]], df2022InbredsWide[[sbFlagLeafHeight]], use = 'complete.obs')))
#   print(earHeightPlot)
#   print(flagLeafHeightPlot)
# }
# 
# 
# inbreds2022.wide <- plotRepCorr(df2022Inbreds, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# sb <- filter(df2022Inbreds, location=='Scottsbluff') %>%
#   filter(earHeight < 400)
# cor(sb$earHeight.1, sb$earHeight.2, use = 'complete.obs')
# aclmv <- filter(df2022Inbreds, location %in% c('Ames', 'Crawfordsville', 'Lincoln', 'Missouri Valley'))
# aclmv.wide <- plotRepCorr(aclmv, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# cor(aclmv$earHeight.1, aclmv$earHeight.2, use = 'complete.obs')
# cor(aclmv$flagLeafHeight.1, aclmv$flagLeafHeight.2, use = 'complete.obs')
# sbWide <- plotRepCorr(sb, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# 
# lnk2023InbredHeights <- read_excel('data/2023/inbreds/231018 Finalized Height Data from Lopez-Corona - Additional info added by Turkus.xlsx', sheet = 'Combined File', col_names = c('experiment', 'plotNumber', 'genotype', 'pi', 'row', 'range', 'earHeight1', 'flagLeafHeight1', 'tasselTipHeight1', 'earHeight2', 'flagLeafHeight2', 'tasselTiplHeight2', 'notes', 'person'), skip = 1)
# lnk2023InbredHeights <- lnk2023InbredHeights %>%
#   rowwise() %>%
#   mutate(genotype = str_to_upper(genotype),
#          earHeight = mean(c(as.numeric(earHeight1), as.numeric(earHeight2)), na.rm = TRUE) * 12 %>%
#            cm(),
#          flagLeafHeight = mean(c(as.numeric(flagLeafHeight1), as.numeric(flagLeafHeight2)), na.rm = TRUE) * 12 %>%
#            cm(),
#          tasselTipHeight = mean(c(as.numeric(tasselTipHeight1), as.numeric(tasselTiplHeight2)), na.rm = TRUE) * 12 %>%
#            cm(), 
#          location = 'Lincoln', 
#          nitrogenTreatment = 'Medium') %>%
#   select(!c(ends_with('1'), ends_with('2'), 'experiment', 'pi', 'person')) %>%
#   relocate(notes, .after = nitrogenTreatment)
# lnk2023.wide <- plotRepCorr(lnk2023InbredHeights, 'nitrogenTreatment', 'genotype', c('earHeight', 'flagLeafHeight', 'tasselTipHeight'), 'location')
# 
# #Export to csv
# write.csv(lnk2023InbredHeights, file = 'outData/2023_Inbred_Height_Data_Lincoln.csv', quote = FALSE, sep = ',', na = '', row.names = FALSE)
# 
# 
# # Check scottsbluff fixed data
# sbFixed <- read.csv('/Users/jensinadavis/Downloads/Scottsbluff_Modified_v3 (2).csv')
# sbFixed <- sbFixed %>%
#   filter(earHeight < 400) %>%
#   mutate(genotype = str_to_upper(genotype))
# sbFixedWide <- plotRepCorr(sbFixed, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# 
# sbEvenRows <- sbFixed %>%
#   filter((row %% 2)==0) %>%
#   mutate(genotype = str_to_upper(genotype))
# sbEvenRowsWide <- plotRepCorr(sbEvenRows, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# 
# sbOddRows <- sbFixed  %>%
#   filter((row %% 2)!=0) %>%
#   mutate(genotype = str_to_upper(genotype))
# sbOddRowsWide <- plotRepCorr(sbOddRows, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# 
# 
# sbIndex <- read_excel("data/Scottsbluff Inbred HIPS - Summary.xlsx",
#                       sheet = "Index (Modified)", col_names = c('row', 'range', 'plotNumber', 'genotype', 'block'),
#                       col_types = c(rep('numeric', 3), 'text', 'text'),
#                       skip = 1) %>%
#   mutate(genotype = str_to_upper(genotype),
#          plotNumberNew = case_when(
#            row==3 & range < 19 ~ plotNumber - 682,
#                                    row==5 ~ plotNumber - 434,
#                                    row==7 ~ plotNumber - 186,
#                                    row==9 ~ plotNumber + 62,
#                                    row==11 ~ plotNumber + 310,
#                                    row==13 ~ plotNumber + 558,
#                                    row==4 ~ plotNumber - 619 + 2*(range - 3),
#                                    row==6 ~ plotNumber - 371 + 2*(range - 3),
#                                    row==8 ~ plotNumber - 123 + 2*(range - 3),
#                                    row==10 ~ plotNumber + 125 + 2*(range - 3),
#                                    row==12 ~ plotNumber + 373 + 2*(range - 3),
#                                    row==14 & range < 19 ~ plotNumber + 621 + 2*(range - 3)))
# sbIndexOrig <- sbIndex %>%
#   select(plotNumber, genotype)
# sbIndex$genotypeNew <- rep(NA, length(sbIndex$plotNumberNew))
# for(i in 1:length(sbIndex$plotNumberNew))
# {
#   sbIndex$genotypeNew[i] <- sbIndexOrig$genotype[which(sbIndexOrig$plotNumber==sbIndex$plotNumberNew[i])][1]
# }
# 
# sbHeightData <- read_excel('data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx', 
#     sheet = "Inbred_height data", 
#     skip = 2, 
#     col_types = c('numeric', 'skip', rep('numeric', 2), 'skip'),
#     col_names = c('plotNumber', 'earHeight', 'flagLeafHeight'))
# sb2.5 <- left_join(sbIndex, sbHeightData, join_by(plotNumberNew == plotNumber)) %>%
#   mutate(nitrogenTreatment = 'High',
#          location = 'Scottsbluff',
#          plotNumber = plotNumberNew)
# sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')
# 
# 
# # 
# # sbOrig <- df2022Inbreds %>%
# #   filter(location=='Scottsbluff') %>%
# #   mutate(plotNumber = as.numeric(plotNumber))
# # 
# # sb2.5 <- left_join(sbIndex, sbOrig, join_by(plotNumberNew==plotNumber), suffix = c('.index', '.data'), keep = FALSE)
# # sb2.5 <- sb2.5 %>%
# #   mutate(row = row.index,
# #          range = range.index, 
# #          genotype = genotype.index,
# #          plotNumber = plotNumberNew) %>%
# #   select(!ends_with('.index'), !ends_with('.data')) %>%
# #   filter(!is.na(qrCode))
# # sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotype', response_vars, 'location')
# 
# sb2 <- sbFixed
# 
# sb2Index <- sb2 %>%
#   select(range, row, plotNumber, genotype)
# 
# sb2.5 <- sb2 %>%
#   rowwise() %>%
#   mutate(genotypeNew = rep(NA, length(genotype)))
# 
# for(i in 1:length(sb2.5$plotNumber))
# {
#   currRow <- sb2.5$row[i]
#   if(currRow < 3)
#   {
#     break
#   }
#   if((currRow %% 2)==0)
#   {
#     sb2.5$genotypeNew[i] <- sb2.5$genotype[i]
#     next
#   }
#   currRange <- sb2.5$range[i]
#   offset <- abs(33 - currRange)*2 + 1
#   index <- NA
#   if(currRange < 34)
#   {
#     index <- which(sb2Index$row==currRow & sb2Index$range==(currRange + offset))
#   }
#   else
#   {
#     index <- which(sb2Index$row==currRow & sb2Index$range==(currRange - offset))
#   }
#   if(length(index)>=1)
#   {
#     sb2.5$genotypeNew[i] <- sb2Index$genotype[index]
#     print(paste0("i= ",i, ' ', "index= ",index))
#   }
# }
# 
# sb2.5Wide <- plotRepCorr(sb2.5, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')
# 
# sb2.5Odd <- sb2.5 %>%
#   filter((row %% 2)!=0)
# sb2.5OddWide <- plotRepCorr(sb2.5Odd, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')
# 
# sb2.5Even <- sb2.5 %>%
#   filter((row %%2)==0|row==13)
# sb2.5EvenWide <- plotRepCorr(sb2.5Even, 'nitrogenTreatment', 'genotypeNew', response_vars, 'location')


# Okay, lets add in the ear data so we can see if this problem exists there too
ears <- read.csv('data/2022 Inbred HIPS Ear Data - Turkus Curated 231117.csv')

yellow <- c("yellow", "pale yellow", "yellow/unevenrows", "yellow ", "dark yellow(ak) then yellow", "light yellow",
            "dark yellow ", "yelllow", "dark yellow", "yellow  ", "yellow/ red dots", "very light yellow", 
            "clear yellow", "yellow whit red stripes", "deep yellow", "yellow red stripe")
white <- c("white ", "white", "white/pink dots", "whilte ", "white  ", "cream", "cream ")
yellowWhite <- c("white / yellow", "yellow/white", "yellow w white",  "white/yellow", "white w yellow", 
                 "white/light yellow ", "yellowish white", "yellow w white ak", "white/yellow mix", 
                 "yellow and white", "yellow/cream", "yellow / white", "yellow, white", "cream/yellow")
missing <- c("na", "", "na ", "n/a", "ak", "no kernels ", "ak/ mold")
yellowOrange <- c("yellow/orange", "organish yellow", "yellow/ orange", "yellow/light orange", "orange/yellow",
                  "yellow-orange")
yellowWhiteBrown <- c("yellow/white/brown")
orange <- c("orange", "orange ", "light orange ", "light orange", "light orange/pale", "dark orange")
yellowPurple <- c("yellow/few purple", "purple/yellow", "yellow/purple", "blue/yellow")
red <- c("red", "redish")
striping <- c("white/pink dots", "yellow/ red dots", "yellow whit red stripes", "yellow red stripe")
yellowBlack <- c("yellow/black", "yellow w black", "black w few yellow", "black/yellow", "black/few yellow ", 
                 "yellow/ black", "black w yellow")
yellowBrownOrange <- c("brown/orange/yellow")
brownBlack <- c("brown/black", "black/ brown")
yellowBrown <- c("yellow/brown", "cream yellow/ brown", "yellow/ brown", "yellow/brown edges", "dark yellow brown")
orangeBrown <- c("orange/brown")
purple <- c("purple", "blue")
black <- c("black")
yellowRed <- c("yellow/red", "reddish-yellow", "pale yellow/pinkish", "pinkish-yellow", "yellow, bits of red", 
               "yellow/ red", "red-yellow", "pink-yellow", "red/yellow", "yellow and red")
orangeBlack <- c("orange w black", "orange/black", "orange/ black", "red yellow")
orangeRed <- c("orange/red")
brown <- c("brown", "light/dark brown", "light brown", "green/brown")
whiteRed <- c("white/ red")
whiteBrown <- c("white/ brown", "brown/white", "white/brown")
purpleBrown <- c("brown/purple")
yellowBrownBlack <- c("black/brown/yellow")


ears <- ears %>%
  rename(order = Original.Order,
         greenAtHarvest = Is.a..G..on.the.Row.Band.,
         primaryEarNumber = Primary.Ear..) %>%
  mutate(person = str_to_upper(Person),
         qrCode = str_to_upper(QR.Code),
         numberPrimaryEars = as.numeric(X..primary.ears),
         numberSecondaryEars = as.numeric(X..secondary.ears),
         looseKernels = as.numeric(Loose.kernel.Count),
         looseKernelMass = case_when(Loose.kernel.weight=='0.0.05' ~ '0.05', 
                                     Loose.kernel.weight=='4..9' ~ '4.9', 
                                     .default = Loose.kernel.weight) %>%
           as.numeric(), 
         secondaryEarKernels = case_when(Secondary.Ear.Kernel.Count==" # from 7th ear 398" ~ '398',
                                         Secondary.Ear.Kernel.Count=="11( smallest and uneven row ear)" ~ '11',
                                         Secondary.Ear.Kernel.Count=='o' ~ '0',
                                         .default = Secondary.Ear.Kernel.Count) %>%
           as.numeric(),
         kernelColor = case_when(Kernel.Color %in% yellow ~ 'yellow',
                                 Kernel.Color %in% white ~ 'white',
                                 Kernel.Color %in% yellowWhite ~ 'yellow/white',
                                 Kernel.Color %in% missing ~ NA,
                                 Kernel.Color %in% yellowOrange ~ 'yellow/orange',
                                 Kernel.Color %in% yellowWhiteBrown ~ 'yellow/white/brown',
                                 Kernel.Color %in% orange ~ 'orange', 
                                 Kernel.Color %in% yellowPurple ~ 'yellow/purple',
                                 Kernel.Color %in% red ~ 'red', 
                                 Kernel.Color %in% yellowBlack ~ 'yellow/black',
                                 Kernel.Color %in% yellowBrownOrange ~ 'yellow/orange/brown',
                                 Kernel.Color %in% brownBlack ~ 'brown/black',
                                 Kernel.Color %in% yellowBrown ~ 'yellow/brown',
                                 Kernel.Color %in% orangeBlack ~ 'orange/black', 
                                 Kernel.Color %in% orangeBrown ~ 'orange/brown',
                                 Kernel.Color %in% purple ~ 'purple',
                                 Kernel.Color %in% black ~ 'black', 
                                 Kernel.Color %in% yellowRed ~ 'yellow/red',
                                 Kernel.Color %in% orangeBlack ~ 'orange/black',
                                 Kernel.Color %in% orangeRed ~ 'orange/red',
                                 Kernel.Color %in% brown ~ 'brown',
                                 Kernel.Color %in% whiteRed ~ 'white/red',
                                 Kernel.Color %in% whiteBrown ~ 'white/brown',
                                 Kernel.Color %in% purpleBrown ~ 'purple/brown', 
                                 Kernel.Color %in% yellowBrownBlack ~ 'yellow/brown/black'),
         kernelStriping = case_when(Kernel.Color %in% striping ~ TRUE, .default = FALSE),
         earWidth = as.numeric(Ear.Width), 
         kernelFillLength = as.numeric(Kernel.Fill.Length), 
         kernelRowNumber = as.numeric(Kernel.Row.Number),
         kernelsPerRow = as.numeric(Kernels.per.Row),
         earMass = as.numeric(Ear.Weight),
         kernelsPerEar = as.numeric(Kernel.Count),
         shelledCobWidth = as.numeric(Cob.Width),
         shelledCobLength = as.numeric(Cob.Length),
         shelledCobMass = as.numeric(Cob.Weight),
         hundredKernelMass = as.numeric(X100.Kernel.weight)) %>%
  unite('notes', c(General.Remarks, Turkus.Notes), sep = '; ', na.rm = TRUE) %>% 
  select(c(order, greenAtHarvest, primaryEarNumber, person, qrCode, numberPrimaryEars, 
           numberSecondaryEars, looseKernels, looseKernelMass, secondaryEarKernels, kernelColor, kernelStriping, 
           earWidth, kernelFillLength, kernelRowNumber, kernelsPerRow, earMass, kernelsPerEar, shelledCobWidth,
           shelledCobLength, shelledCobMass, hundredKernelMass, notes)) %>%
  mutate(kernelMassPerEar = earMass - shelledCobMass)

parseMissouriValleyQRs <- function(data)
{
  df <- data %>%
    rowwise() %>%
    mutate(location = 'Missouri Valley', 
           nitrogenTreatment = 'Medium',
           poundsOfNitrogenPerAcre = 175,
           irrigationProvided = 0, 
           sublocation = 'Missouri Valley',
           plantingDate = '4/29/2022',
           block = str_split_i(qrCode, fixed('$'), 3) %>%
             str_remove('REP') %>%
             as.numeric(),
           plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
             str_remove('PLOT') %>%
             as.numeric() %>%
             case_when(block==2 ~ . + 400, .default = .),
           row = str_split_i(qrCode, fixed('$'), 5) %>%
             str_remove('ROW') %>%
             as.numeric(), 
           range = str_split_i(qrCode, fixed('$'), 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           genotype = str_split_i(qrCode, fixed('$'), 7))
  return(df)
}

parseLincolnQRs <- function(data)
{
  df <- data %>%
    mutate(location = 'Lincoln',
           sublocation = 'Lincoln',
           nitrogenTreatment = 'Medium',
           poundsOfNitrogenPerAcre = 150,
           irrigationProvided = 0,
           plantingDate = '5/5/2022',
           block = str_split_i(qrCode, fixed('$'), 3) %>%
             str_remove('REP') %>%
             as.numeric(), 
           plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
             str_remove('PLOT') %>%
             as.numeric(),
           row = str_split_i(qrCode, fixed('$'), 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           range = str_split_i(qrCode, fixed('$'), 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           genotype = str_split_i(qrCode, fixed('$'), 7))
  return(df)
}

parseScottsbluffQRs <- function(data)
{
  df <- data %>%
    mutate(location = 'Scottsbluff',
           sublocation = 'Scottsbluff',
           nitrogenTreatment = 'High', 
           poundsOfNitrogenPerAcre = 250, 
           irrigationProvided = 16.9, 
           plantingDate = '5/19/2022',
           block = str_split_i(qrCode, fixed('$'), 3) %>%
             str_remove('REP') %>% 
             as.numeric(),
           plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
             str_remove('PLOT') %>%
             as.numeric(),
           row = str_split_i(qrCode, fixed('$'), 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           range = str_split_i(qrCode, fixed('$'), 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           genotype = str_split_i(qrCode, fixed('$'), 7))
  return(df)
}

mv <- ears %>%
  filter(str_detect(qrCode, 'MV')) %>%
  parseMissouriValleyQRs()
lnk <- ears %>%
  filter(str_detect(qrCode, 'LINCOLN')) %>%
  parseLincolnQRs()
sb <- ears %>%
  filter(str_detect(qrCode, 'SCOTTSBLUFF')) %>%
  parseScottsbluffQRs()

ears <- bind_rows(mv, lnk, sb)
earsPlotLevel <- ears %>%
  group_by(location, plotNumber, qrCode, row, range, genotype, block) %>%
  summarise(numberPrimaryEars = mean(numberPrimaryEars, na.rm = TRUE),
            numberSecondaryEars = mean(numberSecondaryEars, na.rm = TRUE), 
            looseKernels = mean(looseKernels, na.rm = TRUE), 
            looseKernelMass = mean(looseKernelMass, na.rm = TRUE),
            secondaryEarKernels = mean(secondaryEarKernels, na.rm = TRUE), 
            kernelColor = first(kernelColor),
            kernelStriping = first(kernelStriping),
            earWidth = mean(earWidth, na.rm = TRUE),
            kernelFillLength = mean(kernelFillLength, na.rm = TRUE),
            kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE), 
            kernelsPerRow = mean(kernelsPerRow, na.rm = TRUE),
            kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE), 
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE),
            shelledCobLength = mean(shelledCobLength, na.rm = TRUE),
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            hundredKernelMass = mean(hundredKernelMass, na.rm = TRUE),
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE),
            notes = paste0(notes, collapse = ';'))

earPlotsWide <- earsPlotLevel %>%
  mutate(kernelMassPerEar = case_when(kernelMassPerEar > 200 ~ NA, .default = kernelMassPerEar),
         hundredKernelMass = case_when(hundredKernelMass > 150 ~ NA, .default = hundredKernelMass),) %>%
  group_by(location, genotype) %>%
  mutate(genotypicRep = 1:n()) %>%
  pivot_wider(id_cols = c(genotype, genotypicRep),
              names_from = location,
              values_from = c(numberPrimaryEars, numberSecondaryEars, looseKernelMass, looseKernels, kernelColor, kernelStriping, 
                              earWidth, kernelFillLength, kernelRowNumber, kernelsPerEar, kernelsPerRow, shelledCobWidth, 
                              shelledCobLength, shelledCobMass, hundredKernelMass, kernelMassPerEar))

# earPhenotypes <- c('earWidth', 'kernelRowNumber', 'kernelsPerEar', 'kernelsPerRow', 'shelledCobWidth',
#                    'shelledCobMass', 'hundredKernelMass', 'kernelMassPerEar')
# for(pheno in earPhenotypes)
# {
#   phenoSB <- paste0(pheno, '_Scottsbluff')
#   phenoMV <- paste0(pheno, '_Missouri Valley')
#   phenoLNK <- paste0(pheno, '_Lincoln')
#   
#   mvPlot <- ggplot(earPlotsWide, aes(.data[[phenoMV]], .data[[phenoSB]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(earPlotsWide[[phenoMV]], earPlotsWide[[phenoSB]], use = 'complete.obs')),
#          x = 'Missouri Valley',
#          y = 'Scottsbluff')
#   
#   lnkPlot <- ggplot(earPlotsWide, aes(.data[[phenoLNK]], .data[[phenoSB]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(earPlotsWide[[phenoLNK]], earPlotsWide[[phenoSB]], use = 'complete.obs')),
#          x = 'Lincoln',
#          y = 'Scottsbluff')
#   
#   mvLNKPlot <- ggplot(earPlotsWide, aes(.data[[phenoMV]], .data[[phenoLNK]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(earPlotsWide[[phenoMV]], earPlotsWide[[phenoLNK]], use = 'complete.obs')),
#          x = 'Missouri Valley',
#          y = 'Lincoln')
#   
#   combinedPlot <- plot_grid(mvLNKPlot, mvPlot, lnkPlot, nrow = 1)
#   print(combinedPlot)
# }
# 
# # Do we need to drop the FT data??
# # Read in v4.5
# # inbreds4.5 <- read.csv("~/Downloads/SAMS_2022_V4.5_INBREDS .csv")
# # inbreds4.5 <- inbreds4.5 %>%
# #   rowwise() %>%
# #   mutate(plantingDate = case_when(location=='Scottsbluff' ~ '5/19/2022', .default = plantingDate), 
# #          plantingMonth = str_split_i(plantingDate, fixed('/'), 1) %>%
# #            as.integer(),
# #          plantingDay = str_split_i(plantingDate, fixed('/'), 2) %>%
# #            as.integer(),
# #          anthesisMonth = str_split_i(anthesisDate, fixed('/'), 1) %>%
# #            as.integer(), 
# #          anthesisDay = str_split_i(anthesisDate, fixed('/'), 2) %>%
# #            as.integer(),
# #          silkMonth = str_split_i(silkDate, fixed('/'), 1) %>%
# #            as.integer(),
# #          silkDay = str_split_i(silkDate, fixed('/'), 2) %>%
# #            as.integer())
# # inbreds4.5 <- inbreds4.5 %>%
# #   rowwise() %>%
# #   mutate(daysToAnthesis = as.integer(difftime(make_date(year = 2022, month = anthesisMonth, day = anthesisDay), make_date(year = 2022, month = plantingMonth, day = plantingDay), units = 'days')),
# #          daysToSilk = as.integer(difftime(make_date(year = 2022, month = silkMonth, day = silkDay), make_date(year = 2022, month = plantingMonth, day = plantingDay), units = 'days'))) %>%
# #   mutate(daysToSilk = case_when(daysToSilk < 0 ~ NA, .default = daysToSilk))
# # 
# # ftWide <- inbreds4.5 %>%
# #   group_by(location, genotype) %>%
# #   mutate(genotypicRep = 1:n()) %>%
# #   pivot_wider(id_cols = c(genotype, genotypicRep),
# #               names_from = location,
# #               values_from = c(daysToAnthesis, daysToSilk))
# 
# for(pheno in c('daysToAnthesis', 'daysToSilk'))
# {
#   phenoSB <- paste0(pheno, '_Scottsbluff')
#   phenoLNK <- paste0(pheno, '_Lincoln')
# 
#   lnkPlot <- ggplot(ftWide, aes(.data[[phenoLNK]], .data[[phenoSB]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(ftWide[[phenoLNK]], ftWide[[phenoSB]], use = 'complete.obs')),
#          x = 'Lincoln',
#          y = 'Scottsbluff')
#   print(lnkPlot)
# }
# 
# # Let's check how the correlation between locations in the hybrids for comparison
# hybrids <- read.csv('outData/HIPS_2022_V3.5_HYBRIDS.csv')
# hybridsWide <- hybrids %>%
#   group_by(location, genotype) %>%
#   mutate(genotypicRep = 1:n()) %>%
#   pivot_wider(id_cols = c(genotype, genotypicRep),
#               names_from = location, 
#               values_from = c(daysToAnthesis, daysToSilk))
# 
# for(pheno in c('daysToAnthesis', 'daysToSilk'))
# {
#   phenoSB <- paste0(pheno, '_Scottsbluff')
#   phenoNP1 <- paste0(pheno, '_North Platte1')
#   phenoLNK <- paste0(pheno, '_Lincoln')
#   
#   np1SBPlot <- ggplot(hybridsWide, aes(.data[[phenoNP1]], .data[[phenoSB]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(hybridsWide[[phenoNP1]], hybridsWide[[phenoSB]], use = 'complete.obs')),
#          x = 'North Platte1',
#          y = 'Scottsbluff')
#   
#   lnkSBPlot <- ggplot(hybridsWide, aes(.data[[phenoLNK]], .data[[phenoSB]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(hybridsWide[[phenoLNK]], hybridsWide[[phenoSB]], use = 'complete.obs')),
#          x = 'Lincoln',
#          y = 'Scottsbluff')
#   
#   np1LNKPlot <- ggplot(hybridsWide, aes(.data[[phenoNP1]], .data[[phenoLNK]])) +
#     geom_point() +
#     labs(title = pheno, 
#          subtitle = paste0('r = ', cor(hybridsWide[[phenoNP1]], hybridsWide[[phenoLNK]], use = 'complete.obs')),
#          x = 'North Platte1',
#          y = 'Lincoln')
#   
#   combinedPlot <- plot_grid(np1LNKPlot, np1SBPlot, np1LNKPlot, nrow = 1)
#   print(combinedPlot)
# }
# 
# sbLNK <- inbreds4.5 %>%
#   filter(location %in% c('Scottsbluff', 'Lincoln'))
# sbLNKWide <- plotRepCorr(sbLNK, 'nitrogenTreatment', 'genotype', c('daysToAnthesis', 'daysToSilk'), 'location')
# 
# sb <- inbreds4.5 %>%
#   filter(location=='Scottsbluff')
# sbWide <- plotRepCorr(sb, 'nitrogenTreatment', 'genotype', c('daysToAnthesis', 'daysToSilk'), 'location')
# 
# inbreds4.5Wide <- plotRepCorr(inbreds4.5, 'nitrogenTreatment', 'genotype', c('earHeight'), 'location')
# 
# inbreds4.7 <- read_csv("~/Downloads/SAMS_2022_V4.7_INBREDS .csv")
# 
# inbreds4.7Wide <- plotRepCorr(inbreds4.7, 'nitrogenTreatment', 'genotype', c(earPhenotypes, 'earHeight'), 'location')

# Read in IA inbred field data
ia_inb <- read_excel("data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx", 
                     sheet = "RawData (2-Row)", col_types = c("skip", 
                                                              "skip", "skip", "skip", "text", "skip", 
                                                              "text", "skip", "text", "skip", "text", 
                                                              "skip", "text", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "skip", "skip", "skip", 
                                                              "skip", "skip", "numeric", "skip", 
                                                              "skip", "skip", "skip", "date", "skip", 
                                                              "skip", "numeric", "skip", 'skip', 
                                                              "skip", "skip", "skip"))
# Save original column names and change 
orig_colnames_ia_inb <- colnames(ia_inb)
colnames(ia_inb) <- c('experiment', 'check', 'qrCode', 'genotype', 'notes', 'plotNumber', 'latitude', 'longitude', 'row', 'range', 'block', 'plantDensity', 'plantingDate', 'totalStandCount')
# Filter to MV
# Move check to notes and drop
# And add metadata
ia_inb <- ia_inb %>%
  rowwise() %>%
  mutate(check = case_when(!is.na(check) ~ 'Check'),
         genotype = str_to_upper(genotype),
         location = case_when(experiment=='LC_2211' ~ 'Missouri Valley',
                              experiment %in% c('LC_2231', 'LC_2232', 'LC_2233') ~ 'Ames',
                              experiment %in% c('LC_2351', 'LC_2352', 'LC_2353') ~ 'Crawfordsville'),
         sublocation = case_when(experiment=='LC_2211' ~ 'Missouri Valley',
                                 experiment %in% c('LC_2231', 'LC_2232') ~ 'Ames B1',
                                 experiment=='LC_2233' ~ 'Ames E1',
                                 experiment %in% c('LC_2351', 'LC_2352') ~ 'Crawfordsville A',
                                 experiment %in% c('LC_2353') ~ 'Crawfordsville B'),
         irrigationProvided = 0,
         nitrogenTreatment = case_when(experiment %in% c('LC_2352', 'LC_2233') ~ 'Low',
                                       experiment %in% c('LC_2211', 'LC_2353', 'LC_2232') ~ 'Medium',
                                       experiment %in% c('LC_2231', 'LC_2351') ~ 'High'), 
         poundsOfNitrogenPerAcre = case_when(experiment %in% c('LC_2352', 'LC_2233') ~ 75,
                                             experiment %in% c('LC_2353', 'LC_2232') ~ 150,
                                             experiment=='LC_2211' ~ 175,
                                             experiment=='LC_2251' ~ 225,
                                             experiment=='LC_2231' ~ 250), 
         plantingDate = case_when(experiment=='LC_2211' ~ '4/29/2022', 
                                  experiment %in% c('LC_2231', 'LC_2232') ~ '5/23/2022', 
                                  experiment=='LC_2233' ~ '5/22/2023',
                                  experiment %in% c('LC_2351', 'LC_2352', 'LC_2353') ~ '5/11/2022')) %>%
  unite('notes', c(notes, check), sep = ';', na.rm = TRUE) %>%
  select(c(experiment, notes, genotype, plotNumber, row, range, block, plantDensity, plantingDate, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, qrCode, totalStandCount))

inbreds <- full_join(earsPlotLevel, ia_inb, join_by(location, row, range), keep = FALSE, suffix = c('.ears', '.ia'))
inbreds <- inbreds %>%
  rowwise() %>%
  mutate(plotNumber = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ plotNumber.ears,
                                location %in% c('Ames', 'Crawfordsville') ~ str_split_i(qrCode.ia, '-', 3) %>%
                                  as.numeric()),
         qrCode = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ qrCode.ears, 
                            location %in% c('Ames', 'Crawfordsville') ~ qrCode.ia), 
         genotype = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ genotype.ears, 
                              location %in% c('Ames', 'Crawfordsville') ~ genotype.ia),
         block = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ block.ears,
                           (sublocation %in% c('Ames B1', 'Crawfordsville B') & nitrogenTreatment=='Medium' & block.ia==1)|
                             (sublocation %in% c('Ames E1', 'Crawfordsville A') & nitrogenTreatment=='Low' & block.ia==1) ~ 1,
                           (sublocation %in% c('Ames B1', 'Crawfordsville B') & nitrogenTreatment=='Medium' & block.ia==2)|
                             (sublocation %in% c('Ames E1', 'Crawfordsville A') & nitrogenTreatment=='Low' & block.ia==2) ~ 2,
                           (sublocation %in% c('Ames B1', 'Crawfordsville A') & nitrogenTreatment=='High' & block.ia==1) ~ 3,
                           (sublocation %in% c('Ames B1', 'Crawfordsville A') & nitrogenTreatment=='High' & block.ia==2) ~ 4)) %>%
  unite('notes', c(notes.ears, notes.ia), sep = ';', na.rm = TRUE) %>%
  select(location, plotNumber, qrCode, row, range, genotype, block, numberPrimaryEars, numberSecondaryEars, looseKernels, 
         looseKernelMass, secondaryEarKernels, kernelColor, kernelStriping, earWidth, kernelFillLength, kernelRowNumber, kernelsPerRow,
         kernelsPerEar, shelledCobWidth, shelledCobLength, shelledCobMass, hundredKernelMass, kernelMassPerEar, notes, experiment,
         plantDensity, plantingDate, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, totalStandCount)

# Missouri Valley height data
mv.plantData.inb <- read_excel('data/Plant_data_MO_Valley_2022.xlsx',
                               sheet = '2211',
                               col_names = c('row', 'range', 'notes', 'flagLeafHeight', 'earHeight', 'genotype'),
                               col_types = c('skip', 'numeric', 'numeric', 'skip', 'skip', 'text', 'numeric', 'numeric', 'text', 'skip'),
                               skip = 1)
mv.plantData.inb <- mv.plantData.inb %>%
  filter(is.na(notes)) %>%
  rowwise() %>%
  mutate(flagLeafHt = case_when(flagLeafHeight=='n/a' ~ NA, .default = flagLeafHeight),
         earHeight = case_when(earHeight=='n/a' ~ NA, .default = earHeight),
         genotype = str_to_upper(genotype),
         location = 'Missouri Valley',
         sublocation = 'Missouri Valley',
         nitrogenTreatment = 'Medium',
         irrigationProvided = 0,
         poundsOfNitrogenPerAcre = 175)

inbreds <- full_join(inbreds, mv.plantData.inb, join_by(location, row, range), keep = FALSE, suffix = c('', '.mv'))
inbreds <- inbreds %>%
  mutate(genotype = case_when(location=='Missouri Valley' ~ genotype.mv,
                              .default = genotype)) %>%
  select(!ends_with('.mv'))

ac.krn <- read_excel('data/2_KRN_trait_inbred_2022_Compiled_v3.xlsx', 
                     sheet = 'compiled data',
                     skip = 4,
                     col_types = c('skip', 'text', 'skip', 'skip', 'numeric', 'text', 'text', 'text', 'skip'),
                     col_names = c('qrCode', 'kernelRowNumber', 'notes', 'smoothCob', 'sweetcorn')) %>%
  rowwise() %>%
  mutate(qrCode = str_c(str_split_i(qrCode, '-', 1), str_split_i(qrCode, '-', 2), str_split_i(qrCode, '-', 3), sep = '-') %>%
           str_to_upper(),
         smoothCob = case_when(!is.na(smoothCob) ~ 'Smooth cob/ovule issue'),
         sweetcorn = case_when(!is.na(sweetcorn) ~ 'Sweetcorn')) %>%
  unite('notes', notes, smoothCob, sweetcorn, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  group_by(qrCode) %>%
  summarise(kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE),
            notes = str_c(notes, collapse = ';'))

ac.ears <- read_excel('data/3_ear_trait_INBRED_2022_compiled.xlsx', 
                      sheet = 'compiled data set',
                      skip = 1,
                      col_types = c('skip', 'text', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'text', 'skip'),
                      col_names = c('sampleID', 'earWidth', 'earMass', 'seedMissing')) %>%
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID),
         seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on either side of ear at widest point'))

ac.cobs1 <- read_excel('data/5_cob_trait_INBRED_2022_compiled_v2.xlsx',
                       sheet = 'data',
                       skip = 1,
                       col_types = c('skip', 'text', 'numeric', 'numeric', 'numeric', 'skip', 'text', 'text', 'skip', 'skip', 'skip'),
                       col_names = c('sampleID', 'earLength', 'shelledCobWidth', 'shelledCobMass', 'cobBroke', 'string')) %>%
  rowwise() %>%
  mutate(cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces'),
         string = case_when(!is.na(string) ~ 'Severe bend in cob: String used to measure length'))

ac.cobs2 <- read_excel('data/5_cob_Traits_missing_entries_2022.xlsx',
                       sheet = 1,
                       skip = 1, 
                       col_types = c('skip', 'skip', 'skip', 'text', 'numeric', 'numeric', 'numeric', 'skip'),
                       col_names = c('sampleID', 'earLength', 'shelledCobWidth', 'shelledCobMass')) %>%
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID))

ac.cobs <- bind_rows(ac.cobs1, ac.cobs2)

ac.seed <- read_excel('data/6_Seed_Trait_INBRED_2022_Compiled_v2.xlsx',
                      sheet = 'compiled data',
                      skip = 4,
                      col_types = c('text', 'skip', 'skip', 'numeric', 'numeric', 'skip', 'text', rep('skip', 5)),
                      col_names = c('sampleID', 'kernelsPerEar', 'kernelMassPerEar', 'notes')) %>%
  rowwise() %>%
  mutate(seedSpilled = case_when(str_detect(notes, 'shelling')|str_detect(notes, 'weighing') ~ TRUE, .default = FALSE))

ac <- full_join(ac.seed, ac.cobs, join_by(sampleID), suffix = c('', ''), keep = FALSE) %>%
  rowwise() %>%
  mutate(sampleID = str_remove(sampleID, '.') %>%
           str_replace('000022-A', '22-A') %>%
           str_replace('022-A', '22-A') %>%
           str_replace('0022-A', '22-A') %>%
           str_replace('r22-C', '22-C')) %>%
  mutate(sampleID = case_when(str_split_i(sampleID, '-', 1)=='2' ~ str_replace(sampleID, '2-', '22-'), 
                              .default = sampleID)) %>%
  group_by(sampleID) %>%
  summarise(kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE),
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE),
            notes = str_c(notes, collapse = ';'),
            seedSpilled = max(seedSpilled, na.rm = TRUE),
            earLength = mean(earLength, na.rm = TRUE),
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE),
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE),
            cobBroke = max(cobBroke, na.rm = TRUE),
            string = max(cobBroke, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~na_if(., -Inf)))

ac <- full_join(ac, ac.ears, join_by(sampleID), suffix = c('', ''), keep = FALSE)
ac <- ac %>%
  rowwise() %>%
  mutate(kernelMassPerEar = case_when(seedSpilled==1 & !is.na(earMass) & !is.na(shelledCobMass) ~ earMass - shelledCobMass, 
                                      .default = kernelMassPerEar),
         qrCode = str_c(str_split_i(sampleID, '-', 1), str_split_i(sampleID, '-', 2), str_split_i(sampleID, '-', 3), sep = '-')) %>%
  unite('notes', seedMissing, cobBroke, string, notes, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  group_by(qrCode) %>%
  summarise(kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE),
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE),
            earLength = mean(earLength, na.rm = TRUE),
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE),
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            earWidth = mean(earWidth, na.rm = TRUE), 
            notes = str_c(notes, collapse = ';'))

ac <- full_join(ac, ac.krn, join_by(qrCode), suffix = c('', '.krn'), keep = FALSE) %>%
  unite('notes', notes, notes.krn, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(location = case_when(str_detect(qrCode, 'A') ~ 'Ames',
                              str_detect(qrCode, 'C') ~ 'Crawfordsville'))

ac.height <- read.table('outData/AmesCrawfordsville2022InbredsHeights.tsv', sep = '\t', header = TRUE, row.names = NULL)
ac <- full_join(ac, ac.height, join_by(qrCode, location), suffix = c('', '.ht'), keep = FALSE)

ac <- unite(ac, 'notes', notes, notes.ht, sep = ';', remove = TRUE, na.rm = FALSE)

inbreds2 <- full_join(inbreds, ac, join_by(location, qrCode), keep = FALSE, suffix = c('', '.ac'))

inbreds2 <- inbreds2 %>%
  rowwise() %>%
  mutate(range = max(range, range.ac, na.rm = TRUE) %>%
           as.integer(),
         row = max(row, row.ac, na.rm = TRUE) %>%
           as.integer(), 
         flagLeafHeight = max(flagLeafHeight, flagLeafHeight.ac, flagLeafHt, na.rm = TRUE), 
         earHeight = max(earHeight, earHeight.ac, na.rm = TRUE),
         plotNumber = max(plotNumber, plotNumber.ac, na.rm = TRUE),
         genotype = max(genotype, genotype.ac, na.rm = TRUE), 
         nitrogenTreatment = max(nitrogenTreatment, nitrogenTreatment.ac, na.rm = TRUE),
         kernelRowNumber = max(kernelRowNumber, kernelRowNumber.ac, na.rm = TRUE), 
         earWidth = max(earWidth, earWidth.ac, na.rm = TRUE),
         earLength = max(shelledCobLength, earLength, na.rm = TRUE),
         shelledCobWidth = max(shelledCobWidth, shelledCobWidth.ac, na.rm = TRUE), 
         kernelsPerEar = max(kernelsPerEar, kernelsPerEar.ac, na.rm = TRUE),
         kernelMassPerEar = max(kernelMassPerEar, kernelMassPerEar.ac, na.rm = TRUE),
         shelledCobMass = max(shelledCobMass, shelledCobMass.ac, na.rm = TRUE)) %>%
  unite('notes', notes, notes.ac, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  select(!ends_with('.ac')) %>%
  select(!c(looseKernels, looseKernelMass, rep, field, irrigation, population))
inbreds2 <- inbreds2 %>%
  mutate(across(where(is.numeric), ~na_if(., as.integer(-Inf))))

lnkFT <- read_excel('data/2022 Digitized Flowering Notes.xlsx', 
                    sheet = 'SAM', 
                    col_types = c(rep('numeric', 3), 'skip', 'date', 'skip', 'date', 'skip', 'text', 'skip'),
                    col_names = c('plotNumber', 'row', 'range', 'anthesisDate', 'silkDate', 'notes')) %>%
  filter(!is.na(plotNumber)) %>%
  mutate(location = 'Lincoln')

inbreds3 <- full_join(inbreds2, lnkFT, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE)
inbreds3 <- inbreds3 %>%
  rowwise() %>%
  unite('notes', notes, notes.lnk, sep = ';', remove = TRUE, na.rm = FALSE) %>%
  select(!ends_with('.lnk'))

lnkStandCt <- read_excel('data/2022 SAM Stand Counts - Summary.xlsx', 
                         sheet = 'Comparison',
                         skip = 2,
                         col_types = c('numeric', rep('skip', 7), 'numeric', 'text', rep('skip', 11)),
                         col_names = c('plotNumber', 'totalStandCount', 'notes')) %>%
  mutate(location = 'Lincoln')
inbreds4 <- full_join(inbreds3, lnkStandCt, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE)
inbreds4 <- inbreds4 %>%
  rowwise() %>%
  mutate(totalStandCount = max(totalStandCount, totalStandCount.lnk, na.rm = TRUE)) %>%
  select(!ends_with('.lnk'))

lnkHt <- read_excel('data/230331 SAM Height and Leaf Dimension Data Data - Digitized and Reviewed.xlsx', 
                    sheet = 1,
                    col_types = c('numeric', 'text', rep('skip', 6), rep('numeric', 2), 'skip', rep('numeric', 2), rep('skip', 11)),
                    col_names = c('plotNumber', 'genotype', 'earHeight1', 'flagLeafHeight1', 'earHeight2', 'flagLeafHeight2')) %>%
  rowwise() %>%
  mutate(location = 'Lincoln',
         genotype = str_to_upper(genotype), 
         earHeight = mean(c(earHeight1, earHeight2), na.rm = TRUE),
         flagLeafHeight = mean(c(flagLeafHeight1, flagLeafHeight2), na.rm = TRUE)) %>%
  select(plotNumber, location, genotype, earHeight, flagLeafHeight)

inbreds5 <- full_join(inbreds4, lnkHt, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE)
inbreds5 <- inbreds5 %>%
  rowwise() %>%
  mutate(genotype = max(genotype, genotype.lnk, na.rm = TRUE),
         earHeight = max(earHeight, earHeight.lnk, na.rm = TRUE),
         flagLeafHeight = max(flagLeafHeight, flagLeafHeight.lnk, na.rm = TRUE)) %>%
  select(!ends_with('.lnk'))

inbreds5 <- filter(inbreds5, !(location=='Lincoln' & genotype=='GENOTYPE'))
inbreds5 <- filter(inbreds5, !(location=='Missouri Valley' & genotype %in% c('FILL', NA)))

inbreds <- inbreds5 %>%
  mutate(genotype = case_when(genotype %in% c("B73 OR MO17 FILLER", "UNKNOWN BUT PROBABLY FILL") ~ NA, 
                              genotype=="MO17 FILLER" ~ 'MO17', 
                              .default = genotype),
         block = case_when(location=='Lincoln' & plotNumber < 2000 ~ 1, 
                           location=='Lincoln' & plotNumber > 2000 ~ 2, 
                           location=='Missouri Valley' & range < 45 ~ 1,
                           location=='Missouri Valley' & range >= 45 ~ 2,
                           .default = block),
         plantingDate = case_when(location=='Lincoln' ~ '5/05/2022',
                                  location=='Scottsbluff' ~ '5/19/2022',
                                  location=='Crawfordsville' ~ '5/11/2022',
                                  .default = plantingDate),
         sublocation = case_when(location %in% c('Lincoln', 'Scottsbluff') ~ location,
                                 location=='Crawfordsville' & nitrogenTreatment %in% c('Low', 'High') ~ 'Crawfordsville A',
                                 location=='Crawfordsville' & nitrogenTreatment=='Medium' ~ 'Crawfordsville B',
                                 .default = sublocation),
         irrigationProvided = case_when(location %in% c('Crawfordsville', 'Ames', 'Missouri Valley', 'Lincoln') ~ 0,
                                        location=='Scottsbluff' ~ 16.9), 
         nitrogenTreatment = case_when(location=='Scottsbluff' ~ 'High',
                                       location=='Lincoln' ~ 'Medium',
                                       .default = nitrogenTreatment),
         poundsOfNitrogenPerAcre = case_when(location=='Scottsbluff' ~ 250,
                                             location=='Lincoln' ~ 150,
                                             location=='Crawfordsville' & nitrogenTreatment=='High' ~ 225,
                                             location=='Crawfordsville' & nitrogenTreatment=='Medium' ~ 150,
                                             location=='Crawfordsville' & nitrogenTreatment=='Low' ~ 75,
                                             .default = poundsOfNitrogenPerAcre)) %>%
  filter(!is.na(genotype)) %>%
  select(!shelledCobLength)

responseVars <- c('numberPrimaryEars', 'numberSecondaryEars', 'secondaryEarKernels', 'earWidth', 'kernelFillLength', 'kernelRowNumber',
                  'kernelsPerRow', 'kernelsPerEar', 'shelledCobWidth', 'shelledCobMass', 'hundredKernelMass', 'kernelMassPerEar',
                  'plantDensity', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'earLength')
# inbredsWide <- plotRepCorr(inbreds, 'nitrogenTreatment', 'genotype', responseVars, 'location')

inbreds <- inbreds %>%
  mutate(earLength =  earLength * 0.1, 
         shelledCobWidth = shelledCobWidth * 0.1,
         earWidth = earWidth * 0.1,
         kernelFillLength = kernelFillLength * 0.1)
inbreds <- mutate(inbreds, across(where(is.numeric), ~case_when(. <= 0 ~ NA, .default = .)))

inbredsWide <- plotRepCorr(inbreds, 'nitrogenTreatment', 'genotype', responseVars, 'location')


# outliers <- list()
# for (i in responseVars)
# {
#   outliers[[i]] <- idOutliers(inbreds, i)
# }

# Histograms
for(i in responseVars)
{
  p <- ggplot(inbreds, aes(.data[[i]])) +
    geom_histogram() +
    facet_grid(vars(location))
  print(p)
}

# Fix outliers where they were measured in cm or due to data entry or just obviously wrong (2x next observation)
# Also calculate some other variables
inbreds <- inbreds %>%
  rowwise() %>%
  mutate(kernelFillLength = case_when(location=='Missouri Valley' & plotNumber==215 ~ kernelFillLength*10, .default = kernelFillLength),
         earWidth = case_when(location=='Lincoln' & plotNumber==1115 ~ mean(c(31.57, 14.34, 20.14, 16.28))*0.1, 
                              location=='Missouri Valley' & plotNumber==215 ~ earWidth*10,
                              .default = earWidth),
         shelledCobWidth = case_when(location=='Missouri Valley' & plotNumber==215 ~ shelledCobWidth*10, 
                                     qrCode=='22-C-1595838' ~ 21.44*0.1,
                                     location=='Lincoln' & plotNumber==1041 ~ mean(c(22.87, 24, 23.21, 22.92, 23.36))*0.1,
                                     location=='Lincoln' & plotNumber==2223 ~ mean(c(22.19, 19.05, 22.86, 24.22, 23.2, 19.62))*0.1,
                                     .default = shelledCobWidth),
         earLength = case_when(location=='Missouri Valley' & plotNumber==215 ~ earLength*10, 
                               earLength > 30 ~ NA, 
                               .default = earLength),
         kernelsPerEar = case_when(location=='Scottsbluff' & plotNumber==162 ~ NA, .default = kernelsPerEar),
         kernelRowNumber = case_when(location=='Scottsbluff' & plotNumber==677 ~ mean(c(10, 12, 13, 13, 13)), .default = kernelRowNumber),
         plotLength = 7.5,
         plantDensity = (totalStandCount/2)*(7.5/17)*1000,
         hundredKernelMass = case_when(is.na(hundredKernelMass) ~ (kernelMassPerEar/kernelsPerEar)*100,
                                       .default = hundredKernelMass))
inbreds <- inbreds %>%
  mutate(plantingDate = mdy(plantingDate),
         daysToAnthesis = difftime(anthesisDate, plantingDate, units = 'days') %>%
           as.integer(),
         daysToSilk = difftime(silkDate, plantingDate, units = 'days') %>%
           as.integer(),
         anthesisSilkingInterval = daysToSilk - daysToAnthesis)
inbreds <- inbreds %>%
  rowwise() %>%
  mutate(GDDToAnthesis = case_when(location=='Lincoln' ~ getCumulativeGDDs(plantingDate, anthesisDate, weather.daily, location), 
                                   .default = NA),
         GDDToSilk = case_when(location=='Lincoln' ~ getCumulativeGDDs(plantingDate, silkDate, weather.daily, location), 
                               .default = NA))
inbreds <- mutate(inbreds, 
                  anthesisSilkingIntervalGDD = GDDToSilk - GDDToAnthesis,
                  hundredKernelMass = case_when(hundredKernelMass <= 0 ~ NA, .default = hundredKernelMass))

responseVars <- c(responseVars, 'daysToAnthesis', 'GDDToAnthesis', 'daysToSilk', 'GDDToSilk', 'anthesisSilkingInterval', 'anthesisSilkingIntervalGDD')

inbredsWide <- plotRepCorr(inbreds, 'nitrogenTreatment', 'genotype', responseVars, 'location')


# outliers <- list()
# for (i in responseVars)
# {
#   outliers[[i]] <- idOutliers(inbreds, i)
# }

# Histograms
for(i in responseVars)
{
  p <- ggplot(inbreds, aes(.data[[i]])) +
    geom_histogram() +
    facet_grid(vars(location))
  print(p)
}

inbreds <- inbreds %>%
  mutate(hundredKernelMass = case_when(hundredKernelMass > 50 ~ NA, .default = hundredKernelMass))
inbreds <- inbreds %>% 
  mutate(earWidth = case_when(earWidth > 10 ~ NA, .default = earWidth))

inbreds <- inbreds %>%
  mutate(across(where(is.character), ~str_replace_all(., ',', ';')))
# Export v3.5
write.table(inbreds, 'outData/HIPS_2022_V3.5_INBREDS.csv', quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)

inbredsWide <- plotRepCorr(inbreds, 'nitrogenTreatment', 'genotype', responseVars, 'location')


# outliers <- list()
# for (i in responseVars)
# {
#   outliers[[i]] <- idOutliers(inbreds, i)
# }

# Histograms
for(i in responseVars)
{
  p <- ggplot(inbreds, aes(.data[[i]])) +
    geom_histogram() +
    facet_grid(vars(location))
  print(p)
}

inbreds <- inbreds %>%
  rowwise() %>%
  mutate(anthesisSilkingInterval = case_when(anthesisSilkingInterval < -10 | anthesisSilkingInterval > 20 ~ NA,
                                             .default = anthesisSilkingInterval),
         anthesisSilkingIntervalGDD = case_when(anthesisSilkingIntervalGDD < -250 | anthesisSilkingIntervalGDD > 500 ~ NA, 
                                                .default = anthesisSilkingIntervalGDD),
         daysToSilk = case_when(daysToSilk > 110 ~ NA, .default = daysToSilk),
         earHeight = case_when(location=='Crawfordsville' & (earHeight < 12 | earHeight > 175) ~ NA, .default = earHeight),
         earLength = case_when((location=='Ames' & earLength > 25) | 
                                 (location=='Crawfordsville' & (earLength < 5 | earLength > 21)) | 
                                 (location=='Missouri Valley' & earLength > 20) ~ NA, 
                               .default = earLength),
         earWidth = case_when((location %in% c('Ames', 'Crawfordsville') & earWidth > 5.5) | 
                                (location=='Lincoln' & (earWidth < 2 | earWidth > 4.25)) |
                                (location=='Missouri Valley' & earWidth > 5) ~ NA, 
                              .default = earWidth),
         flagLeafHeight = case_when((flagLeafHeight > 250) | 
                                      (location=='Ames' & flagLeafHeight < 45) | 
                                      (location=='Lincoln' & flagLeafHeight < 50) | 
                                      (location=='Missouri Valley' & (flagLeafHeight < 50 | flagLeafHeight > 225)) ~ NA,
                                    .default = flagLeafHeight), 
         GDDToSilk = case_when(GDDToSilk > 2200 ~ NA, .default = GDDToSilk),
         hundredKernelMass = case_when((location=='Crawfordsville' & (hundredKernelMass < 10 | hundredKernelMass > 37.5)) |
                                         (location=='Lincoln' & hundredKernelMass > 31) |
                                         (location=='Scottsbluff' & hundredKernelMass > 32) ~ NA, 
                                       .default = hundredKernelMass),
         kernelFillLength = case_when((location=='Lincoln' & (kernelFillLength < 2.5 | kernelFillLength > 17)) |
                                        (location=='Missouri Valley' & kernelFillLength > 20) |
                                        (location=='Scottsbluff' & kernelFillLength > 18) ~ NA, 
                                      .default = kernelFillLength),
         kernelMassPerEar = case_when((location=='Ames' & kernelMassPerEar > 155) | 
                                        (location=='Crawfordsville' & kernelMassPerEar > 150) | 
                                        (location=='Lincoln' & kernelMassPerEar > 100) |
                                        (location=='Missouri Valley' & kernelMassPerEar > 135) |
                                        (location=='Scottsbluff' & kernelMassPerEar > 148) ~ NA,
                                      .default = kernelMassPerEar),
         kernelRowNumber = case_when((location=='Crawfordsville' & kernelRowNumber < 8) | 
                                       (location=='Lincoln' & kernelRowNumber < 5) | 
                                       (location=='Missouri Valley' & kernelRowNumber < 6) | 
                                       (location=='Scottsbluff' & kernelRowNumber > 18) ~ NA,
                                     .default = kernelRowNumber),
         kernelsPerEar = case_when((location=='Ames' & kernelsPerEar > 630) |
                                     (location=='Crawfordsville' & kernelsPerEar > 625) |
                                     (location=='Lincoln' & kernelsPerEar > 500) | 
                                     (location=='Missouri Valley' & kernelsPerEar > 525) |
                                     (location=='Scottsbluff' & kernelsPerEar > 600) ~ NA, 
                                   .default = kernelsPerEar),
         kernelsPerRow = case_when(kernelsPerRow > 40 | (location=='Lincoln' & kernelsPerRow > 37) ~ NA, .default = kernelsPerRow),
         shelledCobMass = case_when(shelledCobMass > 40 |
                                      (location=='Crawfordsville' & shelledCobMass > 35) | 
                                      (location=='Lincoln' & shelledCobMass > 25) | 
                                      (location=='Missouri Valley' & shelledCobMass > 31) ~ NA,
                                    .default = shelledCobMass),
         shelledCobMass = case_when((location=='Crawfordsville' & shelledCobWidth < 1) |
                                      (location=='Lincoln' & (shelledCobWidth < 0.75 | shelledCobWidth > 3)) |
                                      (location=='Scottsbluff' & (shelledCobWidth < 1.25 | shelledCobWidth > 3.5)) ~ NA, 
                                    .default = shelledCobWidth))
inbreds <- inbreds %>%
  mutate(earWidth = round(earWidth, digits = 3),
         earFillLength = round(kernelFillLength, digits = 3),
         kernelRowNumber = round(kernelRowNumber, digits = 1),
         kernelsPerRow = round(kernelsPerRow, digits = 1),
         kernelsPerEar = round(kernelsPerEar, digits = 0),
         shelledCobWidth = round(shelledCobWidth, digits = 3),
         shelledCobMass = round(shelledCobMass, digits = 3),
         hundredKernelMass = round(hundredKernelMass, digits = 3),
         kernelMassPerEar = round(kernelMassPerEar, digits = 1),
         plantDensity = round(plantDensity, digits = 0),
         plantingDate = as.character(plantingDate),
         earHeight = round(earHeight, digits = 0),
         flagLeafHeight = round(flagLeafHeight, digits = 0),
         earLength = round(earLength, digits = 3),
         anthesisDate = as.character(anthesisDate),
         silkDate = as.character(silkDate), 
         GDDToAnthesis = round(GDDToAnthesis, digits = 2),
         GDDToSilk = round(GDDToSilk, digits = 2),
         anthesisSilkingIntervalGDD = round(anthesisSilkingIntervalGDD, digits = 2)) %>%
  select(qrCode, location, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, experiment, plotLength, totalStandCount, block, row, range,
         plotNumber, genotype, plantingDate, anthesisDate, silkDate, daysToAnthesis, daysToSilk, anthesisSilkingInterval, GDDToAnthesis, GDDToSilk,
         anthesisSilkingIntervalGDD, earHeight, flagLeafHeight, plantDensity, earLength, earFillLength, earWidth, shelledCobWidth, kernelsPerRow, kernelRowNumber,
         kernelsPerEar, hundredKernelMass, kernelMassPerEar, shelledCobMass, kernelColor, notes) %>%
  arrange(location, sublocation, block, plotNumber)

# Export v4.8
write.table(inbreds, 'outData/HIPS_2022_V4.8_INBREDS.csv', quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)
responseVars <- c(c('earWidth', 'earFillLength', 'kernelRowNumber',
                    'kernelsPerRow', 'kernelsPerEar', 'shelledCobWidth', 'shelledCobMass', 'hundredKernelMass', 'kernelMassPerEar',
                    'plantDensity', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'earLength', 'daysToAnthesis', 'GDDToAnthesis', 'daysToSilk', 'GDDToSilk', 'anthesisSilkingInterval', 'anthesisSilkingIntervalGDD'))

inbredsWide <- plotRepCorr(inbreds, 'nitrogenTreatment', 'genotype', responseVars, 'location')
