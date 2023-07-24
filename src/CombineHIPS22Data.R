library(tidyverse)
library(readxl)
library(readr)
library(grDevices)
library(lubridate)
# Read in field data
linc22combined <- read_excel("data/Summary of Lincoln Hybrid HIPS 2022 Data.xlsx", 
                             sheet = "Combined Dataset", col_types = c("text", 
                                                                       "numeric", "text", "text", "text", 
                                                                       "text", "numeric", "numeric", "numeric", 
                                                                       "text", "date", "text", "date", "text", 
                                                                       "text", "text", "text", "text", "text", 
                                                                       "text", "text", "text", "text", "text", 
                                                                       "text", "text", "date", "date", "numeric", 
                                                                       "numeric", "numeric", "numeric"))
# Save original column names
orig_colnames <- colnames(linc22combined)
# Rename columns so we can work with them in R --> range and row may look inverted here but they're not! they were inverted in the combined dataset sheet
colnames(linc22combined) <- c('qr', 'plot', 'loc', 'field', 'nLvl', 'irrigation', 'rep', 'row', 'range', 'genotype', 'anthesisDate', 'anthesisCollector', 
                              'silkingDate', 'silkingCollector', 'notes', 'leafLen1', 'leafWidth1', 'leafLen2', 'leafWidth2', 'earHt1', 'flagLeafHt1', 
                              'plantHt1', 'earHt2', 'flagLeafHt2', 'plantHt2', 'leafDimHtCollector', 'leafDimHtDate', 'harvestDate', 'combineYield', 
                              'combineMoisture', 'combineTestWt', 'harvestSeq')
# Convert to tibble so we can use tidyverse and add a population column
linc22combined <- as_tibble(linc22combined) %>%
  mutate(population = 'Hybrid')
# Check formatting in the columns
unique(linc22combined$qr)
unique(linc22combined$plot)
unique(linc22combined$loc)
unique(linc22combined$field)
unique(linc22combined$nLvl)
unique(linc22combined$irrigation)
unique(linc22combined$rep)
unique(linc22combined$range)
unique(linc22combined$row)
unique(linc22combined$genotype) # capitalize
unique(linc22combined$anthesisDate)
unique(linc22combined$anthesisCollector) # 'LIna', 'kyle', '?'
unique(linc22combined$silkingDate)
unique(linc22combined$silkingCollector) # '?', 'kyle', 'N/A'
unique(linc22combined$notes)
unique(linc22combined$leafLen1) # 'Stunting', 'No female'
unique(linc22combined$leafWidth1) # 'Stunted', 'Stunded', 'Stunting', 'No female'
unique(linc22combined$leafLen2) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$leafWidth2) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$earHt1) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$flagLeafHt1) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$plantHt1) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$earHt2) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$flagLeafHt2) # 'Stunded', 'Stunting', 'No female', 'Stunted', need to look at val of 1900
unique(linc22combined$plantHt2) # 'Stunded', 'Stunting', 'No female', 'Stunted'
unique(linc22combined$leafDimHtCollector) # 'Lina and Isabel' --> 'Isabel/Lina', #'AIND' --> 'AI/ND'
unique(linc22combined$harvestDate)
unique(linc22combined$combineYield)
unique(linc22combined$combineMoisture)
unique(linc22combined$combineTestWt)
unique(linc22combined$harvestSeq)

stunting <- c('Stunting', 'Stunted', 'Stunded')
noFemale <- 'No female'
# Fix formatting
linc22combined <- rowwise(linc22combined) %>%
  mutate(genotype = str_to_upper(genotype),
         anthesisCollector = str_replace(anthesisCollector, 'LIna', 'Lina') %>%
           str_replace('kyle', 'Kyle') %>%
           na_if('N/A') %>%
           na_if('?'),
         silkingCollector = str_replace(silkingCollector, 'kyle', 'Kyle') %>%
           str_replace('kyle', 'Kyle') %>%
           na_if('N/A') %>%
           na_if('?'),
         leafDimHtNotes = case_when(leafLen1 %in% stunting|
                                      leafWidth1 %in% stunting|
                                      leafLen2 %in% stunting|
                                      leafWidth2 %in% stunting|
                                      earHt1 %in% stunting|
                                      flagLeafHt1 %in% stunting|
                                      plantHt1 %in% stunting|
                                      earHt2 %in% stunting|
                                      flagLeafHt2 %in% stunting|
                                      plantHt2 %in% stunting ~ 'Stunting', 
                                    leafLen1 %in% noFemale|
                                      leafWidth1 %in% noFemale|
                                      leafLen2 %in% noFemale|
                                      leafWidth2 %in% noFemale|
                                      earHt1 %in% noFemale|
                                      flagLeafHt1 %in% noFemale|
                                      plantHt1 %in% noFemale|
                                      earHt2 %in% noFemale|
                                      flagLeafHt2 %in% noFemale|
                                      plantHt2 %in% noFemale ~ 'No female'),
         leafLen1 = case_when(leafLen1 %in% stunting | leafLen1 %in% noFemale ~ NA, .default = leafLen1) %>%
           as.numeric(),
         leafWidth1 = case_when(leafWidth1 %in% stunting | leafWidth1 %in% noFemale ~ NA, .default = leafWidth1) %>%
           as.numeric(),
         leafLen2 = case_when(leafLen2 %in% stunting | leafLen2 %in% noFemale ~ NA, .default = leafLen2) %>%
           as.numeric(),
         leafWidth2 = case_when(leafWidth2 %in% stunting | leafWidth2 %in% noFemale ~ NA, .default = leafWidth2) %>%
           as.numeric(),
         earHt1 = case_when(earHt1 %in% stunting | earHt1 %in% noFemale ~ NA, .default = earHt1) %>%
           as.numeric(),
         flagLeafHt1 = case_when(flagLeafHt2 %in% stunting | flagLeafHt1 %in% noFemale ~ NA, .default = flagLeafHt1) %>%
           as.numeric(),
         plantHt1 = case_when(plantHt1 %in% stunting | plantHt1 %in% noFemale ~ NA, .default = plantHt1) %>%
           as.numeric(),
         earHt2 = case_when(earHt2 %in% stunting | earHt2 %in% noFemale ~ NA, .default = earHt2) %>%
           as.numeric(),
         flagLeafHt2 = case_when(flagLeafHt2 %in% stunting | flagLeafHt2 %in% noFemale ~ NA, .default = flagLeafHt2) %>%
           as.numeric(),
         plantHt2 = case_when(plantHt2 %in% stunting | plantHt2 %in% noFemale ~ NA, .default = plantHt2) %>% as.numeric(),
         leafDimHtCollector = str_replace(leafDimHtCollector, 'Lina and Isabel', 'Isabel/Lina') %>%
           str_replace('AIND', 'AI/ND'))
# # Read in ear phenotypic data
# eardata22 <- read_csv("data/2022_Hybrid HIPS - Post Harvest Data - Prototype File.csv")
# 
# # Save original column names
# orig_colnames_eardata <- colnames(eardata22)
# # Get rid of the empty extra columns it reads in
# eardata22 <- eardata22[, 1:14]
# # Rename columns so we can work with them in R
# colnames(eardata22) <- c('earDataCollector', 'qr', 'earNum', 'earWidth', 'kernelFillLen', 'kernelRows', 'kernelsPerRow', 'earWt', 'seedColor', 
#                          'totalKernelCt', 'cobLen', 'cobWidth', 'cobWt', 'hundredKernelWt')
# # Convert to tibble so we can use tidyverse
# eardata22 <- as_tibble(eardata22)
# # Split by location so we can parse qr - the formatting varies by location
# eardata22_np1 <- filter(eardata22, str_detect(qr, 'North Platte') & str_detect(qr, 'Full'))
# # unique(eardata22_np1$qr)
# eardata22_np2 <- filter(eardata22, str_detect(qr, 'North Platte') & str_detect(qr, 'Partial'))
# # unique(eardata22_np2$qr)
# eardata22_np3 <- filter(eardata22, (str_detect(qr, 'North Platte') & str_detect(qr, 'No Irrigation')) | (str_detect(qr, 'nORTH pLATTE') & str_detect(qr, 'nO iRRIGATION')))
# # unique(eardata22_np3$qr)
# eardata22_mv <- filter(eardata22, str_detect(qr, 'MV'))
# eardata22_linc <- filter(eardata22, !str_detect(qr, 'North Platte') & !str_detect(qr, 'MV') & !str_detect(qr, 'nORTH pLATTE'))
# Parse np1 qrs
# Function to parse north platte qrs
parseNorthPlatteQR <- function(data)
{
  df <- data
  data_parsed <- rowwise(df) %>%
    mutate(qr = str_to_upper(qr)) %>%
    mutate(plot = str_split_i(qr, '[$]', 4) %>%
             str_remove('PLOT') %>%
             as.numeric(),
           loc = case_when(str_detect(qr,'FULL') ~ 'North Platte1',
                       str_detect(qr, 'PARTIAL') ~ 'North Platte2',
                       str_detect(qr,'NO') ~ 'North Platte3'),
           field = 'Hybrid HIPS',
           nLvl = str_split_i(qr, '[$]', 2) %>%
             str_split_i('-', 3) %>%
             str_remove('NITROGEN') %>%
             str_trim('both') %>%
             str_to_sentence(),
           irrigation = case_when(str_detect(qr, 'FULL') ~ 'Full',
                       str_detect(qr, 'PARTIAL') ~ 'Partial',
                       str_detect(qr, 'NO') ~ 'Dryland'), 
           rep = str_split_i(qr, '[$]', 3) %>%    
             str_remove('REP') %>%
             as.numeric(),
           range = str_split_i(qr, '[$]', 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qr, '[$]', 5) %>% 
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qr, '[$]', 7) %>%
             str_to_upper(),
           population = 'Hybrid')
  return(data_parsed)
}
# np <- filter(eardata22, str_detect(qr, 'Platte')) %>%
#   parseNorthPlatteQR()
# 
# eardata22_np1 <- rowwise(eardata22_np1) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot'),
#          loc = 'North Platte1',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_trim('both'),
#          irrigation = 'Full', 
#          rep = str_split_i(qr, '[$]', 3) %>% 
#            str_remove('Rep'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row'), 
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# # Check np1 qr parsing
# unique(eardata22_np1$plot)
# unique(eardata22_np1$nLvl)
# unique(eardata22_np1$rep)
# unique(eardata22_np1$range) # why are there ones missing? seems like a lot are missing
# unique(eardata22_np1$row)
# unique(eardata22_np1$genotype)
# 
# # Parse np2
# eardata22_np2 <- rowwise(eardata22_np2) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot'),
#          loc = 'North Platte2',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_trim('both'),
#          irrigation = 'Partial', 
#          rep = str_split_i(qr, '[$]', 3) %>% 
#            str_remove('Rep'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row'), 
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# # Check np2 qr parsing
# unique(eardata22_np2$plot)
# unique(eardata22_np2$nLvl)
# unique(eardata22_np2$rep)
# unique(eardata22_np2$range) # why are there ones missing? seems like a lot are missing
# unique(eardata22_np2$row)
# unique(eardata22_np2$genotype)
# 
# # Parse np3
# eardata22_np3 <- rowwise(eardata22_np3) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT'),
#          loc = 'North Platte3',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_remove('nITROGEN') %>%
#            str_trim('both') %>%
#            str_replace('lOW', 'Low') %>%
#            str_replace('hIGH', 'High'),
#          irrigation = 'Dryland', 
#          rep = str_split_i(qr, '[$]', 3) %>% 
#            str_remove('Rep') %>%
#            str_remove('rEP'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'), 
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# # Check np3 qr parsing
# unique(eardata22_np3$plot)
# unique(eardata22_np3$nLvl)
# unique(eardata22_np3$rep)
# unique(eardata22_np3$range) # why are there ones missing? seems like a lot are missing
# unique(eardata22_np3$row)
# unique(eardata22_np3$genotype)

# Function to parse MV qrs
## Plot is the UR_plot (unreplicated) plot number
parseMissouriValleyQR <- function(data)
{
  df <- data
  data_parsed <- df %>%
    rowwise() %>%
    mutate(qr = str_to_upper(qr)) %>%
    mutate(plot = str_split_i(qr, '[$]', 4) %>%
             str_remove('PLOT') %>%
             as.numeric(), 
           loc = 'Missouri Valley',
           field = case_when(str_detect(qr, 'HYBRID') ~ 'Hybrid HIPS',
                       str_detect(qr, 'INBRED') ~ 'Inbred HIPS'),
           rep = str_split_i(qr, '[$]', 3) %>%
             str_remove('REP') %>%
             as.numeric(),
           range = str_split_i(qr, '[$]', 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qr, '[$]', 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qr, '[$]', 7) %>%
             str_to_upper(),
           population = case_when(str_detect(qr, 'HYBRID') ~ 'Hybrid',
                       str_detect(qr, 'INBRED') ~ 'Inbred'),
           irrigation = 'Dryland', 
           nLvl = 'Medium') %>%
    mutate(plot = case_when(population=='Hybrid' & rep==1 & plot<10 ~ as.numeric(str_c('10', plot)),
                            population=='Hybrid' & rep==1 & plot>=10 ~ as.numeric(str_c('1', plot)),
                            population=='Hybrid' & rep==2 & plot<10 ~ as.numeric(str_c('20', plot)),
                            population=='Hybrid' & rep==2 & plot>=10 ~ as.numeric(str_c('2', plot)),
                            population=='Inbred' & rep==1 ~ (plot + 100),
                            population=='Inbred' & rep==2 ~ (plot + 200)))
  return(data_parsed)
}
# mv <- filter(eardata22, str_detect(qr, 'MV')) %>%
#   parseMissouriValleyQR(qr)
# Parse mv qrs
# eardata22_mv <- rowwise(eardata22_mv) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot'),
#          loc = 'MV', 
#          field = str_split_i(qr, '[$]', 2) %>%
#            str_replace('-', ' '),
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row'),
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper())
# # Check mv qr parsing
# unique(eardata22_mv$plot)
# unique(eardata22_mv$field)
# unique(eardata22_mv$nLvl)
# unique(eardata22_mv$irrigation)
# unique(eardata22_mv$rep)
# unique(eardata22_mv$range)
# unique(eardata22_mv$row)
# unique(eardata22_mv$genotype)
# 
# # Split MV into hybrid and inbreds and add a population column, then recombine
# eardata22_mv_hybrid <- filter(eardata22_mv, field == 'Hybrid HIPS') %>%
#   mutate(population = 'Hybrid')
# eardata22_mv_inbred <- filter(eardata22_mv, field == 'Inbred HIPS') %>%
#   mutate(population = 'Inbred')
# eardata22_mv <- bind_rows(eardata22_mv_hybrid, eardata22_mv_inbred)
# 
# #TODO: Add N levels to MV
# # Function to parse Lincoln qrs
parseLincolnQR <- function(data)
{
  df <- data
  
  data_parsed <- df %>%
    rowwise() %>%
    mutate(qr = str_to_upper(qr)) %>%
    mutate(plot = str_split_i(qr, '[$]', 1) %>%
             as.numeric(),
           loc = 'Lincoln',
           field = 'Hybrid HIPS',
           range = str_split_i(qr, '[$]', 3) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qr, '[$]', 2) %>%
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qr, '[$]', 4),
           irrigation = 'Dryland', 
           population = 'Hybrid')
  
  return(data_parsed)
}
# lnk <- filter(eardata22, !str_detect(qr, 'MV') & !str_detect(qr, 'Platte')) %>%
#   parseLincolnQR()
# # Parse Lincoln qr
# eardata22_linc <- rowwise(eardata22_linc) %>%
#   mutate(plot = str_split_i(qr, '[$]', 1),
#          loc = 'Lincoln',
#          field = 'Hybrid HIPS',
#          range = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 2) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'),
#          genotype = str_split_i(qr, '[$]', 4) %>%
#           str_to_upper())
# # Check Lincoln qr parsing
# unique(eardata22_linc$plot)
# unique(eardata22_linc$range)
# unique(eardata22_linc$row)
# unique(eardata22_linc$genotype)
# 
# # Okay, now put all the ear data back together
# eardata22 <- bind_rows(eardata22_np1, eardata22_np2, eardata22_np3, eardata22_mv, eardata22_linc)
# # Let's check formatting
# unique(eardata22$earDataCollector) #'c' --> 'C', 'ISND', IS/ND' --> 'ND/IS', 'NT/IS' --> 'IS/NT', 'AIND' --> 'AI/ND', 'IS/AI' --> 'AI/IS'
# unique(eardata22$qr)
# unique(eardata22$earNum) # 5*, 'N/A', 'EMPTY', '0' 
# unique(eardata22$plot) # "Unknown plot from Lincoln sack #16"
# unique(eardata22$loc)
# unique(eardata22$field)
# unique(eardata22$nLvl)
# unique(eardata22$irrigation)
# unique(eardata22$rep)
# unique(eardata22$range)
# unique(eardata22$row)
# unique(eardata22$genotype) # ''
# unique(eardata22$population)
# unique(eardata22$earWidth) # N/A
# unique(eardata22$kernelFillLen) # N/A
# unique(eardata22$kernelRows) # N/A, NAN
# unique(eardata22$kernelsPerRow) # N/A, 'uneven'
# unique(eardata22$earWt) # N/A
# unique(eardata22$seedColor) # N/A, MORE FORMATTING WILL BE NEEDED
# unique(eardata22$totalKernelCt) # N/A
# unique(eardata22$cobLen) # N/A
# unique(eardata22$cobWidth) # N/A
# unique(eardata22$hundredKernelWt) # N/A, 'not enough', 'Not enough'
# # Fix formatting
# eardata22 <- rowwise(eardata22) %>%
#   mutate(earDataCollector = case_when(earDataCollector=='c' ~ 'C', 
#                                       earDataCollector=='ISND'|earDataCollector=='IS/ND' ~ 'ND/IS',
#                                       earDataCollector=='NT/IS' ~ 'IS/NT', 
#                                       earDataCollector=='AIND' ~ 'AI/ND',
#                                       earDataCollector=='IS/AI' ~ 'AI/IS',
#                                       .default = earDataCollector),
#          earNum = case_when(earNum=='5*' ~ '5',
#                             earNum=='N/A'|earNum=='EMPTY'|earNum=='0' ~ NA, 
#                             .default = earNum),
#          genotype = case_when(genotype=="" ~ NA, .default = genotype),
#          earWidth = na_if(earWidth, 'N/A'),
#          kernelFillLen = na_if(kernelFillLen, 'N/A'),
#          kernelRows = case_when(kernelRows=='N/A'|kernelRows=='NAN' ~ NA, .default = kernelRows),
#          earPhenotypeNotes = case_when(kernelsPerRow=='uneven' ~ 'Uneven number of kernels per row', 
#                                        hundredKernelWt=='not enough'|hundredKernelWt=='Not enough' ~ 'Not enough',
#                                        .default = NA),
#          kernelsPerRow = case_when(kernelsPerRow=='N/A'|kernelsPerRow=='uneven' ~ NA, .default = kernelsPerRow),
#          earWt = na_if(earWt, 'N/A'),
#          seedColor = case_when(seedColor=='N/A' ~ NA, 
#                                seedColor=='yellow'|seedColor=='yelliw'|seedColor=='yellow \\ withe'|seedColor=='YELLOW'|
#                                  seedColor=='yelow'|seedColor=='yelllow'|seedColor=='yeloow'|seedColor=='yelloiw'|seedColor=='0yellow' ~ 'Yellow',
#                                seedColor=='Y/O'|seedColor=='yellow/orange'|seedColor=='yellow \\ orange'|seedColor=='yellow / orange'|
#                                  seedColor=='Yellow \\ orange'|seedColor=='orange yellow'|seedColor=='half orange half yellow'|
#                                  seedColor=='orange to yellow gradient'|seedColor=='orange w/ yellow surface' ~ 'Yellow/Orange', 
#                                seedColor=='orange'|seedColor=='orage' ~ 'Orange',
#                                seedColor=='yellow-red'|seedColor=='yellow with red dashes'|seedColor=='yellow/red stripes' ~ 'Yellow/Red',
#                                seedColor=='yellow with black kernels'|seedColor=='yellow/ few black' ~ 'Yellow/Black',
#                                seedColor=='orange (with purple stripes)' ~ 'Orange/Purple',
#                                seedColor=='red' ~ 'Red',
#                                seedColor=='white' ~ 'White', 
#                                .default = seedColor),
#          totalKernelCt = na_if(totalKernelCt, 'N/A'),
#          cobLen = na_if(cobLen, 'N/A'),
#          cobWidth = na_if(cobWidth, 'N/A'), 
#          hundredKernelWt = case_when(hundredKernelWt=='N/A'|hundredKernelWt=='not enough'|hundredKernelWt=='Not enough' ~ NA, 
#                                      .default = hundredKernelWt))
# # Now pivot wider, because we have multiple measurements per plot
# # First, concat all the data vals for a row together so we can do one pivot
# e22 <- ungroup(eardata22) %>%
#   mutate(across(c(earDataCollector, earWidth:hundredKernelWt, earPhenotypeNotes), str_replace_na)) %>%
#   rowwise() %>%
#   mutate(vals = str_c(earWidth, kernelFillLen, kernelRows, kernelsPerRow, earWt, seedColor, totalKernelCt, cobLen, cobWidth,
#                       cobWt, hundredKernelWt, earDataCollector, earPhenotypeNotes, sep = '-')) %>%
#   select(c(qr:earNum, plot:population, vals))
# # Check how many vals columns we should get
# unique(e22$earNum)
# # Some NAs
# # Look at the entries that have NAs for earNum
# na <- filter(e22, is.na(earNum))
# na <- unique(na$qr)
# na <- filter(e22, qr %in% na)
# # Okay, someone just forgot to put something in this column
# na <- group_by(na, qr) %>%
#   mutate(earNum = 1:n()) %>%
#   ungroup()
# 
# # Replace entries in na in eh22 with the na entries
# na_qrs <- unique(na$qr)
# e22 <- filter(e22, !(qr %in% na_qrs)) %>%
#   mutate(earNum = as.numeric(earNum))
# e22 <- bind_rows(e22, na)
# unique(e22$earNum)
# # Check for duplicates 
# dup <- e22 %>%
#   dplyr::group_by(qr, plot, loc, field, nLvl, irrigation, rep, range, row, genotype, population, earNum) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# # There were some duplicates, so let's figure out what's going on there
# dup_qrs <- unique(dup$qr)
# dup_qrs <- filter(e22, qr %in% dup_qrs)
# # Lets check if there is a missing plot in this range
# range24 <- filter(e22, loc=='North Platte3' & range=='24')
# as.numeric(unique(range24$row))
# # Based on field index, we should have rows 1-12 & 15-22 (rows 13 & 14 are border), but we are missing row 1
# # So throw out all observations for plot 1478 because we don't know if these ears are from row 2 (plot 1478) or row 1 (plot 1477)
# dup_qr_rm <- unique(dup_qrs$qr)[1]
# dup_qrs <- filter(dup_qrs, !(qr==dup_qr_rm))
# # The rest seem to have 5 ears from the same plot, so we'll just reassign their earNum vals
# dup_qrs <- group_by(dup_qrs, qr) %>%
#   mutate(earNum = 1:n()) %>%
#   ungroup()
# # Now replace their rows in e22 with the updated versions
# dup_qrs_unique <- unique(dup_qrs$qr)
# e22 <- filter(e22, !(qr %in% dup_qrs_unique) & !(qr==dup_qr_rm)) %>%
#   bind_rows(dup_qrs) 
# 
# # Let's drop the observations with qr 'Unknown plot from Lincoln sack #16' because we can't assign a genotype to these without plot info
# e22 <- filter(e22, !(qr=='Unknown plot from Lincoln sack #16'))
#  
# # Now drop any observations with earNum > 4 per James's suggestion to keep balanced data
# e22 <- filter(e22, earNum<=4)
# # Check for duplicates one more time
# dup <- e22 %>%
#   dplyr::group_by(qr, plot, loc, field, nLvl, irrigation, rep, range, row, genotype, population, earNum) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# # Okay, NOW we can pivot
# e22 <- pivot_wider(e22, id_cols = c(plot:population), names_from = earNum, values_from = vals, names_prefix = 'vals', unused_fn = max)
# # Now we need to separate the 'vals' columns into individual columns again
# e22 <- rowwise(e22) %>%
#   mutate(earWidth1 = str_split_i(vals1, '-', 1),
#          kernelFillLen1 = str_split_i(vals1, '-', 2),
#          kernelRows1 = str_split_i(vals1, '-', 3), 
#          kernelsPerRow1 = str_split_i(vals1, '-', 4),
#          earWt1 = str_split_i(vals1, '-', 5),
#          seedColor1 = str_split_i(vals1, '-', 6),
#          totalKernelCt1 = str_split_i(vals1, '-', 7),
#          cobLen1 = str_split_i(vals1, '-', 8),
#          cobWidth1 = str_split_i(vals1, '-', 9), 
#          cobWt1 = str_split_i(vals1, '-', 10),
#          hundredKernelWt1 = str_split_i(vals1, '-', 11),
#          earDataCollector1 = str_split_i(vals1, '-', 12),
#          earPhenotypeNotes1 = str_split_i(vals1, '-', 13),
#          earWidth2 = str_split_i(vals2, '-', 1),
#          kernelFillLen2 = str_split_i(vals2, '-', 2),
#          kernelRows2 = str_split_i(vals2, '-', 3), 
#          kernelsPerRow2 = str_split_i(vals2, '-', 4),
#          earWt2 = str_split_i(vals2, '-', 5),
#          seedColor2 = str_split_i(vals2, '-', 6),
#          totalKernelCt2 = str_split_i(vals2, '-', 7),
#          cobLen2 = str_split_i(vals2, '-', 8),
#          cobWidth2 = str_split_i(vals2, '-', 9), 
#          cobWt2 = str_split_i(vals2, '-', 10),
#          hundredKernelWt2 = str_split_i(vals2, '-', 11),
#          earDataCollector2 = str_split_i(vals2, '-', 12),
#          earPhenotypeNotes2 = str_split_i(vals2, '-', 13),
#          earWidth3 = str_split_i(vals3, '-', 1),
#          kernelFillLen3 = str_split_i(vals3, '-', 2),
#          kernelRows3 = str_split_i(vals3, '-', 3), 
#          kernelsPerRow3 = str_split_i(vals3, '-', 4),
#          earWt3 = str_split_i(vals3, '-', 5),
#          seedColor3 = str_split_i(vals3, '-', 6),
#          totalKernelCt3 = str_split_i(vals3, '-', 7),
#          cobLen3 = str_split_i(vals3, '-', 8),
#          cobWidth3 = str_split_i(vals3, '-', 9), 
#          cobWt3 = str_split_i(vals3, '-', 10),
#          hundredKernelWt3 = str_split_i(vals3, '-', 11),
#          earDataCollector3 = str_split_i(vals3, '-', 12),
#          earPhenotypeNotes3 = str_split_i(vals3, '-', 13),
#          earWidth4 = str_split_i(vals4, '-', 1),
#          kernelFillLen4 = str_split_i(vals4, '-', 2),
#          kernelRows4 = str_split_i(vals4, '-', 3), 
#          kernelsPerRow4 = str_split_i(vals4, '-', 4),
#          earWt4 = str_split_i(vals4, '-', 5),
#          seedColor4 = str_split_i(vals4, '-', 6),
#          totalKernelCt4 = str_split_i(vals4, '-', 7),
#          cobLen4 = str_split_i(vals4, '-', 8),
#          cobWidth4 = str_split_i(vals4, '-', 9), 
#          cobWt4 = str_split_i(vals4, '-', 10),
#          hundredKernelWt4 = str_split_i(vals4, '-', 11),
#          earDataCollector4 = str_split_i(vals4, '-', 12),
#          earPhenotypeNotes4 = str_split_i(vals4, '-', 13))
# # Now drop the vals columns
# e22 <- select(e22, !(starts_with('vals')))
# # cast as numeric -- introduces NAs by coercion due to a N\A in the data
# e22 <- e22 %>%
#   ungroup() %>%
#   mutate(across(c(plot, rep, row, range, 
#            earWidth1:earWt1, totalKernelCt1:hundredKernelWt1, 
#            earWidth2:earWt2, totalKernelCt2:hundredKernelWt2, 
#            earWidth3:earWt3, totalKernelCt3:hundredKernelWt3,
#            earWidth4:earWt4, totalKernelCt4:hundredKernelWt4), ~na_if(.,'NA'))) %>%
#   mutate(across(c(plot, rep, row, range,
#                   earWidth1:earWt1, totalKernelCt1:hundredKernelWt1,
#                   earWidth2:earWt2, totalKernelCt2:hundredKernelWt2,
#                   earWidth3:earWt3, totalKernelCt3:hundredKernelWt3,
#                   earWidth4:earWt4, totalKernelCt4:hundredKernelWt4), ~na_if(., 'N/A'))) %>%
#   mutate(across(c(plot, rep, row, range, 
#                   earWidth1:earWt1, totalKernelCt1:hundredKernelWt1, 
#                   earWidth2:earWt2, totalKernelCt2:hundredKernelWt2, 
#                   earWidth3:earWt3, totalKernelCt3:hundredKernelWt3,
#                   earWidth4:earWt4, totalKernelCt4:hundredKernelWt4), ~na_if(., 'not enough seeds'))) %>%
#   mutate(across(c(plot, rep, row, range,
#                   earWidth1:earWt1, totalKernelCt1:hundredKernelWt1,
#                   earWidth2:earWt2, totalKernelCt2:hundredKernelWt2,
#                   earWidth3:earWt3, totalKernelCt3:hundredKernelWt3,
#                   earWidth4:earWt4, totalKernelCt4:hundredKernelWt4), as.numeric))
# # Read in additional ear data
# eardata_np_sb <- read_csv("data/NP-SB_2022.csv", 
#                                col_types = cols(...3 = col_skip(), `Kernel Color Notes` = col_skip(), 
#                                                           `Ear Width Notes` = col_skip(), `Kernel Fill Length Notes` = col_skip(), 
#                                                           `Kernel Row Number Notes` = col_skip(), 
#                                                           `Kernels per Row Notes` = col_skip(), 
#                                                           `Ear Weight Notes` = col_skip(), 
#                                                           `Kernel Count Notes` = col_skip(), 
#                                                           `Cob Width Notes` = col_skip(), `Cob Length Notes` = col_skip(), 
#                                                           `Cob Weight Notes` = col_skip(), 
#                                                           `100 Kernel weight Notes` = col_skip()))
# orig_colnames_np_sb_ears <- colnames(eardata_np_sb)
# colnames(eardata_np_sb) <- c('earDataCollector', 'qr', 'looseKernelCt', 'looseKernelWt', 'earNum', 'earPhenotypeNotes', 'seedColor', 
#                              'earWidth', 'kernelFillLen', 'kernelRows', 'kernelsPerRow', 'earWt', 'totalKernelCt', 'cobWidth', 'cobLen', 'cobWt', 
#                              'hundredKernelWt', 'totalMoisture', 'greenAtHarvest')
# # Fill in gaps in looseKernelCt, looseKernelWt, and totalMoisture with their plot values listed in the first row for their plot
# for (i in 1:length(eardata_np_sb$qr))
# {
#   if (!is.na(eardata_np_sb$looseKernelCt[i]))
#   {
#     j <- i
#     next
#   }
#   eardata_np_sb$looseKernelCt[i] <- eardata_np_sb$looseKernelCt[j]
#   eardata_np_sb$looseKernelWt[i] <- eardata_np_sb$looseKernelWt[j]
#   eardata_np_sb$totalMoisture[i] <- eardata_np_sb$totalMoisture[j]
# }
# # Convert to a tibble so we can use tidyverse
# eardata_np_sb <- as_tibble(eardata_np_sb)
# # Check formatting in the columns
# unique(eardata_np_sb$earDataCollector) # 'cc' --> 'CC', 'Hk' --> 'HK'
# unique(eardata_np_sb$qr) # North Platte, nORTH pLATTE, Scottsbluff, sCOTTSBLUFF
# unique(eardata_np_sb$looseKernelCt) #double --> convert to int
# unique(eardata_np_sb$looseKernelWt)
# unique(eardata_np_sb$earNum)
# unique(eardata_np_sb$earPhenotypeNotes)
# unique(eardata_np_sb$seedColor) # lots of formatting needed here
# unique(eardata_np_sb$earWidth) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$kernelFillLen) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$kernelRows) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$kernelsPerRow) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$earWt) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$totalKernelCt) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$cobWidth) # 'n/a', 'a/n', 'N/A', 'a', '2..5', convert to numeric
# unique(eardata_np_sb$cobLen) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$cobWt) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$hundredKernelWt) # 'n/a', 'a/n', 'N/A', convert to numeric
# unique(eardata_np_sb$totalMoisture) # 'n/a', 'a/n', 'N/A', remove %, convert to numeric
# unique(eardata_np_sb$greenAtHarvest) # 'N/A', 'N./A', 'N', 'no', 'yes', 'n/a', 'na', 'NO', 'mo', 'N/a'
# 
# # Let's define some vectors to use when we fix formatting
# yellow <- c('Yellow', "yellow", "yrllow", "yelloe", "Yelllow", "yello", "Y", "Y clear / Y dark", "yyellow", "yelllow", "yelow")
# orange <- c('Orange', "O", "ORGANGE", "orange", "ORANGE")
# yellow_orange <- c("Yellow /Orange", "yellow/orang e", "yellow/ orange", "Yellow / Orange", "Y/O,", "Yellow/Orange", "Yelllow/Orange", 
#                    "Yellow / orange", "YELLOW/ORANGE", "Y/O", "yellow/orange", "Yellow/orange")
# yellow_black <- c("yellow/ black", "yellow w/ black")
# yellow_red <- c("yellow red stripes", "Yellow / red stripes", "yellowred stripes", "Yellow/Red stripes", "Yellow/red strips", "yellow/red stripes", 
#                 "Yellow/red stripes", "yellow/red strips", "yellow/ red spots", "yellow/red strpes", "Yellow/ red stripes", "yellow w/ pink stripes", 
#                 "yellow/ red stripes", "Yellow/Red", "yellow/Red stripes", "yellow/red  stripes", "yellow. red stripes", "yellow/red striprs", 
#                 "yellow/re\\", "yelllow/red stripes")
# yellow_purple <- c("yellow/purple stripes")
# na <- c('n/a', 'a/n', 'N/A', 'a', 'no', 'N./A', 'N', 'na', 'NO', 'mo', 'N/a')
# red <- c("red", "Red")
# yellow_brown <- c("Yellow / brown")
# orange_red <- c("orange/red stripes")
# yellow_purple_red <- c("yellow/few purple/red stripes", "yellow/red stripes/few purple seeds", "yellow/ few purple/red stripes")
# yellow_white <- c("Y / WHITE")
# orange_purple <- c("orange/purple stripes")
# 
# # Fix formatting
# eNPSB <- rowwise(eardata_np_sb) %>%
#   mutate(earDataCollector = case_when(earDataCollector=='cc' ~ 'CC', earDataCollector=='Hk' ~ 'HK', .default = earDataCollector),
#          looseKernelCt = as.integer(looseKernelCt),
#          seedColor = case_when(seedColor %in% yellow ~ 'Yellow', 
#                                seedColor %in% orange ~ 'Orange',
#                                seedColor %in% yellow_orange ~ 'Yellow/Orange',
#                                seedColor %in% yellow_black ~ 'Yellow/Black',
#                                seedColor %in% yellow_red ~ 'Yellow/Red',
#                                seedColor %in% yellow_purple ~ 'Yellow/Purple',
#                                seedColor %in% na ~ NA,
#                                seedColor %in% red ~ 'Red',
#                                seedColor %in% yellow_brown ~ 'Yellow/Brown',
#                                seedColor %in% orange_red ~ 'Orange/Red',
#                                seedColor %in% yellow_purple_red ~ 'Yellow/Red/Purple',
#                                seedColor %in% yellow_white ~ 'Yellow/White',
#                                seedColor %in% orange_purple ~ 'Orange/Purple',
#                                .default = seedColor),
#          earWidth = case_when(earWidth %in% na ~ NA, .default = earWidth) %>%
#            as.numeric(),
#          kernelFillLen = case_when(kernelFillLen %in% na ~ NA, .default = kernelFillLen) %>%
#            as.numeric(),
#          kernelRows = case_when(kernelRows %in% na ~ NA, .default = kernelRows) %>%
#            as.numeric(),
#          kernelsPerRow = case_when(kernelsPerRow %in% na ~ NA, .default = kernelFillLen) %>%
#            as.numeric(),
#          earWt = case_when(earWt %in% na ~ NA, .default = earWt) %>%
#            as.numeric(),
#          totalKernelCt = case_when(totalKernelCt %in% na ~ NA, .default = kernelFillLen) %>%
#            as.numeric(),
#          cobWidth = case_when(cobWidth %in% na ~ NA,
#                               cobWidth=='2..5' ~ '2.5',
#                               .default = cobWidth) %>%
#            as.numeric(),
#          cobLen = case_when(cobLen %in% na ~ NA, .default = cobLen) %>%
#            as.numeric(),
#          cobWt = case_when(cobWt %in% na ~ NA, 
#                            cobWt=='22.94.' ~ '22.94', 
#                            cobWt=='19..9' ~ '19.9',
#                            .default = cobWt) %>%
#            as.numeric(),
#          hundredKernelWt = case_when(hundredKernelWt %in% na ~ NA,
#                                      hundredKernelWt=='25..51' ~ '25.51',
#                                      hundredKernelWt=='21..14' ~ '21.14',
#                                      hundredKernelWt=='33,.8' ~ '33.8',
#                                      .default = hundredKernelWt) %>%
#            as.numeric(),
#          totalMoisture = case_when(totalMoisture %in% na ~ NA, 
#                                    totalMoisture=='4..35%' ~ '4.35%', 
#                                    totalMoisture=='.4.41%' ~ '4.41%',
#                                    .default = totalMoisture) %>%
#            str_remove('[%]') %>%
#            as.numeric(),
#          greenAtHarvest = case_when(greenAtHarvest %in% na | is.na(greenAtHarvest) ~ FALSE, .default = TRUE))
# # Now split by location so we can parse the qrs
# e_np1 <- filter(eNPSB, (str_detect(qr, 'North Platte')|str_detect(qr, 'nORTH pLATTE')) & (str_detect(qr, 'Full')|str_detect(qr, 'fULL')))
# e_np2 <- filter(eNPSB, (str_detect(qr, 'North Platte')|str_detect(qr, 'nORTH pLATTE')) & (str_detect(qr, 'Partial')|str_detect(qr, 'pARTIAL')))
# e_np3 <- filter(eNPSB, (str_detect(qr, 'North Platte')|str_detect(qr, 'nORTH pLATTE')) & (str_detect(qr, 'No Irrigation')|str_detect(qr, 'nO iRRIGATION')))
# e_sb <- filter(eNPSB, str_detect(qr, 'Scottsbluff')|str_detect(qr, 'sCOTTSBLUFF'))
# qrs_np_sb <- c(unique(e_np1$qr), unique(e_np2$qr), unique(e_np3$qr), unique(e_sb$qr))
# e_linc <- filter(eNPSB, !(qr %in% qrs_np_sb))
# misc_plot <- e_linc[5:8, ]
# e_linc <- e_linc[1:4, ]
# 
# # Parse the qrs for np1
# e_np1 <- rowwise(e_np1) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT'),
#          loc = 'North Platte1',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_remove('nITROGEN') %>%
#            str_trim('both') %>%
#            str_replace('hIGH', 'High') %>%
#            str_replace('mEDIUM', 'Medium') %>%
#            str_replace('lOW', 'Low'),
#          irrigation = 'Full',
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'),
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# 
# # Check np1 parsing
# unique(e_np1$plot)
# unique(e_np1$nLvl)
# unique(e_np1$rep)
# unique(e_np1$range)
# unique(e_np1$row)
# unique(e_np1$genotype)
# 
# # Parse np2 qrs
# e_np2 <- rowwise(e_np2) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT'),
#          loc = 'North Platte2',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_remove('nITROGEN') %>%
#            str_trim('both') %>%
#            str_replace('hIGH', 'High') %>%
#            str_replace('mEDIUM', 'Medium') %>%
#            str_replace('lOW', 'Low'),
#          irrigation = 'Partial',
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'),
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# # Check np2 parsing
# unique(e_np2$plot)
# unique(e_np2$nLvl)
# unique(e_np2$rep)
# unique(e_np2$range)
# unique(e_np2$row)
# unique(e_np2$genotype)
# 
# # Parse np3 qrs
# e_np3 <- rowwise(e_np3) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT'),
#          loc = 'North Platte3',
#          field = 'Hybrid HIPS',
#          nLvl = str_split_i(qr, '[$]', 2) %>%
#            str_split_i('-', 3) %>%
#            str_remove('Nitrogen') %>%
#            str_remove('nITROGEN') %>%
#            str_trim('both') %>%
#            str_replace('hIGH', 'High') %>%
#            str_replace('mEDIUM', 'Medium') %>%
#            str_replace('lOW', 'Low'),
#          irrigation = 'Dryland',
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'),
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid')
# # Check np3 parsing
# unique(e_np3$plot)
# unique(e_np3$nLvl)
# unique(e_np3$rep)
# unique(e_np3$range)
# unique(e_np3$row)
# unique(e_np3$genotype) 

# Parse sb qrs
# Function to parse sb qrs, accounts for the swapped labels and filters out border plots
parseScottsbluffQR <- function(data)
{
  df <- data
  data_parsed <- df %>%
    rowwise() %>%
    mutate(qr = str_to_upper(qr)) %>%
    mutate(plot = str_split_i(qr, '[$]', 4) %>%
             str_remove('PLOT') %>%
             as.numeric(),
           loc = 'Scottsbluff',
           field = case_when(str_detect(qr, 'HYBRID') ~ 'Hybrid HIPS',
                             str_detect(qr, 'INBRED') ~ 'Inbred HIPS'),
           nLvl = case_when(str_detect(qr, 'HIGH') ~ 'Low',
                            str_detect(qr, 'MEDIUM') ~ 'Medium',
                            str_detect(qr, 'LOW') ~ 'High'),
           irrigation = 'Full',
           rep = str_split_i(qr, '[$]', 3) %>%
             str_remove('REP') %>%
             as.numeric(),
           range = str_split_i(qr, '[$]', 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qr, '[$]', 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qr, '[$]', 7) %>%
             case_when(str_detect(., 'FILL') ~ 'BORDER', .default = .),
           population = case_when(str_detect(qr, 'HYBRID') ~ 'Hybrid',
                                  str_detect(qr, 'INBRED') ~ 'Inbred'))
  
  return(data_parsed)
}


# sb <- filter(eNPSB, str_detect(qr, 'Scottsbluff')|str_detect(qr, 'sCOTTSBLUFF')) %>%
#   parseScottsbluffQR()
# # Drop fill (border) plots first
# e_sb <- filter(e_sb, !(str_detect(qr, 'Fill')))
# e_sb <- rowwise(e_sb) %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT'),
#          loc = 'Scottsbluff',
#          field = 'Hybrid HIPS',
#          nLvl = case_when(str_detect(qr, 'High')|str_detect(qr, 'hIGH') ~ 'Low', 
#                           str_detect(qr, 'Low')|str_detect(qr, 'lOW') ~ 'High',
#                           .default = 'Medium'),
#          irrigation = 'Full', ## irrigated 1 hr every Friday from 4-5 pm --> double check what level at North Platte this corresponds to
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP') %>%
#            str_replace('l', '1'),
#          range = str_split_i(qr, '[$]', 6) %>%
#            str_remove('Range') %>%
#            str_remove('rANGE'),
#          row = str_split_i(qr, '[$]', 5) %>%
#            str_remove('Row') %>%
#            str_remove('rOW'),
#          genotype = str_split_i(qr, '[$]', 7) %>%
#            str_to_upper(),
#          population = 'Hybrid') 
# # Check sb qr parsing
# unique(e_sb$plot)
# unique(e_sb$nLvl)
# unique(e_sb$rep)
# unique(e_sb$range)
# unique(e_sb$row)
# unique(e_sb$genotype) 
# # Parse Lincoln qrs
# e_linc <- rowwise(e_linc) %>%
#   mutate(plot = str_split_i(qr, '[$]', 1),
#          loc = 'Lincoln', 
#          field = 'Hybrid HIPS',
#          irrigation = 'Dryland',
#          range = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Range'),
#          row = str_split_i(qr, '[$]', 2) %>%
#            str_remove('Row'),
#          genotype = str_split_i(qr, '[$]', 4) %>%
#            str_to_upper(), 
#          population = 'Hybrid')
# # Merge e_linc, e_np1, e_np2, e_np3, and e_sb
# e2 <- bind_rows(e_linc, e_np1, e_np2, e_np3, e_sb)
# # Now convert NAs to 'NA' in the columns to concatenate
# e2 <- ungroup(e2) %>%
#   mutate(across(c(earDataCollector, earPhenotypeNotes:hundredKernelWt, greenAtHarvest), str_replace_na))
# # Concatenate values so we can do a single pivot
# e2 <- rowwise(e2) %>%
#   mutate(vals = str_c(earDataCollector, earPhenotypeNotes, seedColor, earWidth, kernelFillLen, kernelRows, kernelsPerRow,
#                       earWt, totalKernelCt, cobWidth, cobLen, cobWt, hundredKernelWt, greenAtHarvest, sep = '-')) %>%
#   select(c(qr:earNum, totalMoisture, plot:vals))
# # Check for duplicates
# dup <- e2 %>%
#   dplyr::group_by(qr, plot, loc, field, nLvl, irrigation, rep, range, row, genotype, population, earNum, looseKernelCt, looseKernelWt, totalMoisture) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
# # Pivot
# e2 <- pivot_wider(e2, 
#                   names_from = earNum, 
#                   values_from = vals, 
#                   names_prefix = 'vals', 
#                   id_cols = c(looseKernelCt, looseKernelWt, totalMoisture:population), 
#                   unused_fn = max) 
# # Split vals columns back into their original columns
# e2 <- rowwise(e2) %>%
#   mutate(earDataCollector1 = str_split_i(vals1, '-', 1) %>%
#            na_if('NA'),
#          earPhenotypeNotes1 = str_split_i(vals1, '-', 2) %>%
#            na_if('NA'),
#          seedColor1 = str_split_i(vals1, '-', 3) %>%
#            na_if('NA'),
#          earWidth1 = str_split_i(vals1, '-', 4) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelFillLen1 = str_split_i(vals1, '-', 5) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelRows1 = str_split_i(vals1, '-', 6) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelsPerRow1 = str_split_i(vals1, '-', 7) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          earWt1 = str_split_i(vals1, '-', 8) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          totalKernelCt1 = str_split_i(vals1, '-', 9) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobWidth1 = str_split_i(vals1, '-', 10) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobLen1 = str_split_i(vals1, '-', 11) %>%
#            na_if('NA') %>%
#            as.numeric(), 
#          cobWt1 = str_split_i(vals1, '-', 12) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          hundredKernelWt1 = str_split_i(vals1, '-', 13) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          greenAtHarvest1 = str_split_i(vals1, '-', 14) %>%
#            as.logical(),
#          earDataCollector2 = str_split_i(vals2, '-', 1) %>%
#            na_if('NA'),
#          earPhenotypeNotes2 = str_split_i(vals2, '-', 2) %>%
#            na_if('NA'),
#          seedColor2 = str_split_i(vals2, '-', 3) %>%
#            na_if('NA'),
#          earWidth2 = str_split_i(vals2, '-', 4) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelFillLen2 = str_split_i(vals2, '-', 5) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelRows2 = str_split_i(vals2, '-', 6) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelsPerRow2 = str_split_i(vals2, '-', 7) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          earWt2 = str_split_i(vals2, '-', 8) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          totalKernelCt2 = str_split_i(vals2, '-', 9) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobWidth2 = str_split_i(vals2, '-', 10) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobLen2 = str_split_i(vals2, '-', 11) %>%
#            na_if('NA') %>%
#            as.numeric(), 
#          cobWt2 = str_split_i(vals2, '-', 12) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          hundredKernelWt2= str_split_i(vals2, '-', 13) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          greenAtHarvest2 = str_split_i(vals2, '-', 14) %>%
#            as.logical(),
#          earDataCollector3 = str_split_i(vals3, '-', 1) %>%
#            na_if('NA'),
#          earPhenotypeNotes3 = str_split_i(vals3, '-', 2) %>%
#            na_if('NA'),
#          seedColor3 = str_split_i(vals3, '-', 3) %>%
#            na_if('NA'),
#          earWidth3 = str_split_i(vals3, '-', 4) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelFillLen3 = str_split_i(vals3, '-', 5) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelRows3 = str_split_i(vals3, '-', 6) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelsPerRow3 = str_split_i(vals3, '-', 7) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          earWt3 = str_split_i(vals3, '-', 8) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          totalKernelCt3 = str_split_i(vals3, '-', 9) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobWidth3 = str_split_i(vals3, '-', 10) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobLen3 = str_split_i(vals3, '-', 11) %>%
#            na_if('NA') %>%
#            as.numeric(), 
#          cobWt3 = str_split_i(vals3, '-', 12) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          hundredKernelWt3 = str_split_i(vals3, '-', 13) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          greenAtHarvest3 = str_split_i(vals3, '-', 14) %>%
#            as.logical(),
#          earDataCollector4 = str_split_i(vals4, '-', 1) %>%
#            na_if('NA'),
#          earPhenotypeNotes4 = str_split_i(vals4, '-', 2) %>%
#            na_if('NA'),
#          seedColor4 = str_split_i(vals4, '-', 3) %>%
#            na_if('NA'),
#          earWidth4 = str_split_i(vals4, '-', 4) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelFillLen4 = str_split_i(vals4, '-', 5) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelRows4 = str_split_i(vals4, '-', 6) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          kernelsPerRow4 = str_split_i(vals4, '-', 7) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          earWt4 = str_split_i(vals4, '-', 8) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          totalKernelCt4 = str_split_i(vals4, '-', 9) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobWidth4 = str_split_i(vals4, '-', 10) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          cobLen4 = str_split_i(vals4, '-', 11) %>%
#            na_if('NA') %>%
#            as.numeric(), 
#          cobWt4 = str_split_i(vals4, '-', 12) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          hundredKernelWt4 = str_split_i(vals4, '-', 13) %>%
#            na_if('NA') %>%
#            as.numeric(),
#          greenAtHarvest4 = str_split_i(vals4, '-', 14) %>%
#            as.logical())
# # Remove border plots from the first set and drop the vals columns
# e2 <- e2 %>%
#   filter(!(genotype=='BORDER')) %>%
#   mutate(across(c(plot, rep, range, row), as.numeric)) %>%
#   select(!(starts_with('vals')))
# # Merge two ear datasets together
# eardata_merged <- bind_rows(e2, e22)
# # Look for plot 1099 (in misc_plot) at each loc
# # number not found in indices for hybrid fields?? check again; check inbred fields too
# # Per Ravi's advice, letting Jon track this down
# p1099 <- filter(eardata_merged, plot==1099) 
# sb_range23 <- filter(e_sb, range=='23')
# unique(sb_range23$row)
# # Per James, dropping the plot in misc_plot it because we can't identify it
# Read in sb combine data
# sb_combine <- read_excel("data/Dipak Corn22_HM.xlsx", 
#                          col_types = c("date", "numeric", "numeric", 
#                                        "numeric", "numeric", "numeric", 
#                                        "numeric", "text", "numeric"))
# # Save the original column names
# orig_colnames_sbcombine <- colnames(sb_combine)
# # Change the column names so they match the ones we use for programming in Lincoln data
# colnames(sb_combine) <- c('harvestDate', 'range', 'row', 'plot', 'combineYield', 'combineMoisture', 'combineTestWt', 'combineNotes', 'harvestSeq')
# # All the units match, so now we can add some metadata
# sb_combine <- sb_combine %>%
#   as_tibble() %>%
#   mutate(loc = 'Scottsbluff',
#          field = 'Hybrid HIPS',
#          irrigation = 'Full', 
#          population = 'Hybrid')
# # Read in sb flowering time data
# sb_h_ft <- read_excel("data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx", 
#                       skip = 1)
# # Drop the first column because it's a plot number, but the second column is the plot number that actually corresponds to QR codes (verified with Ramesh)
# sb_h_ft <- sb_h_ft[, 2:4]
# # Save original column names
# orig_colnames_sbhft <- colnames(sb_h_ft)
# # Change the column names so they match the ones we use for programming in Lincoln data
# colnames(sb_h_ft) <- c('plot', 'anthesisDate', 'silkingDate')
# # Fix the formatting in plot column and add some metadata
# sb_h_ft <- sb_h_ft %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(plot = str_remove(plot, 'Hyb-') %>%
#            na_if('fill') %>%
#            as.numeric(),
#          loc = 'Scottsbluff',
#          field = 'Hybrid HIPS', 
#          irrigation = 'Full', 
#          population = 'Hybrid')
# # Read in sb height data
# sb_h_ht <- read_excel("data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx", 
#                       sheet = "Hybrid_height data", skip = 1)
# # Drop first column (see sb_h_ft)
# sb_h_ht <- sb_h_ht[, 2:5]
# # Save the original column names
# orig_colnames_sbhht <- colnames(sb_h_ht)
# # Change the column names so they match the ones we use for programming in Lincoln data
# colnames(sb_h_ht) <- c('plot', 'earHt', 'flagLeafHt', 'plantHt')
# # Fix the formatting in plot column, add some metadata, and convert the measurements in inches to centimeters so they are consistent with Lincoln measurements
# sb_h_ht <- sb_h_ht %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(plot = str_remove(plot, 'Hyb-') %>%
#            na_if('fill') %>%
#            as.numeric(),
#          loc = 'Scottsbluff',
#          field = 'Hybrid HIPS',
#          irrigation = 'Full',
#          population = 'Hybrid', 
#          earHt = cm(earHt),
#          flagLeafHt = cm(flagLeafHt),
#          plantHt = cm(plantHt))
# # Join the sb combine and flowering time datasets
# sb <- full_join(sb_combine, sb_h_ft, keep = FALSE)
# # Filter out observations in sb and sb_h_ht datasets with a NA in the plot column
# sb <- filter(sb, !is.na(plot))
# sb_h_ht <- filter(sb_h_ht, !is.na(plot))
# # Join the sb and sb height datasets
# sb <- full_join(sb, sb_h_ht, keep = FALSE)
# # Read in parsed and cleaned NIR data
# nir <- read_excel("data/HybridHIPS_plotlevelNIR_v1 (1).xlsx") 
# # Save original column names
# orig_colnames_nir <- colnames(nir)
# # Change column names to be consistent with conventions used in Lincoln data for programming
# colnames(nir) <- c('qr', 'loc', 'nLvl', 'irrigation', 'row', 'range', 'genotype', 'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil',
#                    'moistureCorrectedFiber', 'moistureCorrectedAsh')
# # Convert to tibble so we can use tidyverse, add some more metadata
# nir <- nir %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(irrigation = case_when(irrigation=='Rainfed' ~ 'Dryland', .default = irrigation),
#          field = 'Hybrid HIPS',
#          population = 'Hybrid')
# # Split by location so we can parse the plot
# nir_linc <- filter(nir, loc=='Lincoln')
# nir_mv <- filter(nir, loc=='Missouri Valley')
# nir_np <- filter(nir, str_detect(loc, 'North Platte') & !(genotype=='BORDER'))
# nir_sb <- filter(nir, loc=='Scottsbluff')
# # Parse lincoln for the plot
# nir_linc <- nir_linc %>%
#   rowwise() %>%
#   mutate(plot = str_split_i(qr, '[$]', 1) %>%
#            as.numeric())
# # Parse mv for the plot and rep
# nir_mv <- nir_mv %>%
#   rowwise() %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT') %>%
#            as.numeric(),
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP') %>%
#            as.numeric())
# # Parse np for the plot and rep
# nir_np <- nir_np %>%
#   rowwise() %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT') %>%
#            as.numeric(),
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP') %>%
#            as.numeric())
# # Parse sb for the plot and rep
# nir_sb <- nir_sb %>%
#   filter(!(genotype=='FILL')) %>%
#   rowwise() %>%
#   mutate(plot = str_split_i(qr, '[$]', 4) %>%
#            str_remove('Plot') %>%
#            str_remove('pLOT') %>%
#            as.numeric(),
#          rep = str_split_i(qr, '[$]', 3) %>%
#            str_remove('Rep') %>%
#            str_remove('rEP') %>%
#            as.numeric())
# # Put all the nir data back together
# nir <- bind_rows(nir_linc, nir_mv, nir_np, nir_sb)
# # The genotypes in the nir data don't have a space before/after the X between parents so we need to figure out how to fix that so they match
# # Let's see what genotypes X occurs in where it's not between parents
# x_genos <- linc22combined %>%
#   filter(!(str_detect(genotype, ' X ')) & str_detect(genotype, 'X')) %>%
#   select(genotype)
# nir <- nir %>%
#   rowwise() %>%
#   mutate(genotype = case_when(genotype=='PIONEERP0589AMXT' ~ 'PIONEER P0589 AMXT',
#                               genotype=='PIONEER1311AMXT' ~ 'PIONEER 1311 AMXT',
#                               genotype=='HOEGEMEYER7089AMXT' ~ 'HOEGEMEYER 7089 AMXT', 
#                               str_detect(genotype, 'COMMERCIALHYBRID') ~ str_replace(genotype, 'COMMERCIALHYBRID', 'COMMERCIAL HYBRID '),
#                               str_detect(genotype, 'SOLAR') ~ str_replace(genotype, 'SOLAR', 'SOLAR '),
#                               str_detect(genotype, 'SYNGENTANK') ~ str_replace(genotype, 'SYNGENTANK', 'SYNGENTA NK'),
#                               .default = str_replace(genotype, 'X', ' X ')))
# all_parsed_genos <- c(linc22combined$genotype, eardata_merged$genotype, nir$genotype) %>%
#   unique()
# all_parsed_genos
# genos_fix <- all_parsed_genos[160:172]
# genos_correct <- c(all_parsed_genos[21], all_parsed_genos[60], all_parsed_genos[56], all_parsed_genos[7], all_parsed_genos[56], all_parsed_genos[56], 
#                    all_parsed_genos[60], all_parsed_genos[60], all_parsed_genos[7], all_parsed_genos[60], all_parsed_genos[56], all_parsed_genos[21], 
#                    all_parsed_genos[18])
# genotype_fix_key <- tibble(orig = genos_fix, correct = genos_correct)

# Function to change parsed genotypes based on a key with the first column as the original genotypes and the second column with the genotype they should be 
fixGenos <- function(data, givenKey)
{
  key <- givenKey
  df_ok <- filter(data, !(genotype %in% key$orig))
  print(unique(df_ok$genotype))
  df_fix <- filter(data, genotype %in% key$orig)
  print(unique(df_fix$genotype))
  df_fix <- full_join(df_fix, key, join_by(genotype == orig), keep = FALSE, relationship = 'many-to-one')
  df_fix %>% select(starts_with('correct')) %>% print()
  df_fix <- df_fix %>%
    rowwise() %>%
    mutate(genotype = correct) %>%
    select(!starts_with('correct'))
  df <- bind_rows(df_ok, df_fix)
  return (df)
}

hips1.5genos_fix <- c("HOEGEMEYER 7089 AMX", "SYNGENTA NK0760-311", "PIONEER P0589 AMX", "SYNGENTA NK0760-31", "SYNGENTA NK0760-3", "HOEGEMEYER 7089 A", 
                      "HOEGEMEYER 7089 AM", "PIONEER P0589 AM", "HOEGEMEYER 7089", "SYNGENTA NK0760-", "HOEGEMEYER 8065R", "PIONEER 1311 AMX",
                      "COMMERCIAL HYBRID 5", "COMMERCIAL HYBRID 4", "COMMERCIAL HYBRID 3", "COMMERCIAL HYBRID 2", "COMMERCIAL HYBRID 1", "")
hips1.5genos_correct <- c("HOEGEMEYER 7089 AMXT", 'SYNGENTA NK0760-3111', "PIONEER P0589 AMXT", rep('SYNGENTA NK0760-3111', 2), rep("HOEGEMEYER 7089 AMXT", 2), 
                          "PIONEER P0589 AMXT", "HOEGEMEYER 7089 AMXT", 'SYNGENTA NK0760-3111', "HOEGEMEYER 8065RR", "PIONEER 1311 AMXT", 
                          'SYNGENTA NK0760-3111', 'HOEGEMEYER 7089 AMXT', 'HOEGEMEYER 8065RR', 'PIONEER P0589 AMXT', 'PIONEER 1311 AMXT', NA)
hips1.5_genoFixKey <- tibble(orig = hips1.5genos_fix, correct = hips1.5genos_correct)
# nir <- fixGenos(nir, which(colnames(nir) == "genotype"), genotype_fix_key)
# 
# # Join nir data with Lincoln field data
# hips <- full_join(linc22combined, nir, 
#                   by = join_by(loc, plot, row, range, genotype, field, population),
#                   keep = FALSE, 
#                   suffix = c('', '.nir'))
# # Now combine duplicate columns
# hips <- hips %>%
#   rowwise() %>%
#   mutate(qr = max(qr, qr.nir, na.rm = TRUE),
#          nLvl = max(nLvl, nLvl.nir, na.rm = TRUE),
#          irrigation = max(irrigation, irrigation.nir, na.rm = TRUE)) %>%
#   select(!(ends_with('.nir')))
# # Change location of 'MV' in ear data to 'Missouri Valley'
# eardata_merged <- eardata_merged %>%
#   rowwise() %>%
#   mutate(loc = case_when(loc=='MV' ~ 'Missouri Valley', .default = loc))
# # There's a plot with 2 ear entries in range 6 of np1, so let's look at the other rows in this range to see if one is missing
# np1_range6 <- filter(eardata_merged, loc=='North Platte1' & range==6)
# unique(np1_range6$row)
# # We are missing row 13 as well, so we will drop both of these because we can't identify which is correct
# eardata_merged <- filter(eardata_merged, !(qr=='North Platte$HIPS - Full Irrigation - Low Nitrogen$Rep1$Plot99$Row3$Range6$LH145 x LH162'))
# # There's also a duplicate in HIPS from np3, plot 1232, range 9, row 18, so let's look at that
# hips_dup <- filter(hips, loc=='North Platte3', plot==1232, range==9, row==18)``
# # hips_dup <- hips %>%
# #   dplyr::group_by(plot, loc, rep, range, row) %>%
# #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
# #   dplyr::filter(n > 1L)
# # The issue here is 2 different NIR measurements from 2 different QRs that parse to the same location in the field
# # So we will take the average (equivalent to the median() of these two measurements)
# hips_dup <- hips_dup %>%
#   ungroup() %>%
#   summarise(moistureCorrectedStarch = mean(moistureCorrectedStarch),
#             moistureCorrectedProtein = mean(moistureCorrectedProtein),
#             moistureCorrectedOil = mean(moistureCorrectedOil),
#             moistureCorrectedFiber = mean(moistureCorrectedFiber),
#             moistureCorrectedAsh = mean(moistureCorrectedAsh),
#             qr = max(qr), 
#             .by = c(plot:irrigation, row:genotype, population))
# # Remove these observations from hips and replace with the condensed version
# hips <- hips %>%
#   filter(!(loc=='North Platte3' & plot==1232 & range==9 &row==18)) %>%
#   bind_rows(hips_dup)
# # Join ear data with Lincoln field data
# hips <- full_join(hips, eardata_merged, 
#                   by = join_by(plot, loc, field, irrigation, range, row, genotype), 
#                   keep = FALSE, 
#                   suffix = c('', '.ear'))
# # Combine duplicate columns
# hips <- hips %>%
#   rowwise() %>%
#   mutate(population = max(population, population.ear, na.rm = TRUE),
#          qr.ear = max(qr, qr.ear, na.rm = TRUE),
#          nLvl = max(nLvl, nLvl.ear, na.rm = TRUE),
#          rep = max(rep, rep.ear, na.rm = TRUE)) %>%
#   select(!ends_with('.ear'))
# # Cast sb harvest date as datetime
# sb <- mutate(sb, harvestDate2 = as.POSIXct(harvestDate, tz = 'UTC', format = '%m/%e/%Y %l:%M:%S %r'))
# 
# # Join sb data with Lincoln field data
# hips <- full_join(hips, sb, 
#                   by = join_by(plot, loc, field, range, row),
#                   keep = FALSE,
#                   suffix = c('', '.sb'))
# sb_cols <- select(hips, ends_with('.sb'))
# # Join duplicate columns
# hips <- hips %>%
#   rowwise() %>%
#   mutate(harvestDate = max(harvestDate, harvestDate.sb, na.rm = TRUE),
#          combineYield = max(combineYield, combineYield.sb, na.rm = TRUE),
#          combineMoisture = max(combineMoisture, combineMoisture.sb, na.rm = TRUE),
#          combineTestWt = max(combineTestWt, combineTestWt.sb, na.rm = TRUE),
#          harvestSeq = max(harvestSeq, harvestSeq.sb, na.rm = TRUE),
#          irrigation = max(irrigation, irrigation.sb, na.rm = TRUE),
#          population = max(population, population.sb, na.rm = TRUE), 
#          anthesisDate = max(anthesisDate, anthesisDate.sb, na.rm = TRUE),
#          silkingDate = max(silkingDate, silkingDate.sb, na.rm = TRUE),
#          earHt1 = max(earHt1, earHt1.sb, na.rm = TRUE),
#          flagLeafHt1 = max(flagLeafHt1, flagLeafHt1.sb, na.rm = TRUE), 
#          plantHt1 = max(plantHt1, plantHt1.sb, na.rm = TRUE)) %>%
#   select(!ends_with('.sb'))
# # Replace commas in notes fields with a semicolon so we don't get frameshift errors
# hips <- hips %>%
#   ungroup() %>%
#   mutate(across(where(is.character()), ~str_replace(., ',', ';')))
# # Export v1
# write.table(hips, file = 'HIPS_2022_V1.csv', quote = FALSE, sep = ',', row.names = FALSE, 
#             col.names = TRUE)
# Generate list of parsed genotypes across all data files
# Drop border plots
# Check notes columns for observations to drop
# Check Lincoln documentation for flowering time and parsing --> does N/A indicate it never reached?
# Calculate plot level data for ear phenotypes
# Write documentation

# Okay, let's try just putting together the nir and ear data cleaned by schnable
# Read in new nir file
nir_v2 <- read_csv("data/HybridHIPS_plotlevelNIR_v2.5.csv", 
                   col_names = c('qr', 'loc', 'nLvl', 'irrigation', 'rep', 'row', 'range', 'plot', 'genotype',
                                 'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh',
                                 'pctMoistureNIR'),
                   col_types = c(rep('c', 4), rep('i', 4), 'c', rep('d', 7)))
# Change column names
# orig_colnames_nirv2 <- colnames(nir_v2)
# colnames(nir_v2) <- c('qr', 'loc', 'nLvl', 'irrigation', 'row', 'range', 'genotype', 
#                       'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh')
# Separate by location and parse
nir_v2_np <- filter(nir_v2, str_detect(loc, 'Platte')) %>%
  parseNorthPlatteQR()
nir_v2_lnk <- filter(nir_v2, str_detect(loc, 'Lincoln')) %>%
  parseLincolnQR() %>%
  mutate(rep = as.numeric(rep))
nir_v2_mv <- filter(nir_v2, str_detect(loc, 'Missouri Valley')) %>%
  parseMissouriValleyQR()
nir_v2_sb <- filter(nir_v2, str_detect(loc, 'Scottsbluff')|str_detect(str_to_upper(qr), 'ROW 23 RANGE 26 PLOT# 1099')) %>%
  parseScottsbluffQR()
# Bind nir data back into one dataframe
nir_v2 <- bind_rows(nir_v2_np, nir_v2_lnk, nir_v2_mv, nir_v2_sb)
# Fix genotypes
nir_v2 <- fixGenos(nir_v2, hips1.5_genoFixKey)
# Read in plot level ear data
e_v1.5 <- read_csv('./data/plotleveleardata_v2.csv')
# Change column names
orig_colnames_ev15 <- colnames(e_v1.5)
colnames(e_v1.5) <- c('qr', 'earLen', 'earFillLen', 'earWidth', 'shelledCobWidth', 'shelledCobWt', 'kernelsPerEar', 'hundredKernelWt', 
                      'pctMoistureEarPhenotyping', 'kernelsPerRow', 'kernelRows', 'kernelMass', 'kernelColor', 'kernelStriping')
# Separate by location and parse
ev1.5_np <- filter(e_v1.5, str_detect(qr, 'pLATTE')|str_detect(qr, 'Platte')) %>%
  parseNorthPlatteQR()
ev1.5_mv <- filter(e_v1.5, str_detect(qr, 'MV')|str_detect(qr, 'mv')) %>%
  parseMissouriValleyQR()
ev1.5_lnk <- filter(e_v1.5, 
                    !(str_detect(qr, 'pLATTE')|str_detect(qr, 'Platte')|
                        str_detect(qr, 'MV')|str_detect(qr, 'mv')|
                        str_detect(qr, 'Scottsbluff')|str_detect(qr, 'sCOTTSBLUFF'))) %>%
  parseLincolnQR() %>%
  filter(!(qr=='UNKNOWN PLOT FROM LINCOLN SACK #16'|qr=='ROW 23 RANGE 26 PLOT# 1099'))
ev1.5_sb <- filter(e_v1.5, str_detect(qr, 'Scottsbluff')|str_detect(qr, 'sCOTTSBLUFF')|str_detect(str_to_upper(qr), 'ROW 23 RANGE 26 PLOT# 1099')) %>%
  mutate(qr = case_when(str_detect(str_to_upper(qr), 'ROW 23 RANGE 26 PLOT# 1099') ~ 'SCOTTSBLUFF$HYBRID-HIPS - LOW NITROGEN$REP2$PLOT1099$ROW23$RANGE26$PHP02 X PHK76',
                        .default = qr)) %>%
  parseScottsbluffQR()
# Bind ear data back into one data frame
e_v1.5 <- bind_rows(ev1.5_np, ev1.5_mv, ev1.5_lnk, ev1.5_sb)
# Fix genotypes
e_v1.5 <- fixGenos(e_v1.5, hips1.5_genoFixKey)
# Also fix the issue in NP3 plot 1426, 1432, and 1436
# And the issue with LNK plot 5135 --> merging issue in src file, so replacing with means of the vals from original data file 
## Same issue at NP3 1496, 1493, 
# and drop the extra NP2 plot 719 rows and NP3 plot 1478 (see comments above: these ears could be from 1477 or 1478)
err_genos719 <- paste0('PHW52 X PHM', 50:74)
e_v1.5 <- e_v1.5 %>%
  filter(!(genotype %in% err_genos719) & 
           !(loc=='North Platte3' & plot==1478) & 
           !(loc=='Lincoln' & plot==5135) &
           !(loc=='North Platte3' & plot==1493) &
           !(loc=='North Platte3' & plot==1496)) %>%
  rowwise() %>%
  mutate(genotype = case_when(loc=='North Platte3' & plot==1426 ~ 'PHK76 X LH198',
                              loc=='North Platte3' & plot==1436 ~ '2369 X PHZ51',
                              loc=='North Platte3' & plot==1432 ~ 'PHB47 X 3IIH6',
                              loc=='North Platte3' & plot==1333 ~ 'PHP02 X LH198',
                              loc=='North Platte3' & plot==1331 ~ 'PHP02 X PHK76',
                              loc=='North Platte3' & plot==1328 ~ 'PHK76 X LH198',
                              loc=='North Platte3' & plot==1327 ~ 'PHK76 X LH82',
                              loc=='North Platte3' & plot==1326 ~ 'PHK76 X LH145',
                              loc=='North Platte1' & plot==214 ~ 'PHK56 X LH198',
                              .default = genotype)) %>%
  add_row(qr = '5135$ROW19$RANGE8$PHJ40 X LH82',
          earLen = mean(19, 15.5, 14, 14.5),
          earFillLen = mean(17, 15, 12.5, 13),
          earWidth = mean(4, 4, 3.5, 3.5),
          shelledCobWidth = mean(3, 3, 2.5, 2.5),
          shelledCobWt = mean(27.87, 24.25, 15.03, 15.24),
          kernelsPerEar = mean(597, 558, 433, 433),
          hundredKernelWt = mean(25.49, 20.87, 16.41, 18.13),
          kernelMass = mean(187.15 - 27.87, 137.29 - 24.25, 83.44 - 15.03, 96.84 - 15.24),
          kernelsPerRow = mean(36, 36, 28, 29),
          kernelRows = 16,
          kernelColor = 'yellow',
          kernelStriping = 'N',
          plot = 5135,
          loc = 'Lincoln',
          field = 'Hybrid HIPS',
          irrigation = 'Dryland',
          range = 8,
          row = 19,
          genotype = 'PHJ40 X LH82',
          population = 'Hybrid') %>%
  add_row(qr = 'NORTH PLATTE$HIPS - NO IRRIGATION - MEDIUM NITROGEN$REP2$PLOT1493$ROW19$RANGE24$PHP02 X PHJ89',
          earLen = mean(16.5, 17, 16, 17.5),
          earFillLen = mean(12, 13.5, 13, 13.5),
          earWidth = mean(4, 3.5, 4, 4),
          shelledCobWidth = mean(3, 3, 3, 3),
          shelledCobWt = mean(14.2, 15.2, 15.4, 16.3),
          kernelsPerEar = mean(345, 414, 432, 421),
          hundredKernelWt = mean(20.6, 17.2, 16.9, 15.7),
          kernelMass = mean(85.1 - 14.2, 87.5 - 15.2, 90.2 - 15.4, 79.6 - 16.3),
          kernelsPerRow = mean(27, 31, 32, 29),
          kernelRows = mean(14, 14, 14, 16),
          kernelColor = 'yellow', 
          kernelStriping = 'N',
          plot = 1493,
          loc = 'North Platte3',
          field = 'Hybrid HIPS',
          irrigation = 'Dryland',
          range = 24,
          row = 19, 
          genotype = 'PHP02 X PHJ89',
          population = 'Hybrid') %>%
  add_row(qr = 'NORTH PLATTE$HIPS - NO IRRIGATION - MEDIUM NITROGEN$REP2$PLOT1496$ROW22$RANGE24$4N506 X 3IIH6',
          earLen = mean(16, 16, 17, 18),
          earFillLen = mean(13.5, 14.5, 15, 15.5),
          earWidth = mean(4, 4, 4, 4),
          shelledCobWidth = mean(2.5, 2.5, 2.5, 3),
          shelledCobWt = mean(16, 16.8, 16.5, 19.9),
          kernelsPerEar = mean(485, 522, 416, 561),
          hundredKernelWt = mean(18.9, 19.7, 24.5, 22.8),
          kernelMass = mean(109.1 - 16, 117.6 - 16.8, 115.6 - 16.5, 148 - 19.9),
          kernelsPerRow = mean(33, 33, 31, 35),
          kernelRows = mean(16, 18, 14, 16),
          kernelColor = 'yellow', 
          kernelStriping = 'N',
          plot = 1496,
          loc = 'North Platte3',
          field = 'Hybrid HIPS',
          irrigation = 'Dryland',
          range = 24,
          row = 22, 
          genotype = '4N506 X 3IIH6',
          population = 'Hybrid') %>%
  ungroup() %>%
  group_by(loc, plot, genotype, range, row) %>%
  summarise(qr = max(qr),
            nLvl = max(nLvl),
            field = max(field),
            rep = max(rep),
            population = max(population),
            irrigation = max(irrigation),
            earLen = mean(earLen),
            earFillLen = mean(earFillLen),
            earWidth = mean(earWidth),
            shelledCobWidth = mean(shelledCobWidth),
            shelledCobWt = mean(shelledCobWt),
            kernelsPerEar = mean(kernelsPerEar),
            hundredKernelWt = mean(hundredKernelWt),
            pctMoistureEarPhenotyping = mean(pctMoistureEarPhenotyping),
            kernelMass = mean(kernelMass),
            kernelColor = max(kernelColor),
            kernelStriping = max(kernelStriping),
            kernelsPerRow = mean(kernelsPerRow),
            kernelRows = mean(kernelRows))
  
# Join the ear and nir data together
nir_ear <- full_join(nir_v2, e_v1.5, join_by(loc, plot, genotype, range, row), keep = FALSE, suffix = c('.nir', ''))
# Now get rid of the duplicate columns --> taking the max in case one is empty, but the values should be the same except in the case only one of the datasets
# contained this variable
nir_ear <- nir_ear %>%
  rowwise() %>%
  mutate(qr = max(qr, qr.nir, na.rm = TRUE),
         nLvl = max(nLvl, nLvl.nir, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.nir, na.rm = TRUE),
         field = max(field, field.nir, na.rm = TRUE),
         rep = max(rep, rep.nir, na.rm = TRUE),
         population = max(population, population.nir, na.rm = TRUE))
nir_ear <- select(nir_ear, !ends_with('.nir'))

# Export
# write.table(nir_ear, '2022_HIPS_HYBRID_NIR_EARDATA_PLOTLEVEL.csv', quote = FALSE, sep = ',', row.names = FALSE, col.names = TRUE)
# Standardize formatting of linc22combined with the other data
linc22combined <- parseLincolnQR(linc22combined)
# Remove commas in the notes column and leaf dim notes; sub in semicolons instead

linc22combined <- linc22combined %>%
  rowwise() %>%
  mutate(notes = str_replace(notes, ',', ';'),
         leafDimHtNotes = str_replace(leafDimHtNotes, ',', ';'))
# Look at plot notes to determine if any of the leafDim obs should be dropped --> only stunting/missing ear
unique(linc22combined$leafDimHtNotes)
# Probably best to drop the stunted plants and those with no ear because there's something wrong with this plant
l_field <- linc22combined %>%
  ungroup() %>%
  mutate(across(leafLen1:plantHt2, ~case_when(!is.na(leafDimHtNotes) ~ NA, .default = .)))
l_field <- fixGenos(l_field, hips1.5_genoFixKey)
# Now calculate the mean of leaf dim and ht measurements --> with only 2 measurements, this is the same as the median
# And drop the individual measurements
l_field <- l_field %>%
  rowwise() %>%
  mutate(leafLen = mean(leafLen1, leafLen2, na.rm = TRUE),
         leafWidth = mean(leafWidth1, leafWidth2, na.rm = TRUE),
         earHt = mean(earHt1, earHt2, na.rm = TRUE),
         flagLeafHt = mean(flagLeafHt1, flagLeafHt2, na.rm = TRUE),
         plantHt = mean(plantHt1, plantHt2, na.rm = TRUE)) %>%
  select(!(ends_with('1')|ends_with('2')))
# Now combine it with the nir and ear data
nir_ear_lnkf <- full_join(nir_ear, l_field, join_by(loc, plot, genotype, range, row), suffix = c('', '.lnk'), keep = FALSE)
# Now drop the duplicate columns
nir_ear_lnkf <- nir_ear_lnkf %>%
  rowwise() %>%
  mutate(nLvl = max(nLvl, nLvl.lnk, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.lnk, na.rm = TRUE),
         field = max(field, field.lnk, na.rm = TRUE),
         rep = max(rep, rep.lnk, na.rm = TRUE),
         population = max(population, population.lnk, na.rm = TRUE),
         qr = max(qr, qr.lnk, na.rm = TRUE))
nir_ear_lnkf <- select(nir_ear_lnkf, !ends_with('.lnk'))

# Now let's deal with the scottsbluff data
sb_combine <- read_excel("data/Dipak Corn22_HM.xlsx", 
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "text", "numeric"))
# Save the original column names
orig_colnames_sbcombine <- colnames(sb_combine)
# Change the column names so they match the ones we use for programming in Lincoln data
colnames(sb_combine) <- c('harvestDate', 'range', 'row', 'plot', 'combineYield', 'combineMoisture', 'combineTestWt', 'combineNotes', 'harvestSeq')
# All the units match, so now we can add some metadata
sb_combine <- sb_combine %>%
  as_tibble() %>%
  mutate(loc = 'Scottsbluff',
         field = 'Hybrid HIPS',
         irrigation = 'Full', 
         population = 'Hybrid')
# Read in sb flowering time data
sb_h_ft <- read_excel("data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx", 
                      skip = 1)
# Drop the first column because it's a plot number, but the second column is the plot number that actually corresponds to QR codes (verified with Ramesh)
sb_h_ft <- sb_h_ft[, 2:4]
# Save original column names
orig_colnames_sbhft <- colnames(sb_h_ft)
# Change the column names so they match the ones we use for programming in Lincoln data
colnames(sb_h_ft) <- c('plot', 'anthesisDate', 'silkingDate')
# Fix the formatting in plot column and add some metadata
sb_h_ft <- sb_h_ft %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(plot = str_remove(plot, 'Hyb-') %>%
           na_if('fill') %>%
           as.numeric(),
         loc = 'Scottsbluff',
         field = 'Hybrid HIPS', 
         irrigation = 'Full', 
         population = 'Hybrid')
# Read in sb height data
sb_h_ht <- read_excel("data/Corn_data_Scottsbluff-2022_rk_11.11.2022.xlsx", 
                      sheet = "Hybrid_height data", skip = 1)
# Drop first column (see sb_h_ft)
sb_h_ht <- sb_h_ht[, 2:5]
# Save the original column names
orig_colnames_sbhht <- colnames(sb_h_ht)
# Change the column names so they match the ones we use for programming in Lincoln data
colnames(sb_h_ht) <- c('plot', 'earHt', 'flagLeafHt', 'plantHt')
# Fix the formatting in plot column, add some metadata, and convert the measurements in inches to centimeters so they are consistent with Lincoln measurements
sb_h_ht <- sb_h_ht %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(plot = str_remove(plot, 'Hyb-') %>%
           na_if('fill') %>%
           as.numeric(),
         loc = 'Scottsbluff',
         field = 'Hybrid HIPS',
         irrigation = 'Full',
         population = 'Hybrid', 
         earHt = cm(earHt),
         flagLeafHt = cm(flagLeafHt),
         plantHt = cm(plantHt))
# Join the sb combine and flowering time datasets
sb <- full_join(sb_combine, sb_h_ft, keep = FALSE)
# Filter out observations in sb and sb_h_ht datasets with a NA in the plot column
sb <- filter(sb, !is.na(plot))
sb_h_ht <- filter(sb_h_ht, !is.na(plot))
# Join the sb and sb height datasets
sb <- full_join(sb, sb_h_ht, keep = FALSE)
# no qrs here! and it's already plot level! so let's join to the other data we have
# Only join by plot because the range/row systems are different between qrs
# So drop range/row values from sb field data
hips_v1.5 <- full_join(nir_ear_lnkf, sb, join_by(plot, loc, field), suffix = c('', '.sb'), keep = FALSE)
# Combine duplicated columns and drop the '.sb' columns
hips_v1.5 <- hips_v1.5 %>%
  rowwise() %>%
  mutate(harvestDate = max(harvestDate, harvestDate.sb, na.rm = TRUE),
         combineYield = max(combineYield, combineYield.sb, na.rm = TRUE),
         combineMoisture = max(combineMoisture, combineMoisture.sb, na.rm = TRUE),
         combineTestWt = max(combineTestWt, combineTestWt.sb, na.rm = TRUE),
         harvestSeq = max(harvestSeq, harvestSeq.sb, na.rm = TRUE), 
         irrigation = max(irrigation, irrigation.sb, na.rm = TRUE),
         population = max(population, population.sb, na.rm = TRUE),
         anthesisDate = max(anthesisDate, anthesisDate.sb, na.rm = TRUE),
         silkingDate = max(silkingDate, silkingDate.sb, na.rm = TRUE),
         earHt = max(earHt, earHt.sb, na.rm = TRUE),
         flagLeafHt = max(flagLeafHt, flagLeafHt.sb, na.rm = TRUE),
         plantHt = max(plantHt, plantHt.sb, na.rm = TRUE)) %>%
  select(!ends_with('.sb'))
# Replace -Inf with NA
hips_v1.5 <- hips_v1.5 %>%
  mutate(across(c(where(is.numeric), where(is.POSIXct)), ~case_when(.==-Inf ~ NA, .default = .)))
# Check for truncated genotypes
unique(hips_v1.5$genotype) 
# There's a few genotypes that are truncated, so let's fix that
hips1.5genos <- unique(hips_v1.5$genotype)
hips_v1.5 <- fixGenos(hips_v1.5, hips1.5_genoFixKey)
unique(hips_v1.5$genotype)
# Okay, the genotypes are fixed now, so do some visual last checking and export
# Add more info where it's missing and available
# Check NP3 Plot 1426 ('PHK76 x LH198') --only 4 rows in original file: someone just dragged the QR down for the 4 ears in this bag
# & NP2 Plot 719 ('PHW52 x PHM49') genotypes --4 rows that make sense + 104 later: likely border with no QR and an old QR was copy-pasted: drop the later 104; 
# fix genotypes before merge (see NP2 plots) & DO NOT merge based on QR, SB 1436 still no metadata 

hips_v1.5 <- hips_v1.5 %>%
  rowwise() %>%
  mutate(nLvl = case_when(loc=='Scottsbluff' & plot==1436 ~ 'Low',
                          .default = nLvl),
         row = case_when(loc=='Scottsbluff' & plot==1436 ~ 4,
                         .default = row),
         range = case_when(loc=='Scottsbluff' & plot==1436 ~ 23,
                           .default = range),
         genotype = case_when(loc=='Missouri Valley' & rep==2 & (plot %in% c(53, 46, 50, 51, 59)) ~ 'DKC107-33GENVT2PRIB',
                              loc=='North Platte3' & plot==1426 ~ 'PHK76 x LH198',
                              .default = genotype))
# Drop the leaf dimension data since it only exists for lincoln and arrange columns in logical order
hips_v1.5_noleafdim <- hips_v1.5 %>% 
  select(c(loc:leafDimHtNotes, earHt:combineNotes)) %>%
  relocate(c(qr, loc, plot, field, nLvl, irrigation, rep, row, range, genotype, population)) %>%
  relocate(c(anthesisDate, anthesisCollector, silkingDate, silkingCollector, notes, leafDimHtNotes, leafDimHtCollector, leafDimHtDate, earHt, 
             flagLeafHt, plantHt), 
           .after = c(qr, loc, plot, field, nLvl, irrigation, rep, row, range, genotype, population)) %>%
  relocate(c(harvestDate, combineYield, combineMoisture, combineTestWt, harvestSeq, combineNotes), 
           .after = c(anthesisDate, anthesisCollector, silkingDate, silkingCollector, notes, leafDimHtNotes, leafDimHtCollector, leafDimHtDate, earHt, 
                      flagLeafHt, plantHt)) %>%
  relocate(c((starts_with('ear') & !earHt), starts_with('shelled'), starts_with('kernel'), hundredKernelWt, pctMoistureEarPhenotyping), 
           .after = c(harvestDate, combineYield, combineMoisture, combineTestWt, harvestSeq, combineNotes)) %>%
  relocate(starts_with('moisture'), 
           .after = c((starts_with('ear') & !earHt), starts_with('shelled'), starts_with('kernel'), hundredKernelWt, pctMoistureEarPhenotyping))


# Export v1.5
# write.table(hips_v1.5_noleafdim, 'HIPS_2022_V1.5.csv', sep = ',', row.names = FALSE, col.names = TRUE, quote = FALSE)
# # Export Lincoln and Scottsbluff combine data for Lisa because that (seems) to be merged correctly
# metadata_cols <- c('qr', 'loc', 'plot', 'field', 'nLvl', 'irrigation', 'rep', 'row', 'range', 'genotype', 'population')
# combine_cols <- c('harvestDate', 'combineYield', 'combineMoisture', 'combineTestWt', 'harvestSeq', 'combineNotes')
# yield_lnk_sb <- hips_v1.5_noleafdim %>%
#   filter(loc=='Lincoln'|loc=='Scottsbluff') %>%
#   select(c(all_of(metadata_cols), all_of(combine_cols)))
# write.table(yield_lnk_sb, 'HIPS_2022_HYBRID_YIELD_SCOTTSBLUFF_LINCOLN.tsv', sep = '\t', row.names = FALSE, col.names = TRUE, quote = FALSE)

# SOLVED:
# Something went wrong with the matching here
# It is likely that some measurements use the original field layout for range/row assignments and qrs use the modified layout
# sb_mismatch <- filter(hips_v1.5, loc=='Scottsbluff') %>%
#   select(qr, loc, plot, range, row, rep)
# sb_by_plot <- full_join(sb_combine, sb_h_ht, join_by(plot), keep = FALSE, suffix = c('.c', '.ht'))
# sb_by_plot <- full_join(sb_by_plot, sb_h_ft, join_by(plot), keep = FALSE, suffix = c('', '.ft'))
# sb_nir_ear <- filter(nir_ear, loc=='Scottsbluff')
# sb_by_plot <- full_join(sb_by_plot, sb_nir_ear, join_by(plot), keep = FALSE, suffix = c('', '.ear'))
# sb_by_plot <- select(sb_by_plot, starts_with('plot')|starts_with('range')|starts_with('qr')|starts_with('row'))

# Read in MV field and yield data
mv_hyb <- read_excel("data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx", 
                     sheet = "RawData (4-Row)", col_types = c("skip", 
                                                              "skip", "skip", "text", "skip", "skip", 
                                                              "text", "skip", "skip", "skip", "text", 
                                                              "skip", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "text", "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "date", "date", "text", "numeric", 
                                                              "numeric", "numeric", "text", "text", 
                                                              "text", "text"))
# Save original colnames
orig_colnames_mv_hyb <- colnames(mv_hyb)
# Rename columns to match other dataframes
colnames(mv_hyb) <- c('exp', 'check', 'genotype', 'plot', 'latitude', 'longitude', 'row', 'range', 'rep', 'plotDiscarded', 'combineYield', 
                      'combineMoisture', 'yieldPerAc', 'combineTestWt', 'plantDensity', 'rootLodgeNum', 'pctRootLodge', 'stalkLodgeNum', 
                      'pctStalkLodge', 'plantDate', 'harvestDate', 'combineNotes', 'totalStandCt', 'standCt1', 'standCt2', 'solarPanel', 
                      'tattooSensor', 'nitrateSensor', 'soilMoistureSensor')
# Move check, solarPanel, tattooSensor, nitrateSensor, and soilMoistureSensor into the notes column
# And transform all the genotypes to uppercase 
# And drop all rows where plotDiscarded isn't NA or the genotype is empty: this is a fill plot per Lisa's notes
# And add metadata
# And drop plotDiscarded & columns moved to notes
mv_hyb <- mv_hyb %>%
  filter(is.na(plotDiscarded) & !is.na(genotype) & exp == 'ISU.IA.MOValley') %>%
  rowwise() %>%
  mutate(notes = case_when(check == 'Y' ~ 'Check'),
         genotype = str_to_upper(genotype),
         loc = 'Missouri Valley',
         field = 'Hybrid HIPS',
         nLvl = 'Medium',
         irrigation = 'Dryland',
         population = 'Hybrid') %>%
  mutate(notes = case_when(solarPanel=='x' ~ str_c(notes, 'Solar Panel', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(tattooSensor=='x' ~ str_c(notes, 'tattoo Sensor', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(nitrateSensor=='x' ~ str_c(notes, 'Nitrate Sensor', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(soilMoistureSensor=='x' ~ str_c(notes, 'Soil Moisture Sensor', sep = ';'), .default =  notes)) %>%
  select(!(c(plotDiscarded, check, solarPanel, tattooSensor, nitrateSensor, soilMoistureSensor)))
# Now join the data with the nir, ear, lnk, and sb data
hips_v2 <- full_join(hips_v1.5_noleafdim, mv_hyb, join_by(loc, plot, genotype), keep = FALSE, suffix = c('', '.mv_hyb'))
# Deal with duplicate columns
hips_v2 <- hips_v2 %>%
  rowwise() %>%
  mutate(combineYield = max(combineYield, combineYield.mv_hyb, na.rm = TRUE),
         combineMoisture = max(combineMoisture, combineMoisture.mv_hyb, na.rm = TRUE),
         combineTestWt = max(combineTestWt, combineTestWt.mv_hyb, na.rm = TRUE), 
         harvestDate = max(harvestDate, harvestDate.mv_hyb, na.rm = TRUE),
         combineNotes = max(combineNotes, combineNotes.mv_hyb, na.rm = TRUE),
         notes = str_c(notes, notes.mv_hyb, sep = ';'),
         field = max(field, field.mv_hyb, na.rm = TRUE),
         nLvl = max(nLvl, nLvl.mv_hyb, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.mv_hyb, na.rm = TRUE),
         population = max(population, population.mv_hyb, na.rm = TRUE)) %>%
  select(!ends_with('.mv_hyb'))
hips_v2 <- select(hips_v2, !exp)
# Read in Missouri Valley inbred data
mv_inb <- read_excel("data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx", 
                     sheet = "RawData (2-Row)", col_types = c("skip", 
                                                              "skip", "skip", "text", "skip", "skip", 
                                                              "text", "skip", "skip", "skip", "text", 
                                                              "skip", "text", "numeric", "numeric", 
                                                              "numeric", "numeric", "numeric", 
                                                              "numeric", "skip", "skip", "skip", 
                                                              "skip", "skip", "numeric", "skip", 
                                                              "skip", "skip", "skip", "date", "skip", 
                                                              "skip", "numeric", "numeric", "numeric", 
                                                              "text", "text", "numeric"))
# Save original column names and change 
orig_colnames_mv_inb <- colnames(mv_inb)
colnames(mv_inb) <- c('exp', 'check', 'genotype', 'notes', 'plot', 'latitude', 'longitude', 'row', 'range', 'rep', 'plantDensity', 'plantDate', 
                      'totalStandCt', 'standCt1', 'standCt2', 'histFT', 'histPlantHt', 'block')
# Filter to MV
# Move check to notes and drop
# And add metadata
mv_inb <- mv_inb %>%
  filter(is.na(notes) & exp=='ISU.IA.MOValley') %>%
  rowwise() %>%
  mutate(notes = case_when(check=='Y' ~ 'Check', .default = notes),
         genotype = str_to_upper(genotype),
         loc = 'Missouri Valley',
         field = 'Inbred HIPS',
         irrigation = 'Dryland',
         population = 'Inbred') %>%
  select(!(c(exp, check)))
# Merge into hips_v2
hips_v2 <- full_join(hips_v2, mv_inb, join_by(loc, range, row, genotype, plot, rep), keep = FALSE, suffix = c('', '.mv_inb'))
# Deal with duplicate columns
hips_v2 <- hips_v2 %>%
  rowwise() %>%
  mutate(latitude = max(latitude, latitude.mv_inb, na.rm = TRUE),
         longitude = max(longitude, longitude.mv_inb, na.rm = TRUE),
         plantDensity = max(plantDensity, plantDensity.mv_inb, na.rm = TRUE),
         plantDate = max(plantDate, plantDate.mv_inb, na.rm = TRUE),
         totalStandCt = max(totalStandCt, totalStandCt.mv_inb, na.rm = TRUE),
         standCt1 = max(standCt1, standCt1.mv_inb, na.rm = TRUE),
         standCt2 = max(standCt2, standCt2.mv_inb, na.rm = TRUE),
         field = max(field, field.mv_inb, na.rm = TRUE))
hips_v2 <- select(hips_v2, !ends_with('.mv_inb') & !starts_with('exp'))

# Read in North Platte1 data
np1_colnames <- c('plot', 'genotype', 'genotypeNote', 'standCt1', 'standCt2', 'silkingDate', 'anthesisDate', 'plantHt', 'earHt', 'stalkLodgeNum', 'earDropNum', 
                  'rootLodgeNum', 'harvestDate', 'harvestTime', 'range', 'row', 'combineYield', 'combineMoisture', 'combineTestWt', 'plotLen', 'adjYield', 'combineNotes', 
                  'notes_LC', 'notes_HL', 'solarPanel', 'tattooSensor', 'nitrateSensor', 'soilMoistureSensor', 'waterPotentialSensor', 'commercialWaterPotentialSensor')

np1_types <- c('skip', 'numeric', 'skip', 'skip', 'text', 'text', 'skip', 'skip', 'numeric', 'numeric', 'skip', 'date', 'date', 'skip', 'skip', 'numeric', 'numeric', 'numeric', 'numeric', 
               'numeric', 'text', 'text', 'numeric', 'numeric', 'skip', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', rep('text', 9))
np1 <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx', sheet = 'Full Data', col_names = np1_colnames, col_types = np1_types, skip = 4) %>%
  mutate(loc = 'North Platte1',
         irrigation = 'Full')

np2_colnames <- np1_colnames[-24]
np2_types <- np1_types[-34]
np2 <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx', sheet = 'Reduced Irr Data', col_names = np2_colnames, col_types = np2_types, skip = 4)
# Add loc and irrigation info
np2 <- np2 %>%
  mutate(loc = 'North Platte2',
         irrigation = 'Partial')
# Read in North Platte3 data
np3_types <- np2_types[-30]
np3_colnames <- np2_colnames[-20]
np3 <- read_excel('data/2022_Schnable_HIPS_data_v4.xlsx', sheet = 'No Irr Data', col_names = np3_colnames, col_types = np3_types, skip = 4)
# Add loc and irrigation info
# The range and row system used on this field for combine & in-field measurements didn't count the 2 border rows but the QR codes did
# So we will add 2 for any row greater than 12
np3 <- np3 %>%
  mutate(loc = 'North Platte3',
         irrigation = 'Dryland') %>%
  rowwise() %>%
  mutate(row = case_when(row>12 ~ (row + 2), .default = row))
# Bind np1, 2, & 3 together
np <- bind_rows(np1, np2, np3)
# Convert genotypes to uppercase and heights to centimeters from meters, add field and population info, and reformat harvestDate
# Move solarPanel, tattooSensor, nitrateSensor, soilMoistureSensor, waterPotentialSensor, and commercialWaterPotentialSensor to notes
np <- np %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype),
         plantHt = plantHt*100,
         earHt = earHt*100,
         field = 'Hybrid HIPS',
         population = 'Hybrid',
         harvestDate = case_when(harvestDate=='44572' ~ '01/11/2022', .default = harvestDate),
         harvestHr = (as.numeric(harvestTime)*24),
         totalStandCt = (standCt1 + standCt2),
         notes = case_when(solarPanel=='x' ~ 'Solar Panel')) %>%
  mutate(notes = case_when(tattooSensor=='x' ~ str_c(notes, 'tattoo Sensor', sep = ';'), .default = notes),
         harvestDate = as.POSIXct(str_c(harvestDate, ' ', harvestTime), 
                                  format = '%d/%m/%Y %H')) %>%
  mutate(notes = case_when(nitrateSensor=='x' ~ str_c(notes, 'Nitrate Sensor', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(soilMoistureSensor=='x' ~ str_c(notes, 'Soil Moisture Sensor', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(waterPotentialSensor=='x' ~ str_c(notes, 'Water Potential Sensor', sep = ';'), .default = notes)) %>%
  mutate(notes = case_when(commercialWaterPotentialSensor=='x' ~ str_c(notes, 'Commercial Water Potential Sensor', sep = ';'), .default = notes))
# Drop the columns moved to notes / harvestTime(merged into harvestDate)
np <- select(np, !c(solarPanel, tattooSensor, nitrateSensor, soilMoistureSensor, waterPotentialSensor, commercialWaterPotentialSensor, harvestTime))
# Look at the notes columns to determine where we should drop observations
drop_yield <- c(unique(np$combineNotes)[c(1:6, 11, 12, 16, 17)], unique(np$notes_LC)[c(5, 8)])
drop_combineMoisture <- c(unique(np$combineNotes)[c(18, 21)])
drop_all <- c(unique(np$notes_LC)[c(1, 3:4, 6)])
# Drop observations
# NP1 Plot 35 is missing range and row assignments so we'll match it from the field map
np <- np %>%
  filter(!(notes_LC %in% drop_all)) %>%
  mutate(combineYield = case_when(combineNotes %in% drop_yield | notes_LC %in% drop_yield ~ NA, .default = combineYield),
         combineMoisture = case_when(combineNotes %in% drop_yield | notes_LC %in% drop_yield | combineNotes %in% drop_combineMoisture ~ NA, 
                                     .default = combineMoisture),
         combineTestWt = case_when(combineNotes %in% drop_yield | notes_LC %in% drop_yield ~ NA, .default = combineTestWt),
         yieldPerAc = case_when(combineNotes %in% drop_yield | notes_LC %in% drop_yield ~ NA, .default = adjYield),
         notes = str_c(notes, notes_LC, notes_HL, sep = ';'),
         range = case_when(loc=='North Platte1' & plot==35 ~ 12, .default = range),
         row = case_when(loc=='North Platte1' & plot==35 ~ 3, .default = row)) %>%
  select(!c(notes_HL, notes_LC, adjYield))
np <- select(np, !harvestHr)
np <- fixGenos(np, hips1.5_genoFixKey)
hips_v2 <- hips_v2 %>%
  rowwise() %>%
  mutate(genotype = str_to_upper(genotype))
# Join with hips_v2
# Gives many-to-many warning: Row 2821 of `x` matches multiple rows in `y`. & Row 1591 of `y` matches multiple rows in `x`.:
## Verified OK - JD: these rows only have a genotype but no data
hips_v2.1 <- full_join(hips_v2, np, join_by(loc, plot, range, row, genotype), keep = FALSE, suffix = c('', '.np'))
# Check for duplicate rows
# dup_hips2.1 <- hips_v2.1 %>%
#     filter(str_detect(loc, 'North Platte')) %>%
#     dplyr::group_by(plot, loc) %>%
#     dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#     dplyr::filter(n > 1L)
# Deal with duplicate cols
hips_v2.1 <- hips_v2.1 %>%
  mutate(totalStandCt = max(totalStandCt, totalStandCt.np, na.rm = TRUE),
         silkingDate = max(silkingDate, silkingDate.np, na.rm = TRUE),
         anthesisDate = max(anthesisDate, anthesisDate.np, na.rm = TRUE),
         plantHt = max(plantHt, plantHt.np, na.rm = TRUE),
         earHt = max(earHt, earHt.np, na.rm = TRUE),
         stalkLodgeNum = max(stalkLodgeNum, stalkLodgeNum.np, na.rm = TRUE),
         rootLodgeNum = max(rootLodgeNum, rootLodgeNum.np, na.rm = TRUE),
         harvestDate = max(harvestDate, harvestDate.np, na.rm = TRUE),
         combineYield = max(combineYield, combineYield.np, na.rm = TRUE),
         combineMoisture = max(combineMoisture, combineMoisture.np, na.rm = TRUE),
         combineTestWt = max(combineTestWt, combineTestWt.np, na.rm = TRUE),
         combineNotes = max(combineNotes, combineNotes.np, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.np, na.rm = TRUE),
         field = max(field, field.np, na.rm = TRUE),
         population = max(population, population.np, na.rm = TRUE),
         notes = max(notes, notes.np, na.rm = TRUE),
         yieldPerAc = max(yieldPerAc, yieldPerAc.np, na.rm = TRUE)) %>%
  select(!ends_with('.np'))
# Change -Inf to NA


hips_v2.1 <- hips_v2.1 %>%
  ungroup() %>%
  mutate(across(c(where(is.numeric), where(is.POSIXct)), ~case_when(.==-Inf ~ NA, .default = .)))
# Export to tsv
#write.table(hips_v2.1, 'HIPS_2022_V2.1.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
# Figure out how plantDensity, yieldPerAc are calculated in each loc & choose one way
#%>%


# NP uses this formula: accounts for moisture and converts to standard bushels (56 lbs at 15.5% moisture)
# This is a pretty standard formula; equivalent to those given by UW-Madison and other Extension sources 
# Does not account for differences in test weight
buPerAc15.5 <- function(harvestWeightLbs, harvestMoisture, plotLen)
{
  if (is.na(harvestWeightLbs) | is.na(harvestMoisture) | is.na(plotLen))
  {
    return (NA)
  }
  else
  {
    dryMatter <- harvestWeightLbs*(100 - harvestMoisture)/100
    stdMoistureBu <- dryMatter/47.32 # At 15.5% moisture, there are 47.32 lbs dry matter in a bushel with a test weight of 56 lbs
  portionAc <- 43560/(5*plotLen) # sq ft/ac divided by square feet per harvested plot area: 2 rows with 30" spacing = 5' x plot length in ft
  buPerAc <- stdMoistureBu*portionAc
  return(buPerAc)
  }
}
# For values from a MV plot: Gives 159.2332 bu/ac
print(buPerAc15.5(16.78, 9.8, 17.5)) 

# I think this is how the combine is calculating 
# Does NOT account for the change in test weight after drying but converts to 15.5% moisture
# buPerAcTestWt <- function(harvestWeightLbs, harvestMoisture, testWt, plotLen)
# {
#   dryMatter <- harvestWeightLbs*(100 - harvestMoisture)/100
#   # Formula for calculating the change in test weight if you dry to 15.5% moisture - commented out because it doesn't seem to be used
#   #stdMoistureTestWt <- 84.5/(100 - harvestMoisture)*testWt
#   stdMoistureBu <- dryMatter/testWt/0.845
#   portionAc <- 43560/(5*plotLen) 
#   buPerAc <- stdMoistureBu*portionAc
#   return(buPerAc)
# }
# # For same MV plot values (and the given test weight): 
# #Gives 147.1462 bu/ac: 
# # Actual spreadsheet lists 144.5 but this is a reasonable difference 
# # within typical error window for a combine yield monitor and some differences in rounding may be occurring
# print(buPerAcTestWt(16.78, 9.8, 60.6, 17.5)) 

# Calculate plantDensity and yieldPerAc for all locations
hips_v2.2 <- hips_v2.1 %>%
  rowwise() %>%
  mutate(plotLen = case_when(loc=='Scottsbluff' & population=='Hybrid' ~ 22.5,
                             loc=='Scottsbluff' & population=='Inbred' ~ 7.5,
                             str_detect(loc, 'North Platte') & is.na(plotLen) ~ 17,
                             loc=='Missouri Valley' ~ 17.5,
                             loc=='Lincoln' & population=='Hybrid' ~ 17.5,
                             loc=='Lincoln' & population=='Inbred' ~ 7.5, 
                             .default = 17.5)) %>%
  mutate(plantDensity = case_when(is.na(plantDensity) & !is.na(totalStandCt) ~ ((totalStandCt/2)*(17.5/plotLen)*1000), .default = plantDensity)) %>%
  mutate(yieldPerAc = buPerAc15.5(combineYield, combineMoisture, plotLen)) %>%
  mutate(plantDate = case_when(loc=='Lincoln' ~ as.POSIXct('2022-05-22'),
                               loc=='Scottsbluff' ~ as.POSIXct('2022-05-19'),
                               loc=='North Platte1'|loc=='North Platte2' ~ as.POSIXct('2022-05-17'),
                               loc=='North Platte3' ~ as.POSIXct('2022-05-18'),
                               loc=='Crawfordsville' ~ as.POSIXct('2022-05-11'),
                               loc=='Ames' ~ as.POSIXct('2022-05-23'),
                               .default = plantDate),
         notes = case_when(str_detect(genotypeNote, 'commercial')|str_detect(genotypeNote, 'Solar') ~ str_c(notes, genotypeNote, sep = ';'), 
                           .default = notes),
         pctStalkLodge = case_when(is.na(pctStalkLodge) & !is.na(stalkLodgeNum) & !is.na(totalStandCt) ~ stalkLodgeNum/totalStandCt*100,
                                   .default = pctStalkLodge),
         pctRootLodge = case_when(is.na(pctRootLodge) & !is.na(rootLodgeNum) & ! is.na(totalStandCt) ~ rootLodgeNum/totalStandCt*100,
                                  .default = pctRootLodge))
# Drop genotypeNote, stand 1, stand2
hips_v2.2 <- select(hips_v2.2, !c(genotypeNote, standCt1, standCt2, stalkLodgeNum, rootLodgeNum, earDropNum))

# Export v2.2
#write.table(hips_v2.2, file = 'HIPS_2022_V2.2.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

# Let's drop some outliers that should be exclude due to data collection or entry errors - generally an order of magnitude off from rest of data: 
combineNotes_drop <- c("Left row ran over", "Right row gone", "Run over", "Clog", "Animal damage", "Animal damage, not enough grain for accurate moisture", "One row", 
                       "Same as last", "11/9/2022, 11:45:04 AM, Tare Warning, Test Weight: 5.75", "Plot lost to belt issue")
combineVars <- c('combineYield', 'combineMoisture', 'combineTestWt', 'yieldPerAc')
combineNotes_dropMoisture <- c( "Water stress, not enough grain for accurate moisture", "Not enough grain for accurate moisture")

hips_v2.3 <- hips_v2.2 %>%
  mutate(across(all_of(combineVars), ~case_when(combineNotes %in% combineNotes_drop ~ NA, .default = .))) %>%
  rowwise() %>%
  mutate(combineMoisture = case_when(combineNotes %in% combineNotes_dropMoisture | combineMoisture==0 ~ NA, .default = combineMoisture),
         earHt = case_when(loc=='Lincoln' & plot %in% c(5244, 5283, 5141) ~ NA, .default = earHt),
         combineYield = case_when(loc=='North Platte1' & plot==250 ~ NA, .default = combineYield),
         yieldPerAc = case_when(loc=='North Platte1' & plot==250 ~ NA, .default = yieldPerAc),
         combineTestWt = na_if(combineTestWt, 0),
         earLen = case_when(loc=='North Platte1' & plot==426 ~ NA, .default = earLen),
         earFillLen = case_when(loc=='North Platte1' & plot==426 ~ NA,
                                loc=='North Platte2' & plot==879 ~ mean(15.5, 16, 12), 
                                .default = earFillLen),
         earWidth = case_when(loc=='North Plattte1' & plot==426 ~ NA, .default = earWidth),
         shelledCobWidth = case_when(loc=='North Platte1' & plot==426 ~ NA, .default = shelledCobWidth))
# Create variables for days to silk and days to anthesis
hips_v2.3 <- hips_v2.3 %>%
  rowwise() %>%
  mutate(daysToAnthesis = as.integer(difftime(as.Date(anthesisDate), as.Date(plantDate), units = 'days')),
         daysToSilk = as.integer(difftime(as.Date(silkingDate), as.Date(plantDate), units = 'days')))
# Export 2.3 as tsv
#write.table(hips_v2.3, file = 'HIPS_2022_V2.3.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
# Remove an egregious outliers
hips_v2.4 <- hips_v2.3 %>%
  rowwise() %>%
  mutate(earWidth = case_when(loc=='North Platte1' & plot==426 ~ NA, .default = earWidth))
# Remove rows missing plot, range, row, rep, and qr --> we can't id these plots
hips_v2.4 <- hips_v2.4 %>%
  filter(!(is.na(plot) & is.na(range) & is.na(row) & is.na(rep) & is.na(qr)))
# Export 2.4 as tsv --> changes were made upstream in the code to include kernel rows and kernels per row
#write.table(hips_v2.4, file = 'HIPS_2022_V2.4.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

# Integrate MO Valley plant data taken by ISU
# Hybrids first
# JD: the reps in the file for hybrids seem to be flipped. 
# According to the readme, the data was taken in-field according to the range/row coordinates, so these are the most reliable for matching data
# So some of the code below looks like a bug but isn't
mv.plantData.hyb <- read_excel('data/Plant_data_MO_Valley_2022.xlsx', 
                               sheet = '4211', 
                               col_names = c('row', 'range', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                               col_types = c('skip', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
                               skip = 1)
mv.plantData.hyb <- mv.plantData.hyb %>%
  rowwise() %>%
  mutate(plot = case_when(rep==1 ~ plot + 200,
                          rep==2 ~ plot + 100,
                          .default = plot),
         genotype = str_to_upper(genotype),
         loc = 'Missouri Valley',
         field = 'Hybrid HIPS',
         nLvl = 'Medium',
         irrigation = 'Dryland',
         population = 'Hybrid',
         plantHt = case_when(plantHt=='n/a - solar' ~ NA, .default = plantHt),
         earHt = case_when(earHt=='n/a - solar' ~ NA, .default = earHt),
         rep = case_when(rep==1 ~ 2,
                         rep==2 ~ 1)) %>%
  fixGenos(hips1.5_genoFixKey)
# Repeat for the inbreds
mv.plantData.inb <- read_excel('data/Plant_data_MO_Valley_2022.xlsx',
                               sheet = '2211',
                               col_names = c('row', 'range', 'notes', 'plantHt', 'earHt', 'genotype'),
                               col_types = c('skip', 'numeric', 'numeric', 'skip', 'skip', 'text', 'numeric', 'numeric', 'text', 'skip'),
                               skip = 1)
mv.plantData.inb <- mv.plantData.inb %>%
  rowwise() %>%
  mutate(plantHt = case_when(plantHt=='n/a' ~ NA, .default = plantHt),
         earHt = case_when(earHt=='n/a' ~ NA, .default = earHt),
         genotype = str_to_upper(genotype),
         loc = 'Missouri Valley',
         field = 'Inbred HIPS',
         nLvl = 'Medium',
         irrigation = 'Dryland',
         population = 'Inbred')
mv.plantData <- bind_rows(mv.plantData.hyb, mv.plantData.inb)

hips_v2.5 <- full_join(hips_v2.4, mv.plantData, join_by(loc, plot, population, rep, range, row), keep = FALSE, suffix = c('', '.mv'))
# Deal with duplicate columns
# Drop genotype.mv: Due to the rep switch, this genotype info is flipped between reps
hips_v2.5 <- hips_v2.5 %>%
  rowwise() %>%
  mutate(plantHt = max(plantHt, plantHt.mv, na.rm = TRUE),
         earHt = max(earHt, earHt.mv, na.rm = TRUE),
         field = max(field, field.mv, na.rm = TRUE),
         nLvl = max(nLvl, nLvl.mv, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.mv, na.rm = TRUE),
         notes = str_c(notes, notes.mv, sep = ';'),
         moistureCorrectedKernelMass = kernelMass/(1 - pctMoistureNIR),
         moistureCorrectedHundredKernelWt = hundredKernelWt/(1 - pctMoistureNIR)) %>%
  mutate(plantHt = case_when(loc=='Scottsbluff' & plot==1172 ~ NA, .default = plantHt),
         shelledCobWidth = case_when(shelledCobWidth > earWidth ~ NA, .default = shelledCobWidth)) %>%
  select(!contains('.mv')) %>%
  ungroup() %>%
  mutate(across(where(is.double) & !where(is.POSIXct), ~na_if(., -Inf))) %>%
  mutate(across(where(is.integer), ~na_if(., as.integer(-Inf)))) %>%
  mutate(across(where(is.POSIXct), ~na_if(., as.POSIXct(-Inf))))
# Remove some outliers id'ed based on visual plots or fix them when 1 obs is throwing them off
hips_v2.5 <- hips_v2.5 %>%
  rowwise() %>%
  mutate(moistureCorrectedHundredKernelWt = case_when(loc=='Lincoln' & plot==5122 ~ mean(23.06, 19.64, 13.53)/(1 - pctMoistureNIR), 
                                                      loc=='Lincoln' & plot==5245 ~ mean(22.9, 24, 18)/(1 - pctMoistureNIR),
                                                      loc=='North Platte2' & plot==835 ~ mean(27.26, 28.78, 27.51)/(1 - pctMoistureNIR),
                                                      loc=='North Platte2' & plot==838 ~ mean(39.9, 32.48, 36)/(1 - pctMoistureNIR),
                                                      loc=='North Platte3' & plot==1169 ~ mean(24.17, 23.59, 27.22)/(1- pctMoistureNIR),
                                                      loc=='North Platte3' & plot==1211 ~ mean(20.3, 19.8, 17.7)/(1 - pctMoistureNIR),
                                                      .default = moistureCorrectedHundredKernelWt),
         hundredKernelWt = case_when(loc=='Lincoln' & plot==5122 ~ mean(23.06, 19.64, 13.53), 
                                     loc=='Lincoln' & plot==5245 ~ mean(22.9, 24, 18),
                                     loc=='North Platte2' & plot==835 ~ mean(27.26, 28.78, 27.51), 
                                     loc=='North Platte2' & plot==838 ~ mean(39.9, 32.48, 36),
                                     loc=='North Platte3' & plot==1169 ~ mean(24.17, 23.59, 27.22),
                                     loc=='North Platte3' & plot==1211 ~ mean(20.3, 19.8, 17.7),
                                     .default = hundredKernelWt),
         kernelRows = case_when(loc=='North Platte1' & plot==367 ~ mean(14, 18, 14), .default = kernelRows),
         kernelsPerRow = case_when(loc=='Lincoln' & plot==4209 ~ mean(26, 28, 22), .default = kernelsPerRow),
         daysToSilk = case_when((loc=='North Platte3' & plot==1397)|(loc=='Scottsbluff' & plot==1346) ~ NA, 
                                .default = daysToSilk),
         shelledCobWt = case_when(loc=='North Platte3' & plot==1226 ~ mean(15.5, 18, 19), .default = shelledCobWt),
         earWidth = case_when(loc=='Lincoln' & plot==6129 ~ mean(rep(3.5, 3)), .default = earWidth),
         earFillLen = case_when(loc=='North Platte2' & plot==837 ~ mean(16.5, 16, 16), .default = earFillLen),
         kernelsPerEar = case_when(loc=='North Platte3' & plot==1381 ~ mean(412, 492, 487), .default = kernelsPerEar),
         moistureCorrectedKernelMass = case_when(loc=='Missouri Valley' & plot==256 ~ mean(232.86 - 22.57, 168.35 - 18.65, 139.89 - 14.55)/(1 - pctMoistureNIR), 
                                                 loc=='North Platte2' & plot==664 ~ mean(91.33 - 17.5, 170.48 - 22.56, 236.75 - 24.17)/(1 - pctMoistureNIR),
                                                 .default = moistureCorrectedKernelMass),
         kernelMass = case_when(loc=='Missouri Valley' & plot==256 ~ mean(232.86 - 22.57, 168.35 - 18.65, 139.89 - 14.55), 
                                loc=='North Platte2' & plot==664 ~ mean(91.33 - 17.5, 170.48 - 22.56, 236.75 - 24.17),
                                .default = kernelMass),
         flagLeafHt = case_when(loc=='Scottsbluff' & plot==1172 ~ NA, .default = flagLeafHt))
# Export v2.5
write.table(hips_v2.5, 'outData/HIPS_2022_V2.5.tsv', sep = '\t', row.names = FALSE, col.names = TRUE)

# Work on summarizing ames and crawfordsville data
ac.ears <- tibble(loc = NULL, plot = NULL, qr = NULL, population = NULL, irrigation = NULL, kernelRows = NULL, notes = NULL, earLen = NULL, earWidth = NULL, earWt = NULL, .rows = 0)
# Read in and parse KRN data
krn.hyb.files <- list.files(path = 'data/2 KRN And Ear Documenation/Hybrid', pattern = '2_KRN_and_ear_documenation_', full.names = TRUE)

for (currFile in krn.hyb.files)
{
  curr.df <- read_excel(currFile, 
                        skip = 3,
                        col_names = c('qr', 'kernelRows', 'notes', 'sweetcorn'),
                        col_types = c('text', 'numeric', 'text', 'text')) %>%
    filter(!is.na(qr)|!is.na(kernelRows)) %>%
    rowwise() %>%
    mutate(qr = str_c(str_split_i(qr, '-', 1), str_split_i(qr, '-', 2), str_split_i(qr, '-', 3), sep = '-') %>%
             str_to_upper() %>%
             str_remove('.') %>%
             str_remove('`') %>%
             str_remove('U') %>%
             str_remove('R'),
           notes = case_when(!is.na(sweetcorn) ~ str_c(notes, 'sweetcorn', sep = ';'), .default = notes),
           loc = case_when(str_split_i(qr, '-', 2)=='C' ~ 'Crawfordsville',
                           str_split_i(qr, '-', 2)=='A' ~ 'Ames'),
           plot = str_split_i(qr, '-', 3) %>%
             as.numeric(), 
           population = 'Hybrid', 
           irrigation = 'Dryland')
  ac.ears <- bind_rows(ac.ears, curr.df)
}

krn.inb.files <- list.files(path = 'data/2 KRN And Ear Documenation', pattern = '2_KRN_and_ear_documenation_inbred', full.names = TRUE, recursive = TRUE, 
                            include.dirs = FALSE)

for (currFile in krn.inb.files)
{
  curr.df <- read_excel(currFile,
                        skip = 3,
                        col_names = c('qr', 'kernelRows', 'notes', 'smoothCob', 'sweetcorn'),
                        col_types = c('text', 'numeric', rep('text', 3))) %>%
    filter(!is.na(qr)|!is.na(kernelRows)) %>%
    rowwise() %>%
    mutate(qr = str_c(str_split_i(qr, '-', 1), str_split_i(qr, '-', 2), str_split_i(qr, '-', 3), sep = '-') %>%
             str_remove('.') %>%
             str_remove('`') %>%
             str_remove('u') %>%
             str_to_upper() %>%
             str_remove('R'),
           notes = case_when(!is.na(sweetcorn) & is.na(smoothCob) ~ str_c(notes, 'sweetcorn', sep = ';'),
                             !is.na(smoothCob) & is.na(sweetcorn) ~ str_c(notes, 'smooth cob - ovule issue', sep = ';'),
                             !is.na(sweetcorn) & !is.na(smoothCob) ~ str_c(notes, 'sweetcorn', 'smooth cob - ovule issue', sep = ';'), 
                             .default = notes),
           loc = case_when(str_split_i(qr, '-', 2)=='C' ~ 'Crawfordsville',
                           str_split_i(qr, '-', 2)=='A' ~ 'Ames'),
           plot = str_split_i(qr, '-', 3) %>%
             as.numeric(), 
           population = 'Inbred', 
           irrigation = 'Dryland', 
           field = case_when(str_detect(currFile, '2231')|str_detect(currFile, '2232') ~ 'B1',
                             str_detect(currFile, '2233') ~ 'E1',
                             str_detect(currFile, '2351')|str_detect(currFile, '2352') ~ 'A',
                             str_detect(currFile, '2353') ~ 'B'),
           nLvl = case_when(str_detect(currFile, '2233')|str_detect(currFile, '2352') ~ 'Low', 
                            str_detect(currFile, '2232')|str_detect(currFile, '2353') ~ 'Medium',
                            str_detect(currFile, '2231')|str_detect(currFile, '2351') ~ 'High'))
  ac.ears <- bind_rows(ac.ears, curr.df)
}

ac.ears <- ac.ears %>%
  rowwise() %>%
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr)) %>%
  group_by(qr, loc, plot, population, irrigation, nLvl, field) %>%
  summarise(kernelRows = mean(kernelRows, na.rm = TRUE),
            notes = max(notes, na.rm = TRUE))
  

ear.files <- list.files(path = 'data/3 Ear Traits Station', pattern = '3_Ear_Traits_', full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
ear.df <- tibble(loc = NULL, plot = NULL, qr = NULL, population = NULL, irrigation = NULL, notes = NULL, earLen = NULL, earWidth = NULL, earWt = NULL, .rows = 0)
for (currFile in ear.files)
{
  curr.df <- read_excel(currFile, 
                        col_names = c('qr', 'earLen', 'earWidth', 'earWt', 'collectionStation', 'string', 'seedMissingAtWidest'),
                        col_types = c('text', rep('numeric', 3), rep('text', 3))) %>%
    filter(!is.na(qr)|!is.na(earLen)|!is.na(earWidth)|!is.na(earWt)) %>%
    rowwise() %>%
    mutate(qr = str_c(str_split_i(qr, '-', 1), str_split_i(qr, '-', 2), str_split_i(qr, '-', 3), sep = '-') %>%
             str_remove('.') %>%
             str_remove('`') %>%
             str_remove('u') %>%
             str_to_upper() %>%
             str_remove('R'),
           earLen = earLen/10,
           earWidth = earWidth/10,
           notes = case_when(!is.na(string) & is.na(seedMissingAtWidest) ~ 'Severe bend in ear. String used to measure ear length',
                             is.na(string) & !is.na(seedMissingAtWidest) ~ 'Seed missing on both sides at widest point of ear',
                             !is.na(string) & !is.na(seedMissingAtWidest) ~ 'Severe bend in ear. String used to measure ear length; Seed missing on both sides at widest point of ear'),
           loc = case_when(str_split_i(qr, '-', 2)=='C' ~ 'Crawfordsville',
                                 str_split_i(qr, '-', 2)=='A' ~ 'Ames'),
           plot = str_split_i(qr, '-', 3) %>%
             as.numeric(), 
           population = case_when(str_detect(currFile, 'Hybrid') ~ 'Hybrid',
                                  str_detect(currFile, 'Inbred') ~ 'Inbred'),
           irrigation = 'Dryland',
           field = case_when(str_detect(currFile, '2231')|str_detect(currFile, '2232') ~ 'B1',
                             str_detect(currFile, '2233') ~ 'E1',
                             str_detect(currFile, '2351')|str_detect(currFile, '2352') ~ 'A',
                             str_detect(currFile, '2353') ~ 'B'),
           nLvl = case_when(str_detect(currFile, '2233')|str_detect(currFile, '2352') ~ 'Low', 
                            str_detect(currFile, '2232')|str_detect(currFile, '2353') ~ 'Medium',
                            str_detect(currFile, '2231')|str_detect(currFile, '2351') ~ 'High'))
  ear.df <- bind_rows(ear.df, curr.df)
}
ear.df <- ear.df %>%
  rowwise() %>%
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr)) %>%
  group_by(qr, loc, plot, population, irrigation) %>%
  summarise(earLen = mean(earLen, na.rm = TRUE),
            earWidth = mean(earWidth, na.rm = TRUE),
            earWt = mean(earWt, na.rm = TRUE), 
            notes = max(notes, na.rm = TRUE),
            nLvl = max(nLvl, na.rm = TRUE))
# Remove example rows: they have no qrs
ac.ears <- filter(ac.ears, !is.na(qr))
ear.df <- filter(ear.df, !is.na(qr))
# Merge ear traits with krn data
ac.ears <- full_join(ac.ears, ear.df, join_by(qr), suffix = c('', '.3'), keep = FALSE)
ac.ears <- ac.ears %>%
  rowwise() %>%
  mutate(loc = max(loc, loc.3, na.rm = TRUE),
         plot = max(plot, plot.3, na.rm = TRUE),
         population = max(population, population.3, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.3, na.rm = TRUE), 
         nLvl = max(nLvl, nLvl.3, na.rm = TRUE),
         notes = case_when(is.na(notes) & !is.na(notes.3) ~ notes.3,
                           !is.na(notes) & is.na(notes.3) ~ notes,
                           !is.na(notes) & !is.na(notes.3) ~ str_c(notes, notes.3, sep = ';'))) %>%
  select(!ends_with('.3'))

cob.files <- list.files(path = 'data/5 Cob Traits Station', pattern = '5_cob_Traits_', full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
cob.df <- tibble(loc = NULL, plot = NULL, qr = NULL, population = NULL, irrigation = NULL, notes = NULL, shelledCobLen = NULL, shelledCobWidth = NULL, shelledCobWt = NULL, .rows = 0)

for(currFile in cob.files)
{
  curr.df <- read_excel(currFile, 
                        sheet = 'Sheet1', 
                        col_names = c('qr', 'shelledCobLen', 'shelledCobWidth', 'shelledCobWt', 'station', 'brokenCob', 'string', 'box'),
                        col_types = c('text', rep('numeric', 3), rep('text', 4)))
  curr.df <- curr.df %>%
    filter(!is.na(qr)|!is.na(shelledCobLen)|!is.na(shelledCobWidth)|!is.na(shelledCobWt)) %>%
    rowwise() %>%
    mutate(qr = str_c(str_split_i(qr, '-', 1), str_split_i(qr, '-', 2), str_split_i(qr, '-', 3), sep = '-') %>%
             str_remove('.') %>%
             str_remove('`') %>%
             str_remove('u') %>%
             str_to_upper() %>%
             str_remove('R'),
           shelledCobLen = shelledCobLen/10,
           shelledCobWidth = shelledCobWidth/10,
           notes = case_when(is.na(brokenCob) & !is.na(string) ~ 'Severe bend in ear. String used to measure cob length',
                             !is.na(brokenCob) & is.na(string) ~ 'Cob broke in pieces during shelling',
                             !is.na(brokenCob) & !is.na(string) ~ str_c('Severe bend in ear. String used to measure cob length', 'Cob broke in pieces during shelling', sep = ';')),
           loc = case_when(str_split_i(qr, '-', 2)=='C' ~ 'Crawfordsville',
                           str_split_i(qr, '-', 2)=='A' ~ 'Ames'),
           plot = str_split_i(qr, '-', 3) %>%
             as.numeric(), 
           population = case_when(str_detect(currFile, 'Hybrid') ~ 'Hybrid',
                                  str_detect(currFile, 'Inbred') ~ 'Inbred'),
           irrigation = 'Dryland',
           field = case_when(str_detect(currFile, '2231')|str_detect(currFile, '2232') ~ 'B1',
                             str_detect(currFile, '2233') ~ 'E1',
                             str_detect(currFile, '2351')|str_detect(currFile, '2352') ~ 'A',
                             str_detect(currFile, '2353') ~ 'B'),
           nLvl = case_when(str_detect(currFile, '2233')|str_detect(currFile, '2352') ~ 'Low', 
                            str_detect(currFile, '2232')|str_detect(currFile, '2353') ~ 'Medium',
                            str_detect(currFile, '2231')|str_detect(currFile, '2351') ~ 'High'))
  cob.df <- bind_rows(cob.df, curr.df)
}

cob.df <- cob.df %>%
  rowwise() %>%
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr)) %>%
  group_by(qr, loc, plot, irrigation) %>%
  summarise(shelledCobLen = mean(shelledCobLen, na.rm = TRUE),
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE),
            shelledCobWt = mean(shelledCobWt, na.rm = TRUE),
            notes = max(notes, na.rm = TRUE),
            nLvl = max(nLvl, na.rm = TRUE),
            field = max(field, na.rm = TRUE),
            population = max(population, na.rm = TRUE))
cob.df <- cob.df %>%
  filter(!is.na(qr))

ac.ears <- full_join(ac.ears, cob.df, join_by(qr), suffix = c('', '.cob'), keep = FALSE)

ac.ears <- ac.ears %>%
  rowwise() %>%
  mutate(loc = max(loc, loc.cob, na.rm = TRUE),
         plot = max(plot, plot.cob, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.cob, na.rm = TRUE), 
         notes = case_when(is.na(notes) & !is.na(notes.cob) ~ notes.cob,
                           !is.na(notes) & is.na(notes.cob) ~ notes,
                           !is.na(notes) & is.na(notes.cob) ~ str_c(notes, notes.cob, sep = ';')),
         nLvl = max(nLvl, nLvl.cob, na.rm = TRUE),
         field = max(field, field.cob, na.rm = TRUE), 
         population = max(population, population.cob, na.rm = TRUE)) %>%
  select(!ends_with('.cob'))


# Plant data
# Keep unique ids for Ames, Crawfordsville as qrs so we can bring in ear data processed at ISU
ames_hyb1 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '4231',
                        col_names = c('range', 'row', 'qr', 'notes', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'text', 'numeric', 'numeric', 'skip', 'numeric', 'numeric', 'skip', 'text'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'High',
         field = 'B1',
         irrigation = 'Dryland',
         population = 'Hybrid',
         plot = str_split_i(qr, '-', 3)) %>%
  fixGenos(hips1.5_genoFixKey)

ames_hyb2 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '4232',
                        col_names = c('range', 'row', 'qr', 'notes', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'text', 'numeric', 'numeric', 'skip', 'numeric', 'numeric', 'skip', 'text'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'Medium',
         field = 'B1',
         irrigation = 'Dryland',
         population = 'Hybrid',
         plot = str_split_i(qr, '-', 3)) %>%
  fixGenos(hips1.5_genoFixKey)

ames_hyb3 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '4233',
                        col_names = c('range', 'row', 'qr', 'plantHt', 'earHt','rep', 'plot', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'skip', 'numeric', 'numeric', 'skip', 'text'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'Low',
         field = 'E1',
         irrigation = 'Dryland',
         population = 'Hybrid',
         plot = str_split_i(qr, '-', 3)) %>%
  fixGenos(hips1.5_genoFixKey)

ames_inb1 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '2231',
                        col_names = c('range', 'row', 'qr', 'notes', 'plantHt', 'earHt', 'notes2', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'text', 'numeric', 'numeric', 'text', 'text', 'skip'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         notes = str_c(notes, notes2, sep = ';'),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'High',
         field = 'B1',
         irrigation = 'Dryland',
         population = 'Inbred',
         plantHt = case_when(plantHt=='n/a' ~ NA, .default = plantHt),
         earHt = case_when(earHt=='n/a' ~ NA, .default = earHt), 
         plot = str_split_i(qr, '-', 3)) %>%
  select(!notes2)

ames_inb2 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '2232',
                        col_names = c('range', 'row', 'qr', 'notes', 'plantHt', 'earHt', 'notes2', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'text', 'text', 'numeric', 'text', 'text', 'skip'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         notes = str_c(notes, notes2, sep = ';'),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'Medium',
         field = 'B1',
         irrigation = 'Dryland',
         population = 'Inbred',
         earHt = case_when(earHt=='n/a' ~ NA, .default = earHt),
         plot = str_split_i(qr, '-', 3)) %>%
  mutate(notes = case_when(plantHt=='break' ~ str_c(notes, plantHt, sep = ';'), .default = notes),
         plantHt = case_when(plantHt %in% c('break', 'n/a') ~ NA, .default = plantHt) %>%
           as.numeric()) %>%
  select(!notes2)

ames_inb3 <- read_excel('data/Plant_data_Ames_2022.xlsx',
                        sheet = '2233',
                        col_names = c('range', 'row', 'qr', 'notes', 'plantHt', 'earHt', 'notes2', 'genotype'),
                        col_types = c('numeric', 'numeric', 'skip', 'text', 'text', 'text', 'numeric', 'text', 'text', 'skip'),
                        skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         notes = case_when(plantHt=='break' ~ str_c(notes, notes2, plantHt), .default = str_c(notes, notes2)),
         plantHt = case_when(plantHt %in% c('break', 'n/a') ~ NA, .default = plantHt) %>%
           as.numeric(),
         earHt = case_when(str_detect(earHt, 'n/a') ~ NA, .default = earHt),
         genotype = str_to_upper(genotype),
         loc = 'Ames',
         nLvl = 'Low',
         field = 'E1',
         irrigation = 'Dryland',
         population = 'Inbred',
         plot = str_split_i(qr, '-', 3)) %>%
  select(!notes2)

c_hyb1 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '4351 (east)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plot = str_split_i(qr, '-', 3),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'High',
         field = 'A',
         irrigation = 'Dryland',
         population = 'Hybrid') %>%
  fixGenos(hips1.5_genoFixKey)

c_hyb2 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '4352 (west)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plot = str_split_i(qr, '-', 3),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'Low',
         field = 'A',
         irrigation = 'Dryland',
         population = 'Hybrid') %>%
  fixGenos(hips1.5_genoFixKey)

c_hyb3 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '4353 (south)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'rep', 'plot', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plot = str_split_i(qr, '-', 3),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'Medium',
         field = 'B',
         irrigation = 'Dryland',
         population = 'Hybrid') %>%
  fixGenos(hips1.5_genoFixKey)

c_inb1 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '2351 (east)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'notes', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'text', 'text', 'skip'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plantHt = case_when(str_detect(plantHt, 'n/a') ~ NA, .default = plantHt),
         earHt = case_when(str_detect(earHt, 'n/a') ~ NA, .default = earHt),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'High',
         field = 'A',
         irrigation = 'Dryland',
         population = 'Inbred',
         plot = str_split_i(qr, '-', 3))

c_inb2 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '2352 (west)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'notes', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'text', 'text', 'skip'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plantHt = case_when(str_detect(plantHt, 'n/a') ~ NA, .default = plantHt),
         earHt = case_when(str_detect(earHt, 'n/a') ~ NA, .default = earHt),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'Low',
         field = 'A',
         irrigation = 'Dryland',
         population = 'Inbred',
         plot = str_split_i(qr, '-', 3))

c_inb3 <- read_excel('data/Plant_data_Crawfordsville_2022.xlsx',
                     sheet = '2353 (south)',
                     col_names = c('row', 'range', 'qr', 'plantHt', 'earHt', 'notes', 'genotype'),
                     col_types = c('skip', 'numeric', 'numeric', 'skip', 'text', 'numeric', 'numeric', 'text', 'text', 'skip'),
                     skip = 1) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         plantHt = case_when(str_detect(plantHt, 'filler') ~ NA, .default = plantHt),
         earHt = case_when(str_detect(earHt, 'n/a') ~ NA, .default = earHt),
         genotype = str_to_upper(genotype),
         loc = 'Crawfordsville',
         nLvl = 'Medium',
         field = 'B',
         irrigation = 'Dryland',
         population = 'Inbred', 
         plot = str_split_i(qr, '-', 3))

plantData.ac <- bind_rows(ames_hyb1, ames_hyb2, ames_hyb3, ames_inb1, ames_inb2, ames_inb3,
                           c_hyb1, c_hyb2, c_hyb3, c_inb1, c_inb2, c_inb3)

# Bind plant data and the ear data together
# First, fix the truncation of 22- to 2- in the ear data
ac.ears <- ac.ears %>%
  rowwise() %>%
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr))
# And remove observations in plant data we can't id (with out a range, row, or qr)
plantData.ac <- plantData.ac %>%
  filter(!is.na(qr)|!is.na(range)|!is.na(row))

ac.df <- full_join(plantData.ac, ac.ears, join_by(qr), suffix = c('', '.ear'), keep = FALSE)

ac.df <- ac.df %>%
  rowwise() %>%
  mutate(loc = max(loc, loc.ear, na.rm = TRUE), 
         plot = max(plot, plot.ear, na.rm = TRUE),
         population = max(population, population.ear, na.rm = TRUE),
         irrigation = max(irrigation, irrigation.ear, na.rm = TRUE),
         nLvl = max(nLvl, nLvl.ear, na.rm = TRUE), 
         field = min(field, field.ear, na.rm = TRUE),
         notes = case_when(is.na(notes) & !is.na(notes.ear) ~ notes.ear,
                           !is.na(notes) & is.na(notes.ear) ~ notes,
                           !is.na(notes) & !is.na(notes.ear) ~ str_c(notes, notes.ear, sep = ';'))) %>%
  select(!ends_with('.ear'))

seed.files <- list.files(path = 'data/6 Seed Traits Station', pattern = '6_Seed_Traits_', full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
seed.df <- tibble(qr = NULL, kernelsPerEar = NULL, kernelMass = NULL, notes = NULL)

for(currFile in seed.files)
{
  # Skip the redo files for now
  if(str_detect(currFile,'redo'))
  {
    next
  }
  curr.df <- read_excel(currFile, 
                        skip = 3,
                        sheet = 'Sheet1',
                        col_names = c('qr', 'kernelsPerEar', 'kernelMass', 'avgKernelWt', 'notes', 'station', 'box'),
                        col_types = c('text', rep('numeric', 3), rep('text', 3)))
  curr.df <- curr.df %>%
    rowwise() %>%
    mutate(plot = str_split_i(qr, '-', 3),
           loc = case_when(str_detect(qr, 'A') ~ 'Ames',
                           str_detect(qr, 'C') ~ 'Crawfordsville'),
           population = case_when(str_detect(currFile, 'Hybrid') ~ 'Hybrid',
                                   str_detect(currFile, 'Inbred') ~ 'Inbred'),
           irrigation = 'Dryland',
           field = case_when(str_detect(currFile, '2231')|str_detect(currFile, '2232') ~ 'B1',
                             str_detect(currFile, '2233') ~ 'E1',
                             str_detect(currFile, '2351')|str_detect(currFile, '2352') ~ 'A',
                             str_detect(currFile, '2353') ~ 'B'),
           nLvl = case_when(str_detect(currFile, '2233')|str_detect(currFile, '2352') ~ 'Low', 
                            str_detect(currFile, '2232')|str_detect(currFile, '2353') ~ 'Medium',
                            str_detect(currFile, '2231')|str_detect(currFile, '2351') ~ 'High')) %>%
    mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr))
  seed.df <- bind_rows(seed.df, curr.df)
}

seed.redo.files <- list.files(path = 'data/6 Seed Traits Station/Left station/redo', pattern = '6_Seed_Traits_', full.names = TRUE)
seed.redo.df <- tibble(qr = NULL, kernelMass = NULL)
for(currFile in seed.redo.files)
{
  curr.df <- read_excel(currFile, 
                        skip = 3,
                        sheet = 'Sheet1',
                        col_names = c('qr', 'kernelMass', 'station', 'box'),
                        col_types = c('text', 'numeric', rep('text', 2)))
  curr.df <- curr.df %>%
    rowwise() %>%
    mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr)) %>%
    select(qr, kernelMass) %>%
    filter(!is.na(kernelMass))
  seed.redo.df <- bind_rows(seed.redo.df, curr.df)
}

# Replace kernelMass when it was redone
seed.df <- full_join(seed.df, seed.redo.df, join_by(qr), suffix = c('', '.redo'), keep = FALSE)
seed.df <- seed.df %>%
  rowwise() %>%
  mutate(kernelMass = case_when(!is.na(kernelMass.redo) ~ kernelMass.redo, .default = kernelMass),
         qr = str_c(str_split_i(qr, '-', 1), str_split_i(qr, '-', 2), str_split_i(qr, '-', 3), sep = '-')) %>%
  group_by(qr) %>%
  summarise(kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE), 
            kernelMass = mean(kernelMass, na.rm = TRUE),
            notes = max(notes, na.rm = TRUE),
            plot = max(plot, na.rm = TRUE),
            loc = max(loc, na.rm = TRUE), 
            population = max(population, na.rm = TRUE),
            irrigation = max(irrigation, na.rm = TRUE),
            field = max(field, na.rm = TRUE), 
            nLvl = max(nLvl, na.rm = TRUE))

ac.df <- full_join(ac.df, seed.df, join_by(qr), suffix = c('', '.seed'), keep = FALSE)
ac.df <- ac.df %>%
  rowwise() %>%
  mutate(notes = case_when(is.na(notes) & !is.na(notes.seed) ~ notes.seed,
                           !is.na(notes) & is.na(notes.seed) ~ notes,
                           .default = str_c(notes, notes.seed, sep = ';')), 
         plot = max(plot, plot.seed, na.rm = TRUE) %>%
           as.numeric(),
         loc = max(loc, loc.seed, na.rm = TRUE),
         population = max(population, population.seed, na.rm = TRUE), 
         irrigation = max(irrigation, irrigation.seed, na.rm = TRUE),
         field = min(field, field.seed, na.rm = TRUE), 
         nLvl = max(nLvl, nLvl.seed)) %>%
  select(!ends_with('.seed'))


# Integrate combine data for Ames & Crawfordsville
ac.yield.hyb <- read_excel('data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx', 
                           sheet = 'RawData (4-Row)', 
                           col_names = c('loc', 'exp', 'check', 'qr', 'genotype', 'latitude', 'longitude', 'row', 'range', 'rep', 'plotDiscarded', 'combineYield', 'combineMoisture',
                                         'combineTestWt','plantDensity', 'pctRootLodge', 'pctStalkLodge', 'plantDate', 'harvestDate', 'combineNotes', 'totalStandCt', 'solar', 
                                         'tattooSensor', 'nitrateSensor', 'commercialSoilMoistureSensor'),
                           col_types = c(rep('skip', 3), 'text', 'text', rep('skip', 2), 'text', 'text', 'skip', 'text', 'skip', 'skip', rep('numeric', 5), 'text', rep('numeric', 2),
                                         'skip', rep('numeric', 2), 'skip', 'numeric', 'skip', 'numeric', 'date', 'date', 'text', 'numeric', 'skip', 'skip', rep('text', 4)),
                           skip = 1)
ac.yield.hyb <- ac.yield.hyb %>%
  filter(loc %in% c("ISU.IA.Ames", "ISU.IA.Crawfordsville") & is.na(plotDiscarded) & !is.na(genotype)) %>%
  rowwise() %>%
  mutate(loc = case_when(str_detect(qr, 'A') ~ 'Ames',
                         str_detect(qr, 'C') ~ 'Crawfordsville'),
         nLvl = case_when(exp %in% c('LC_4233', 'LC_4352') ~ 'Low',
                          exp %in% c('LC_4232', 'LC_4353') ~ 'Medium',
                          exp %in% c('LC_4231', 'LC_4351') ~ 'High'),
         plot = str_split_i(qr, '-', 3) %>%
           as.numeric(),
         genotype = str_to_upper(genotype),
         plotLen = 17.5, 
         field = case_when(exp %in% c('LC_4233') ~ 'E1',
                           exp %in% c('LC_4231', 'LC_4232') ~ 'B1',
                           exp %in% c('LC_4352', 'LC_4351') ~ 'A',
                           exp %in% c('LC_4353') ~ 'B'),
         irrigation = 'Dryland',
         population = 'Hybrid',
         yieldPerAc = case_when(!is.na(combineYield) & !is.na(combineMoisture) ~ buPerAc15.5(combineYield, combineMoisture, plotLen)),
         solar = case_when(!is.na(solar) ~ 'Solar panel'),
         check = case_when(!is.na(check) ~ 'Check'),
         nitrateSensor = case_when(!is.na(nitrateSensor) ~ 'Nitrate sensor'),
         commercialSoilMoistureSensor = case_when(!is.na(commercialSoilMoistureSensor) ~ 'Commercial soil moisture sensor'),
         tattooSensor = case_when(!is.na(tattooSensor) ~ 'Tattoo sensor')) %>%
  unite('notes', c(check, solar, nitrateSensor, commercialSoilMoistureSensor, tattooSensor), na.rm = TRUE, sep = ';', remove = TRUE) %>%        
  select(!c(plotDiscarded, exp)) %>%
  fixGenos(hips1.5_genoFixKey)

ac.yield.inb <- read_excel('data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx', 
                           sheet = 'RawData (2-Row)', 
                           skip = 1,
                           col_types = c(rep('skip', 3), rep('text', 2), 'skip', 'text', 'skip', 'text', 'skip', 'text', 'skip', 'text', 'skip', rep('numeric', 5), 'text', rep('skip', 4),
                                         'numeric', rep('skip', 4), 'date', rep('skip', 2), 'numeric', rep('skip', 2), rep('numeric', 3)),
                           col_names = c('loc', 'exp', 'check', 'qr', 'genotype', 'notes', 'latitude', 'longitude', 'row', 'range', 'rep', 'plotDiscarded', 'plantDensity', 'plantDate',
                                         'totalStandCt', 'histFT', 'histPlantHt', 'block'))
ac.yield.inb <- ac.yield.inb %>%
  filter(loc %in% c("ISU.IA.Ames", "ISU.IA.Crawfordsville") & is.na(plotDiscarded) & !is.na(genotype)) %>%
  rowwise() %>%
  mutate(qr = str_to_upper(qr),
         loc = case_when(str_detect(qr, 'A') ~ 'Ames',
                         str_detect(qr, 'C') ~ 'Crawfordsville'),
         nLvl = case_when(exp %in% c('LC_2233', 'LC_2352') ~ 'Low',
                          exp %in% c('LC_2232', 'LC_2353') ~ 'Medium',
                          exp %in% c('LC_2231', 'LC_2351') ~ 'High'),
         plot = str_split_i(qr, '-', 3) %>%
           as.numeric(),
         genotype = str_to_upper(genotype),
         plotLen = 7.5,
         field = case_when(exp %in% c('LC_2233') ~ 'E1',
                           exp %in% c('LC_2231', 'LC_2232') ~ 'B1',
                           exp %in% c('LC_2351', 'LC_2352') ~ 'A',
                           exp %in% c('LC_2353') ~ 'B'),
         irrigation = 'Dryland',
         population = 'Inbred', 
         check = case_when(!is.na(check) ~ 'Check')) %>%
  unite('notes', c(notes, check), sep = ';', na.rm = TRUE, remove = TRUE) %>%
  select(!c(exp, plotDiscarded))
ac.yield <- bind_rows(ac.yield.hyb, ac.yield.inb)

# Get qrs in each treatment x loc combo so we can set nLvl for ac.df before we merge
low <- filter(ac.yield, nLvl=='Low') %>%
  select(qr) %>%
  as.vector()
med <- filter(ac.yield, nLvl=='Medium') %>%
  select(qr) %>%
  as.vector()
high <- filter(ac.yield, nLvl=='High') %>%
  select(qr) %>%
  as.vector()
a <- filter(ac.yield, field=='A') %>%
  select(qr) %>%
  as.vector()
b <- filter(ac.yield, field=='B') %>%
  select(qr) %>%
  as.vector()
b1 <- filter(ac.yield, field=='B1') %>%
  select(qr) %>%
  as.vector()
e1 <- filter(ac.yield, field=='E1') %>%
  select(qr) %>%
  as.vector()
# Remove two filler row that accidentally got assigned the same qrs as actual check plots in another nitrogen block
ac.df <- ac.df %>%
  rowwise() %>%
  mutate(field = case_when(qr %in% a$qr ~ 'A',
                           qr %in% b$qr ~ 'B',
                           qr %in% b1$qr ~ 'B1',
                           qr %in% e1$qr ~ 'E1'),
         nLvl = case_when(qr %in% low$qr ~ 'Low',
                          qr %in% med$qr ~ 'Medium',
                          qr %in% high$qr ~ 'High')) %>%
  filter(!c(qr %in% c('22-A-1585271', '22-A-1585190') & str_detect(notes, 'toss')))
ac.all.df <- full_join(ac.df, ac.yield, join_by(range, row, loc, field, nLvl, population), suffix = c('', '.yield'))
ac.all.df <- ac.all.df %>%
  rowwise() %>%
  mutate(qr = max(qr, qr.yield, na.rm = TRUE),
         rep = max(rep, rep.yield, na.rm = TRUE),
         plot = max(plot, plot.yield, na.rm = TRUE), 
         irrigation = 'Dryland', 
         field = case_when((loc=='Ames' & range>=25 & population=='Hybrid' & is.na(field)) | 
                             (loc=='Ames' & plot %in% c(1550244:1550300, 1571923:1572000, 1583701:1583710, 1585301:1585331, 1583799:1583886, 1585420:1585507) & is.na(field)) | 
                             (loc=='Ames' & row>10 & is.na(field)) ~ 'B1',
                           loc=='Ames' & plot %in% c(1570100:1585275) & is.na(field) & population=='Hybrid' ~ 'E1', 
                           loc=='Crawfordsville' & plot %in% c(1584342:1584700, 1584901:1585063) & is.na(field) |
                             (loc=='Crawfordsville' & row<40 & is.na(field) & population=='Hybrid') ~ 'A',
                           (loc=='Crawfordsville' & plot %in% c(1585064:1585100, 1585801:1585933) & is.na(field)) | 
                             (loc=='Crawfordsville' & row>40 & is.na(field) & population=='Hybrid') ~ 'B',
                           .default = field)) %>%
  unite('notes', c(notes, notes.yield), na.rm = TRUE, sep = ';', remove = TRUE) %>%
  mutate(notes = case_when(str_detect(genotype, 'SOLAR') & is.na(notes) ~ 'Solar panel',
                           str_detect(genotype, 'SOLAR') & !is.na(notes) ~ str_c(notes, 'Solar panel', sep = ';'),
                           .default = notes),
         genotype = genotype.yield) %>%
  select(!ends_with('.yield'))
hips_v2.5 <- hips_v2.5 %>%
  mutate(histFT = as.double(histFT),
         histPlantHt = as.double(histPlantHt))

hips_v3 <- bind_rows(hips_v2.5, ac.all.df)
# Remove empty rows
hips_v3 <- hips_v3 %>%
  filter(!is.na(loc)|!is.na(plot)|!is.na(row)|!is.na(range))
# Drop collector columns
hips_v3 <- hips_v3 %>%
  select(!ends_with('Collector'))
# Drop pctMoistureEarPhenotyping - only present for part of UNL ear phenotyping & not used to calculate other values
hips_v3 <- hips_v3 %>%
  select(!pctMoistureEarPhenotyping)
# Change -Inf to NA
hips_v3 <- hips_v3 %>%
  ungroup() %>%
  mutate(across(where(is.double) & !where(is.POSIXct), ~na_if(., -Inf))) %>%
  mutate(across(where(is.POSIXct), ~na_if(., as.POSIXct(-Inf))))
# Remove plot where the qr is a box number that has no observations
hips_v3 <- filter(hips_v3, qr!='C-2351-006')

meanNIRMoisturePCT <- mean(hips_v3$pctMoistureNIR, na.rm = TRUE)

hips_v3 <- hips_v3 %>%
  rowwise() %>%
  mutate(kernelMass = case_when(str_detect(notes, 'spill') ~ earWt - shelledCobWt, .default = kernelMass),
         moistureCorrectedKernelMass = case_when(loc %in% c('Ames', 'Crawfordsville') & !is.na(kernelMass) ~ kernelMass/(1 - meanNIRMoisturePCT),
                                                      .default = moistureCorrectedKernelMass),
         moistureCorrectedHundredKernelWt = case_when(loc %in% c('Ames', 'Crawfordsville') & !is.na(moistureCorrectedKernelMass) & !is.na(kernelsPerEar)
                                                      ~ moistureCorrectedKernelMass/kernelsPerEar*100,
                                                      .default = moistureCorrectedHundredKernelWt),
         hundredKernelWt = case_when(loc %in% c('Ames', 'Crawfordsville') & !is.na(kernelMass) & !is.na(kernelsPerEar) ~ kernelMass/kernelsPerEar*100),
         earWidth = case_when(loc=='Ames' & plot==1585236 ~ mean(4.292, 4.328, 4.461), .default = earWidth))
# Export v3, there will still be some data cleaning to do here
write.table(hips_v3, file = 'outData/HIPS_2022_V3.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

# Sweetcorn: average kernel mass is off (also, calculate this for other locs & calc hundredKernelWt --> add var T/F for sweetcorn (56 plots between Ames & Crawfordsville)
# --- we did not note sweetcorn at UNL, but fairly heritable, so we need to calc heritability for this and decide if we list TRUE for those lines in other locs
###  broad sense heritability of sweetcorn = 0.54
# -- these should be moisture adjusted but we have no moisture data for Ames and Crawfordsville? --> if small range of moistures seen at UNL, adjust to the mean
## ---- min = 0.02, max = 0.12, mean = 0.055, sd = 0.018, median = 0.05--> still need to plot histogram --- it was fairly normal
# Cob broke - cob len is off? --> check pct data affected: 38 plots; these all look reasonable
# Seed spilled - kernel mass is off; use diff between earWt and cobWt instead? --> check pct of data within locs: 44 plots between Ames and Crawfordsville
# Seed missing on both sides - ear width is off? --> check pct of data affected: 103 plots between Crawfordsville & Ames, maybe this measurement isn't off: this is how it is 