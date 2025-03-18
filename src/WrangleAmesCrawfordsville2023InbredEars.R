library(tidyverse)
library(readxl)
source('src/Functions.R')

inbreds2022 <- read_csv('outData/HIPS_2022_V5.1_INBREDS.csv') # this one is right
min(inbreds2022$kernelMassPerEar, na.rm = TRUE)

krn <- read_excel('data/2023/inbreds/2_KRN_trait_Inbred_2023_Compiled.xlsx',
                  sheet = 'data',
                  col_types = c('skip', 'text', 'numeric', rep('text', 4), 'skip'),
                  col_names = c('sampleID', 'kernelRowNumber', 'wonky', 'smoothCob', 'sweetcorn',
                              'multipleCobsConnected'),
                  skip = 4) %>% 
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID), 
         smoothCob = case_when(!is.na(smoothCob) ~ 'Smooth cob/ovule issue'), 
         sweetcorn = 'Sweetcorn', 
         multipleCobsConnected = case_when(!is.na(multipleCobsConnected) ~ 'Multiple cobs connected')) %>%
  filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
  mutate(qrCode = str_split_i(sampleID, ' ', 1)) %>% 
  dplyr::select(!sampleID) %>%
  distinct(qrCode, kernelRowNumber, .keep_all = TRUE)

earFileList <- list.files(path ='./data/2023/inbreds/', pattern = glob2rx('3_Ear*'), full.names = TRUE)

ears <- tibble()
for(file in earFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 3), 'skip', 'text', 'text', 'skip'),
                   col_names = c('sampleID', 'earLength', 'earWidth', 'earMass', 'string', 'seedMissing'), 
                   skip = 1) %>%
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID),
           string = case_when(!is.na(string) ~ 'Severe bend in ear. String used to measure ear length'), 
           seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on both sides at widest point of ear')) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split_i(sampleID, ' ', 1)) %>% 
    select(!sampleID)
  ears <- bind_rows(ears, df)
}

cobFileList <- list.files(path ='./data/2023/inbreds/', pattern = glob2rx('5_cob*'), full.names = TRUE)
cobs <- tibble()

for(file in cobFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 3), 'skip', 'text', 'text', 'skip', 'skip'),
                   col_names = c('sampleID', 'cobLength', 'cobWidth', 'cobMass', 'stringCob', 'cobBroke'), 
                   skip = 1) %>%
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID),
           stringCob = case_when(!is.na(stringCob) ~ 'Severe bend in ear. String used to measure cob length'), 
           cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces during shelling')) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split_i(sampleID, ' ', 1)) %>% 
    select(!sampleID)
  cobs <- bind_rows(cobs, df)
}

seedFileList <- list.files(path ='./data/2023/inbreds/', pattern = glob2rx('6_Seed*'), full.names = TRUE)
seeds <- tibble()

for(file in seedFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 2), 'skip', 'text', 'skip', 'skip', 'skip'),
                   col_names = c('sampleID', 'kernelsPerEar', 'kernelMassPerEar', 'seedSpilled'), 
                   skip = 4) %>%
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID)) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split_i(sampleID, ' ', 1)) %>% 
    select(!sampleID)
  seeds <- bind_rows(seeds, df)
}

# Drop cob where it is ambiguous which measurement is correct and duplicate entries
cobs <- filter(cobs, qrCode!='23-A-1730667-24') %>% 
  distinct(qrCode, cobLength, cobWidth, cobMass, .keep_all = TRUE) %>% 
  rowwise() %>% 
  mutate(cobWidth = case_when(cobWidth > 35 ~ NA, .default = cobWidth),
         cobLength = case_when(cobLength > 225 ~ NA, .default = cobLength))
# Drop seed measurements where it is ambiguous which measurement is correct
seeds <- filter(seeds, !(qrCode %in% c('23-A-1737313-22', '23-A-1737699-26'))) %>% 
  rowwise() %>% 
  mutate(kernelsPerEar = case_when(kernelsPerEar > 750 ~ NA, .default = kernelsPerEar))
ears <- ears %>% 
  distinct(qrCode, earLength, earWidth, earMass, .keep_all = TRUE) %>%
  rowwise() %>% 
  mutate(earLength = case_when(!between(earLength, 50, 225) ~ NA, .default = earLength), 
         earWidth = case_when(earWidth > 55 ~ NA, .default = earWidth), 
         earMass = case_when(earMass < 250 ~ NA, .default = earMass))

ear_level_df <- full_join(krn, ears) %>%
  full_join(cobs) %>%  
  full_join(seeds) %>% 
  rowwise() %>% 
  mutate(kernelMassPerEar = case_when(!is.na(seedSpilled)|is.na(kernelMassPerEar) ~ earMass - cobMass, 
                                      kernelMassPerEar > 200 ~ NA,
                                      .default = kernelMassPerEar))

phenotypes <- c('earLength', 'earWidth', 'earMass', 'kernelRowNumber', 'cobLength', 'cobWidth', 'kernelsPerEar',
                'kernelMassPerEar')
for(p in phenotypes)
{
  printHistogram(ear_level_df, p, title = p)
}
# Address additional notes cols from krn in this call
ia_eardata <- ear_level_df %>% 
  select(!c(earMass, earLength)) %>%
  rename(shelledCobMass = cobMass, 
         shelledCobWidth = cobWidth) %>%
  rowwise() %>% 
  mutate(qrCode = str_c(str_split_i(qrCode, fixed('-'), 1), 
                        str_split_i(qrCode, fixed('-'), 2), 
                        str_split_i(qrCode, fixed('-'), 3), 
                        sep = '-'), 
         hundredKernelMass = (kernelMassPerEar/kernelsPerEar)*100) %>%
  group_by(qrCode) %>%
  summarise(kernelRowNumber = mean(kernelRowNumber, na.rm = TRUE), 
            earLength = mean(cobLength, na.rm = TRUE)/10, 
            earWidth = mean(earWidth, na.rm = TRUE)/10,
            shelledCobWidth = mean(shelledCobWidth, na.rm = TRUE)/10, 
            shelledCobMass = mean(shelledCobMass, na.rm = TRUE), 
            kernelsPerEar = mean(kernelsPerEar, na.rm = TRUE), 
            kernelMassPerEar = mean(kernelMassPerEar, na.rm = TRUE), 
            hundredKernelMass = mean(hundredKernelMass, na.rm = TRUE),
            wonky = max(wonky, na.rm = TRUE),
            smoothCob = max(smoothCob, na.rm = TRUE), 
            sweetcorn = max(sweetcorn, na.rm = TRUE), 
            multipleCobsConnected = max(multipleCobsConnected, na.rm = TRUE),
            string = max(string, na.rm = TRUE),
            seedMissing = max(seedMissing, na.rm = TRUE), 
            stringCob = max(stringCob, na.rm = TRUE), 
            cobBroke = max(cobBroke, na.rm = TRUE), 
            seedSpilled = max(seedSpilled, na.rm = TRUE)) %>% 
  unite('notes', wonky, smoothCob, sweetcorn, multipleCobsConnected, string, seedMissing, stringCob, cobBroke,
        seedSpilled, sep = ';', na.rm = TRUE, remove = TRUE)

inbredsV2 <- full_join(inbredsV1, ia_eardata, join_by(qrCode), suffix = c('', '.ia')) %>% 
  rowwise() %>% 
  mutate(earLength = max(c(earLength, earLength.ia), na.rm = TRUE),
         earWidth = max(c(earWidth, earWidth.ia), na.rm = TRUE), 
         kernelMassPerEar = max(c(kernelMassPerEar, kernelMassPerEar.ia), na.rm = TRUE), 
         kernelRowNumber = max(c(kernelRowNumber, kernelRowNumber.ia), na.rm = TRUE), 
         kernelsPerEar = max(c(kernelsPerEar, kernelsPerEar.ia), na.rm = TRUE), 
         shelledCobMass = max(c(shelledCobMass, shelledCobMass.ia), na.rm = TRUE),
         shelledCobWidth = max(c(shelledCobWidth, shelledCobWidth.ia), na.rm = TRUE), 
         hundredKernelMass = max(c(hundredKernelMass, hundredKernelMass.ia), na.rm = TRUE),
         locationYear = str_c(year, location, sep = ' '), 
         environment = str_c(year, location, nitrogenTreatment, sep = ' ')) %>%
  unite('notes', notes, notes.ia, sep = ';', na.rm = TRUE, remove = TRUE) %>% 
  select(!contains('.ia')) %>%
  mutate(across(is.numeric, ~case_when(.==-Inf | .==Inf ~ NA, .default = .)))

phenotypes <- c('daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval', 'GDDToAnthesis', 'GDDToSilk', 
                'anthesisSilkingIntervalGDD', 'earHeight', 'flagLeafHeight', 'plantDensity', 'earLength',
                'earFillLength', 'earWidth', 'shelledCobWidth', 'kernelsPerRow', 'kernelRowNumber',
                'kernelsPerEar', 'hundredKernelMass', 'kernelMassPerEar', 'shelledCobMass')
amesPhenotypes <- c('earHeight', 'flagLeafHeight', 'earLength', 'earWidth', 'shelledCobWidth',
                    'kernelRowNumber', 'kernelsPerEar', 'hundredKernelMass', 'kernelMassPerEar', 'shelledCobMass')
plotRepCorr2(inbredsV2, phenotypes = phenotypes, facet = 'locationYear')
ames2023 <- filter(inbredsV2, locationYear=='2023 Ames')
cf2023 <- filter(inbredsV2, locationYear=='2023 Crawfordsville')
plotRepCorr2(ames2023, phenotypes = c('kernelRowNumber'), treatmentVar ='row', facet = 'nitrogenTreatment')
plotRepCorr(cf2023, phenotypes = amesPhenotypes, treatmentVar = 'locationYear', facet = 'nitrogenTreatment')

write.csv(ames2023, '../ames2023Inbreds_Jan16.csv', quote = FALSE, row.names = FALSE)
