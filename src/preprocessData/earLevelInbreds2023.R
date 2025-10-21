library(tidyverse)
library(readxl)
source('src/Functions.R')

krn <- read_excel('rawData/2023/inbreds/2_KRN_trait_Inbred_2023_Compiled.xlsx',
                  sheet = 'data',
                  col_types = c('skip', 'text', 'numeric', rep('text', 4), 'skip'),
                  col_names = c('sampleID', 'kernelRowNumber', 'wonky', 'smoothCob', 'sweetcorn',
                                'multipleCobsConnected'),
                  skip = 4) %>% 
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID) %>% 
           str_remove(' OP'), 
         smoothCob = case_when(!is.na(smoothCob) ~ 'Smooth cob/ovule issue'), 
         sweetcorn = 'Sweetcorn', 
         multipleCobsConnected = case_when(!is.na(multipleCobsConnected) ~ 'Multiple cobs connected')) %>%
  filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
  mutate(qrCode = str_split(sampleID, '-', simplify = TRUE)[1:3] %>% 
           str_flatten(collapse = '-'))  %>%
  distinct(sampleID, kernelRowNumber, .keep_all = TRUE)

earFileList <- list.files(path ='./rawData/2023/inbreds/', pattern = glob2rx('3_Ear*'), full.names = TRUE)

ears <- tibble()
for(file in earFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 3), 'skip', 'text', 'text', 'skip'),
                   col_names = c('sampleID', 'earLength', 'earWidth', 'earMass', 'string', 'seedMissing'), 
                   skip = 1) %>%
    filter(!str_detect(sampleID, 'COB')) %>% 
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID) %>% 
             str_remove(' OP') %>% 
             str_replace('--', '-'),
           string = case_when(!is.na(string) ~ 'Severe bend in ear. String used to measure ear length'), 
           seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on both sides at widest point of ear')) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split(sampleID, '-', simplify = TRUE)[1:3] %>% 
             str_flatten(collapse = '-'))
  ears <- bind_rows(ears, df)
}

cobFileList <- list.files(path ='./rawData/2023/inbreds/', pattern = glob2rx('5_cob*'), full.names = TRUE)
cobs <- tibble()

for(file in cobFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 3), 'skip', 'text', 'text', 'skip', 'skip'),
                   col_names = c('sampleID', 'cobLength', 'cobWidth', 'cobMass', 'stringCob', 'cobBroke'), 
                   skip = 1) %>%
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID) %>% 
             str_remove('COB') %>% 
             str_squish() %>% 
             str_replace('-25-', '125') %>% 
             str_replace('-22-', '-22'),
           stringCob = case_when(!is.na(stringCob) ~ 'Severe bend in ear. String used to measure cob length'), 
           cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces during shelling')) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split(sampleID, '-', simplify = TRUE)[1:3] %>% 
             str_flatten(collapse = '-')) 
  cobs <- bind_rows(cobs, df)
}

seedFileList <- list.files(path ='./rawData/2023/inbreds/', pattern = glob2rx('6_Seed*'), full.names = TRUE)
seeds <- tibble()

for(file in seedFileList)
{
  df <- read_excel(file, 
                   col_types = c('skip', 'text', rep('numeric', 2), 'skip', 'text', 'skip', 'skip', 'skip'),
                   col_names = c('sampleID', 'kernelsPerEar', 'kernelMassPerEar', 'seedSpilled'), 
                   skip = 4) %>%
    rowwise() %>% 
    mutate(sampleID = str_to_upper(sampleID) %>% 
             str_remove(' OP')) %>%
    filter(str_detect(sampleID, 'A')|str_detect(sampleID, 'C')) %>%
    mutate(qrCode = str_split(sampleID, '-', simplify = TRUE)[1:3] %>% 
             str_flatten(collapse = '-'))
  seeds <- bind_rows(seeds, df)
}

# Drop cob where it is ambiguous which measurement is correct and duplicate entries
cobs <- cobs %>% 
  filter(!str_detect(sampleID, ' OP')) %>% 
  filter(!str_detect(sampleID, '23-A-1730667-24')) %>% 
  distinct(sampleID, qrCode, cobLength, cobWidth, cobMass, .keep_all = TRUE) %>% 
  rowwise() %>% 
  mutate(cobWidth = case_when(cobWidth > 35 ~ NA, .default = cobWidth),
         cobLength = case_when(cobLength > 225 ~ NA, .default = cobLength))
# Drop seed measurements where it is ambiguous which measurement is correct
seeds <- filter(seeds, !(sampleID %in% c('23-A-1737313-22', '23-A-1737699-26'))) %>% 
  rowwise() %>% 
  mutate(kernelsPerEar = case_when(kernelsPerEar > 750 ~ NA, .default = kernelsPerEar))
ears <- ears %>% 
  distinct(sampleID, earLength, earWidth, earMass, .keep_all = TRUE) %>%
  rowwise() %>% 
  mutate(earLength = case_when(!between(earLength, 50, 225) ~ NA, .default = earLength), 
         earWidth = case_when(earWidth > 55 ~ NA, .default = earWidth), 
         earMass = case_when(earMass < 250 ~ NA, .default = earMass))
krn_ears <- full_join(krn, ears, join_by(sampleID, qrCode))
krn_ears_cobs <- full_join(krn_ears, cobs, join_by(sampleID, qrCode)) 
ac <- full_join(krn_ears_cobs, seeds, join_by(sampleID, qrCode)) %>% 
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
ia_eardata <- ac %>% 
  select(!c(earMass, earLength)) %>%
  rename(shelledCobMass = cobMass, 
         shelledCobWidth = cobWidth) %>%
  rowwise() %>% 
  mutate(hundredKernelMass = (kernelMassPerEar/kernelsPerEar)*100) %>%
  unite('notes', wonky, smoothCob, sweetcorn, multipleCobsConnected, string, seedMissing, stringCob, cobBroke,
        seedSpilled, sep = ';', na.rm = TRUE, remove = TRUE)
ac_plotdata <- read_excel('rawData/2023/2023_yield_ICIA_v3.xlsx', 
                          sheet = '2-row plots', 
                          col_names = c('qrCode', 'poundsOfNitrogenPerAcre', 'genotype', 'plantingDate', 'experiment', 'rep', 'row', 'range',
                                        'flagLeafHeight', 'earHeight'), 
                          col_types = c(rep('skip', 5), 'text', 'numeric', 'skip', 'text', 'skip', 'date', 'text', rep('skip', 4), 'numeric', rep('skip', 5),
                                        'numeric', 'numeric', 'skip', 'skip', 'numeric', 'numeric', 'skip'), 
                          skip = 1) %>% 
  mutate(genotype = str_remove(genotype, '@'))

ia_data <- full_join(ia_eardata, ac_plotdata, join_by(qrCode), relationship = 'many-to-one') %>%
  mutate()

responseVars <- c('kernelRowNumber', 'earWidth', 'cobLength', 'shelledCobWidth', 'shelledCobMass', 'kernelsPerEar', 'kernelMassPerEar', 'hundredKernelMass',
                  'flagLeafHeight', 'earHeight')

# ne_eardata <- read_csv('')
