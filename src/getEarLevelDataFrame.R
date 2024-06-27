library(tidyverse)
library(readxl)

# 2022 hybrid ears phenotyped at UNL
hybridEars22UNL <- read.csv('data/earphenotypesformatted.csv')
colnames(hybridEars22UNL) <- c('collector', 'qrCode', 'earNumber', 'kernelColor', 'earWidth', 'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'kernelsPerEar', 'shelledCobWidth', 'earLength', 'shelledCobMass', 'hundredKernelMass', 'percentMoisture')
hybridEars22UNL2 <- read.csv('data/earphenotypesformatted_part2.csv')
colnames(hybridEars22UNL2) <- c('collector', 'qrCode', 'earNumber', 'kernelColor', 'earWidth', 'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'kernelsPerEar', 'shelledCobWidth', 'earLength', 'shelledCobMass', 'hundredKernelMass', 'percentMoisture')
hybridEars22UNL2 <- hybridEars22UNL2 %>%
  mutate(earNumber = as.integer(earNumber))
hybridEars22UNL <- bind_rows(hybridEars22UNL, hybridEars22UNL2) %>%
  mutate(qrCode = str_to_upper(qrCode)) %>%
  select(!percentMoisture)

sb.index <- read_excel('data/Scottsbluff Hybrid HIPS - Summary.xlsx', sheet = 'Index (Original)')
sb.index <- sb.index[, c(1, 5, 7)]
colnames(sb.index) <- c('plotNumber', 'genotype', 'seedFillNote')
sb.index <- sb.index %>%
  group_by(plotNumber, genotype) %>%
  summarise(seedFillNote = max(seedFillNote, na.rm = TRUE)) %>%
  rowwise() %>% 
  mutate(genotype = str_to_upper(genotype))

parseNorthPlatte2022QR <- function(data)
{
  df <- data %>%
    rowwise() %>%
    mutate(irrigationProvided = case_when(str_detect(qrCode, 'FULL IRRIGATION') ~ 8.6,
                                          str_detect(qrCode, 'PARTIAL IRRIGATION') ~ 4.3,
                                          str_detect(qrCode, 'NO IRRIGATION') ~ 0),
           nitrogenTreatment = str_split_i(qrCode, fixed('$'), 2) %>%
             str_split_i(fixed(' - '), 3) %>%
             str_remove(' NITROGEN') %>%
             str_to_sentence(), 
           rep = str_split_i(qrCode, fixed('$'), 3) %>%
             str_remove('REP') %>%
             as.integer(),
           plotNumber = str_split_i(qrCode, fixed('$'), 4) %>%
             str_remove('PLOT') %>%
             as.integer(),
           row = str_split_i(qrCode, fixed('$'), 5) %>%
             str_remove('ROW') %>%
             as.integer(),
           range = str_split_i(qrCode, fixed('$'), 6) %>%
             str_remove('RANGE') %>%
             as.integer(), 
           genotype = str_split_i(qrCode, fixed('$'), 7)) %>%
    mutate(location = case_when(irrigationProvided==8.6 ~ 'North Platte1',
                                irrigationProvided==4.3 ~ 'North Platte2',
                                irrigationProvided==0 ~ 'North Platte3'),
           poundsOfNitrogenPerAcre = case_when(nitrogenTreatment=='High' ~ 225, 
                                               nitrogenTreatment=='Medium' ~ 150,
                                               nitrogenTreatment=='Low' ~ 75),
           block = case_when(nitrogenTreatment=='Medium' ~ rep + 2,
                             nitrogenTreatment=='High' ~ rep + 4,
                             .default = rep)) %>%
    mutate(sublocation = location) %>%
    select(!rep)
  return(df)
}

parseLincoln2022HybridQR <- function(data)
{
  df <- data %>%
    rowwise() %>%
    mutate(plotNumber = str_split_i(qrCode, fixed('$'), 1) %>%
             as.integer(),
           row = str_split_i(qrCode, fixed('$'), 2) %>%
             str_remove('ROW') %>% 
             as.integer(),
           range = str_split_i(qrCode, fixed('$'), 3) %>%
             str_remove('RANGE') %>%
             as.integer(),
           genotype = str_split_i(qrCode, fixed('$'), 4), 
           location = 'Lincoln',
           sublocation = 'Lincoln',
           irrigationProvided = 0) %>%
    mutate(nitrogenTreatment = case_when(plotNumber < 5000 ~ 'Medium', 
                                         plotNumber %in% 5000:6000 ~ 'Low',
                                         plotNumber > 6000 ~ 'High'), 
           poundsOfNitrogenPerAcre = case_when(plotNumber < 5000 ~ 150, 
                                               plotNumber %in% 5000:6000 ~ 75,
                                               plotNumber > 6000 ~ 225),
           block = case_when(plotNumber %in% 5100:5199 ~ 1,
                             plotNumber %in% 5200:5299 ~ 2,
                             plotNumber %in% 4100:4199 ~ 3,
                             plotNumber %in% 4200:4299 ~ 4,
                             plotNumber %in% 6100:6199 ~ 5, 
                             plotNumber %in% 6200:6299 ~ 6))
  return(df)
}

parseScottsbluffHybridQR <- function(data)
{
  df <- data
  data_parsed <- df %>%
    filter(str_detect(qrCode, 'HYBRID')) %>%
    filter(!str_detect(qrCode, 'FILL')) %>%
    rowwise() %>%
    mutate(plotNumber = str_split_i(qrCode, '[$]', 4) %>%
             str_remove('PLOT') %>%
             as.numeric(),
           location = 'Scottsbluff',
           sublocation = 'Scottsbluff', 
           nitrogenTreatment = case_when(str_detect(qrCode, 'HIGH') ~ 'Low',
                            str_detect(qrCode, 'MEDIUM') ~ 'Medium',
                            str_detect(qrCode, 'LOW') ~ 'High'),
           irrigationProvided = 16.9,
           rep = str_split_i(qrCode, '[$]', 3) %>%
             str_remove('REP') %>%
             as.numeric(),
           range = str_split_i(qrCode, '[$]', 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qrCode, '[$]', 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qrCode, '[$]', 7)) %>%
    mutate(plotNumber = case_when( !c(plotNumber %in% c(1021:1025)) & row==26 ~ plotNumber + 490,
                             row==25 ~ plotNumber + 440,
                             row==24 ~ plotNumber + 390,
                             row==23 ~ plotNumber + 340,
                             row==22 ~ plotNumber + 290,
                             row==21 ~ plotNumber + 240,
                             row==20 ~ plotNumber + 190,
                             !c(plotNumber %in% c(1191:1195)) & row==17 ~ plotNumber + 150,
                             row==16 ~ plotNumber + 100,
                             row==15 ~ plotNumber + 50,
                             row==14 ~ plotNumber,
                             row==13 ~ plotNumber - 50,
                             row==12 ~ plotNumber - 100,
                             row==11 ~ plotNumber - 150,
                             !c(plotNumber %in% c(1361:1365)) & row==7 ~ plotNumber - 190,
                             row==6 ~ plotNumber - 240,
                             row==5 ~ plotNumber - 290,
                             row==4 ~ plotNumber - 340,
                             row==3 ~ plotNumber - 390,
                             row==2 ~ plotNumber - 440,
                             row==1 ~ plotNumber - 490,
                            .default = plotNumber)) %>%
    mutate(plotNumber = case_when(row==1 & range==23 ~ 1021,
                             row==1 & range==24 ~ 1022,
                             row==1 & range==25 ~ 1023,
                             row==1 & range==26 ~ 1024,
                             row==1 & range==27 ~ 1025,
                             row==20 & range==23 ~ 1361,
                             row==20 & range==24 ~ 1362,
                             row==20 & range==25 ~ 1363, 
                             row==20 & range==26 ~ 1364,
                             row==20 & range==27 ~ 1365,
                             row==11 & range==23 ~ 1191,
                             row==11 & range==24 ~ 1192,
                             row==11 & range==25 ~ 1193,
                             row==11 & range==26 ~ 1194,
                             row==11 & range==27 ~ 1195,
                             row==26 & range %in% 23:27 ~ NA,
                             row==17 & range %in% 23:27 ~ NA,
                             row==7 & range %in% 23:27 ~ NA,
                            .default = plotNumber)) %>%
    mutate(genotype = case_when(row %in% c(26, 17, 7) & range %in% 23:27 ~ NA,
                                .default = sb.index$genotype[match(plotNumber, sb.index$plotNumber)]),
           nitrogenTreatment = case_when(plotNumber %in% 1021:1025 ~ 'Low',
                            plotNumber %in% 1191:1195 ~ 'Medium',
                            plotNumber %in% 1361:1365 ~ 'High',
                            .default = nitrogenTreatment)) %>%
    filter(!is.na(genotype) & !is.na(plotNumber)) %>%
    mutate(block = case_when(nitrogenTreatment=='Medium' ~ rep + 2,
                             nitrogenTreatment=='High' ~ rep + 4,
                             .default = rep),
           poundsOfNitrogenPerAcre = case_when(nitrogenTreatment=='Low' ~ 75,
                                               nitrogenTreatment=='Medium' ~ 150,
                                               nitrogenTreatment=='High' ~ 225)) %>%
    select(!rep)
  
  return(data_parsed)
}

parseMissouriValleyHybrid2022QR <- function(data)
{
  df <- data
  data_parsed <- df %>%
    filter(str_detect(qrCode, 'HYBRID')) %>%
    rowwise() %>%
    mutate(plotNumber = str_split_i(qrCode, '[$]', 4) %>%
             str_remove('PLOT') %>%
             as.numeric(), 
           location = 'Missouri Valley',
           sublocation = 'Missouri Valley',
           block = str_split_i(qrCode, '[$]', 3) %>%
             str_remove('REP') %>%
             as.numeric(),
           range = str_split_i(qrCode, '[$]', 6) %>%
             str_remove('RANGE') %>%
             as.numeric(),
           row = str_split_i(qrCode, '[$]', 5) %>%
             str_remove('ROW') %>%
             as.numeric(),
           genotype = str_split_i(qrCode, '[$]', 7) %>%
             str_to_upper(),
           irrigationProvided = 0, 
           nitrogenTreatment = 'Medium',
           poundsOfNitrogenPerAcre = 175) %>%
    mutate(block = case_when(block==1 ~ 2,
                           block==2 ~ 1),
           plotNumber = case_when(block==1 ~ (plotNumber + 100),
                            block==2 ~ (plotNumber + 200)))
  return(data_parsed)
}

sb22 <- hybridEars22UNL %>%
  filter(str_detect(qrCode, 'SCOTTSBLUFF')) %>%
  parseScottsbluffHybridQR()

lnk22 <- hybridEars22UNL %>%
  filter(!(str_detect(qrCode, 'SCOTTSBLUFF')|str_detect(qrCode, 'NORTH PLATTE')|str_detect(qrCode, 'MV'))) %>%
  parseLincoln2022HybridQR()

np22 <- hybridEars22UNL %>%
  filter(str_detect(qrCode, 'NORTH PLATTE')) %>%
  parseNorthPlatte2022QR()

mv22 <- hybridEars22UNL %>%
  filter(str_detect(qrCode, 'MV')) %>%
  parseMissouriValleyHybrid2022QR()

hybridEars22UNL <- bind_rows(sb22, np22, lnk22, mv22) %>%
  mutate(across(c(earWidth, earFillLength, kernelRowNumber, kernelsPerRow, earMass, kernelsPerEar, shelledCobWidth, earLength, shelledCobMass, 
                  hundredKernelMass), as.numeric)) %>%
  rowwise() %>%
  mutate(kernelMassPerEar = earMass - shelledCobMass)

# 2022 hybrid ears phenotyped at ISU
acKRN22 <- read_excel('data/2_KRN_trait_Hybrid_2022_Compiled.xlsx', 
                      skip = 4,
                      col_types = c('skip', 'text', 'numeric', 'text', 'text', 'skip'),
                      col_names = c('qrCode', 'kernelRowNumber', 'smoothCob', 'sweetcorn')) %>%
  rowwise() %>%
  mutate(qrCode = str_split_i(qrCode, ' ', 1) %>%
           str_to_upper(), 
         smoothCob = case_when(!is.na(smoothCob) ~ 'smooth cob/ovule issue'),
         sweetcorn = case_when(!is.na(sweetcorn) ~ 'sweetcorn'))

acEar22 <- read_excel('data/3_ear_trait_HYBRID_2022_compiled.xlsx',
                      sheet = 'compiled data set',
                      range = 'B2:H4188', 
                      col_types = c('text', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'text'), 
                      col_names = c('qrCode', 'earWidth', 'earMass', 'seedMissing')) %>%
  rowwise() %>%
  mutate(qrCode = str_split_i(qrCode, ' ', 1) %>%
           str_to_upper(),
         earWidth = 0.1*earWidth,
         seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on both sides of widest part of ear'))

acCob22 <- read_excel('data/5_cob_trait_HYBRID_2022_compiled.xlsx',
                      sheet = 'compiled data sets',
                      range = 'B2:H3075', 
                      col_types = c('text', 'numeric', 'numeric', 'numeric', 'skip', 'text', 'text'),
                      col_names = c('qrCode', 'earLength', 'shelledCobWidth', 'shelledCobMass', 'cobBroke', 'string')) %>%
  rowwise() %>%
  mutate(qrCode = str_split_i(qrCode, ' ', 1) %>%
           str_to_upper(),
         earLength = 0.1*earLength, 
         shelledCobWidth = 0.1*shelledCobWidth,
         cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces'), 
         string = case_when(!is.na(string) ~ 'Severe bend in cob - string used to measure length'))

acSeed22 <- read_excel('data/6_Seed_trait_HYBRID_2022_Compiled.xlsx', 
                       sheet = 'COMPILED',
                       range = 'A5:E3157', 
                       col_types = c('text', 'numeric', 'numeric', 'skip', 'text'),
                       col_names = c('qrCode', 'kernelsPerEar', 'kernelMassPerEar', 'notes')) %>%
  rowwise() %>%
  mutate(qrCode = str_split_i(qrCode, ' ', 1) %>%
           str_to_upper())

ac22 <- full_join(acKRN22, acEar22, join_by(qrCode), keep = FALSE, suffix = c('', '')) %>%
  full_join(acCob22, join_by(qrCode), keep = FALSE, suffix = c('', '')) %>%
  full_join(acSeed22, join_by(qrCode), keep = FALSE, suffix = c('', '')) %>%
  rowwise() %>%
  mutate(location = case_when(str_detect(qrCode, 'A') ~ 'Ames', 
                              str_detect(qrCode, 'C') ~ 'Crawfordsville'),
         plotNumber = str_split_i(qrCode, fixed('-'), 3),
         earNumber = str_split_i(qrCode, fixed('-'), 4), 
         kernelMassPerEar = case_when(str_detect(notes, 'shelling') ~ earMass - shelledCobMass, 
                                      .default = kernelMassPerEar)) %>%
  mutate(hundredKernelMass = (kernelMassPerEar/kernelsPerEar)*100) %>%
  unite('notes', smoothCob, sweetcorn, seedMissing, cobBroke, string, notes, sep = ';', remove = TRUE, na.rm = TRUE)

ac22Index <- read_excel('data/YTMC_ Lisa_Plot_Coordinates_v4.xlsx', 
                        sheet = 'RawData (4-Row)', 
                        range = 'E2:R1227', 
                        col_types = c('text', 'skip', 'skip', 'skip', 'text', 'skip', 'text', 'text', 'skip', 'skip', 'skip', 'numeric', 'numeric', 'numeric'), 
                        col_names = c('experiment', 'qrCode', 'genotype', 'genotypeNote', 'row', 'range', 'rep')) %>%
  filter(experiment!='LC_4211') %>%
  rowwise() %>%
  mutate(qrCode = str_to_upper(qrCode), 
         genotype = str_to_upper(genotype), 
         genotypeNote = str_to_upper(genotypeNote), 
         sublocation = case_when(experiment=='LC_4233' ~ 'Ames E1',
                                 experiment=='LC_4353' ~ 'Crawfordsville B',
                                 experiment %in% c('LC_4351', 'LC_4352') ~ 'Crawfordsville A',
                                 experiment %in% c('LC_4231', 'LC_4232') ~ 'Ames B1'), 
         nitrogenTreatment = case_when(experiment %in% c('LC_4233', 'LC_4352') ~ 'Low',
                                       experiment %in% c('LC_4351', 'LC_4231') ~ 'High',
                                       experiment %in% c('LC _4353', 'LC_4232') ~ 'Medium'), 
         irrigationProvided = 0,
         poundsOfNitrogenPerAcre = case_when(experiment=='LC_4231' ~ 250,
                                             experiment=='LC_4351' ~ 225,
                                             experiment %in% c('LC_4232', 'LC_4353') ~ 150, 
                                             experiment %in% c('LC_4233', 'LC_4352') ~ 75)) %>%
  mutate(genotype = case_when(str_detect(genotype, 'SOLAR') ~ 'SOLAR', 
                       is.na(genotype) & !is.na(genotypeNote) ~ genotypeNote, 
                       .default = genotype)) %>%
  select(!genotypeNote)
ac22 <- left_join(ac22, ac22Index, join_by(qrCode), keep = FALSE, suffix = c('', ''), relationship = 'many-to-one') %>%
  mutate(across(c(plotNumber, earNumber), as.numeric))

hybrid22Ears <- bind_rows(ac22, hybridEars22UNL) %>%
  rowwise() %>%
  mutate(genotype = case_when(genotype %in% c('', 'BORDER') ~ NA,
                              genotype %in% c('2369 X PHZ52', '2369 X PHZ53', '2369 X PHZ54') ~ '2369 X PHZ51', 
                              genotype=='4N506 X 3IIH!6' ~ '4N506 X 3IIH6', 
                              genotype=='COMMERCIAL HYBRID 1' ~ 'PIONEER 1311 AMXT',
                              genotype=='COMMERCIAL HYBRID 2' ~ 'PIONEER P0589 AMXT', 
                              genotype=='COMMERCIAL HYBRID 3' ~ 'HOEGEMEYER 8065RR',
                              genotype=='COMMERCIAL HYBRID 4' ~ 'HOEGEMEYER 7089 AMXT',
                              genotype=='COMMERCIAL HYBRID 5' ~ 'SYNGENTA NK0760-3111', 
                              genotype %in% c('PHB47 X 311H7', 'PHB47 X 3IIH8', 'PHB47 X 3IIH9') ~ 'PHB47 X 3IIH6',
                              .default = genotype))
unique(hybrid22Ears$genotype) %>% sort()
