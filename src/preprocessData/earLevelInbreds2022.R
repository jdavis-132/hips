library(tidyverse)
library(readxl)
library(cowplot)
library(lubridate)
library(nasapower)
source('src/Functions.R')
# source('src/preprocessData/WrangleWeatherData.R')

ears <- read.csv('rawData/2022/2022 Inbred HIPS Ear Data - Turkus Curated 231117.csv')

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

ears <- ears %>%
  mutate(kernelMassPerEar = case_when(kernelMassPerEar > 200 ~ NA, .default = kernelMassPerEar),
         hundredKernelMass = case_when(hundredKernelMass > 150 ~ NA, .default = hundredKernelMass))

ia_inb <- read_excel("rawData/2022/YTMC_ Lisa_Plot_Coordinates_v4.xlsx", 
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

inbreds <- full_join(ears, ia_inb, join_by(location, row, range), keep = FALSE, suffix = c('', '.ia'), relationship = 'many-to-one')
inbreds <- inbreds %>%
  rowwise() %>%
  mutate(plotNumber = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ plotNumber,
                                location %in% c('Ames', 'Crawfordsville') ~ str_split_i(qrCode.ia, '-', 3) %>%
                                  as.numeric()),
         qrCode = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ qrCode, 
                            location %in% c('Ames', 'Crawfordsville') ~ qrCode.ia), 
         genotype = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ genotype, 
                              location %in% c('Ames', 'Crawfordsville') ~ genotype.ia),
         block = case_when(location %in% c('Scottsbluff', 'Lincoln', 'Missouri Valley') ~ block,
                           (sublocation %in% c('Ames B1', 'Crawfordsville B') & nitrogenTreatment=='Medium' & block.ia==1)|
                             (sublocation %in% c('Ames E1', 'Crawfordsville A') & nitrogenTreatment=='Low' & block.ia==1) ~ 1,
                           (sublocation %in% c('Ames B1', 'Crawfordsville B') & nitrogenTreatment=='Medium' & block.ia==2)|
                             (sublocation %in% c('Ames E1', 'Crawfordsville A') & nitrogenTreatment=='Low' & block.ia==2) ~ 2,
                           (sublocation %in% c('Ames B1', 'Crawfordsville A') & nitrogenTreatment=='High' & block.ia==1) ~ 3,
                           (sublocation %in% c('Ames B1', 'Crawfordsville A') & nitrogenTreatment=='High' & block.ia==2) ~ 4)) %>%
  unite('notes', c(notes, notes.ia), sep = ';', na.rm = TRUE) %>%
  select(location, plotNumber, qrCode, row, range, genotype, block, numberPrimaryEars, numberSecondaryEars, looseKernels, 
         looseKernelMass, secondaryEarKernels, kernelColor, kernelStriping, earWidth, kernelFillLength, kernelRowNumber, kernelsPerRow,
         kernelsPerEar, shelledCobWidth, shelledCobLength, shelledCobMass, hundredKernelMass, kernelMassPerEar, notes, experiment,
         plantDensity, plantingDate, sublocation, irrigationProvided, nitrogenTreatment, poundsOfNitrogenPerAcre, totalStandCount)

# Missouri Valley height data
mv.plantData.inb <- read_excel('rawData/2022/Plant_data_MO_Valley_2022.xlsx',
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

inbreds <- full_join(inbreds, mv.plantData.inb, join_by(location, row, range), keep = FALSE, suffix = c('', '.mv'), relationship = 'many-to-one')
inbreds <- inbreds %>%
  mutate(genotype = case_when(location=='Missouri Valley' ~ genotype.mv,
                              .default = genotype)) %>%
  select(!ends_with('.mv'))

ac.krn <- read_excel('rawData/2022/2_KRN_trait_inbred_2022_Compiled_v3.xlsx', 
                     sheet = 'compiled data',
                     skip = 4,
                     col_types = c('skip', 'text', 'skip', 'skip', 'numeric', 'text', 'text', 'text', 'skip'),
                     col_names = c('sampleID', 'kernelRowNumber', 'notes', 'smoothCob', 'sweetcorn')) %>%
  rowwise() %>%
  mutate(qrCode = str_c(str_split_i(sampleID, '-', 1), str_split_i(sampleID, '-', 2), str_split_i(sampleID, '-', 3), sep = '-') %>%
           str_to_upper(),
         sampleID = str_remove(sampleID, ' OP'),
         smoothCob = case_when(!is.na(smoothCob) ~ 'Smooth cob/ovule issue'),
         sweetcorn = case_when(!is.na(sweetcorn) ~ 'Sweetcorn')) %>%
  unite('notes', notes, smoothCob, sweetcorn, sep = ';', remove = TRUE, na.rm = TRUE) %>% 
  filter(!(sampleID %in% c('22-A-1593312-26'))) %>% 
  add_case(sampleID = '22-A-1593312-26', kernelRowNumber = 16, qrCode = '22-A-1593312')

ac.ears <- read_excel('rawData/2022/3_ear_trait_INBRED_2022_compiled.xlsx', 
                      sheet = 'compiled data set',
                      skip = 1,
                      col_types = c('skip', 'text', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'text', 'skip'),
                      col_names = c('sampleID', 'earWidth', 'earMass', 'seedMissing')) %>%
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID) %>% 
           str_remove(' OP'),
         seedMissing = case_when(!is.na(seedMissing) ~ 'Seed missing on either side of ear at widest point')) %>% 
  filter(!(sampleID %in% c('22-C-1595260-21', '22-C-1595381-21', '22-C-1595282-21', '22-C-1595356-26', '22-C-1595867-22'))) # two measurements, unclear which is correct

ac.cobs1 <- read_excel('rawData/2022/5_cob_trait_INBRED_2022_compiled_v2.xlsx',
                       sheet = 'data',
                       skip = 1,
                       col_types = c('skip', 'text', 'numeric', 'numeric', 'numeric', 'skip', 'text', 'text', 'skip', 'skip', 'skip'),
                       col_names = c('sampleID', 'earLength', 'shelledCobWidth', 'shelledCobMass', 'cobBroke', 'string')) %>%
  rowwise() %>%
  mutate(cobBroke = case_when(!is.na(cobBroke) ~ 'Cob broke in pieces'),
         string = case_when(!is.na(string) ~ 'Severe bend in cob: String used to measure length'), 
         sampleID = str_to_upper(sampleID) %>% 
           str_remove(' OP'))

ac.cobs2 <- read_excel('rawData/2022/5_cob_Traits_missing_entries_2022.xlsx',
                       sheet = 1,
                       skip = 1, 
                       col_types = c('skip', 'skip', 'skip', 'text', 'numeric', 'numeric', 'numeric', 'skip'),
                       col_names = c('sampleID', 'earLength', 'shelledCobWidth', 'shelledCobMass')) %>%
  rowwise() %>%
  mutate(sampleID = str_to_upper(sampleID) %>% 
           str_remove(' OP'))

ac.cobs <- bind_rows(ac.cobs1, ac.cobs2) %>%
  rowwise() %>%
  mutate(sampleID = str_remove(sampleID, '.') %>%
           str_replace('000022-A', '22-A') %>%
           str_replace('022-A', '22-A') %>%
           str_replace('0022-A', '22-A') %>%
           str_replace('r22-C', '22-C')) %>%
  mutate(sampleID = case_when(str_split_i(sampleID, '-', 1)=='2' ~ str_replace(sampleID, '2-', '22-'), 
                              .default = sampleID)) %>% 
  filter(!(sampleID %in% c('22-C-1595255-23', '22-C-1595218-22', '22-C-1595134-21'))) # two measurements; unclear which is correct

ac.seed <- read_excel('rawData/2022/6_Seed_Trait_INBRED_2022_Compiled_v2.xlsx',
                      sheet = 'compiled data',
                      skip = 4,
                      col_types = c('text', 'skip', 'skip', 'numeric', 'numeric', 'skip', 'text', rep('skip', 5)),
                      col_names = c('sampleID', 'kernelsPerEar', 'kernelMassPerEar', 'notes')) %>%
  rowwise() %>%
  mutate(seedSpilled = case_when(str_detect(notes, 'shelling')|str_detect(notes, 'weighing') ~ TRUE, .default = FALSE), 
         sampleID = str_to_upper(sampleID) %>% 
           str_remove(' OP')) %>%
  rowwise() %>%
  mutate(sampleID = str_remove(sampleID, '.') %>%
           str_replace('000022-A', '22-A') %>%
           str_replace('022-A', '22-A') %>%
           str_replace('0022-A', '22-A') %>%
           str_replace('r22-C', '22-C')) %>%
  mutate(sampleID = case_when(str_split_i(sampleID, '-', 1)=='2' ~ str_replace(sampleID, '2-', '22-'), 
                              .default = sampleID))

ac.seed.cobs <- full_join(ac.seed, ac.cobs, join_by(sampleID), suffix = c('', ''), keep = FALSE) %>%
  rowwise() %>%
  mutate(sampleID = str_remove(sampleID, '.') %>%
           str_replace('000022-A', '22-A') %>%
           str_replace('022-A', '22-A') %>%
           str_replace('0022-A', '22-A') %>%
           str_replace('r22-C', '22-C')) %>%
  mutate(sampleID = case_when(str_split_i(sampleID, '-', 1)=='2' ~ str_replace(sampleID, '2-', '22-'), 
                              .default = sampleID))

ac.seed.cobs.ears <- full_join(ac.seed.cobs, ac.ears, join_by(sampleID), suffix = c('', ''), keep = FALSE) %>%
  rowwise() %>%
  mutate(kernelMassPerEar = case_when(seedSpilled==1 & !is.na(earMass) & !is.na(shelledCobMass) ~ earMass - shelledCobMass, 
                                      .default = kernelMassPerEar),
         qrCode = str_c(str_split_i(sampleID, '-', 1), str_split_i(sampleID, '-', 2), str_split_i(sampleID, '-', 3), sep = '-')) %>%
  unite('notes', seedMissing, cobBroke, string, notes, sep = ';', remove = TRUE, na.rm = TRUE)

ac <- full_join(ac.seed.cobs.ears, ac.krn, join_by(sampleID), suffix = c('', '.krn'), keep = FALSE) %>%
  unite('notes', notes, notes.krn, sep = ';', remove = TRUE, na.rm = TRUE) %>%
  rowwise() %>%
  mutate(location = case_when(str_detect(qrCode, 'A') ~ 'Ames',
                              str_detect(qrCode, 'C') ~ 'Crawfordsville'))

ac.height <- read.table('rawData/2022/AmesCrawfordsville2022InbredsHeights.tsv', sep = '\t', header = TRUE, row.names = NULL)
ac <- full_join(ac, ac.height, join_by(qrCode, location), suffix = c('', '.ht'), keep = FALSE, relationship = 'many-to-one')

ac <- unite(ac, 'notes', notes, notes.ht, sep = ';', remove = TRUE, na.rm = FALSE)

inbreds2 <- full_join(inbreds, ac, join_by(location, qrCode), keep = FALSE, suffix = c('', '.ac'))

inbreds2 <- inbreds2 %>%
  rowwise() %>%
  mutate(range = max(range, range.ac, na.rm = TRUE) %>%
           as.integer(),
         row = max(row, row.ac, na.rm = TRUE) %>%
           as.integer(), 
         qrCode = max(qrCode, qrCode.krn, na.rm = TRUE),
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
  select(!c(looseKernels, looseKernelMass, rep, field, irrigation, population, qrCode.krn))
inbreds2 <- inbreds2 %>%
  mutate(across(where(is.numeric), ~na_if(., as.integer(-Inf))))

lnkFT <- read_excel('rawData/2022/2022 Digitized Flowering Notes.xlsx', 
                    sheet = 'SAM', 
                    col_types = c(rep('numeric', 3), 'skip', 'date', 'skip', 'date', 'skip', 'text', 'skip'),
                    col_names = c('plotNumber', 'row', 'range', 'anthesisDate', 'silkDate', 'notes')) %>%
  filter(!is.na(plotNumber)) %>%
  mutate(location = 'Lincoln')

inbreds3 <- full_join(inbreds2, lnkFT, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE, relationship = 'many-to-one')
inbreds3 <- inbreds3 %>%
  rowwise() %>%
  unite('notes', notes, notes.lnk, sep = ';', remove = TRUE, na.rm = FALSE) %>%
  select(!ends_with('.lnk'))

lnkStandCt <- read_excel('rawData/2022/2022 SAM Stand Counts - Summary.xlsx', 
                         sheet = 'Comparison',
                         skip = 2,
                         col_types = c('numeric', rep('skip', 7), 'numeric', 'text', rep('skip', 11)),
                         col_names = c('plotNumber', 'totalStandCount', 'notes')) %>%
  mutate(location = 'Lincoln')
inbreds4 <- full_join(inbreds3, lnkStandCt, join_by(location, plotNumber), suffix = c('', '.lnk'), keep = FALSE, relationship = 'many-to-one')
inbreds4 <- inbreds4 %>%
  rowwise() %>%
  mutate(totalStandCount = max(totalStandCount, totalStandCount.lnk, na.rm = TRUE)) %>%
  select(!ends_with('.lnk'))

lnkHt <- read_excel('rawData/2022/230331 SAM Height and Leaf Dimension Data Data - Digitized and Reviewed.xlsx', 
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
  filter(!is.na(genotype))

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
inbreds <- mutate(inbreds, 
                  year = 2022, 
                  locationYear = str_c('2022 ', location), 
                  environment = str_c(locationYear, ' ', nitrogenTreatment))
inbredsWide <- plotRepCorr2(inbreds, phenotypes = responseVars)

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
         earWidth = case_when(location=='Missouri Valley' & plotNumber==215 ~ earWidth*10,
                              .default = earWidth),
         shelledCobWidth = case_when(location=='Missouri Valley' & plotNumber==215 ~ shelledCobWidth*10, 
                                     location=='Lincoln' & plotNumber==1041 & shelledCobWidth > 10 ~ NA,
                                     location=='Lincoln' & plotNumber==2223 & shelledCobWidth > 10 ~ NA,
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

inbreds_final_plotlevel_2022 <- read_csv('finalData/HIPS_INBREDS_V12_1.csv') %>% 
  filter(location=='Lincoln' & year==2022) %>% 
    select(plotNumber, location, year, GDDToAnthesis, GDDToSilk, anthesisSilkingIntervalGDD)

inbreds_plusft <- left_join(inbreds, inbreds_final_plotlevel_2022, join_by(location, year, plotNumber), keep = FALSE)
inbreds <- mutate(inbreds_plusft, hundredKernelMass = case_when(hundredKernelMass <= 0 ~ NA, .default = hundredKernelMass))

responseVars <- c(responseVars, 'daysToAnthesis', 'GDDToAnthesis', 'daysToSilk', 'GDDToSilk', 'anthesisSilkingInterval', 'anthesisSilkingIntervalGDD')

inbredsWide <- plotRepCorr2(inbreds, phenotypes = responseVars)


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
# start here
inbreds <- inbreds %>%
  mutate(hundredKernelMass = case_when(hundredKernelMass > 50 ~ NA, .default = hundredKernelMass))
inbreds <- inbreds %>% 
  mutate(earWidth = case_when(earWidth > 10 ~ NA, .default = earWidth))

inbreds <- inbreds %>%
  mutate(across(where(is.character), ~str_replace_all(., ',', ';')))

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
         earLength = case_when(earLength > 25  ~ NA, 
                               .default = earLength),
         earWidth = case_when(!between(earWidth, 1.25, 6.25) ~ NA, 
                              .default = earWidth),
         flagLeafHeight = case_when((flagLeafHeight > 250) | 
                                      (location=='Ames' & flagLeafHeight < 45) | 
                                      (location=='Lincoln' & flagLeafHeight < 50) | 
                                      (location=='Missouri Valley' & (flagLeafHeight < 50 | flagLeafHeight > 225)) ~ NA,
                                    .default = flagLeafHeight), 
         GDDToSilk = case_when(GDDToSilk > 2200 ~ NA, .default = GDDToSilk),
         hundredKernelMass = case_when(!between(hundredKernelMass, 5, 40) ~ NA, 
                                       .default = hundredKernelMass),
         kernelMassPerEar = case_when(kernelMassPerEar > 155 ~ NA,
                                      .default = kernelMassPerEar),
         kernelRowNumber = case_when(kernelRowNumber < 5 ~ NA,
                                     .default = kernelRowNumber),
         kernelsPerEar = case_when(kernelsPerEar > 650 ~ NA, 
                                   .default = kernelsPerEar),
         kernelsPerRow = case_when(kernelsPerRow > 45 ~ NA, .default = kernelsPerRow),
         shelledCobMass = case_when(shelledCobMass > 40  ~ NA,
                                    .default = shelledCobMass),
         shelledCobWidth = case_when(!between(shelledCobWidth, 0.75, 3.5) ~ NA, 
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

responseVars <- c(c('earWidth', 'earFillLength', 'kernelRowNumber',
                    'kernelsPerRow', 'kernelsPerEar', 'shelledCobWidth', 'shelledCobMass', 'hundredKernelMass', 'kernelMassPerEar',
                    'plantDensity', 'totalStandCount', 'flagLeafHeight', 'earHeight', 'earLength', 'daysToAnthesis', 'GDDToAnthesis', 'daysToSilk', 'GDDToSilk', 'anthesisSilkingInterval', 'anthesisSilkingIntervalGDD'))

# inbredsWide <- plotRepCorr(inbreds4.9, 'nitrogenTreatment', 'genotype', responseVars, 'location')

lnkIndex <- read_excel('rawData/2022/2022 Inbred HIPS - Lincoln Summary File.xlsx',
                       sheet = 'Index', 
                       skip = 1,
                       col_types = c('numeric', rep('skip', 4), 'numeric', 'numeric'),
                       col_names = c('plotNumber', 'row', 'range'))

mvIndex <- read_excel('rawData/2022/Missouri Valley - Inbred - Summary.xlsx',
                      sheet = 'Index', 
                      skip = 1,
                      col_types = c(rep('skip', 12), 'numeric', rep('skip', 5), 'numeric', 'numeric', rep('skip', 3)),
                      col_names = c('plotNumber', 'range', 'row')) %>%
  rowwise() %>%
  mutate(plotNumber = case_when(range > 44 ~ plotNumber + 400, .default = plotNumber))

inbreds4.9 <- inbreds %>%
  rowwise() %>%
  mutate(genotype = case_when(genotype == 'IC I740' ~ 'ICI 740',
                              genotype == 'CI3A' ~ 'CI31A',
                              .default = genotype) %>% 
           str_remove('@'),
         irrigationProvided = case_when(is.na(irrigationProvided) ~ 0, .default = irrigationProvided),
         row = case_when(location == 'Lincoln' ~ ifelse(is.na(row), lnkIndex$row[lnkIndex$plotNumber == .data$plotNumber], row), .default = row),
         range = case_when(location == 'Lincoln' ~ ifelse(is.na(range), lnkIndex$range[lnkIndex$plotNumber == .data$plotNumber], range), 
                           .default = range),
         plotNumber = case_when(location == 'Missouri Valley' ~ ifelse(is.na(plotNumber), 
                                                                       mvIndex$plotNumber[mvIndex$range == range & mvIndex$row == row], plotNumber),
                                .default = plotNumber))



# v5 <- read.csv('outData/HIPS_2022_V5_INBREDS.csv') %>%
#   rename(nitrogenTreatment = N,
#          row = column,
#          plotNumber = plot,
#          flagLeafHeight = plantH) %>%
#   rowwise() %>%
#   mutate(anthesisDate = mdy(anthesisDate),
#          silkDate = mdy(silkDate),
#          plantingDate = mdy(plantingDate)) %>%
#   select(!rep)

v495 <- inbreds4.9 %>%
  rowwise() %>%
  mutate(range = case_when(location=='Scottsbluff' & row==13 & range < 49 ~ range - 1, .default = range), 
         year = 2022, 
         locationYear = str_c(year, location, sep = ' '), 
         environment = str_c(locationYear, nitrogenTreatment, sep = ' '))
v495_wide <- plotRepCorr2(v495, phenotypes = responseVars)
# compFix <- v5 %>%
#   filter(location=='Scottsbluff') %>% 
#   full_join(sb495, join_by(row, range), keep = FALSE, suffix = c('.5', '.495')) %>%
#   select(qrCode.5, qrCode.495, row, range, genotype.5, genotype.495, plotNumber.5, plotNumber.495, kernelRowNumber.5, kernelRowNumber.495)

write_csv(v495, 'finalData/HIPS_INBREDS_2022_EARLEVEL.csv')

