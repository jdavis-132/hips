library(tidyverse)
library(readxl)

# Ames and Crawfordsville data inbred data
ac.krn <- read_excel('data/2_KRN_trait_inbred_2022_Compiled_v3.xlsx', 
                     sheet = 'compiled data',
                     skip = 4,
                     col_types = c('skip', 'text', 'skip', 'skip', 'numeric', 'text', 'text', 'text', 'text'),
                     col_names = c('sampleID', 'kernelRowNumber', 'notes', 'smoothCob', 'sweetcorn', 'kernelRowNumberFile')) %>%
  rowwise() %>%
  mutate(smoothCob = case_when(!is.na(smoothCob) ~ 'Smooth cob/ovule issue'),
         sweetcorn = case_when(!is.na(sweetcorn) ~ 'Sweetcorn')) %>%
  unite('notes', notes, smoothCob, sweetcorn, sep = ';', remove = TRUE, na.rm = TRUE)

ac.ears <- read_excel('data/3_ear_trait_INBRED_2022_compiled.xlsx', 
                      sheet = 'compiled data set',
                      skip = 1,
                      col_types = c('skip', 'text', 'numeric', 'numeric', 'numeric', 'skip', 'skip', 'text', 'skip'),
                      col_names = c('sampleID', 'earLength', 'earWidth', 'earMass', 'seedMissing')) %>%
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
