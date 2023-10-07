library(tidyverse)
library(readxl)
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
    mutate(qr = qr %>%
             str_to_upper(),# %>%
             # str_remove('.') %>%
             # str_remove('`') %>%
             # str_remove('U') %>%
             #str_remove('R'),
           notes = case_when(!is.na(sweetcorn) ~ str_c(notes, 'sweetcorn', sep = ';'), .default = notes),
           loc = case_when(str_split_i(qr, '-', 2)=='C' ~ 'Crawfordsville',
                           str_split_i(qr, '-', 2)=='A' ~ 'Ames'),
           plot = str_split_i(qr, '-', 3) %>%
             as.numeric(), 
           population = 'Hybrid', 
           irrigation = 'Dryland')
  ac.ears <- bind_rows(ac.ears, curr.df)
}

duplicateKRNHyb <- ac.ears %>%
  group_by(qr) %>%
  summarise(n = n()) %>%
  filter(n>1)

duplicatedHybEars <- filter(ac.ears, qr %in% duplicateKRNHyb$qr) %>%
  filter(qr!='SAMPLE ID SCAN')

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
    mutate(qr = qr, #%>%
             # str_remove('.') %>%
             # str_remove('`') %>%
             # str_remove('u') %>%
             # str_to_upper() %>%
             # str_remove('R'),
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

# ac.ears <- ac.ears %>%
#   rowwise() %>%
#   mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr))

# Check for duplicates:
duplicateKRNInb <- ac.ears %>%
  group_by(qr) %>%
  summarise(n = n()) %>%
  filter(n>1)
duplicatedInbEars <- ac.ears %>%
  filter(qr %in% duplicateKRNInb$qr & qr!='SAMPLE ID SCAN' & qr!='Sample ID Scan' & !(qr %in% duplicateKRNHyb$qr))

ear.files <- list.files(path = 'data/3 Ear Traits Station', pattern = '3_Ear_Traits_', full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
ear.df <- tibble(loc = NULL, plot = NULL, qr = NULL, population = NULL, irrigation = NULL, notes = NULL, earLen = NULL, earWidth = NULL, earWt = NULL, .rows = 0)
for (currFile in ear.files)
{
  curr.df <- read_excel(currFile, 
                        col_names = c('qr', 'earLen', 'earWidth', 'earWt', 'collectionStation', 'string', 'seedMissingAtWidest'),
                        col_types = c('text', rep('numeric', 3), rep('text', 3))) %>%
    filter(!is.na(qr)|!is.na(earLen)|!is.na(earWidth)|!is.na(earWt)) %>%
    rowwise() %>%
    mutate(qr = qr %>%
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
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr))
# Remove example rows: they have no qrs
ac.ears <- filter(ac.ears, !is.na(qr) & qr!='AMPLE ID SCAN')
ear.df <- filter(ear.df, !is.na(qr) & qr!='AMPLE ID SCAN')
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
    mutate(qr = qr %>%
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
  mutate(qr = case_when(str_detect(qr, '2-') & str_detect(qr, '22-', negate = TRUE) ~ str_replace(qr, '2-', '22-'), .default = qr))
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

ac.ears <- ac.ears %>%
  rowwise() %>%
  mutate(qr = case_when(str_detect(qr, '2-') & !str_detect(qr, '22-') ~ str_replace(qr, '2-', '22-'), .default = qr))

ac.ears <- ac.ears %>%
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
  mutate(kernelMass = case_when(!is.na(kernelMass.redo) ~ kernelMass.redo, .default = kernelMass)) 

ac.df <- full_join(ac.ears, seed.df, join_by(qr), suffix = c('', '.seed'), keep = FALSE)
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