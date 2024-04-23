library(tidyverse)

df <- read.csv('../../../Downloads/2023_inbred_HIPS_data_04_17_2024.csv')
genotypesData <- str_to_upper(df$genotype) %>%
  unique()
phenotypeGenotype <- tibble(phenotypeGenotype = genotypesData) %>%
  rowwise() %>%
  mutate(str_replace(phenotypeGenotype, 'KUI', 'KI'))
reseqGenotypes <- read.table('../../../Downloads/genotypes_grzybowski.txt', row.names = NULL, quote = '', comment.char = '', header = TRUE)
reseqGenotypes <- colnames(reseqGenotypes) %>% 
  str_remove('X')
reseqGenotypes <- reseqGenotypes[10:length(reseqGenotypes)]
reseqGenotypes <- tibble(vcfGenotype = reseqGenotypes, 
                         phenotypeGenotype = str_to_upper(reseqGenotypes)) %>%
  rowwise() %>%
  mutate(phenotypeGenotype = case_when(phenotypeGenotype=='33.16' ~ '33-16',
                                       phenotypeGenotype=='38.11' ~ '38-11',
                                       phenotypeGenotype=='A441.5' ~ 'A441-5',
                                       phenotypeGenotype=='CH701.30' ~ 'CH701-30',
                                       phenotypeGenotype=='CI7GOODMAN.BUCKLER' ~ 'CI.7',
                                       phenotypeGenotype=='CI_187.2' ~ 'CI187-2',
                                       phenotypeGenotype=='CI_21E' ~ 'CI21E',
                                       phenotypeGenotype=='CI_28A' ~ 'CI28A',
                                       phenotypeGenotype=='CI_3A' ~ 'CI3A',
                                       phenotypeGenotype=='CI_64' ~ 'CI64',
                                       phenotypeGenotype=='CI_91B' ~ 'CI91B', 
                                       str_detect(phenotypeGenotype, 'CML_') ~ str_remove(phenotypeGenotype, '_'),
                                       phenotypeGenotype=='DE2' ~ 'DE_2',
                                       phenotypeGenotype=='HI27GOODMAN.BUCKLER' ~ 'HI27',
                                       phenotypeGenotype=='I_205' ~ 'I205',
                                       phenotypeGenotype=='ICI_740' ~ 'ICI 740',
                                       phenotypeGenotype=='KI44' ~ 'KUI44',
                                       phenotypeGenotype=='KI2021' ~ 'KUI2021', 
                                       phenotypeGenotype=='L_127' ~ 'L127', 
                                       phenotypeGenotype =='LP1_NR_HT' ~ 'LP1 NR HT',
                                       phenotypeGenotype=='DKMBNA' ~ 'MBNA',
                                       phenotypeGenotype=='SEAGULL_SEVENTEEN' ~ 'SEAGULL SEVENTEEN',
                                       phenotypeGenotype=='RS_710' ~ 'RS 710',
                                       phenotypeGenotype=='SG_1533' ~ 'SG1533',
                                       phenotypeGenotype=='SG_18' ~ 'SG18',
                                       phenotypeGenotype=='YU796NS' ~ 'YU796_NS',
                                       phenotypeGenotype=='DK78004' ~ '78004',
                                       phenotypeGenotype=='PH207' ~ '207',
                                       phenotypeGenotype=='AS5707' ~ '5707',
                                       phenotypeGenotype=='NK807' ~ '807', 
                                       phenotypeGenotype=='DK4676A' ~ '4676A',
                                       phenotypeGenotype=='DK78371A' ~ '78371A',
                                       phenotypeGenotype=='NKH8431' ~ 'H8431', 
                                       phenotypeGenotype=='DK3IIH6' ~ '3IIH6',
                                       phenotypeGenotype=='DKFAPW' ~ 'FAPW',
                                       phenotypeGenotype=='DKFBHJ' ~ 'FBHJ',
                                       phenotypeGenotype=='DKHBA1' ~ 'HBA1',
                                       phenotypeGenotype=='I1677A' ~ 'IL677A', 
                                       phenotypeGenotype=='NK778' ~ '778',
                                       .default = phenotypeGenotype))

genotypes <- full_join(reseqGenotypes, phenotypeGenotype, join_by(phenotypeGenotype), keep = TRUE, suffix = c('.vcf', '.pheno'))

sam <- filter(genotypes, !is.na(phenotypeGenotype.pheno)) %>%
  filter(!str_detect(phenotypeGenotype.pheno, 'AK-')) %>%
  filter(!str_detect(phenotypeGenotype.pheno, 'UFMU'))
samMissing <- filter(sam, is.na(phenotypeGenotype.vcf)) %>%
  # poor germ, marker lines, single gene change from other lines in panel, hybrid filler
  filter(!(phenotypeGenotype.pheno %in% c("MDF-13D", "PB80", "W22 R1-R", "WHITE VARIEGATED", "[WHITE VARIEGATED (ISO. FROM BM4 AC3252) (B73-1)]",
                                    "B73HTRHM", "PIONEER P1185AM"))) %>%
  arrange(phenotypeGenotype.pheno)

samMissing$phenotypeGenotype.pheno

reseqNotAssignedPheno <- filter(genotypes, is.na(phenotypeGenotype.pheno))

samToReseq <- sam %>%
  rename(SAM2022 = phenotypeGenotype.pheno, 
         reseq = vcfGenotype) %>%
  select(SAM2022, reseq) %>%
  filter(!str_detect(SAM2022, 'AK-')) %>%
  filter(!str_detect(SAM2022, 'UFMU')) %>%
  filter(!(SAM2022 %in% c("MDF-13D", "PB80", "W22 R1-R", "WHITE VARIEGATED", 
                          "[WHITE VARIEGATED (ISO. FROM BM4 AC3252) (B73-1)]", "B73HTRHM", "PIONEER P1185AM"))) %>%
  arrange(SAM2022)
write.csv(samToReseq, 'outData/conversionToGrzybowskiResequencing.csv', row.names = FALSE)
