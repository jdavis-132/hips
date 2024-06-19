library(tidyverse)
library(readxl)

# 2022 hybrid ears phenotyped at UNL
hybridEars22UNL <- read.csv('data/earphenotypesformatted.csv')
colnames(hybridEars22UNL) <- c('collector', 'qrCode', 'earNumber', 'kernelColor', 'earWidth', 'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'kernelsPerEar', 'shelledCobWidth', 'earLength', 'shelledCobMass', 'hundredKernelMass', 'percentMoisture')
hybridEars22UNL2 <- read.csv('data/earphenotypesformatted_part2.csv')
colnames(hybridEars22UNL2) <- c('collector', 'qrCode', 'earNumber', 'kernelColor', 'earWidth', 'earFillLength', 'kernelRowNumber', 'kernelsPerRow', 'earMass', 'kernelsPerEar', 'shelledCobWidth', 'earLength', 'shelledCobMass', 'hundredKernelMass', 'percentMoisture')
hybridEars22UNL <- bind_rows(hybridEars22UNL, hybridEars22UNL2)
