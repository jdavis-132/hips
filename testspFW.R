library(spFW)

earHeight <- read.csv('../../../S24/CSCE879/project/earHeight.csv')

y <- earHeight$earHeight
geno <- earHeight$genotype
environment <- earHeight$sourceEnvironment

spFWModel <- HFWM_est(Y=y, VAR=geno, ENV=environment)
