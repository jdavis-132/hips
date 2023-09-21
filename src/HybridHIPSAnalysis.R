library(lme4)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
library(scales)
library(FW)
library(PerformanceAnalytics)
library(ggcorrplot)
library(svglite)
# Read in data and order nitrogenTreatment
# Also calculate moisture correction for kernelMass and hundredKernelMass
hybrids <- read.csv('outData/HIPS_2022_V3.5_HYBRIDS.csv')
meanPercentMoisture <- mean(hybrids$percentMoisture, na.rm = TRUE)
hybrids <-  hybrids %>% 
  mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'), ordered = TRUE),
         moistureCorrectedKernelMassPerEar = case_when(!is.na(kernelMassPerEar) & !is.na(percentMoisture) ~ kernelMassPerEar/(1 - (percentMoisture/100)),
                                                 !is.na(kernelMassPerEar) & location %in% c('Ames', 'Crawfordsville') ~ kernelMassPerEar/(1 - (meanPercentMoisture/100))),
         moistureCorrectedHundredKernelMass = case_when(!is.na(hundredKernelMass) & !is.na(percentMoisture) ~ hundredKernelMass/(1 - (percentMoisture/100)),
                                                        !is.na(hundredKernelMass) & location %in% c('Ames', 'Crawfordsville') ~ hundredKernelMass/(1 - (meanPercentMoisture/100))))
# Let's look at how yield varies across sublocations
yieldMap <- ggplot(hybrids, aes(range, row, fill = combineYield, color = 'white')) +
  geom_raster() +
  facet_wrap(vars(location, sublocation)) + 
  scale_x_continuous(breaks = 0:40) +
  scale_y_continuous(breaks = 0:40) +
  scale_fill_viridis(option = 'turbo', direction = -1) +
  theme_minimal()
yieldMap
  
# Now let's make this into a function to make maps for every response variable:
mapResponse <- function(data, trait)
{
  plot <- ggplot(data, aes(range, row, fill = .data[[trait]], color = 'white')) + 
    geom_raster() +
    facet_wrap(vars(location, sublocation)) + 
    scale_x_continuous(breaks = 0:40) +
    scale_y_continuous(breaks = 0:40) +
    scale_fill_viridis(option = 'turbo', direction = -1) +
    theme_minimal() + 
    theme(axis.text = element_text(angle = 45))
  print(plot)
}

response_vars <- c('earHeight', 'flagLeafHeight', 'combineMoisture', 'combineTestWeight', 
                   'earFillLength', 'earWidth', 'shelledCobWidth', 'shelledCobMass', 'earLength', 'kernelsPerEar',
                   'percentStarch', 'percentProtein', 'percentOil', 'percentFiber', 'percentAsh', 
                   'yieldPerAcre', 'GDDToAnthesis', 'GDDToSilk', 'kernelsPerRow', 'kernelRowNumber', 'moistureCorrectedKernelMassPerEar',
                   'moistureCorrectedHundredKernelMass', 'anthesisSilkingIntervalGDD', 'daysToAnthesis', 'daysToSilk', 'anthesisSilkingInterval')

locations <- c('Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville')

# Define 'pretty' labels for response vars
response_labels <- c('Ear Height (cm)', 'Flag Leaf Height (cm)', 'Harvest Moisture (%)', 'Test Weight (lbs/bushel)', 'Ear Fill Length (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)', 'Shelled Cob Mass (g)', 'Ear Length (cm)', 'Kernels Per Ear', 'Starch (%)', 'Protein (%)', 'Oil (%)', 'Fiber (%)', 'Moisture Corrected Ash (%)', 'Yield (Bushels/Acre)', 'GDD to Anthesis', 'GDD to Silk', 'Kernels Per Row', 'Kernel Row Number', 'Moisture Corrected Kernel Mass Per Ear (g)', 'Moisture Corrected Hundred Kernel Mass (g)', 'Anthesis Silking Interval (GDD)', 'Days to Anthesis', 'Days to Silk', 'Anthesis Silking Interval (Days)')

mapResponse(hybrids, c('percentProtein'))

for(i in response_vars)
{
  print(i)
  mapResponse(hybrids, i)
}

idOutliers <- function(data, trait)
{
  df <- data %>% 
    mutate(trait = .data[[trait]])
  q1 <- quantile(df$trait, probs = 0.25, na.rm = TRUE)
  q3 <- quantile(df$trait, probs = 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lowCutoff <- q1 - (1.5*iqr)
  highCutoff <- q3 + (1.5*iqr)
  
  df_filt <- filter(df, trait<lowCutoff|trait>highCutoff) %>%
    select(!trait)
  return(df_filt)
}

outliers <- list()
for (i in c('kernelMassPerEar', 'hundredKernelMass'))
{
  outliers[[i]] <- idOutliers(hybrids, i)
}

# Histograms
for(i in c('kernelMassPerEar', 'hundredKernelMass'))
{
  p <- ggplot(hybrids, aes(.data[[i]])) +
    geom_histogram()
  print(p)
}

# Violins
for (i in locations)
{
  location.df <- hybrids %>%
    filter(location==i) %>%
    pivot_longer(all_of(c('percentProtein')), names_to = 'phenotype', values_to = 'val')
  
  p <- ggplot(location.df, aes(nitrogenTreatment, val)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE, fill = 'blue') + 
    facet_wrap(vars(phenotype), scales = 'free_y') + 
    labs(title = i)
  print(p)
}

# Check NP2 yield and protein
p <- hybrids %>% 
  filter(location=='North Platte2') %>%
  ggplot(aes(range, yieldPerAcre, group = range)) + 
  geom_boxplot(fill = 'blue') +
  geom_vline(xintercept = 12) + 
  geom_vline(xintercept = 24)
print(p)

p <- hybrids %>% 
  filter(location=='North Platte2') %>%
  ggplot(aes(range, percentProtein, group = range)) + 
  geom_boxplot(fill = 'blue') +
  geom_vline(xintercept = 12) + 
  geom_vline(xintercept = 24)
print(p)

plotRepCorr <- function(data, treatmentVar, genotype, phenotypes, facet)
{
  df.wide <- data %>%
    group_by(.data[[genotype]], .data[[treatmentVar]], .data[[facet]]) %>%
    mutate(rep = 1:n()) %>%
    ungroup() %>%
    pivot_longer(all_of(phenotypes), names_to = 'var', values_to = 'val') %>%
    select(c(all_of(genotype), all_of(treatmentVar), all_of(facet), rep, var, val)) %>%
    pivot_wider(id_cols = c(.data[[genotype]], .data[[treatmentVar]], .data[[facet]]), names_from = c(var, rep), values_from = val, names_sep = '.')
  
  for(i in phenotypes)
  {
    rep1 <- paste0(i, '.1')
    rep2 <- paste0(i, '.2')
    print(i)
    
    p <- ggplot(df.wide, aes(.data[[rep1]], .data[[rep2]], color = .data[[treatmentVar]])) + 
      geom_point() + 
      facet_wrap(vars(all_of(.data[[facet]])))
    print(p)
  }
  return(df.wide)
}

hybrids.wide <- plotRepCorr(hybrids, 'nitrogenTreatment', 'genotype', c('hundredKernelMass', 'kernelMassPerEar'), 'location')

# Correlation plot for 2 vars
plotVarCorr <- function(data, x, y)
{
  x.str <- deparse(substitute(x))
  y.str <- deparse(substitute(y))
  p <- ggplot({{data}}, aes({{x}}, {{y}}, color = nitrogenTreatment)) + 
    geom_point() +
    facet_wrap(vars(location)) +
    labs(subtitle = str_c('R = ', cor(data[[x.str]], data[[y.str]], use = 'complete.obs')))
  print(p)
}

plotVarCorr(hybrids, moistureCorrectedHundredKernelMass, percentProtein)
plotVarCorr(hybrids, yieldPerAcre, moistureCorrectedKernelMassPerEar)
plotVarCorr(hybrids, anthesisSilkingInterval, anthesisSilkingIntervalGDD)
plotVarCorr(hybrids, daysToSilk, GDDToSilk)
plotVarCorr(hybrids, daysToAnthesis, GDDToAnthesis)
plotVarCorr(hybrids, GDDToAnthesis, GDDToSilk)
plotVarCorr(hybrids, kernelRowNumber, yieldPerAcre) # not very correlated
plotVarCorr(hybrids, kernelRowNumber, moistureCorrectedKernelMassPerEar) # not very correlated
plotVarCorr(hybrids, kernelRowNumber, kernelsPerRow) # not very correlated
plotVarCorr(hybrids, kernelRowNumber, earWidth)
plotVarCorr(hybrids, kernelRowNumber, shelledCobWidth)
plotVarCorr(hybrids, kernelRowNumber, kernelsPerEar)
plotVarCorr(hybrids, kernelsPerRow, earFillLength)
plotVarCorr(hybrids, kernelsPerRow, earLength)
plotVarCorr(hybrids, kernelsPerRow, kernelsPerEar)
plotVarCorr(hybrids, kernelsPerEar, yieldPerAcre)
plotVarCorr(hybrids, kernelsPerEar, moistureCorrectedKernelMassPerEar)
plotVarCorr(hybrids, kernelsPerEar, moistureCorrectedHundredKernelMass)
plotVarCorr(hybrids, percentStarch, yieldPerAcre)
plotVarCorr(hybrids, shelledCobMass, yieldPerAcre)
plotVarCorr(hybrids, shelledCobMass, shelledCobWidth)
plotVarCorr(hybrids, shelledCobMass, earLength)
plotVarCorr(hybrids, shelledCobWidth, earWidth)
plotVarCorr(hybrids, earLength, earFillLength)
plotVarCorr(hybrids, earLength, kernelsPerEar)
plotVarCorr(hybrids, moistureCorrectedHundredKernelMass, combineTestWeight)
plotVarCorr(hybrids, combineMoisture, combineTestWeight)
plotVarCorr(hybrids, flagLeafHeight, earHeight)
# Cast nitrogenTreatment, genotype, and irrigationProvided as factors
#hybrids <- mutate(hybrids, across(c(nitrogenTreatment, genotype, irrigationProvided, location), as.factor))

# # Function to do variance partitioning for each locationation for a given response variable
# partitionVariance <- function(data, response)
# {
#   # Loop over locationations
#   locations <- c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3')
#   vc.df <- tibble(modelTerm = NULL, Variance = NULL, SD = NULL, `log10l(lambda)` = NULL, pctVar = NULL, location = NULL, .rows = 0) 
#   for(i in locations)
#   {
#     location.df <- filter(data, location==i & !is.na(range) & !is.na(row) & !is.na(.data[[response]]))
#     print(length(location.df$plot))
#     if(length(location.df$plotNumber)==0)
#     {
#       next
#     }
#     # Fit model
#     rangeKnots <- floor(max(location.df$range, na.rm = TRUE)/2) + 1
#     rowKnots <- floor(max(location.df$row, na.rm = TRUE)/2) + 1
#     print(i)
#     print(rangeKnots)
#     print(rowKnots)
#     print(response)
#     if(i=='Missouri Valley')
#     {
#       model <- SpATS(response, genotype = 'genotype', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)),
#                      data = location.df)
#     }
#     else
#     {
#       model <- SpATS(response, genotype = 'genotype', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)),
#                    random = ~ nitrogenTreatment + nitrogenTreatment:genotype, data = location.df)
#     }
#     # Extract variance components


#     summary <- summary.SpATS(model, 'all')
#     vc <- summary$p.table.vc %>%
#       as_tibble(rownames = 'modelTerm') %>%
#       filter(modelTerm!='NA.') %>%
#       mutate(across(!modelTerm, as.numeric))
#     # Calculate total variance
#     totalVar <- sum(vc$Variance)
#     # Calculate pctVar
#     vc <- vc %>%
#       rowwise() %>%
#       mutate(pctVar = Variance/totalVar*100, 
#              location = i)
#     summary(vc)
#     # Bind to df
#     vc.df <- bind_rows(vc.df, vc)
#     
#     # Plot the spatial output
#     plot.SpATS(model, main = paste0(response, ':', i))
#   }
#   vc.df <- filter(vc.df, !is.na(pctVar))
#   vc.plot <- ggplot(vc.df, aes(1, pctVar, fill = modelTerm)) + 
#     geom_col(position = 'stack') + 
#     facet_wrap(vars(location)) + 
#     labs(title = response)
#     theme_minimal()
#   
#   print(vc.plot)
#   return(vc.df)
# }
# 
# vp <- list()
# for (j in response_vars)
# {
#   vp[[j]] <- partitionVariance(hybrids, j)
# }
# 
# lnk_lowN <- hybrids %>%
#   filter(location=='Lincoln' & nitrogenTreatment=='Low') %>%
#   mutate(plot = as.factor(plotNumber)) %>%
#   select(c(plotNumber, range, row, yieldPerAcre))
# m <- SpATS('yieldPerAcre', genotype = 'plotNumber', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(8, 14)), fixed = NULL, data = lnk_lowN)
# s <- summary(m)
# plot.SpATS(m)
# p.blups <- s$coeff %>%
#   as_tibble(rownames = 'plotNumber')

# Okay, now let's write a function to get the spatial BLUES for each response on a plot level
# Will fit model by individual locationation, nitrogen treatment combination
getSpatialCorrections <- function(data, response)
{
  # Declare empty df and levels of locations
  df.sp <- tibble(location = NULL, plotNumber = NULL, '{response}':= NULL, nitrogenTreatment = NULL)
  locations <-  c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Ames', 'Crawfordsville')
  # Loop over locationations
  for(currlocation in locations)
  {
    location.df <- filter(data, location==currlocation & !is.na(row) & !is.na(range) & !is.na(.data[[response]] & !is.na(nitrogenTreatment)))
    if(length(location.df$plotNumber)==0)
    {
      print(paste0('No data for ', response, ' at ', currlocation))
      next
    }
    nitrogenTreatments <- unique(location.df$nitrogenTreatment)
      
    # Loop over nitrogen treatments
    for(currTrt in nitrogenTreatments)
    {
      if(is.na(currTrt)|currTrt=='Border')
      {
        next
      }
      location.n.df <- filter(location.df, nitrogenTreatment==currTrt) %>%
        mutate(as.factor(plotNumber))
      rangeKnots <- floor(max(location.n.df$range, na.rm = TRUE)/2) + 1
      rowKnots <- floor(max(location.n.df$row, na.rm = TRUE)/2) + 1
      print(currlocation)
      print(currTrt)
      model <- SpATS(response, genotype = 'plotNumber', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)), data = location.n.df)
      # Plot model
      plot.SpATS(model, main = paste0(response, ':', currlocation, ':', currTrt))
      # Extract BLUPS
      summary <- summary(model)
      if(cor(location.n.df[[response]], summary$fitted + summary$residuals) > 0.99)
      {
        sp <- tibble(location = currlocation,
                     nitrogenTreatment = currTrt, 
                     plotNumber = location.n.df$plotNumber,
                     '{response}':=summary$fitted)
      }
      else
      {
        print(paste0('Fitted values misordered. r =', cor(location.n.df[[response]], summary$fitted + summary$residuals), '; ', currlocation, '; ', currTrt))
        next
      }
      # Bind to df
      df.sp <- bind_rows(df.sp, sp) %>%
        mutate(plotNumber = as.numeric(plotNumber))
    }
  }
  print(length(df.sp$plotNumber))
  # Return df
  return(df.sp)
}

for(i in response_vars)
{
  hybrids <- full_join(hybrids, getSpatialCorrections(hybrids, i), by = join_by(location, plotNumber, nitrogenTreatment), suffix = c('', '.sp'), keep = FALSE)
}

hybrids <- hybrids %>%
  rowwise() %>%
  mutate(across(everything(), ~case_when(is.null(.) ~ NA, .default = .)))

partitionVariance2 <- function(df, response) 
{
  df <- filter(df, !is.na(response))
  lm_formula <- as.formula(paste(response, "~ (1|location/nitrogenTreatment) + (1|genotype) + (1|location:genotype) + (1|nitrogenTreatment:genotype)"))
  model <- lmer(lm_formula, data = df, na.action = na.omit)
  vc <- as.data.frame(VarCorr(model), row.names = TRUE, order = 'cov.last', comp = 'Variance') %>%
    as_tibble() %>%
    mutate(responseVar = response)
  totalVar <- sum(vc$vcov)
  vc <- vc %>%
    rowwise() %>%
    mutate(pctVar = vcov/totalVar*100) %>%
    select(responseVar, grp, vcov, pctVar)
  return(vc)
}



vc_all <- tibble(grp = NULL, responseVar = NULL, vcov = NULL, pctVar = NULL)
spatiallyCorrectedResponseVars <- paste0(response_vars, '.sp')

# Is there shrinkage toward the mean of a treatment ?
for(i in 1:length(response_vars))
{
  sp.correction.plot <- ggplot(hybrids, (aes(.data[[response_vars[i]]], .data[[spatiallyCorrectedResponseVars[i]]], color = nitrogenTreatment))) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(vars(location)) 
  print(sp.correction.plot)
}

#spatiallyCorrectedResponseVars <- paste0(spatiallyCorrectedResponseVars, '.blup')
# Don't use the spatially corrected vals when there's fitting issues
hybrids.vp <- hybrids %>%
  rowwise() %>%
  mutate(anthesisSilkingInterval.sp = case_when(location %in% c('North Platte1', 'North Platte2') ~ anthesisSilkingInterval, 
                            .default = anthesisSilkingInterval.sp),
         anthesisSilkingIntervalGDD.sp = case_when(location %in% c('North Platte1', 'North Platte2') ~ anthesisSilkingIntervalGDD, 
                                                 .default = anthesisSilkingIntervalGDD.sp), 
         percentProtein.sp = case_when(location %in% c('Lincoln', 'North Platte2', 'North Platte3', 'Scottsbluff') ~ percentProtein, 
                                       .default = percentProtein.sp),
         kernelsPerEar.sp = case_when(location=='Scottsbluff' ~ kernelsPerEar,
                                      .default = kernelsPerEar.sp),
         earLength.sp = case_when(location=='Scottsbluff' ~ earLength, 
                                  .default = earLength.sp),
         shelledCobMass.sp = case_when(location=='Scottsbluff' ~ shelledCobMass, 
                                       .default = shelledCobMass.sp),
         combineMoisture.sp = case_when(location=='Ames' ~ combineMoisture, 
                                        .default = combineMoisture.sp),
         nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'), ordered = TRUE))

for(i in spatiallyCorrectedResponseVars)
{
  vc_all <- bind_rows(vc_all, partitionVariance2(hybrids.vp, i))
}
vc_all <- vc_all %>%
  rowwise() %>%
  mutate(grp = case_when(grp=='genotype' ~ 'Genotype Main Effect',
                         grp=='location' ~ 'Location Main Effect',
                         grp=='location:genotype' ~ 'Genotype x Location Interaction',
                         grp=='nitrogenTreatment:genotype' ~ 'Genotype x Nitrogen Treatment Interaction',
                         grp=='nitrogenTreatment:location' ~ 'Nitrogen Treatment Main Efffect',
                         .default = grp))
vp.plot <- ggplot(vc_all, aes(responseVar, pctVar, fill = grp)) +
  geom_col(position = 'stack') + 
  scale_fill_viridis(option = 'turbo', discrete = TRUE) +
  labs(x = 'Phenotype', y = 'Percent Variance', fill = 'Variance Component') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
vp.plot

for(i in spatiallyCorrectedResponseVars)
{
  df.n <- hybrids.vp %>%
    group_by(location, nitrogenTreatment) %>%
    mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High'), ordered = TRUE))
  p <- ggplot(df.n, aes(nitrogenTreatment, .data[[i]])) +
    geom_violin(fill = 'blue', draw_quantiles = c(0.25, 0.5, 0.75)) + 
    facet_wrap(vars(location), scales = 'free')
  print(p)
}

sb.np2 <- filter(hybrids.vp, location=='North Platte2'|(location=='Scottsbluff' & nitrogenTreatment %in% c('Low', 'Medium'))|(location=='Scottsbluff' & nitrogenTreatment=='High' & yieldPerAcre.sp >=100))

getNitrogenPlasticityByLocation <- function(data, response, locations)
{
  print(response)
  response.out <- response %>% 
    str_replace('.sp', '.pl')
  response.df <- tibble(location = NULL, genotype = NULL, '{response.out}':= NULL,)
  for (currlocation in locations)
  {
    location.df <- filter(data, !is.na(genotype) & location==currlocation & nitrogenTreatment!='Border' & !is.na(nitrogenTreatment)) %>%
      group_by(genotype, nitrogenTreatment) %>%
      summarise('{response}' := mean(.data[[response]], na.rm = TRUE))
    fw <- FW(y = location.df[[response]], VAR = location.df$genotype, ENV = location.df$nitrogenTreatment, saveAt = paste0('analysis/gibbs-samples-', response, '-', currlocation),
             nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656, saveVAR = c(1:2), saveENV = c(1:2))
    pl <- fw$b %>%
      as_tibble(rownames = 'genotype') %>%
      mutate(location = currlocation, 
             '{response.out}':= Init1 + 1) %>%
      select(!Init1)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

# plasticitySBNP2.df <- getNitrogenPlasticityBylocation(sb.np2, 'yieldPerAcre.sp')
# 
# plasticitySBNP2.wide <- plasticitySBNP2.df %>%
#   pivot_wider(id_cols = genotype, names_from = location, values_from = yieldPerAcre.sp)
# 
# plasticityCorrPlot <- ggplot(plasticitySBNP2.wide, aes(`North Platte2`, `Scottsbluff`)) + 
#   geom_point() + 
#   labs(x = 'Nitrogen Plasticity of Yield at North Platte Under Partial irrigationProvided',
#        y = 'Nitrogen Plasticity of Yield at Scottsbluff',
#        subtitle = expression(R^2~'='~'0.04074947'))
# plasticityCorrPlot
# 
# sb.np2.summary <- sb.np2 %>%
#   group_by(genotype, location) %>%
#   summarise(meanYield = mean(yieldPerAcre.sp, na.rm = TRUE)) %>%
#   full_join(plasticitySBNP2.df, by = join_by(genotype, location), keep = FALSE, suffix = c('', ''))
# 
# plasticityVsYield <- ggplot(sb.np2.summary, aes(meanYield, yieldPerAcre.sp, color = location)) + 
#   geom_point() +
#   geom_hline(yintercept = 1) +
#   facet_grid(cols = vars(location)) +
#   labs(x = 'Mean Genotype Yield (Bushels Per Acre)', y = 'Linear Plasticity (Finlay-Wilkinson)')
# plasticityVsYield



nitrogenPlasticityLocations <- c('Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Lincoln',
                                 'Ames', 'Crawfordsville') 
# Summarize to genotype level within a treatment in a location
hybrids.pl <- hybrids.vp %>%
  group_by(location, nitrogenTreatment, genotype) %>%
  summarise(across(all_of(spatiallyCorrectedResponseVars), ~mean(.x, na.rm = TRUE)))

plasticity.df <- getNitrogenPlasticityByLocation(hybrids.pl, spatiallyCorrectedResponseVars[1], nitrogenPlasticityLocations)

for(i in spatiallyCorrectedResponseVars[2:length(spatiallyCorrectedResponseVars)])
{
  plasticity.df <- full_join(plasticity.df, getNitrogenPlasticityByLocation(hybrids.pl, i, nitrogenPlasticityLocations), join_by(genotype, location), suffix = c('', ''), keep = FALSE)
}

summary.df <- hybrids.vp %>%
  group_by(location, genotype) %>%
  summarise(earHeight.mu = mean(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.mu = mean(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.mu = mean(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.mu = mean(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.mu = mean(earFillLength.sp, na.rm = TRUE),
            earWidth.mu = mean(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.mu = mean(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.mu = mean(shelledCobMass.sp, na.rm = TRUE),
            earLength.mu = mean(earLength.sp, na.rm = TRUE),
            kernelsPerEar.mu = mean(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.mu = mean(percentStarch.sp, na.rm = TRUE),
            percentProtein.mu = mean(percentProtein.sp, na.rm = TRUE),
            percentOil.mu = mean(percentOil.sp, na.rm = TRUE),
            percentFiber.mu = mean(percentFiber.sp, na.rm = TRUE),
            percentAsh.mu = mean(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.mu = mean(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.mu = mean(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.mu = mean(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.mu = mean(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.mu = mean(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.mu = mean(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.mu = mean(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.mu = mean(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.mu = mean(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.mu = mean(daysToSilk.sp, na.rm = TRUE),
            anthesisSilkingInterval.mu = mean(anthesisSilkingInterval.sp, na.rm = TRUE),
            earHeight.max = max(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.max = max(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.max = max(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.max = max(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.max = max(earFillLength.sp, na.rm = TRUE),
            earWidth.max = max(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.max = max(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.max = max(shelledCobMass.sp, na.rm = TRUE),
            earLength.max = max(earLength.sp, na.rm = TRUE),
            kernelsPerEar.max = max(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.max = max(percentStarch.sp, na.rm = TRUE),
            percentProtein.max = max(percentProtein.sp, na.rm = TRUE),
            percentOil.max = max(percentOil.sp, na.rm = TRUE),
            percentFiber.max = max(percentFiber.sp, na.rm = TRUE),
            percentAsh.max = max(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.max = max(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.max = max(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.max = max(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.max = max(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.max = max(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.max = max(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.max = max(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.max = max(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.max = max(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.max = max(daysToSilk.sp, na.rm = TRUE),
            earHeight.min = min(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.min = min(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.min = min(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.min = min(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.min = min(earFillLength.sp, na.rm = TRUE),
            earWidth.min = min(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.min = min(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.min = min(shelledCobMass.sp, na.rm = TRUE),
            earLength.min = min(earLength.sp, na.rm = TRUE),
            kernelsPerEar.min = min(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.min = min(percentStarch.sp, na.rm = TRUE),
            percentProtein.min = min(percentProtein.sp, na.rm = TRUE),
            percentOil.min = min(percentOil.sp, na.rm = TRUE),
            percentFiber.min = min(percentFiber.sp, na.rm = TRUE),
            percentAsh.min = min(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.min = min(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.min = min(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.min = min(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.min = min(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.min = min(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.min = min(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.min = min(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.min = min(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.min = min(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.min = min(daysToSilk.sp, na.rm = TRUE))
summary.df <- full_join(summary.df, plasticity.df, join_by(genotype, location), keep = FALSE, suffix = c('', ''))
summary.df <- filter(summary.df, location!='Missouri Valley')
summary.df <- summary.df %>%
  mutate(across(where(is.numeric), ~na_if(., -Inf)))
# Export summary.df so we don't have to re-run FW regression
write.table(summary.df, 'analysis/genotypeSummaryByLocation.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE, na = '')

summary.df <- read.table('analysis/genotypeSummaryByLocation.tsv', sep = '\t', header = TRUE)
  
# Plot nitrogen plasticity by location vs. trait mean
for (i in 1:length(response_vars))
{
  response <- response_vars[i]
  response.mu <- paste0(response, '.mu')
  response.pl <- paste0(response, '.pl')
  plot.scatter <- ggplot(summary.df, aes(.data[[response.mu]], .data[[response.pl]])) +
    geom_point(color = '#00BFC4') + 
    geom_hline(yintercept = 1) +
    facet_wrap(vars(location)) + 
    labs(x = paste0('Mean ', response_labels[i]), y = paste0(response_labels[i], ' Nitrogen Plasticity')) + 
    theme(text = element_text(color = 'black', size = 16),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  print(plot.scatter)
  ggsave(filename = paste0('analysis/', response, 'PlasticityVsMean.png'), plot = plot.scatter)
}
  
# Correlation of plasticities by location

for(i in response_vars)
{
response.pl <- paste0(i, '.pl')
df <- summary.df %>%
  filter(!is.na(.data[[response.pl]])) %>%
  select(c(genotype, location, all_of(response.pl))) %>%
  pivot_wider(names_from = location, values_from = .data[[response.pl]]) %>%
  select(!genotype)
cp <- chart.Correlation(df, histogram = TRUE, method = 'pearson')
mtext(response.pl)
print(cp)

cm <- cor(df)
cp2 <- ggcorrplot(cm, title = response.pl)
print(cp2)
ggsave(paste0('analysis/', i, 'NitrogenPlasticityCorrelationAcrossLocs.png'), plot = cp2, width = 1745, height = 945, units = 'px')
}
  
# df.n <- filter(hybrids.vp, location!='Missouri Valley')
#   
# plasticity.yield <- summary.df %>%
#   group_by(location) %>%
#   summarise(max.pl = max(yieldPerAcre.sp, na.rm = TRUE),
#             min.pl = min(yieldPerAcre.sp, na.rm = TRUE))
#   qrs.h <- c()
#   qrs.l <- c()
#   for(i in c('North Platte1', 'North Platte2', 'North Platte3', 'Scottsbluff', 'Lincoln', 'Ames', 'Crawfordsville'))
#   {
#     vals.pl <- filter(plasticity.yield, location==i)
#     genotypes.h <- filter(summary.df, location==i & yieldPerAcre.sp==vals.pl$max.pl)
#     genotypes.h <- genotypes.h$genotype
#     genotypes.l <- filter(summary.df, location==i & yieldPerAcre.sp==vals.pl$min.pl)
#     genotypes.l <- genotypes.l$genotype
#     vals.h <- filter(df.n, location==i & genotype %in% genotypes.h)
#     qrs.h <- c(qrs.h, vals.h$qr)
#     vals.l <- filter(df.n, location==i & genotype %in% genotypes.l)
#     qrs.l <- c(qrs.l, vals.l$qr)
#   }
#   
#   pl.h <- filter(df.n, qr %in% qrs.h)
#   pl.l <- filter(df.n, qr %in% qrs.l)
#   df.n <- filter(df.n, !(qr %in% c(qrs.h, qrs.l)))
#   
#   df.n <- df.n %>%
#     group_by(location, genotype, poundsOfNitrogenPerAcre) %>%
#     summarise(yieldPerAcre.sp = mean(yieldPerAcre.sp, na.rm = TRUE))
#   pl.l <- pl.l %>%
#     group_by(location, genotype, poundsOfNitrogenPerAcre) %>%
#     summarise(yieldPerAcre.sp = mean(yieldPerAcre.sp))
#   pl.h <- pl.h %>%
#     group_by(location, genotype, poundsOfNitrogenPerAcre) %>%
#     summarise(yieldPerAcre.sp = mean(yieldPerAcre.sp))
#   write.table(df.n, 'analysis/yieldByN_locationAll.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
#   write.table(pl.l, 'analysis/yieldByN_location_LowPlasticity.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
#   write.table(pl.h, 'analysis/yieldByN_location_HighPlasticity.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
#   
#   
#   ggplot(df.n, aes(x = poundsOfNitrogenPerAcre, y = yieldPerAcre.sp, group = poundsOfNitrogenPerAcre, color = poundsOfNitrogenPerAcre)) +
#     geom_boxplot(fill = hue_pal()(21), color = 'black', outlier.color = 'black', notch = FALSE, na.rm = TRUE) +
#     geom_line(data = df.n, aes(group = genotype), color = 'grey', alpha = 0.25) +  
#     geom_line(data = pl.l, aes(group = genotype), color = '#377EB8', alpha = 0.5) +
#     geom_line(data = pl.h, aes(group = genotype), color = 'purple', alpha = 0.5) +
#     labs(x = 'Nitrogen Fertilizer (lbs/ac)', y = 'Yield (bu/ac)', title = 'Pair Plot with Box Plots and Connecting Lines') +
#     theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
#           axis.line = element_line(color = 'black', size = 0.5),  
#           legend.position = 'none') +
#     facet_wrap(vars(location))#+
#     # scale_x_continuous(limits = c(60, 85), breaks = seq(55, 85, by = 5)) +
#     # scale_y_continuous(limits = c(45, 105))  
#   
#   for (i in spatiallyCorrectedResponseVars)
#   {
#     location.trt <- unite(hybrids.vp, col = 'location.trt', c(location, poundsOfNitrogenPerAcre), na.rm = TRUE, sep = ';', remove = FALSE) %>%
#       filter(str_detect(location.trt, ';') & !is.na(.data[[i]]))
#       nlocation.trt <- unique(location.trt$location.trt) %>%
#       length()
#    p <- ggplot(hybrids.vp, aes(x = poundsOfNitrogenPerAcre, y = .data[[i]], group = poundsOfNitrogenPerAcre, color = poundsOfNitrogenPerAcre)) +
#       geom_boxplot(fill = hue_pal()(nlocation.trt), color = 'black', outlier.color = 'black', notch = FALSE, na.rm = TRUE) +
#       geom_line(data = hybrids.vp, aes(group = genotype), color = 'grey', alpha = 0.25) +  
#       labs(x = 'Nitrogen Fertilizer (lbs/ac)', y = i, title = 'Pair Plot with Box Plots and Connecting Lines') +
#       theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
#             axis.line = element_line(color = 'black', size = 0.5),  
#             legend.position = 'none') +
#       facet_wrap(vars(location))#+
#    print(p)
#   }
# Plasticities across locations: each nitrogen treatment within a locationation is an environment
# Create variable
locationTreatment.df <- hybrids.vp %>%
  unite('locationTreatment', c(location, nitrogenTreatment), sep = '.', remove = FALSE, na.rm = T) %>%
  rowwise() %>%
  mutate(locationTreatment = case_when(str_detect(locationTreatment, 'Border')|!str_detect(locationTreatment, '.') ~ NA, .default = locationTreatment)) %>%
  filter(!is.na(genotype) & nitrogenTreatment!='Border' & !is.na(locationTreatment) & genotype!='BORDER')
fw.1 <- FW(y = locationTreatment.df[[spatiallyCorrectedResponseVars[1]]], VAR = locationTreatment.df$genotype, ENV = locationTreatment.df$locationTreatment, 
           saveAt = paste0('analysis/gibbs-samples-allenv-', spatiallyCorrectedResponseVars[1]), 
           nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656)
pl.allenv <- fw.1$b %>%
  as_tibble(rownames = 'genotype') %>%
  mutate('{spatiallyCorrectedResponseVars[1]}':= Init1) %>%
  select(!Init1)

for(i in 2:length(spatiallyCorrectedResponseVars))
{
  fw <- FW(y = locationTreatment.df[[spatiallyCorrectedResponseVars[i]]], VAR = locationTreatment.df$genotype, ENV = locationTreatment.df$locationTreatment, 
           saveAt = paste0('analysis/gibbs-samples-allenv-', spatiallyCorrectedResponseVars[i]), 
           nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656)
  pl <- fw$b %>%
    as_tibble(rownames = 'genotype') %>%
    mutate('{spatiallyCorrectedResponseVars[i]}':= Init1) %>%
    select(!Init1)
  pl.allenv <- full_join(pl.allenv, pl, join_by(genotype), suffix = c('', ''), keep = FALSE)
}

colnames(pl.allenv) <- str_replace_all(colnames(pl.allenv), '.sp', '.pl')
# export pl.allenv
write.table(pl.allenv, 'analysis/PlasticityAcrossLocations.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
pl.allenv <- read.table('analysis/PlasticityAcrossLocations.tsv', sep = '\t', header = TRUE)
# get genotypes with the 20 highest and 20 lowest plasticity vals
high.plasticity <- pl.allenv %>%
  arrange(desc(yieldPerAcre.sp))
high.plasticity.genos <- high.plasticity$genotype[1:20]

low.plasticity <- pl.allenv %>%
  arrange(yieldPerAcre.sp)
low.plasticity.genos <- low.plasticity$genotype[1:20]

locationTreatment.df <- locationTreatment.df %>%
  mutate(locationTreatment = factor(locationTreatment, levels = c( 'Scottsbluff.Low', 'Scottsbluff.Medium', 'Scottsbluff.High', 'North Platte1.Low', 'North Platte1.Medium',
                                            'North Platte1.High', 'North Platte2.Low', 'North Platte2.Medium', 'North Platte2.High', 'North Platte3.Low', 
                                            'North Platte3.Medium', 'North Platte3.High', 'Lincoln.Low', 'Lincoln.Medium', 'Lincoln.High', 'Missouri Valley.Medium',
                                            'Ames.Low', 'Ames.Medium', 'Ames.High','Crawfordsville.Low', 'Crawfordsville.Medium', 'Crawfordsville.High')))



for(i in 1:length(response_vars))
{
  response.pl <- paste0(response_vars[i], '.pl')
  response.sp <- paste0(response_vars[i], '.sp')
  high.plasticity <- pl.allenv %>%
    arrange(desc(.data[[response.pl]]))
  high.plasticity.genos <- high.plasticity$genotype[1:20]
  
  low.plasticity <- pl.allenv %>%
    arrange(.data[[response.pl]])
  low.plasticity.genos <- low.plasticity$genotype[1:20]
  
  df.plot <- filter(locationTreatment.df, !is.na(.data[[response.sp]])) %>%
    group_by(genotype, locationTreatment) %>%
    summarise('{response.sp}' := mean(.data[[response.sp]], na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(relativePlasticity = case_when(genotype %in% high.plasticity.genos ~ 'High', 
                                     genotype %in% low.plasticity.genos ~ 'Low',
                                     .default = 'Medium'))
  orderedlocationTreatments <- df.plot %>%
    group_by(locationTreatment) %>%
    summarise(traitMean = mean(.data[[response.sp]])) %>%
    arrange(traitMean)
  orderedlocationTreatments <- orderedlocationTreatments$locationTreatment

  # df.plot <- df.plot %>%
  #   mutate(locationTreatment = factor(locationTreatment, levels = orderedlocationTreatments))
    
  p <- ggplot(df.plot, aes(x = locationTreatment, y = .data[[response.sp]], group = locationTreatment)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = '#00BFC4', na.rm = TRUE) +
    geom_line(data = df.plot, aes(group = genotype, color = relativePlasticity), alpha = 0.25) +  
    labs(x = 'Environment', y = response_labels[i]) +
    scale_color_manual(values = c('#C77CFF', '#F8766D', 'azure4')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.line = element_line(color = 'black', size = 1),  
          legend.position = 'none',
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA),
          panel.grid = element_blank(),
          text = element_text(size = 16))
  print(p)
  ggsave(paste0('analysis/', response_vars[i], 'ViolinWithRelativePlasticity.svg'), plot = p)
}

# yield.Ames <- locationTreatment.df %>%
#   filter(!is.na(yieldPerAcre.sp) & location=='Ames') %>%
#   group_by(locationTreatment, genotype) %>%
#   summarise(yieldPerAcre.sp = mean(yieldPerAcre.sp)) %>%
#   mutate(locationTreatment = factor(locationTreatment, levels = c('Ames.Medium', 'Ames.High', 'Ames.Low'))) %>%
#   ggplot(aes(locationTreatment, yieldPerAcre.sp, group = locationTreatment)) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = '#00BFC4', na.rm = TRUE) +
#   labs(x = 'Environment', y = 'Yield (Bushels/Acre)') +
#   theme(text = element_text(size = 16),
#         axis.line = element_line(color = 'black', size = 1),
#         legend.position = 'none', 
#         plot.background = element_blank(), 
#         panel.grid = element_blank())
# yield.Ames

summary.allenv <- hybrids.vp %>%
  group_by(genotype) %>%
  summarise(earHeight.mu = mean(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.mu = mean(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.mu = mean(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.mu = mean(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.mu = mean(earFillLength.sp, na.rm = TRUE),
            earWidth.mu = mean(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.mu = mean(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.mu = mean(shelledCobMass.sp, na.rm = TRUE),
            earLength.mu = mean(earLength.sp, na.rm = TRUE),
            kernelsPerEar.mu = mean(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.mu = mean(percentStarch.sp, na.rm = TRUE),
            percentProtein.mu = mean(percentProtein.sp, na.rm = TRUE),
            percentOil.mu = mean(percentOil.sp, na.rm = TRUE),
            percentFiber.mu = mean(percentFiber.sp, na.rm = TRUE),
            percentAsh.mu = mean(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.mu = mean(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.mu = mean(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.mu = mean(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.mu = mean(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.mu = mean(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.mu = mean(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.mu = mean(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.mu = mean(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.mu = mean(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.mu = mean(daysToSilk.sp, na.rm = TRUE),
            anthesisSilkingInterval.mu = mean(anthesisSilkingInterval.sp, na.rm = TRUE),
            earHeight.max = max(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.max = max(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.max = max(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.max = max(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.max = max(earFillLength.sp, na.rm = TRUE),
            earWidth.max = max(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.max = max(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.max = max(shelledCobMass.sp, na.rm = TRUE),
            earLength.max = max(earLength.sp, na.rm = TRUE),
            kernelsPerEar.max = max(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.max = max(percentStarch.sp, na.rm = TRUE),
            percentProtein.max = max(percentProtein.sp, na.rm = TRUE),
            percentOil.max = max(percentOil.sp, na.rm = TRUE),
            percentFiber.max = max(percentFiber.sp, na.rm = TRUE),
            percentAsh.max = max(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.max = max(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.max = max(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.max = max(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.max = max(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.max = max(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.max = max(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.max = max(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.max = max(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.max = max(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.max = max(daysToSilk.sp, na.rm = TRUE),
            anthesisSilkingInterval.max = max(anthesisSilkingInterval.sp, na.rm = TRUE),
            earHeight.min = min(earHeight.sp, na.rm = TRUE),
            flagLeafHeight.min = min(flagLeafHeight.sp, na.rm = TRUE),
            combineMoisture.min = min(combineMoisture.sp, na.rm = TRUE),
            combineTestWeight.min = min(combineTestWeight.sp, na.rm = TRUE),
            earFillLength.min = min(earFillLength.sp, na.rm = TRUE),
            earWidth.min = min(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.min = min(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobMass.min = min(shelledCobMass.sp, na.rm = TRUE),
            earLength.min = min(earLength.sp, na.rm = TRUE),
            kernelsPerEar.min = min(kernelsPerEar.sp, na.rm = TRUE),
            percentStarch.min = min(percentStarch.sp, na.rm = TRUE),
            percentProtein.min = min(percentProtein.sp, na.rm = TRUE),
            percentOil.min = min(percentOil.sp, na.rm = TRUE),
            percentFiber.min = min(percentFiber.sp, na.rm = TRUE),
            percentAsh.min = min(percentAsh.sp, na.rm = TRUE),
            yieldPerAcre.min = min(yieldPerAcre.sp, na.rm = TRUE),
            GDDToAnthesis.min = min(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.min = min(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.min = min(kernelsPerRow.sp, na.rm = TRUE),
            kernelRowNumber.min = min(kernelRowNumber.sp, na.rm = TRUE),
            moistureCorrectedKernelMassPerEar.min = min(moistureCorrectedKernelMassPerEar.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelMass.min = min(moistureCorrectedHundredKernelMass.sp, na.rm = TRUE),
            anthesisSilkingIntervalGDD.min = min(anthesisSilkingIntervalGDD.sp, na.rm = TRUE),
            daysToAnthesis.min = min(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.min = min(daysToSilk.sp, na.rm = TRUE),
            anthesisSilkingInterval.min = min(anthesisSilkingInterval.sp, na.rm = TRUE)) %>%
  filter(!is.na(genotype) & genotype!='BORDER') %>%
  full_join(pl.allenv, join_by(genotype), suffix = c('', ''), keep = FALSE) %>%
  mutate(across(where(is.numeric), ~na_if(., -Inf)))


# Tradeoff between plasticity and good performance - there doesn't seem to be one
for (i in 1:length(response_vars))
{
  response.sp <- paste0(response_vars[i], '.pl')
  response.mu <- paste0(response_vars[i], '.mu')
  response.max <- paste0(response_vars[i], '.max')
  response.min <- paste0(response_vars[i], '.min')
  label.sp <- 'Plasticity'
  label.mu <- paste0('Mean ', response_labels[i])
  label.max <- paste0('Maximum ', response_labels[i])
  label.min <- paste0('Minimum ', response_labels[i])
  
  p <- ggplot(summary.allenv) +
    geom_point(aes(.data[[response.sp]], .data[[response.max]], color = label.max)) + 
    geom_point(aes(.data[[response.sp]], .data[[response.mu]], color = label.mu)) +
    geom_point(aes(.data[[response.sp]], .data[[response.min]], color = label.min)) +
    scale_color_manual(name = NULL, 
                       labels = c(str_wrap(label.max, width = 10), str_wrap(label.mu, width = 10), str_wrap(label.min, width = 10)),
                       values = c('#F8766D', '#00BFC4', '#C77CFF'),
                       breaks = c(label.max, label.mu, label.min)) +
    labs(x = label.sp, y = response_labels[i]) +
    theme(text = element_text(color = 'black', size = 16),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  print(p)
  ggsave(paste0('analysis/', response_vars[i], 'PlasticityVsMeanMinMax.png'), plot = p)
}

for(i in 1:length(response_vars))
{
  response.mu <- paste0(response_vars[i], '.mu')
  meanLabel <- paste0('Mean ', response_labels[i])
  response.pl <- paste0(response_vars[i], '.pl')
  plasticityLabel <- paste0(response_labels[i], ' Linear Plasticity')
  
  p <- ggplot(summary.allenv, aes(.data[[response.mu]], .data[[response.pl]])) + 
    geom_point(color = '#00BFC4') +
    labs(x = meanLabel, y = plasticityLabel) + 
    theme(text = element_text(color = 'black', size = 16),
          axis.line = element_line(color = 'black', size = 1),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = 'right',
          legend.background = element_rect(color = 'black'))
  print(p)
  
  ggsave(paste0('analysis/', response_vars[i], 'PlasticityAcrossLocationsVsMean.png'), plot = p)
}

# unl_phenos <- c('earFillLength', 'earWidth', 'shelledCobWidth', 'shelledCobMass', 'earLength', 'kernelsPerEar',
#                 'percentStarch', 'percentProtein', 'percentOil', 'percentFiber', 'percentAsh', 
#                 'kernelsPerRow', 'kernelRowNumber', 'moistureCorrectedKernelMassPerEar', 'moistureCorrectedHundredKernelMass')
# rm_phenos <- c('earHeight', 'flagLeafHeight', 'GDDToAnthesis', 'GDDToSilk', 'anthesisSilkingIntervalGDD')
# dp_phenos <- c('combineMoisture', 'combineTestWeight','combineYield')
# response_vars.sb <- response_vars[c(1:5, 7:25)]
# # Look at scottsbluff
# sb <- filter(hybrids, location=='Scottsbluff' & genotype!='BORDER') %>%
#   pivot_longer(all_of(response_vars.sb), names_to = 'var', values_to = 'val') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')),
#          source = case_when(var %in% c(unl_phenos, paste0(unl_phenos, '.sp')) ~ 'Chidu/Lina',
#                             var %in% c(rm_phenos, paste0(rm_phenos, '.sp')) ~ 'Ramesh',
#                             var %in% c(dp_phenos, paste0(dp_phenos, '.sp')) ~ 'Dipak')) %>%
#   select(genotype, var, nitrogenTreatment, source, val)
# 
# plot.raw <- sb %>%
#   group_by(genotype, var, nitrogenTreatment, source) %>%
#   summarise(val = mean(val)) %>%
#   ggplot(aes(nitrogenTreatment, val)) + 
#   geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
#   geom_line(color = 'darkgrey') +
#   facet_wrap(vars(var), scales = 'free_y') +
#   labs(title = 'Mean of Raw Phenotype Values', x = 'Nitrogen Level')
# plot.raw
# 
# plot.sp <- filter(sb, str_detect(var, '.sp')) %>%
#   group_by(genotype, var, nitrogenTreatment, source) %>%
#   summarise(val = mean(val)) %>%
#   ggplot(aes(nitrogenTreatment, val)) + 
#   geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
#   geom_line(color = 'darkgrey') +
#   facet_wrap(vars(var), scales = 'free_y') +
#   labs(title = 'Mean of Spatially-Corrected Phenotype Values', x = 'Nitrogen Level')
# plot.sp
# 
# # Check correlations of genotypic reps within a treatment 
# ## Pivot
# hybrids.wide <- hybrids.vp %>%
#   filter(nitrogenTreatment!='Border' & !is.na(genotype)) %>%
#   group_by(genotype, nitrogenTreatment, location) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of('combineMoisture'), names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# ## Get plots of r1 & r2
# for (i in response_vars)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   
#   p <- ggplot(hybrids.wide, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) + 
#     geom_point() + 
#     facet_wrap(vars(location))
#   print(p)
# }
# # For Scottsbluff only:
# for (i in 'combineMoisture')
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   
#   p <- hybrids.wide %>%
#     filter(location=='North Platte1') %>%
#     ggplot(aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) + 
#     geom_point() + 
#     facet_wrap(vars(nitrogenTreatment)) + 
#     labs(subtitle = paste0('R = ', cor(np1.hips[[rep1]], np1.hips[[rep2]], use = 'complete.obs')))
#   print(p)
# }
# np1.hips <- filter(hybrids.wide, location=='North Platte1')
# sb.hips <- filter(hybrids.wide, location=='Scottsbluff')

# # Test theory that the row numbers inc W to E in Scottsbluff
# sb <- hybrids %>%
#   filter(location=='Scottsbluff') %>%
#   select(!c(contains('combine'), contains('harvest'), contains('Ht'), contains('GDD'), 
#             contains('anthesis'), contains('silk'), contains('anthesisSilkingInterval'), contains('.sp')))
# sb.combine2 <- sb_combine %>%
#   rowwise() %>%
#   mutate(row = case_when(row %in% 8:14 ~ row + 3,
#                          row > 14 ~ row + 5, 
#                          .default = row),
#          range = range + 2)
# 
# sb.data2 <- full_join(sb.combine2, sb_h_ft, join_by(plot, location, sublocation, irrigationProvided, population), 
#                       suffix = c('', ''), keep = FALSE)
# sb.data2 <- full_join(sb.data2, sb_h_ht, join_by(plot, location, sublocation, irrigationProvided, population), 
#                       suffix = c('', ''), keep = FALSE)
# sb.test <- full_join(sb, sb.data2, join_by(range, row, location, sublocation, irrigationProvided, population))
# sb.test <- sb.test %>%
#   filter(!is.na(genotype) & genotype!='BORDER') %>%
#   rowwise() %>%
#   mutate(plot = plot.x)
# sb.repcorr <- sb.test %>%
#   filter(nitrogenTreatment!='Border') %>%
#   group_by(genotype, nitrogenTreatment, location) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(earFillLength, earWidth, earLength, shelledCobWidth, shelledCobMass, kernelsPerEar, kernelMass, 
#                  kernelsPerRow, kernelRowNumber, hundredKernelMass, percentStarch, percentProtein, percentOil, percentFiber,
#                  percentAsh, percentMoisture, moistureCorrectedKernelMassPerEar, moistureCorrectedHundredKernelMass, combineYield, combineMoisture, 
#                  combineTestWeight, earHeight, flagLeafHeight), 
#                names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# sb.test_phenos <- c('earFillLength', 'earWidth', 'earLength', 'shelledCobWidth', 'shelledCobMass', 'kernelsPerEar', 'kernelMass', 
#                     'kernelsPerRow', 'kernelRowNumber', 'hundredKernelMass', 'percentStarch', 'percentProtein', 
#                     'percentOil', 'percentFiber', 'percentAsh', 'percentMoisture', 'moistureCorrectedKernelMassPerEar',
#                     'moistureCorrectedHundredKernelMass', 'combineYield', 'combineMoisture', 
#                     'combineTestWeight', 'earHeight', 'flagLeafHeight')
# for(i in sb.test_phenos)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(sb.repcorr, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) +
#     geom_point() +
#     facet_wrap(vars(nitrogenTreatment)) +
#     labs(title = 'Scottsbluff Corrected')
#   print(p)
# }
# 
# # check the qrs for sb have the correct genotype
# sb.index <- read_excel('data/Scottsbluff Hybrid HIPS - Summary.xlsx', sheet = 'Index (Original)')
# sb.index <- sb.index[, c(1, 5, 7)]
# colnames(sb.index) <- c('plot', 'genotype', 'seedFillNote')
# sb.index <- sb.index %>%
#   group_by(plot, genotype) %>%
#   summarise(seedFillNote = max(seedFillNote, na.rm = TRUE))
# sb.index <- full_join(sb.index, sb, join_by(plot), keep = FALSE, suffix = c('.index', '.qr')) %>%
#   select(c(plot, genotype.qr, genotype.index)) %>%
#   filter(genotype.qr!='BORDER')
# sb.index <- sb.index %>% 
#   mutate(genotype.index = str_to_upper(genotype.index),
#          match = case_when(genotype.index==genotype.qr ~ TRUE, 
#                            .default = FALSE)) 
# # the qrs match the index we have from lisa
# # check if the sublocation was planted starting in the SW corner but labeled from the SE corner
# # then reassign qrs to pair plot numbers correctly (and thus genotypes correctly)
# sb.sw <- sb #%>%
#   filter(!c(plot %in% c(1021:1025, 1191:1195, 1361:1365))) %>%
#   rowwise() %>%
#   mutate(plot = case_when(row==26 ~ plot + 490,
#                           row==25 ~ plot + 440,
#                           row==24 ~ plot + 390,
#                           row==23 ~ plot + 340,
#                           row==22 ~ plot + 290,
#                           row==21 ~ plot + 240,
#                           row==20 ~ plot + 190,
#                           row==17 ~ plot + 150,
#                           row==16 ~ plot + 100,
#                           row==15 ~ plot + 50,
#                           row==14 ~ plot,
#                           row==13 ~ plot - 50,
#                           row==12 ~ plot - 100,
#                           row==11 ~ plot - 150,
#                           row==7 ~ plot - 190,
#                           row==6 ~ plot - 240,
#                           row==5 ~ plot - 290,
#                           row==4 ~ plot - 340,
#                           row==3 ~ plot - 390,
#                           row==2 ~ plot - 440,
#                           row==1 ~ plot - 490)) %>%
#   filter(!is.na(plot)) %>%
#   select(!c(genotype, qr))
# sb.qrs <- sb %>%
#   select(qr, genotype, plot) %>% 
#   filter(!is.na(plot))
# sb.sw <- full_join(sb.sw, sb.qrs, join_by(plot), keep = FALSE, suffix = c('', '')) 
# 
# sb.sw.repcorr <- sb.sw %>%
#   group_by(genotype, nitrogenTreatment, location) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of(unl_phenos), 
#                names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   filter(!is.na(val)) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# for(i in unl_phenos[1:length(unl_phenos)])
# {
#   rep1 <- paste0(i, '.1')
#   print(i)
#   rep2 <- paste0(i, '.2')
#   
#   p <- ggplot(sb.sw.repcorr, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) +
#     geom_point() + 
#     facet_wrap(vars(nitrogenTreatment)) +
#     labs(title = 'Scottsbluff SW Plant Start')
#   print(p)
#   
#   print(cor(sb.sw.repcorr[[rep1]], sb.sw.repcorr[[rep2]], use = 'complete.obs'))
# }
# 
# # Check correlation of traits collected at UNL for sites other than SB + MV
# corr.df <- filter(hybrids.wide, location!='Scottsbluff' & location!='Missouri Valley')
# 
# for(i in unl_phenos)
# {
#   print(i)
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(cor(corr.df[[rep1]], corr.df[[rep2]], use = 'complete.obs'))
# }
# 
# np1.df <- filter(hybrids.wide, location=='North Platte1')
# 
# for(i in unl_phenos)
# {
#   print(i)
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(cor(sb.repcorr[[rep1]], sb.repcorr[[rep2]], use = 'complete.obs'))
# }
# 
# sb.combine <- sb_combine %>%
#   rowwise() %>%
#   mutate(plot = case_when(population=='Hybrid' & !c(plot %in% c(1021:1025)) & row==26 ~ plot + 490,
#          population=='Hybrid' & row==25 ~ plot + 440,
#          population=='Hybrid' & row==24 ~ plot + 390,
#          population=='Hybrid' & row==23 ~ plot + 340,
#          population=='Hybrid' & row==22 ~ plot + 290,
#          population=='Hybrid' & row==21 ~ plot + 240,
#          population=='Hybrid' & row==20 ~ plot + 190,
#          population=='Hybrid' & !c(plot %in% c(1191:1195)) & row==17 ~ plot + 150,
#          population=='Hybrid' & row==16 ~ plot + 100,
#          population=='Hybrid' & row==15 ~ plot + 50,
#          population=='Hybrid' & row==14 ~ plot,
#          population=='Hybrid' & row==13 ~ plot - 50,
#          population=='Hybrid' & row==12 ~ plot - 100,
#          population=='Hybrid' & row==11 ~ plot - 150,
#          population=='Hybrid' & !c(plot %in% c(1361:1365)) & row==7 ~ plot - 190,
#          population=='Hybrid' & row==6 ~ plot - 240,
#          population=='Hybrid' & row==5 ~ plot - 290,
#          population=='Hybrid' & row==4 ~ plot - 340,
#          population=='Hybrid' & row==3 ~ plot - 390,
#          population=='Hybrid' & row==2 ~ plot - 440,
#          population=='Hybrid' & row==1 ~ plot - 490,
#          .default = plot)) %>%
#   mutate(plot = case_when(population=='Hybrid' & row==1 & range==23 ~ 1021,
#                           population=='Hybrid' & row==1 & range==24 ~ 1022,
#                           population=='Hybrid' & row==1 & range==25 ~ 1023,
#                           population=='Hybrid' & row==1 & range==26 ~ 1024,
#                           population=='Hybrid' & row==1 & range==27 ~ 1025,
#                           population=='Hybrid' & row==20 & range==23 ~ 1361,
#                           population=='Hybrid' & row==20 & range==24 ~ 1362,
#                           population=='Hybrid' & row==20 & range==25 ~ 1363, 
#                           population=='Hybrid' & row==20 & range==26 ~ 1364,
#                           population=='Hybrid' & row==20 & range==27 ~ 1365,
#                           population=='Hybrid' & row==11 & range==23 ~ 1191,
#                           population=='Hybrid' & row==11 & range==24 ~ 1192,
#                           population=='Hybrid' & row==11 & range==25 ~ 1193,
#                           population=='Hybrid' & row==11 & range==26 ~ 1194,
#                           population=='Hybrid' & row==11 & range==27 ~ 1195,
#                           population=='Hybrid' & row==26 & range %in% 23:27 ~ NA,
#                           population=='Hybrid' & row==17 & range %in% 23:27 ~ NA,
#                           population=='Hybrid' & row==7 & range %in% 23:27 ~ NA,
#                           .default = plot))
# 
# sb.sw.all <- full_join(sb.sw, sb_combine, join_by(plot), suffix = c('', '.yield'), keep = FALSE) %>%
#   select(!ends_with('.yield'))
# sb.sw.all <- full_join(sb.sw.all, sb_h_ft, join_by(plot), suffix = c('', '.ft'), keep = FALSE) %>%
#   select(!ends_with('.ft'))
# sb.sw.all <- full_join(sb.sw.all, sb_h_ht, join_by(plot), suffix = c('', '.ht'), keep = FALSE) %>%
#   select(!ends_with('.ht'))
# 
# sb.sw.all.vars <- c(response_vars.sb[c(1:16, 20:23)], 'combineYield', 'combineMoisture', 'combineTestWeight')
# sb.sw.long <- sb.sw.all %>%
#   pivot_longer(any_of(sb.sw.all.vars), names_to = 'var', values_to = 'val') %>%
#   mutate(nitrogenTreatment = factor(nitrogenTreatment, levels = c('Low', 'Medium', 'High')),
#          source = case_when(var %in% c(unl_phenos, paste0(unl_phenos, '.sp')) ~ 'Chidu/Lina',
#                             var %in% c(rm_phenos, paste0(rm_phenos, '.sp')) ~ 'Ramesh',
#                             var %in% c(dp_phenos, paste0(dp_phenos, '.sp')) ~ 'Dipak')) %>%
#   group_by(genotype, var, nitrogenTreatment) %>%
#   mutate(rep = 1:n()) %>%
#   select(genotype, var, nitrogenTreatment, source, val, rep)
# 
# plot.raw <- sb.sw.long %>%
#   filter(!is.na(nitrogenTreatment)) %>%
#   group_by(genotype, var, nitrogenTreatment, source) %>%
#   summarise(val = mean(val)) %>%
#   ggplot(aes(nitrogenTreatment, val)) + 
#   geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
#   geom_line(color = 'darkgrey') +
#   facet_wrap(vars(var), scales = 'free_y') +
#   labs(title = 'Mean of Raw Phenotype Values', x = 'Nitrogen Level')
# plot.raw
# 
# sb.sw.wide <- pivot_wider(sb.sw.long, 
#                           id_cols = c(nitrogenTreatment, genotype), 
#                           names_from = c(var, rep), 
#                           values_from = val, 
#                           names_sep = '.')
# for(i in sb.sw.all.vars)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(sb.sw.wide, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) +
#     geom_point() +
#     facet_wrap(vars(nitrogenTreatment))
#   print(p)
#   print(i)
#   print(cor(sb.sw.wide[[rep1]], sb.sw.wide[[rep2]], use = 'complete.obs'))
# }
# 
# plotRepCorr(sb.sw.all, 'nitrogenTreatment', 'genotype', sb.sw.all.vars, 'location')
# 
# np1.df <- hybrids.vp %>%
#   filter(nitrogenTreatment!='Border' & !is.na(genotype) & location=='North Platte1') %>%
#   group_by(genotype, nitrogenTreatment, location) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(combineYield, combineMoisture, combineTestWeight, earHeight, flagLeafHeight), 
#                names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
#   
# for(i in c('combineYield', 'combineMoisture', 'combineTestWeight', 'earHeight', 'flagLeafHeight'))
# {
#   print(i)
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(cor(sb.sw.wide[[rep1]], sb.sw.wide[[rep2]], use = 'complete.obs'))
# }
# 
# # Now onto figuring out what is up with the MV data
# ## Let's look at the height data, but not flip the reps-- maybe the sublocation was planted with rep2 and rep1's locationations flipped & Lisa knew but forgot to mention
# mv.plantData.hyb <- read_excel('data/Plant_data_MO_Valley_2022.xlsx', 
#                                sheet = '4211', 
#                                col_names = c('row', 'range', 'flagLeafHeight', 'earHeight', 'rep', 'plot', 'genotype'),
#                                col_types = c('skip', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
#                                skip = 1)
# mv.plantData.hyb <- mv.plantData.hyb %>%
#   rowwise() %>%
#   mutate(plotNumber = case_when(rep==1 ~ plotNumber + 100,
#                           rep==2 ~ plotNumber + 200,
#                           .default = plotNumber),
#          genotype = str_to_upper(genotype),
#          location = 'Missouri Valley',
#          sublocation = 'Hybrid HIPS',
#          nitrogenTreatment = 'Medium',
#          irrigationProvided = 'Dryland',
#          population = 'Hybrid',
#          flagLeafHeight = case_when(flagLeafHeight=='n/a - solar' ~ NA, .default = flagLeafHeight),
#          earHeight = case_when(earHeight=='n/a - solar' ~ NA, .default = earHeight)) %>%
#   fixGenos(hips1.5_genoFixKey)
# 
# # Pivot wide
# mv.ht.wide <- mv.plantData.hyb %>%
#   group_by(genotype) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(earHeight, flagLeafHeight), 
#                names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# for(i in c('earHeight', 'flagLeafHeight'))
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(mv.ht.wide, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) +
#     geom_point()
#   print(p)
#   print(i)
#   print(cor(mv.ht.wide[[rep1]], mv.ht.wide[[rep2]], use = 'complete.obs'))
# }
# # What happens if we bind UNL data to plant data by range & row & use plot numbers and genotype info from plant data?
# mv <- hybrids %>%
#   filter(location=='Missouri Valley') %>%
#   select(c(any_of(unl_phenos), 'qr', 'plotNumber', 'rep', 'range', 'row', 'genotype'))
# mv.unl.ht <- full_join(mv, mv.plantData.hyb, join_by(range, row), suffix = c('.unl', '.ht'), keep = FALSE) %>%
#   mutate(plotNumber = plotNumber.ht,
#          genotype = genotype.ht,
#          rep = rep.ht) %>%
#   select(!c(contains('.unl'), contains('.ht'))) %>%
#   filter(!is.na(row)|!is.na(range))
# 
# mv.unl.ht.wide <- mv.unl.ht %>%
#   group_by(genotype) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of(unl_phenos), 
#                names_to = 'var', values_to = 'val') %>%
#   select(location, genotype, rep, nitrogenTreatment, val, var) %>%
#   pivot_wider(id_cols = c(location, nitrogenTreatment, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# 
# for(i in unl_phenos)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(mv.unl.ht.wide, aes(.data[[rep1]], .data[[rep2]], color = nitrogenTreatment)) +
#     geom_point()
#   print(p)
#   print(i)
#   print(cor(mv.unl.ht.wide[[rep1]], mv.unl.ht.wide[[rep2]], use = 'complete.obs'))
# }
# 
# for (i in unl_phenos)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(i)
#   df <- hybrids.wide %>%
#     filter(location=='Missouri Valley')
#   print(cor(df[[rep1]], df[[rep2]], use = 'complete.obs'))
# } 
# # Some really good correlations between genotypic reps & some really bad ones -- is this something wrong or is it a feature of this data?
# # Okay, now what if we bind the combine data in by range and row
# mv.all <- full_join(mv.unl.ht, mv_hyb, join_by(range, row)) %>%
#   mutate(location = 'Missouri Valley',
#          sublocation = 'Hybrid HIPS')
# mv.all.wide <- mv.all %>%
#   group_by(genotype) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of(c(dp_phenos, 'earHeight', 'flagLeafHeight', unl_phenos)), 
#                names_to = 'var', values_to = 'val') %>%
#   select(genotype, val, var, rep) %>%
#   pivot_wider(id_cols = c(genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# # Low corrs here possibly due to outliers
# for (i in c(dp_phenos, 'earHeight', 'flagLeafHeight', unl_phenos))
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(mv.all.wide, aes(.data[[rep1]], .data[[rep2]])) +
#     geom_point() + 
#     labs(title = 'Missouri Valley Corrected', subtitle = cor(mv.all.wide[[rep1]], mv.all.wide[[rep2]], use = 'complete.obs'))
#   print(p)
#   print(i)
#   print(cor(mv.all.wide[[rep1]], mv.all.wide[[rep2]], use = 'complete.obs'))
# }
# 
# # how was the combine info correct but not the rest? this is my own curiosity
# mv.c.qr <- full_join(mv, mv_hyb, join_by(range, row), keep = FALSE, suffix = c('.unl', '.c'))
# 
# ames.hyb <- filter(hybrids, location=='Ames')
# ames.hyb.genoLvl <- ames.hyb %>%
#   group_by(genotype, nitrogenTreatment) %>%
#   summarise(yieldPerAcre = mean(yieldPerAcre)) %>%
#   pivot_wider(names_from = nitrogenTreatment, values_from = yieldPerAcre) %>%
#   mutate(oppositeNResponse = case_when(Low > High ~ TRUE, .default = FALSE))
# ames.hyb.check <- filter(ames.hyb, genotype %in% c('F42 X MO17', 'B73 X PHZ51', 'LH185 X LH82', 'PHP02 X PHJ89'))

# Function to plot correlation of the first 2 reps of a genotype within a treatment
# Data is the data frame, subset as desired. This should be an object in your R environment
# Treatment Var may be nitrogen level, locationation, year, etc. Each unique combination of these and genotype will be a point in the plot. This should be a string
# Genotype is the name of the genotype column. This should be a string.
# Phenotypes is a string vector containing the names of the phenotype columns to plot.
# Note: in the case genotype(s) have more than 2 replicates, only 2 of the replicates are used here
# Facet is a string specifying the variable to facet wrap the plot by



# mapResponse(mv.all, 'combineYield')

# # How many hybrids do we have both parents for? -- 18
# inbreds <- filter(hips, population=='Inbred')
# inbreds.genos <- unique(inbreds$genotype)
# hybrid.genos <- tibble(hybrid = unique(hybrids$genotype))
# hybrid.genos <- hybrid.genos %>%
#   rowwise() %>%
#   mutate(P1 = str_split_i(hybrid, ' X ', 1),
#          P2 = str_split_i(hybrid, ' X ', 2), 
#          P1InPanel = P1 %in% inbreds.genos,
#          P2InPanel = P2 %in% inbreds.genos,
#          BothInPanel = case_when(P1InPanel & P2InPanel ~ TRUE, .default = FALSE))
# sum(hybrid.genos$BothInPanel)
