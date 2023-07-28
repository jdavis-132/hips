library(lme4)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
library(scales)
library(FW)
# Read in data and change NP so all NP fields have the same loc but different fields --> range/row are unique within a field
hips <- read.table('outData/HIPS_2022_V3.tsv', header = TRUE, sep = '\t')
hybrids <-  filter(hips, population == 'Hybrid') %>%
  mutate(field = case_when(loc=='North Platte1' ~ 'Full Irrigation',
                           loc=='North Platte2' ~ 'Partial Irrigation',
                           loc=='North Platte3' ~ 'Dryland',
                           .default = field))
# Let's look at how yield varies across fields
yieldMap <- ggplot(hybrids, aes(range, row, fill = combineYield, color = 'white')) +
  geom_raster() +
  facet_wrap(vars(loc, field)) + 
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
    facet_wrap(vars(loc, field)) + 
    scale_x_continuous(breaks = 0:40) +
    scale_y_continuous(breaks = 0:40) +
    scale_fill_viridis(option = 'turbo', direction = -1) +
    theme_minimal() + 
    theme(axis.text = element_text(angle = 45))
  print(plot)
}

response_vars <- c('earHt', 'flagLeafHt', 'plantHt', 'combineMoisture', 'combineTestWt', 
                   'earLen', 'earFillLen', 'earWidth', 'shelledCobWidth', 'shelledCobWt', 'shelledCobLen', 'kernelsPerEar',
                   'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh', 
                   'yieldPerAc', 'daysToAnthesis', 'daysToSilk', 
                   'kernelsPerRow', 'kernelRows', 'moistureCorrectedKernelMass', 'moistureCorrectedHundredKernelWt', 'pctMoistureNIR')
mapResponse(hybrids, 'kernelMass')
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
for (i in response_vars)
{
  outliers[[i]] <- idOutliers(hybrids, i)
}
# Cast nLvl, genotype, and irrigation as factors
#hybrids <- mutate(hybrids, across(c(nLvl, genotype, irrigation, loc), as.factor))

# # Function to do variance partitioning for each location for a given response variable
# partitionVariance <- function(data, response)
# {
#   # Loop over locations
#   locs <- c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3')
#   vc.df <- tibble(modelTerm = NULL, Variance = NULL, SD = NULL, `log10l(lambda)` = NULL, pctVar = NULL, loc = NULL, .rows = 0) 
#   for(i in locs)
#   {
#     loc.df <- filter(data, loc==i & !is.na(range) & !is.na(row) & !is.na(.data[[response]]))
#     print(length(loc.df$plot))
#     if(length(loc.df$plot)==0)
#     {
#       next
#     }
#     # Fit model
#     rangeKnots <- floor(max(loc.df$range, na.rm = TRUE)/2) + 1
#     rowKnots <- floor(max(loc.df$row, na.rm = TRUE)/2) + 1
#     print(i)
#     print(rangeKnots)
#     print(rowKnots)
#     print(response)
#     if(i=='Missouri Valley')
#     {
#       model <- SpATS(response, genotype = 'genotype', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)),
#                      data = loc.df)
#     }
#     else
#     {
#       model <- SpATS(response, genotype = 'genotype', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)),
#                    random = ~ nLvl + nLvl:genotype, data = loc.df)
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
#              loc = i)
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
#     facet_wrap(vars(loc)) + 
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
#   filter(loc=='Lincoln' & nLvl=='Low') %>%
#   mutate(plot = as.factor(plot)) %>%
#   select(c(plot, range, row, yieldPerAc))
# m <- SpATS('yieldPerAc', genotype = 'plot', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(8, 14)), fixed = NULL, data = lnk_lowN)
# s <- summary(m)
# plot.SpATS(m)
# p.blups <- s$coeff %>%
#   as_tibble(rownames = 'plot')

# Okay, now let's write a function to get the spatial BLUES for each response on a plot level
# Will fit model by individual location, nitrogen treatment combination
getSpatialCorrections <- function(data, response)
{
  # Declare empty df and levels of locs
  df.sp <- tibble(loc = NULL, plot = NULL, '{response}':= NULL, nLvl = NULL)
  locs <-  c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Ames', 'Crawfordsville')
  # Loop over locations
  for(currLoc in locs)
  {
    loc.df <- filter(data, loc==currLoc & !is.na(row) & !is.na(range) & !is.na(.data[[response]] & !is.na(nLvl)))
    if(length(loc.df$plot)==0)
    {
      print(paste0('No data for ', response, ' at ', currLoc))
      next
    }
    nLvls <- unique(loc.df$nLvl)
      
    # Loop over nitrogen treatments
    for(currTrt in nLvls)
    {
      if(is.na(currTrt)|currTrt=='Border')
      {
        next
      }
      loc.n.df <- filter(loc.df, nLvl==currTrt) %>%
        mutate(as.factor(plot))
      rangeKnots <- floor(max(loc.n.df$range, na.rm = TRUE)/2) + 1
      rowKnots <- floor(max(loc.n.df$row, na.rm = TRUE)/2) + 1
      print(currLoc)
      print(currTrt)
      model <- SpATS(response, genotype = 'plot', genotype.as.random = TRUE, spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)), data = loc.n.df)
      # Plot model
      plot.SpATS(model, main = paste0(response, ':', currLoc, ':', currTrt))
      # Extract BLUPS
      summary <- summary(model)
      if(cor(loc.n.df[[response]], summary$fitted + summary$residuals) > 0.99)
      {
        sp <- tibble(loc = currLoc,
                     nLvl = currTrt, 
                     plot = loc.n.df$plot,
                     '{response}':=summary$fitted)
      }
      else
      {
        print(paste0('Fitted values misordered. r =', cor(loc.n.df[[response]], summary$fitted + summary$residuals), '; ', currLoc, '; ', currTrt))
        next
      }
      # Bind to df
      df.sp <- bind_rows(df.sp, sp) %>%
        mutate(plot = as.numeric(plot))
    }
  }
  print(length(df.sp$plot))
  # Return df
  return(df.sp)
}

for(i in response_vars)
{
  hybrids <- full_join(hybrids, getSpatialCorrections(hybrids, i), by = join_by(loc, plot, nLvl), suffix = c('', '.sp'), keep = FALSE)
}

hybrids <- hybrids %>%
  rowwise() %>%
  mutate(across(everything(), ~case_when(is.null(.) ~ NA, .default = .)))

partitionVariance2 <- function(df, response) 
{
  df <- filter(df, !is.na(response))
  lm_formula <- as.formula(paste(response, "~ (1|loc/nLvl) + (1|genotype) + (1|loc:genotype) + (1|nLvl:genotype)"))
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
#spatiallyCorrectedResponseVars <- paste0(spatiallyCorrectedResponseVars, '.blup')
# Don't use the spatially corrected vals when there's fitting issues
hybrids.vp <- hybrids %>%
  rowwise() %>%
  mutate(kernelRows.sp = case_when(loc=='North Platte1' ~ kernelRows, .default = kernelRows.sp),
         kernelsPerRow.sp = case_when(loc=='North Platte1' ~ kernelsPerRow, .default = kernelsPerRow.sp),
         moistureCorrectedFiber.sp = case_when(loc=='North Platte1' ~ moistureCorrectedFiber, .default = moistureCorrectedFiber.sp),
         moistureCorrectedProtein.sp = case_when(loc=='North Platte1' ~ moistureCorrectedProtein, .default = moistureCorrectedProtein.sp),
         earFillLen.sp = case_when(loc %in% c('North Platte1', 'North Platte3') ~ earFillLen, .default = earFillLen.sp),
         moistureCorrectedOil.sp = case_when(loc=='North Platte1' ~ moistureCorrectedOil, .default = moistureCorrectedOil.sp),
         moistureCorrectedHundredKernelWt.sp = case_when(loc=='North Platte1' ~ moistureCorrectedHundredKernelWt, .default = moistureCorrectedHundredKernelWt.sp),
         moistureCorrectedKernelMass.sp = case_when(loc=='North Platte1' ~ moistureCorrectedKernelMass, .default = moistureCorrectedKernelMass.sp))

for(i in spatiallyCorrectedResponseVars)
{
  vc_all <- bind_rows(vc_all, partitionVariance2(hybrids.vp, i))
}

vp.plot <- ggplot(vc_all, aes(responseVar, pctVar, fill = grp)) +
  geom_col(position = 'stack') + 
  scale_fill_viridis(option = 'turbo', discrete = TRUE) +
  labs(fill = 'Variance Component') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
vp.plot

# Is there shrinkage toward the max of a treatment ?
for(i in 1:length(response_vars))
{
  sp.correction.plot <- ggplot(hybrids.vp, (aes(.data[[response_vars[i]]], .data[[spatiallyCorrectedResponseVars[i]]], color = nLvl))) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(vars(loc)) 
  print(sp.correction.plot)
}

getNitrogenPlasticityByLoc <- function(data, response)
{
  locs <- c('Ames', 'Crawfordsville', 'Lincoln', 'North Platte1', 'North Platte2', 'North Platte3', 'Scottsbluff')
  response.df <- tibble(loc = NULL, genotype = NULL, '{response}':= NULL,)
  for (currLoc in locs)
  {
    loc.df <- filter(data, !is.na(genotype) & loc==currLoc & nLvl!='Border' & !is.na(nLvl))
    fw <- FW(y = loc.df[[response]], VAR = loc.df$genotype, ENV = loc.df$nLvl, saveAt = paste0('analysis/gibbs-samples-', response, '-', currLoc), 
             nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656)
    pl <- fw$b %>%
      as_tibble(rownames = 'genotype') %>%
      mutate(loc = currLoc, '{response}':= Init1) %>%
      select(!Init1)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

plasticity.df <- getNitrogenPlasticityByLoc(hybrids.vp, spatiallyCorrectedResponseVars[1])
for(i in spatiallyCorrectedResponseVars[2:25])
{
  plasticity.df <- full_join(plasticity.df, getNitrogenPlasticityByLoc(hybrids.vp, i), join_by(genotype, loc), suffix = c('', ''), keep = FALSE)
}

summary.df <- hybrids.vp %>%
  group_by(loc, genotype) %>%
  summarise(earHt.mu = mean(earHt.sp, na.rm = TRUE),
            flagLeafHt.mu = mean(flagLeafHt.sp, na.rm = TRUE),
            plantHt.mu = mean(plantHt.sp, na.rm = TRUE),
            combineMoisture.mu = mean(combineMoisture.sp, na.rm = TRUE),
            combineTestWt.mu = mean(combineTestWt.sp, na.rm = TRUE),
            earLen.mu = mean(earLen.sp, na.rm = TRUE),
            earFillLen.mu = mean(earFillLen.sp, na.rm = TRUE),
            earWidth.mu = mean(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.mu = mean(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobWt.mu = mean(shelledCobWt.sp, na.rm = TRUE),
            shelledCobLen.mu = mean(shelledCobLen.sp, na.rm = TRUE),
            kernelsPerEar.mu = mean(kernelsPerEar.sp, na.rm = TRUE),
            moistureCorrectedStarch.mu = mean(moistureCorrectedStarch.sp, na.rm = TRUE),
            moistureCorrectedProtein.mu = mean(moistureCorrectedProtein.sp, na.rm = TRUE),
            moistureCorrectedOil.mu = mean(moistureCorrectedOil.sp, na.rm = TRUE),
            moistureCorrectedFiber.mu = mean(moistureCorrectedFiber.sp, na.rm = TRUE),
            moistureCorrectedAsh.mu = mean(moistureCorrectedAsh.sp, na.rm = TRUE),
            yieldPerAc.mu = mean(yieldPerAc.sp, na.rm = TRUE),
            daysToAnthesis.mu = mean(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.mu = mean(daysToSilk.sp, na.rm = TRUE),
            kernelsPerRow.mu = mean(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.mu = mean(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.mu = mean(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.mu = mean(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            pctMoistureNIR.mu = mean(pctMoistureNIR.sp, na.rm = TRUE),
            earHt.max = max(earHt.sp, na.rm = TRUE),
            flagLeafHt.max = max(flagLeafHt.sp, na.rm = TRUE),
            plantHt.max = max(plantHt.sp, na.rm = TRUE),
            combineMoisture.max = max(combineMoisture.sp, na.rm = TRUE),
            combineTestWt.max = max(combineTestWt.sp, na.rm = TRUE),
            earLen.max = max(earLen.sp, na.rm = TRUE),
            earFillLen.max = max(earFillLen.sp, na.rm = TRUE),
            earWidth.max = max(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.max = max(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobWt.max = max(shelledCobWt.sp, na.rm = TRUE),
            shelledCobLen.max = max(shelledCobLen.sp, na.rm = TRUE),
            kernelsPerEar.max = max(kernelsPerEar.sp, na.rm = TRUE),
            moistureCorrectedStarch.max = max(moistureCorrectedStarch.sp, na.rm = TRUE),
            moistureCorrectedProtein.max = max(moistureCorrectedProtein.sp, na.rm = TRUE),
            moistureCorrectedOil.max = max(moistureCorrectedOil.sp, na.rm = TRUE),
            moistureCorrectedFiber.max = max(moistureCorrectedFiber.sp, na.rm = TRUE),
            moistureCorrectedAsh.max = max(moistureCorrectedAsh.sp, na.rm = TRUE),
            yieldPerAc.max = max(yieldPerAc.sp, na.rm = TRUE),
            daysToAnthesis.max = max(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.max = max(daysToSilk.sp, na.rm = TRUE),
            kernelsPerRow.max = max(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.max = max(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.max = max(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.max = max(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            pctMoistureNIR.max = max(pctMoistureNIR.sp, na.rm = TRUE))
summary.df <- full_join(summary.df, plasticity.df, join_by(genotype, loc), keep = FALSE, suffix = c('', ''))
summary.df <- filter(summary.df, loc!='Missouri Valley')
summary.df <- summary.df %>%
  rowwise() %>%
  mutate(earHt.pl = earHt.sp + earHt.mu,
         flagLeafHt.pl = flagLeafHt.sp + flagLeafHt.mu,
         plantHt.pl = plantHt.sp + plantHt.mu,
         combineMoisture.pl = combineMoisture.sp + combineMoisture.mu,
         combineTestWt.pl = combineTestWt.sp + combineTestWt.mu,
         earLen.pl = earLen.sp + earLen.mu,
         earFillLen.pl = earFillLen.sp + earFillLen.mu,
         earWidth.pl = earWidth.sp + earWidth.mu,
         shelledCobWidth.pl = shelledCobWidth.sp + shelledCobWidth.mu,
         shelledCobWt.pl = shelledCobWt.sp + shelledCobWt.mu,
         shelledCobLen.pl = shelledCobLen.sp + shelledCobLen.mu,
         kernelsPerEar.pl = kernelsPerEar.sp + kernelsPerEar.mu,
         moistureCorrectedStarch.pl = moistureCorrectedStarch.sp + moistureCorrectedStarch.mu,
         moistureCorrectedProtein.pl = moistureCorrectedProtein.sp + moistureCorrectedProtein.mu,
         moistureCorrectedOil.pl = moistureCorrectedOil.sp + moistureCorrectedOil.mu,
         moistureCorrectedFiber.pl = moistureCorrectedFiber.sp + moistureCorrectedFiber.mu,
         moistureCorrectedAsh.pl = moistureCorrectedAsh.sp + moistureCorrectedAsh.mu,
         yieldPerAc.pl = yieldPerAc.sp + yieldPerAc.mu,
         daysToAnthesis.pl = daysToAnthesis.sp + daysToAnthesis.mu,
         daysToSilk.pl = daysToSilk.sp + daysToSilk.mu,
         kernelsPerRow.pl = kernelsPerRow.sp + kernelsPerRow.mu,
         kernelRows.pl = kernelRows.sp + kernelRows.mu,
         moistureCorrectedKernelMass.pl = moistureCorrectedKernelMass.sp + moistureCorrectedKernelMass.mu,
         moistureCorrectedHundredKernelWt.pl = moistureCorrectedHundredKernelWt.sp + moistureCorrectedHundredKernelWt.mu,
         pctMoistureNIR.pl = pctMoistureNIR.sp + pctMoistureNIR.mu)
# Export summary.df so we don't have to re-run FW regression
write.table(summary.df, 'analysis/genotypeSummaryByLoc.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

pl.mu.df <- summary.df %>%
  select(c(genotype, loc, ends_with('.pl'), ends_with('.mu'))) %>%
  pivot_longer(c(ends_with('.pl'), ends_with('.mu')), names_to = 'var', values_to = 'val')

for (i in response_vars)
{
  plot.df <- pl.mu.df %>%
    filter(str_detect(var, i) & !is.na(val))
  
  plot <- ggplot(plot.df, aes(val, fill = var, alpha = var)) +
    geom_histogram() + 
    facet_wrap(vars(loc)) +
    scale_alpha_manual(values = c(1, 0.5))
  print(plot)
}

for (i in response_vars)
{
  response.max <- paste0(i, '.max')
  response.mu <- paste0(i, '.mu')
  response.pl <- paste0(i, '.sp')
  
  plot <- ggplot(summary.df) +
    geom_histogram(aes(.data[[response.max]]), fill = '#F8766D') +
    geom_histogram(aes(.data[[response.mu]]), fill = '#00BFC4') +
    geom_histogram(aes(.data[[response.pl]]), fill = '#C77CFF') + 
    facet_wrap(vars(loc))
  print(plot)
}

for (i in response_vars)
{
  response.max <- paste0(i, '.max')
  response.mu <- paste0(i, '.mu')
  response.pl <- paste0(i, '.sp')
  plot.scatter <- ggplot(summary.df, aes(.data[[response.mu]], .data[[response.pl]])) +
    geom_point() + 
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = mean(summary.df[[response.mu]], na.rm = TRUE)) +
    facet_wrap(vars(loc)) 
  print(plot.scatter)
}