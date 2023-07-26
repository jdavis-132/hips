library(lme4)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
library(FW)
# Read in data and change NP so all NP fields have the same loc but different fields --> range/row are unique within a field
hybrids <- read.table('outData/HIPS_2022_V3.tsv', header = TRUE, sep = '\t') %>%
  filter(population == 'Hybrid') %>%
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

# Is there shrinkage toward the mean of a treatment ?
for(i in 1:length(response_vars))
{
  sp.correction.plot <- ggplot(hybrids.vp, (aes(.data[[response_vars[i]]], .data[[spatiallyCorrectedResponseVars[i]]], color = nLvl))) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(vars(loc)) 
  print(sp.correction.plot)
}

# plasticity.df <- hybrids.vp %>%
#   group_by(loc, genotype, nLvl) %>%
#   summarise(earHt = mean(earHt.sp, na.rm = TRUE),
#             flagLeafHt = mean(flagLeafHt.sp, na.rm = TRUE),
#             plantHt = mean(plantHt.sp, na.rm = TRUE),
#             combineMoisture = mean(combineMoisture.sp, na.rm = TRUE),
#             combineTestWt = mean(combineTestWt.sp, na.rm = TRUE), 
#             earLen = mean(earLen.sp, na.rm = TRUE),
#             earFillLen = mean(earFillLen.sp, na.rm = TRUE), 
#             earWidth = mean(earWidth.sp, na.rm = TRUE), 
#             shelledCobWidth = mean(shelledCobWidth.sp, na.rm = TRUE),
#             shelledCobWt = mean(shelledCobWt.sp, na.rm = TRUE),
#             shelledCobLen = mean(shelledCobLen.sp, na.rm = TRUE), 
#             kernelsPerEar = mean(kernelsPerEar.sp, na.rm = TRUE),
#             moistureCorrectedStarch = mean(moistureCorrectedStarch.sp, na.rm = TRUE), 
#             moistureCorrectedProtein = mean(moistureCorrectedProtein.sp, na.rm = TRUE),
#             moistureCorrectedOil = mean(moistureCorrectedOil.sp, na.rm = TRUE),
#             moistureCorrectedFiber = mean(moistureCorrectedFiber.sp, na.rm = TRUE),
#             moistureCorrectedAsh = mean(moistureCorrectedAsh.sp, na.rm = TRUE),
#             yieldPerAc = mean(yieldPerAc.sp, na.rm = TRUE),
#             daysToAnthesis = mean(daysToAnthesis.sp, na.rm = TRUE),
#             daysToSilk = mean(daysToSilk.sp, na.rm = TRUE),
#             kernelsPerRow = mean(kernelsPerRow.sp, na.rm = TRUE),
#             kernelRows = mean(kernelRows.sp, na.rm = TRUE), 
#             moistureCorrectedKernelMass = mean(moistureCorrectedKernelMass.sp, na.rm = TRUE),
#             moistureCorrectedHundredKernelWt = mean(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE), 
#             pctMoistureNIR = mean(pctMoistureNIR.sp, na.rm = TRUE))

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
      as_tibble(rownames = genotype) %>%
      mutate(loc = currLoc, '{response}':= Init1) %>%
      select(!Init1)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

plasticiy.df <- tibble(loc = NULL, genotype = NULL)
for(i in spatiallyCorrectedResponseVars)
{
  plasticity.df <- full_join(plasticity.df, getNitrogenPlasticityByLoc(hybrids.vp, i), join_by(genotype, loc), suffix = c('', ''), keep = FALSE)
}