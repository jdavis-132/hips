library(lme4)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
library(scales)
library(FW)
library(reshape2)
library(corrplot)
# Read in data and change NP so all NP fields have the same loc but different fields --> range/row are unique within a field
hips <- read.table('outData/HIPS_2022_V3.1.tsv', header = TRUE, sep = '\t')
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

response_vars <- c('earHt', 'flagLeafHt', 'tasselTipHt', 'combineMoisture', 'combineTestWt', 
                   'earLen', 'earFillLen', 'earWidth', 'shelledCobWidth', 'shelledCobWt', 'shelledCobLen', 'kernelsPerEar',
                   'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh', 
                   'yieldPerAc', 'daysToAnthesis', 'daysToSilk', 
                   'kernelsPerRow', 'kernelRows', 'moistureCorrectedKernelMass', 'moistureCorrectedHundredKernelWt', 'ASI')
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

for(i in spatiallyCorrectedResponseVars)
{
  df.n <- hybrids.vp %>%
    group_by(loc, lbsNPerAc) %>%
    summarise('{i}':= mean(.data[[i]], na.rm = TRUE))
  p <- ggplot(df.n, aes(lbsNPerAc, .data[[i]], color = loc)) +
    geom_line()
  print(p)
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
              tasselTipHt.mu = mean(tasselTipHt.sp, na.rm = TRUE),
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
              ASI.mu = mean(ASI.sp, na.rm = TRUE),
              earHt.max = max(earHt.sp, na.rm = TRUE),
              flagLeafHt.max = max(flagLeafHt.sp, na.rm = TRUE),
              tasselTipHt.max = max(tasselTipHt.sp, na.rm = TRUE),
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
              ASI.max = max(ASI.sp, na.rm = TRUE))
  summary.df <- full_join(summary.df, plasticity.df, join_by(genotype, loc), keep = FALSE, suffix = c('', ''))
  summary.df <- filter(summary.df, loc!='Missouri Valley')
  summary.df <- summary.df %>%
    rowwise() %>%
    mutate(earHt.pl = earHt.sp + earHt.mu,
           flagLeafHt.pl = flagLeafHt.sp + flagLeafHt.mu,
           tasselTipHt.pl = tasselTipHt.sp + tasselTipHt.mu,
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
           ASI.pl = ASI.sp + ASI.mu)
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
  
  df.n <- filter(hybrids.vp, loc!='Missouri Valley')
  
  plasticity.yield <- summary.df %>%
    group_by(loc) %>%
    summarise(max.pl = max(yieldPerAc.sp, na.rm = TRUE),
              min.pl = min(yieldPerAc.sp, na.rm = TRUE))
  qrs.h <- c()
  qrs.l <- c()
  for(i in c('North Platte1', 'North Platte2', 'North Platte3', 'Scottsbluff', 'Lincoln', 'Ames', 'Crawfordsville'))
  {
    vals.pl <- filter(plasticity.yield, loc==i)
    genotypes.h <- filter(summary.df, loc==i & yieldPerAc.sp==vals.pl$max.pl)
    genotypes.h <- genotypes.h$genotype
    genotypes.l <- filter(summary.df, loc==i & yieldPerAc.sp==vals.pl$min.pl)
    genotypes.l <- genotypes.l$genotype
    vals.h <- filter(df.n, loc==i & genotype %in% genotypes.h)
    qrs.h <- c(qrs.h, vals.h$qr)
    vals.l <- filter(df.n, loc==i & genotype %in% genotypes.l)
    qrs.l <- c(qrs.l, vals.l$qr)
  }
  
  pl.h <- filter(df.n, qr %in% qrs.h)
  pl.l <- filter(df.n, qr %in% qrs.l)
  df.n <- filter(df.n, !(qr %in% c(qrs.h, qrs.l)))
  
  df.n <- df.n %>%
    group_by(loc, genotype, lbsNPerAc) %>%
    summarise(yieldPerAc.sp = mean(yieldPerAc.sp, na.rm = TRUE))
  pl.l <- pl.l %>%
    group_by(loc, genotype, lbsNPerAc) %>%
    summarise(yieldPerAc.sp = mean(yieldPerAc.sp))
  pl.h <- pl.h %>%
    group_by(loc, genotype, lbsNPerAc) %>%
    summarise(yieldPerAc.sp = mean(yieldPerAc.sp))
  write.table(df.n, 'analysis/yieldByN_LocAll.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
  write.table(pl.l, 'analysis/yieldByN_Loc_LowPlasticity.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
  write.table(pl.h, 'analysis/yieldByN_Loc_HighPlasticity.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
  
  
  ggplot(df.n, aes(x = lbsNPerAc, y = yieldPerAc.sp, group = lbsNPerAc, color = lbsNPerAc)) +
    geom_boxplot(fill = hue_pal()(21), color = 'black', outlier.color = 'black', notch = FALSE, na.rm = TRUE) +
    geom_line(data = df.n, aes(group = genotype), color = 'grey', alpha = 0.25) +  
    geom_line(data = pl.l, aes(group = genotype), color = '#377EB8', alpha = 0.5) +
    geom_line(data = pl.h, aes(group = genotype), color = 'purple', alpha = 0.5) +
    labs(x = 'Nitrogen Fertilizer (lbs/ac)', y = 'Yield (bu/ac)', title = 'Pair Plot with Box Plots and Connecting Lines') +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
          axis.line = element_line(color = 'black', size = 0.5),  
          legend.position = 'none') +
    facet_wrap(vars(loc))#+
    # scale_x_continuous(limits = c(60, 85), breaks = seq(55, 85, by = 5)) +
    # scale_y_continuous(limits = c(45, 105))  
  
  for (i in spatiallyCorrectedResponseVars)
  {
    loc.trt <- unite(hybrids.vp, col = 'loc.trt', c(loc, lbsNPerAc), na.rm = TRUE, sep = ';', remove = FALSE) %>%
      filter(str_detect(loc.trt, ';') & !is.na(.data[[i]]))
      nLoc.trt <- unique(loc.trt$loc.trt) %>%
      length()
   p <- ggplot(hybrids.vp, aes(x = lbsNPerAc, y = .data[[i]], group = lbsNPerAc, color = lbsNPerAc)) +
      geom_boxplot(fill = hue_pal()(nLoc.trt), color = 'black', outlier.color = 'black', notch = FALSE, na.rm = TRUE) +
      geom_line(data = hybrids.vp, aes(group = genotype), color = 'grey', alpha = 0.25) +  
      labs(x = 'Nitrogen Fertilizer (lbs/ac)', y = i, title = 'Pair Plot with Box Plots and Connecting Lines') +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
            axis.line = element_line(color = 'black', size = 0.5),  
            legend.position = 'none') +
      facet_wrap(vars(loc))#+
   print(p)
  }
  
  # Correlation of plasticities by location
  for(i in spatiallyCorrectedResponseVars)
  {
    df <- summary.df %>%
      filter(!is.na(.data[[i]])) %>%
      select(c(genotype, loc, .data[[i]])) %>%
      pivot_wider(names_from = loc, values_from = .data[[i]]) %>%
      select(!genotype)
    cp <- chart.Correlation(df)
    print(cp)
  }
# Plasticities across locs: each nitrogen treatment within a location is an environment
# Create variable
locTrt.df <- hybrids.vp %>%
  unite('locTrt', c(loc, nLvl), sep = '.', remove = FALSE, na.rm = T) %>%
  rowwise() %>%
  mutate(locTrt = case_when(str_detect(locTrt, 'Border')|!str_detect(locTrt, '.') ~ NA, .default = locTrt)) %>%
  filter(!is.na(genotype) & nLvl!='Border' & !is.na(locTrt) & genotype!='BORDER')
fw.1 <- FW(y = locTrt.df[[spatiallyCorrectedResponseVars[1]]], VAR = locTrt.df$genotype, ENV = locTrt.df$locTrt, 
           saveAt = paste0('analysis/gibbs-samples-allenv-', spatiallyCorrectedResponseVars[1]), 
           nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656)
pl.allenv <- fw.1$b %>%
  as_tibble(rownames = 'genotype') %>%
  mutate('{spatiallyCorrectedResponseVars[1]}':= Init1) %>%
  select(!Init1)

for(i in 2:length(spatiallyCorrectedResponseVars))
{
  fw <- FW(y = locTrt.df[[spatiallyCorrectedResponseVars[i]]], VAR = locTrt.df$genotype, ENV = locTrt.df$locTrt, 
           saveAt = paste0('analysis/gibbs-samples-allenv-', spatiallyCorrectedResponseVars[i]), 
           nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656)
  pl <- fw$b %>%
    as_tibble(rownames = 'genotype') %>%
    mutate('{spatiallyCorrectedResponseVars[i]}':= Init1) %>%
    select(!Init1)
  pl.allenv <- full_join(pl.allenv, pl, join_by(genotype), suffix = c('', ''), keep = FALSE)
}

# export pl.allenv
write.table(pl.allenv, 'analysis/PlasticityAcrossLocs.tsv', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
# get genotypes with the 20 highest and 20 lowest plasticity vals
high.plasticity <- pl.allenv %>%
  arrange(desc(yieldPerAc.sp))
high.plasticity.genos <- high.plasticity$genotype[1:20]

low.plasticity <- pl.allenv %>%
  arrange(yieldPerAc.sp)
low.plasticity.genos <- low.plasticity$genotype[1:20]

locTrt.df <- locTrt.df %>%
  mutate(locTrt = factor(locTrt, levels = c( 'Scottsbluff.Low', 'Scottsbluff.Medium', 'Scottsbluff.High', 'North Platte1.Low', 'North Platte1.Medium',
                                            'North Platte1.High', 'North Platte2.Low', 'North Platte2.Medium', 'North Platte2.High', 'North Platte3.Low', 
                                            'North Platte3.Medium', 'North Platte3.High', 'Lincoln.Low', 'Lincoln.Medium', 'Lincoln.High', 'Missouri Valley.Medium',
                                            'Ames.Low', 'Ames.Medium', 'Ames.High','Crawfordsville.Low', 'Crawfordsville.Medium', 'Crawfordsville.High')))

# Define 'pretty' labels for response vars
response_labels <- c('Ear Height (cm)', 'Flag Leaf Height (cm)', 'Tassel Tip Height (cm)', 'Harvest Moisture (%)', 'Test Weight (lbs/bushel)', 'Ear Length (cm)', 'Ear Fill Length (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)', 'Shelled Cob Weight (g)', 'Shelled Cob Length (cm)', 'Kernels Per Ear', 'Moisture Corrected Starch (%)', 'Moisture Corrected Protein (%)', 'Moisture Corrected Oil (%)', 'Moisture Corrected Fiber (%)', 'Moisture Corrected Ash (%)', 'Yield (Bushels/Acre)', 'Days to Anthesis', 'Days to Silking', 'Kernels Per Row', 'Kernel Rows', 'Moisture Corrected Kernel Mass (g)', 'Moisture Corrected Hundred Kernel Weight (g)', 'ASI (Days)')

for(i in 1:length(spatiallyCorrectedResponseVars))
{
  high.plasticity <- pl.allenv %>%
    arrange(desc(.data[[spatiallyCorrectedResponseVars[i]]]))
  high.plasticity.genos <- high.plasticity$genotype[1:20]
  
  low.plasticity <- pl.allenv %>%
    arrange(.data[[spatiallyCorrectedResponseVars[i]]])
  low.plasticity.genos <- low.plasticity$genotype[1:20]
  
  df.plot <- filter(locTrt.df, !is.na(.data[[spatiallyCorrectedResponseVars[i]]])) %>%
    group_by(genotype, locTrt) %>%
    summarise('{spatiallyCorrectedResponseVars[i]}' := mean(.data[[spatiallyCorrectedResponseVars[i]]], na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(relPlasticity = case_when(genotype %in% high.plasticity.genos ~ 'High', 
                                     genotype %in% low.plasticity.genos ~ 'Low',
                                     .default = 'Medium'))
  # orderedLocTrts <- df.plot %>%
  #   group_by(locTrt) %>%
  #   summarise(traitMean = mean(.data[[spatiallyCorrectedResponseVars[i]]])) %>%
  #   arrange(traitMean)
  # orderedLocTrts <- orderedLocTrts$locTrt
  # 
  # df.plot <- df.plot %>%
  #   mutate(locTrt = factor(locTrt, levels = orderedLocTrts))
    
  p <- ggplot(df.plot, aes(x = locTrt, y = .data[[spatiallyCorrectedResponseVars[i]]], group = locTrt)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = '#00BFC4', na.rm = TRUE) +
    geom_line(data = df.plot, aes(group = genotype, color = relPlasticity), alpha = 0.25) +  
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
}

yield.Ames <- locTrt.df %>%
  filter(!is.na(yieldPerAc.sp) & loc=='Ames') %>%
  group_by(locTrt, genotype) %>%
  summarise(yieldPerAc.sp = mean(yieldPerAc.sp)) %>%
  mutate(locTrt = factor(locTrt, levels = c('Ames.Medium', 'Ames.High', 'Ames.Low'))) %>%
  ggplot(aes(locTrt, yieldPerAc.sp, group = locTrt)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), fill = '#00BFC4', na.rm = TRUE) +
  labs(x = 'Environment', y = 'Yield (Bushels/Acre)') +
  theme(text = element_text(size = 16),
        axis.line = element_line(color = 'black', size = 1),
        legend.position = 'none', 
        plot.background = element_blank(), 
        panel.grid = element_blank())
yield.Ames

summary.allenv <- hybrids.vp %>%
  group_by(genotype) %>%
  summarise(earHt.mu = mean(earHt.sp, na.rm = TRUE),
            flagLeafHt.mu = mean(flagLeafHt.sp, na.rm = TRUE),
            tasselTipHt.mu = mean(tasselTipHt.sp, na.rm = TRUE),
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
            ASI.mu = mean(ASI.sp, na.rm = TRUE),
            earHt.max = max(earHt.sp, na.rm = TRUE),
            flagLeafHt.max = max(flagLeafHt.sp, na.rm = TRUE),
            tasselTipHt.max = max(tasselTipHt.sp, na.rm = TRUE),
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
            ASI.max = max(ASI.sp, na.rm = TRUE),
            earHt.min = min(earHt.sp, na.rm = TRUE),
            flagLeafHt.min = min(flagLeafHt.sp, na.rm = TRUE),
            tasselTipHt.min = min(tasselTipHt.sp, na.rm = TRUE),
            combineMoisture.min = min(combineMoisture.sp, na.rm = TRUE),
            combineTestWt.min = min(combineTestWt.sp, na.rm = TRUE),
            earLen.min = min(earLen.sp, na.rm = TRUE),
            earFillLen.min = min(earFillLen.sp, na.rm = TRUE),
            earWidth.min = min(earWidth.sp, na.rm = TRUE),
            shelledCobWidth.min = min(shelledCobWidth.sp, na.rm = TRUE),
            shelledCobWt.min = min(shelledCobWt.sp, na.rm = TRUE),
            shelledCobLen.min = min(shelledCobLen.sp, na.rm = TRUE),
            kernelsPerEar.min = min(kernelsPerEar.sp, na.rm = TRUE),
            moistureCorrectedStarch.min = min(moistureCorrectedStarch.sp, na.rm = TRUE),
            moistureCorrectedProtein.min = min(moistureCorrectedProtein.sp, na.rm = TRUE),
            moistureCorrectedOil.min = min(moistureCorrectedOil.sp, na.rm = TRUE),
            moistureCorrectedFiber.min = min(moistureCorrectedFiber.sp, na.rm = TRUE),
            moistureCorrectedAsh.min = min(moistureCorrectedAsh.sp, na.rm = TRUE),
            yieldPerAc.min = min(yieldPerAc.sp, na.rm = TRUE),
            daysToAnthesis.min = min(daysToAnthesis.sp, na.rm = TRUE),
            daysToSilk.min = min(daysToSilk.sp, na.rm = TRUE),
            kernelsPerRow.min = min(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.min = min(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.min = min(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.min = min(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            ASI.min = min(ASI.sp, na.rm = TRUE)) %>%
  filter(!is.na(genotype) & genotype!='BORDER') %>%
  full_join(pl.allenv, join_by(genotype), suffix = c('', ''), keep = FALSE) %>%
  mutate(across(where(is.numeric), ~na_if(., -Inf)))
# Tradeoff between plasticity and good performance - there doesn't seem to be one
for (i in 1:length(response_vars))
{
  response.sp <- paste0(response_vars[i], '.sp')
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
}