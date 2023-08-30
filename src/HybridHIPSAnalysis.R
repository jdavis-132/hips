library(lme4)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
library(scales)
library(FW)
# Read in data and change NP so all NP fields have the same loc but different fields --> range/row are unique within a field
hybrids <- read.table('outData/HIPS_2022_V3.3_HYBRIDS.tsv', header = TRUE, sep = '\t')
hybrids <-  hybrids %>% 
  mutate(nLvl = factor(nLvl, levels = c('Low', 'Medium', 'High'), ordered = TRUE))
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
                   'yieldPerAc', 'GDDToAnthesis', 'GDDToSilk', 'kernelsPerRow', 'kernelRows', 'moistureCorrectedKernelMass',
                   'moistureCorrectedHundredKernelWt', 'ASI.GDD', 'daysToAnthesis', 'daysToSilk', 'ASI')

locs <- c('Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3', 'Lincoln', 'Missouri Valley', 'Ames', 'Crawfordsville')

mapResponse(hybrids, c('moistureCorrectedProtein'))

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

# Histograms
for(i in response_vars)
{
  p <- ggplot(hybrids, aes(.data[[i]])) +
    geom_histogram()
  print(p)
}

# Violins
for (i in locs)
{
  loc.df <- hybrids %>%
    filter(loc==i) %>%
    pivot_longer(all_of(response_vars), names_to = 'phenotype', values_to = 'val')
  
  p <- ggplot(loc.df, aes(nLvl, val)) + 
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE, fill = 'blue') + 
    facet_wrap(vars(phenotype), scales = 'free_y') + 
    labs(title = i)
  print(p)
}

# Check NP2 yield and protein
p <- hybrids %>% 
  filter(loc=='North Platte2') %>%
  ggplot(aes(range, yieldPerAc, group = range)) + 
  geom_boxplot(fill = 'blue') +
  geom_vline(xintercept = 12) + 
  geom_vline(xintercept = 24)
print(p)

p <- hybrids %>% 
  filter(loc=='North Platte2') %>%
  ggplot(aes(range, moistureCorrectedProtein, group = range)) + 
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

hybrids.wide <- plotRepCorr(hybrids, 'nLvl', 'genotype', response_vars, 'loc')

# Correlation plot for 2 vars
plotVarCorr <- function(data, x, y)
{
  x.str <- deparse(substitute(x))
  y.str <- deparse(substitute(y))
  p <- ggplot({{data}}, aes({{x}}, {{y}}, color = nLvl)) + 
    geom_point() +
    facet_wrap(vars(loc)) +
    labs(subtitle = str_c('R = ', cor(data[[x.str]], data[[y.str]], use = 'complete.obs')))
  print(p)
}


plotVarCorr(hybrids, yieldPerAc, moistureCorrectedKernelMass)
plotVarCorr(hybrids, ASI, ASI.GDD)
plotVarCorr(hybrids, daysToSilk, GDDToSilk)
plotVarCorr(hybrids, daysToAnthesis, GDDToAnthesis)
plotVarCorr(hybrids, GDDToAnthesis, GDDToSilk)
plotVarCorr(hybrids, kernelRows, yieldPerAc) # not very correlated
plotVarCorr(hybrids, kernelRows, moistureCorrectedKernelMass) # not very correlated
plotVarCorr(hybrids, kernelRows, kernelsPerRow) # not very correlated
plotVarCorr(hybrids, kernelRows, earWidth)
plotVarCorr(hybrids, kernelRows, shelledCobWidth)
plotVarCorr(hybrids, kernelRows, kernelsPerEar)
plotVarCorr(hybrids, kernelsPerRow, earFillLen)
plotVarCorr(hybrids, kernelsPerRow, shelledCobLen)
plotVarCorr(hybrids, kernelsPerRow, kernelsPerEar)
plotVarCorr(hybrids, kernelsPerEar, yieldPerAc)
plotVarCorr(hybrids, kernelsPerEar, moistureCorrectedKernelMass)
plotVarCorr(hybrids, kernelsPerEar, moistureCorrectedHundredKernelWt)
plotVarCorr(hybrids, moistureCorrectedStarch, yieldPerAc)
plotVarCorr(hybrids, shelledCobWt, yieldPerAc)
plotVarCorr(hybrids, shelledCobWt, shelledCobWidth)
plotVarCorr(hybrids, shelledCobWt, shelledCobLen)
plotVarCorr(hybrids, shelledCobWidth, earWidth)
plotVarCorr(hybrids, shelledCobLen, earFillLen)
plotVarCorr(hybrids, shelledCobLen, kernelsPerEar)
plotVarCorr(hybrids, moistureCorrectedHundredKernelWt, combineTestWt)
plotVarCorr(hybrids, combineMoisture, combineTestWt)
plotVarCorr(hybrids, tasselTipHt, flagLeafHt)
plotVarCorr(hybrids, tasselTipHt, earHt)
plotVarCorr(hybrids, flagLeafHt, earHt)
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

for(i in c('yieldPerAc.sp'))
{
  df.n <- hybrids.vp %>%
    group_by(loc, lbsNPerAc) #%>%
    #summarise('{i}':= mean(.data[[i]], na.rm = TRUE))
  p <- ggplot(hybrids.vp, aes(nLvl, .data[[i]])) +
    geom_violin() + 
    facet_wrap(vars(loc))
  print(p)
}
sb.np2 <- filter(hybrids.vp, loc=='North Platte2'|(loc=='Scottsbluff' & nLvl %in% c('Low', 'Medium'))|(loc=='Scottsbluff' & nLvl=='High' & yieldPerAc.sp >=100))
getNitrogenPlasticityByLoc <- function(data, response)
{
  locs <- c('North Platte2', 'Scottsbluff')
  response.df <- tibble(loc = NULL, genotype = NULL, '{response}':= NULL,)
  for (currLoc in locs)
  {
    loc.df <- filter(data, !is.na(genotype) & loc==currLoc & nLvl!='Border' & !is.na(nLvl))
    fw <- FW(y = loc.df[[response]], VAR = loc.df$genotype, ENV = loc.df$nLvl, saveAt = paste0('analysis/gibbs-samples-', response, '-', currLoc), 
             nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656, saveVAR = c(1:2))
    pl <- fw$b %>%
      as_tibble(rownames = 'genotype') %>%
      mutate(loc = currLoc, '{response}':= Init1) %>%
      select(!Init1)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

plasticitySBNP2.df <- getNitrogenPlasticityByLoc(sb.np2, 'yieldPerAc.sp')

plasticitySBNP2.wide <- plasticitySBNP2.df %>%
  pivot_wider(id_cols = genotype, names_from = loc, values_from = yieldPerAc.sp)

plasticityCorrPlot <- ggplot(plasticitySBNP2.wide, aes(`North Platte2`, `Scottsbluff`)) + 
  geom_point() + 
  labs(x = 'Nitrogen Plasticity of Yield at North Platte Under Partial Irrigation',
       y = 'Nitrogen Plasticity of Yield at Scottsbluff',
       subtitle = expression(R^2~'='~'0.04074947'))
plasticityCorrPlot

sb.np2.summary <- sb.np2 %>%
  group_by(genotype, loc) %>%
  summarise(meanYield = mean(yieldPerAc.sp, na.rm = TRUE)) %>%
  full_join(plasticitySBNP2.df, by = join_by(genotype, loc), keep = FALSE, suffix = c('', ''))

plasticityVsYield <- ggplot(sb.np2.summary, aes(meanYield, yieldPerAc.sp, color = loc)) + 
  geom_point() +
  geom_hline(yintercept = 1) +
  facet_grid(cols = vars(loc)) +
  labs(x = 'Mean Genotype Yield (Bushels Per Acre)', y = 'Linear Plasticity (Finlay-Wilkinson)')
plasticityVsYield

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
              GDDToAnthesis.mu = mean(GDDToAnthesis.sp, na.rm = TRUE),
              GDDToSilk.mu = mean(GDDToSilk.sp, na.rm = TRUE),
              kernelsPerRow.mu = mean(kernelsPerRow.sp, na.rm = TRUE),
              kernelRows.mu = mean(kernelRows.sp, na.rm = TRUE),
              moistureCorrectedKernelMass.mu = mean(moistureCorrectedKernelMass.sp, na.rm = TRUE),
              moistureCorrectedHundredKernelWt.mu = mean(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
              ASI.GDD.mu = mean(ASI.GDD.sp, na.rm = TRUE),
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
              GDDToAnthesis.max = max(GDDToAnthesis.sp, na.rm = TRUE),
              GDDToSilk.max = max(GDDToSilk.sp, na.rm = TRUE),
              kernelsPerRow.max = max(kernelsPerRow.sp, na.rm = TRUE),
              kernelRows.max = max(kernelRows.sp, na.rm = TRUE),
              moistureCorrectedKernelMass.max = max(moistureCorrectedKernelMass.sp, na.rm = TRUE),
              moistureCorrectedHundredKernelWt.max = max(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
              ASI.GDD.max = max(ASI.GDD.sp, na.rm = TRUE))
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
           GDDToAnthesis.pl = GDDToAnthesis.sp + GDDToAnthesis.mu,
           GDDToSilk.pl = GDDToSilk.sp + GDDToSilk.mu,
           kernelsPerRow.pl = kernelsPerRow.sp + kernelsPerRow.mu,
           kernelRows.pl = kernelRows.sp + kernelRows.mu,
           moistureCorrectedKernelMass.pl = moistureCorrectedKernelMass.sp + moistureCorrectedKernelMass.mu,
           moistureCorrectedHundredKernelWt.pl = moistureCorrectedHundredKernelWt.sp + moistureCorrectedHundredKernelWt.mu,
           ASI.GDD.pl = ASI.GDD.sp + ASI.GDD.mu)
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
response_labels <- c('Ear Height (cm)', 'Flag Leaf Height (cm)', 'Tassel Tip Height (cm)', 'Harvest Moisture (%)', 'Test Weight (lbs/bushel)', 'Ear Length (cm)', 'Ear Fill Length (cm)', 'Ear Width (cm)', 'Shelled Cob Width (cm)', 'Shelled Cob Weight (g)', 'Shelled Cob Length (cm)', 'Kernels Per Ear', 'Moisture Corrected Starch (%)', 'Moisture Corrected Protein (%)', 'Moisture Corrected Oil (%)', 'Moisture Corrected Fiber (%)', 'Moisture Corrected Ash (%)', 'Yield (Bushels/Acre)', 'GDD to Anthesis', 'GDD to Silk', 'Kernels Per Row', 'Kernel Rows', 'Moisture Corrected Kernel Mass (g)', 'Moisture Corrected Hundred Kernel Weight (g)', 'ASI (GDD)', 'Days to Anthesis', 'Days to Silk', 'ASI (Days)')

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
  orderedLocTrts <- df.plot %>%
    group_by(locTrt) %>%
    summarise(traitMean = mean(.data[[spatiallyCorrectedResponseVars[i]]])) %>%
    arrange(traitMean)
  orderedLocTrts <- orderedLocTrts$locTrt

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
            GDDToAnthesis.mu = mean(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.mu = mean(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.mu = mean(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.mu = mean(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.mu = mean(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.mu = mean(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            ASI.GDD.mu = mean(ASI.GDD.sp, na.rm = TRUE),
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
            GDDToAnthesis.max = max(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.max = max(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.max = max(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.max = max(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.max = max(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.max = max(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            ASI.GDD.max = max(ASI.GDD.sp, na.rm = TRUE),
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
            GDDToAnthesis.min = min(GDDToAnthesis.sp, na.rm = TRUE),
            GDDToSilk.min = min(GDDToSilk.sp, na.rm = TRUE),
            kernelsPerRow.min = min(kernelsPerRow.sp, na.rm = TRUE),
            kernelRows.min = min(kernelRows.sp, na.rm = TRUE),
            moistureCorrectedKernelMass.min = min(moistureCorrectedKernelMass.sp, na.rm = TRUE),
            moistureCorrectedHundredKernelWt.min = min(moistureCorrectedHundredKernelWt.sp, na.rm = TRUE),
            ASI.GDD.min = min(ASI.GDD.sp, na.rm = TRUE)) %>%
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

unl_phenos <- c('earFillLen', 'earWidth', 'shelledCobWidth', 'shelledCobWt', 'shelledCobLen', 'kernelsPerEar',
                'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh', 
                'kernelsPerRow', 'kernelRows', 'moistureCorrectedKernelMass', 'moistureCorrectedHundredKernelWt')
rm_phenos <- c('earHt', 'flagLeafHt', 'tasselTipHt', 'GDDToAnthesis', 'GDDToSilk', 'ASI.GDD')
dp_phenos <- c('combineMoisture', 'combineTestWt','combineYield')
response_vars.sb <- response_vars[c(1:5, 7:25)]
# Look at scottsbluff
sb <- filter(hybrids, loc=='Scottsbluff' & genotype!='BORDER') %>%
  select(!contains('earLen')) %>%
  pivot_longer(all_of(response_vars.sb), names_to = 'var', values_to = 'val') %>%
  mutate(nLvl = factor(nLvl, levels = c('Low', 'Medium', 'High')),
         source = case_when(var %in% c(unl_phenos, paste0(unl_phenos, '.sp')) ~ 'Chidu/Lina',
                            var %in% c(rm_phenos, paste0(rm_phenos, '.sp')) ~ 'Ramesh',
                            var %in% c(dp_phenos, paste0(dp_phenos, '.sp')) ~ 'Dipak')) %>%
  select(genotype, var, nLvl, source, val)

plot.raw <- sb %>%
  group_by(genotype, var, nLvl, source) %>%
  summarise(val = mean(val)) %>%
  ggplot(aes(nLvl, val)) + 
  geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
  geom_line(color = 'darkgrey') +
  facet_wrap(vars(var), scales = 'free_y') +
  labs(title = 'Mean of Raw Phenotype Values', x = 'Nitrogen Level')
plot.raw

plot.sp <- filter(sb, str_detect(var, '.sp')) %>%
  group_by(genotype, var, nLvl, source) %>%
  summarise(val = mean(val)) %>%
  ggplot(aes(nLvl, val)) + 
  geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
  geom_line(color = 'darkgrey') +
  facet_wrap(vars(var), scales = 'free_y') +
  labs(title = 'Mean of Spatially-Corrected Phenotype Values', x = 'Nitrogen Level')
plot.sp

# Check correlations of genotypic reps within a treatment 
## Pivot
hybrids.wide <- hybrids.vp %>%
  filter(nLvl!='Border' & !is.na(genotype)) %>%
  group_by(genotype, nLvl, loc) %>%
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  pivot_longer(any_of('combineMoisture'), names_to = 'var', values_to = 'val') %>%
  select(loc, genotype, rep, nLvl, val, var) %>%
  pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
## Get plots of r1 & r2
for (i in response_vars)
{
  rep1 <- paste0(i, '.1')
  rep2 <- paste0(i, '.2')
  
  p <- ggplot(hybrids.wide, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) + 
    geom_point() + 
    facet_wrap(vars(loc))
  print(p)
}
# For Scottsbluff only:
for (i in 'combineMoisture')
{
  rep1 <- paste0(i, '.1')
  rep2 <- paste0(i, '.2')
  
  p <- hybrids.wide %>%
    filter(loc=='North Platte1') %>%
    ggplot(aes(.data[[rep1]], .data[[rep2]], color = nLvl)) + 
    geom_point() + 
    facet_wrap(vars(nLvl)) + 
    labs(subtitle = paste0('R = ', cor(np1.hips[[rep1]], np1.hips[[rep2]], use = 'complete.obs')))
  print(p)
}
np1.hips <- filter(hybrids.wide, loc=='North Platte1')
sb.hips <- filter(hybrids.wide, loc=='Scottsbluff')

# # Test theory that the row numbers inc W to E in Scottsbluff
# sb <- hybrids %>%
#   filter(loc=='Scottsbluff') %>%
#   select(!c(contains('combine'), contains('harvest'), contains('Ht'), contains('GDD'), 
#             contains('anthesis'), contains('silk'), contains('ASI'), contains('.sp')))
# sb.combine2 <- sb_combine %>%
#   rowwise() %>%
#   mutate(row = case_when(row %in% 8:14 ~ row + 3,
#                          row > 14 ~ row + 5, 
#                          .default = row),
#          range = range + 2)
# 
# sb.data2 <- full_join(sb.combine2, sb_h_ft, join_by(plot, loc, field, irrigation, population), 
#                       suffix = c('', ''), keep = FALSE)
# sb.data2 <- full_join(sb.data2, sb_h_ht, join_by(plot, loc, field, irrigation, population), 
#                       suffix = c('', ''), keep = FALSE)
# sb.test <- full_join(sb, sb.data2, join_by(range, row, loc, field, irrigation, population))
# sb.test <- sb.test %>%
#   filter(!is.na(genotype) & genotype!='BORDER') %>%
#   rowwise() %>%
#   mutate(plot = plot.x)
# sb.repcorr <- sb.test %>%
#   filter(nLvl!='Border') %>%
#   group_by(genotype, nLvl, loc) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(earFillLen, earWidth, shelledCobLen, shelledCobWidth, shelledCobWt, kernelsPerEar, kernelMass, 
#                  kernelsPerRow, kernelRows, hundredKernelWt, moistureCorrectedStarch, moistureCorrectedProtein, moistureCorrectedOil, moistureCorrectedFiber,
#                  moistureCorrectedAsh, pctMoistureNIR, moistureCorrectedKernelMass, moistureCorrectedHundredKernelWt, combineYield, combineMoisture, 
#                  combineTestWt, earHt, flagLeafHt, tasselTipHt), 
#                names_to = 'var', values_to = 'val') %>%
#   select(loc, genotype, rep, nLvl, val, var) %>%
#   pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# sb.test_phenos <- c('earFillLen', 'earWidth', 'shelledCobLen', 'shelledCobWidth', 'shelledCobWt', 'kernelsPerEar', 'kernelMass', 
#                     'kernelsPerRow', 'kernelRows', 'hundredKernelWt', 'moistureCorrectedStarch', 'moistureCorrectedProtein', 
#                     'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh', 'pctMoistureNIR', 'moistureCorrectedKernelMass',
#                     'moistureCorrectedHundredKernelWt', 'combineYield', 'combineMoisture', 
#                     'combineTestWt', 'earHt', 'flagLeafHt', 'tasselTipHt')
# for(i in sb.test_phenos)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(sb.repcorr, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) +
#     geom_point() +
#     facet_wrap(vars(nLvl)) +
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
# # check if the field was planted starting in the SW corner but labeled from the SE corner
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
#   group_by(genotype, nLvl, loc) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of(unl_phenos), 
#                names_to = 'var', values_to = 'val') %>%
#   select(loc, genotype, rep, nLvl, val, var) %>%
#   filter(!is.na(val)) %>%
#   pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# for(i in unl_phenos[1:length(unl_phenos)])
# {
#   rep1 <- paste0(i, '.1')
#   print(i)
#   rep2 <- paste0(i, '.2')
#   
#   p <- ggplot(sb.sw.repcorr, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) +
#     geom_point() + 
#     facet_wrap(vars(nLvl)) +
#     labs(title = 'Scottsbluff SW Plant Start')
#   print(p)
#   
#   print(cor(sb.sw.repcorr[[rep1]], sb.sw.repcorr[[rep2]], use = 'complete.obs'))
# }
# 
# # Check correlation of traits collected at UNL for sites other than SB + MV
# corr.df <- filter(hybrids.wide, loc!='Scottsbluff' & loc!='Missouri Valley')
# 
# for(i in unl_phenos)
# {
#   print(i)
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(cor(corr.df[[rep1]], corr.df[[rep2]], use = 'complete.obs'))
# }
# 
# np1.df <- filter(hybrids.wide, loc=='North Platte1')
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
# sb.sw.all.vars <- c(response_vars.sb[c(1:16, 20:23)], 'combineYield', 'combineMoisture', 'combineTestWt')
# sb.sw.long <- sb.sw.all %>%
#   pivot_longer(any_of(sb.sw.all.vars), names_to = 'var', values_to = 'val') %>%
#   mutate(nLvl = factor(nLvl, levels = c('Low', 'Medium', 'High')),
#          source = case_when(var %in% c(unl_phenos, paste0(unl_phenos, '.sp')) ~ 'Chidu/Lina',
#                             var %in% c(rm_phenos, paste0(rm_phenos, '.sp')) ~ 'Ramesh',
#                             var %in% c(dp_phenos, paste0(dp_phenos, '.sp')) ~ 'Dipak')) %>%
#   group_by(genotype, var, nLvl) %>%
#   mutate(rep = 1:n()) %>%
#   select(genotype, var, nLvl, source, val, rep)
# 
# plot.raw <- sb.sw.long %>%
#   filter(!is.na(nLvl)) %>%
#   group_by(genotype, var, nLvl, source) %>%
#   summarise(val = mean(val)) %>%
#   ggplot(aes(nLvl, val)) + 
#   geom_violin(aes(fill = source), draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) + 
#   geom_line(color = 'darkgrey') +
#   facet_wrap(vars(var), scales = 'free_y') +
#   labs(title = 'Mean of Raw Phenotype Values', x = 'Nitrogen Level')
# plot.raw
# 
# sb.sw.wide <- pivot_wider(sb.sw.long, 
#                           id_cols = c(nLvl, genotype), 
#                           names_from = c(var, rep), 
#                           values_from = val, 
#                           names_sep = '.')
# for(i in sb.sw.all.vars)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(sb.sw.wide, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) +
#     geom_point() +
#     facet_wrap(vars(nLvl))
#   print(p)
#   print(i)
#   print(cor(sb.sw.wide[[rep1]], sb.sw.wide[[rep2]], use = 'complete.obs'))
# }
# 
# plotRepCorr(sb.sw.all, 'nLvl', 'genotype', sb.sw.all.vars, 'loc')
# 
# np1.df <- hybrids.vp %>%
#   filter(nLvl!='Border' & !is.na(genotype) & loc=='North Platte1') %>%
#   group_by(genotype, nLvl, loc) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(combineYield, combineMoisture, combineTestWt, earHt, flagLeafHt), 
#                names_to = 'var', values_to = 'val') %>%
#   select(loc, genotype, rep, nLvl, val, var) %>%
#   pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
#   
# for(i in c('combineYield', 'combineMoisture', 'combineTestWt', 'earHt', 'flagLeafHt', 'tasselTipHt'))
# {
#   print(i)
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   print(cor(sb.sw.wide[[rep1]], sb.sw.wide[[rep2]], use = 'complete.obs'))
# }
# 
# # Now onto figuring out what is up with the MV data
# ## Let's look at the height data, but not flip the reps-- maybe the field was planted with rep2 and rep1's locations flipped & Lisa knew but forgot to mention
# mv.plantData.hyb <- read_excel('data/Plant_data_MO_Valley_2022.xlsx', 
#                                sheet = '4211', 
#                                col_names = c('row', 'range', 'flagLeafHt', 'earHt', 'rep', 'plot', 'genotype'),
#                                col_types = c('skip', 'skip', 'numeric', 'numeric', 'skip', 'skip', 'numeric', 'numeric', 'numeric', 'numeric', 'skip', 'text'),
#                                skip = 1)
# mv.plantData.hyb <- mv.plantData.hyb %>%
#   rowwise() %>%
#   mutate(plot = case_when(rep==1 ~ plot + 100,
#                           rep==2 ~ plot + 200,
#                           .default = plot),
#          genotype = str_to_upper(genotype),
#          loc = 'Missouri Valley',
#          field = 'Hybrid HIPS',
#          nLvl = 'Medium',
#          irrigation = 'Dryland',
#          population = 'Hybrid',
#          flagLeafHt = case_when(flagLeafHt=='n/a - solar' ~ NA, .default = flagLeafHt),
#          earHt = case_when(earHt=='n/a - solar' ~ NA, .default = earHt)) %>%
#   fixGenos(hips1.5_genoFixKey)
# 
# # Pivot wide
# mv.ht.wide <- mv.plantData.hyb %>%
#   group_by(genotype) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(c(earHt, flagLeafHt), 
#                names_to = 'var', values_to = 'val') %>%
#   select(loc, genotype, rep, nLvl, val, var) %>%
#   pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# for(i in c('earHt', 'flagLeafHt'))
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(mv.ht.wide, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) +
#     geom_point()
#   print(p)
#   print(i)
#   print(cor(mv.ht.wide[[rep1]], mv.ht.wide[[rep2]], use = 'complete.obs'))
# }
# # What happens if we bind UNL data to plant data by range & row & use plot numbers and genotype info from plant data?
# mv <- hybrids %>%
#   filter(loc=='Missouri Valley') %>%
#   select(c(any_of(unl_phenos), 'qr', 'plot', 'rep', 'range', 'row', 'genotype'))
# mv.unl.ht <- full_join(mv, mv.plantData.hyb, join_by(range, row), suffix = c('.unl', '.ht'), keep = FALSE) %>%
#   mutate(plot = plot.ht,
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
#   select(loc, genotype, rep, nLvl, val, var) %>%
#   pivot_wider(id_cols = c(loc, nLvl, genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# 
# for(i in unl_phenos)
# {
#   rep1 <- paste0(i, '.1')
#   rep2 <- paste0(i, '.2')
#   p <- ggplot(mv.unl.ht.wide, aes(.data[[rep1]], .data[[rep2]], color = nLvl)) +
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
#     filter(loc=='Missouri Valley')
#   print(cor(df[[rep1]], df[[rep2]], use = 'complete.obs'))
# } 
# # Some really good correlations between genotypic reps & some really bad ones -- is this something wrong or is it a feature of this data?
# # Okay, now what if we bind the combine data in by range and row
# mv.all <- full_join(mv.unl.ht, mv_hyb, join_by(range, row)) %>%
#   mutate(loc = 'Missouri Valley',
#          field = 'Hybrid HIPS')
# mv.all.wide <- mv.all %>%
#   group_by(genotype) %>%
#   mutate(rep = 1:n()) %>%
#   ungroup() %>%
#   pivot_longer(any_of(c(dp_phenos, 'earHt', 'flagLeafHt', unl_phenos)), 
#                names_to = 'var', values_to = 'val') %>%
#   select(genotype, val, var, rep) %>%
#   pivot_wider(id_cols = c(genotype), names_from = c(var, rep), values_from = val, names_sep = '.')
# # Low corrs here possibly due to outliers
# for (i in c(dp_phenos, 'earHt', 'flagLeafHt', unl_phenos))
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
# ames.hyb <- filter(hybrids, loc=='Ames')
# ames.hyb.genoLvl <- ames.hyb %>%
#   group_by(genotype, nLvl) %>%
#   summarise(yieldPerAc = mean(yieldPerAc)) %>%
#   pivot_wider(names_from = nLvl, values_from = yieldPerAc) %>%
#   mutate(oppositeNResponse = case_when(Low > High ~ TRUE, .default = FALSE))
# ames.hyb.check <- filter(ames.hyb, genotype %in% c('F42 X MO17', 'B73 X PHZ51', 'LH185 X LH82', 'PHP02 X PHJ89'))

# Function to plot correlation of the first 2 reps of a genotype within a treatment
# Data is the data frame, subset as desired. This should be an object in your R environment
# Treatment Var may be nitrogen level, location, year, etc. Each unique combination of these and genotype will be a point in the plot. This should be a string
# Genotype is the name of the genotype column. This should be a string.
# Phenotypes is a string vector containing the names of the phenotype columns to plot.
# Note: in the case genotype(s) have more than 2 replicates, only 2 of the replicates are used here
# Facet is a string specifying the variable to facet wrap the plot by



# mapResponse(mv.all, 'combineYield')

# How many hybrids do we have both parents for? -- 18
inbreds <- filter(hips, population=='Inbred')
inbreds.genos <- unique(inbreds$genotype)
hybrid.genos <- tibble(hybrid = unique(hybrids$genotype))
hybrid.genos <- hybrid.genos %>%
  rowwise() %>%
  mutate(P1 = str_split_i(hybrid, ' X ', 1),
         P2 = str_split_i(hybrid, ' X ', 2), 
         P1InPanel = P1 %in% inbreds.genos,
         P2InPanel = P2 %in% inbreds.genos,
         BothInPanel = case_when(P1InPanel & P2InPanel ~ TRUE, .default = FALSE))
sum(hybrid.genos$BothInPanel)
