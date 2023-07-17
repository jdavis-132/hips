library(lme4)
library(nlme)
library(car)
library(SpATS)
library(tidyverse)
library(viridis)
# Read in data and change NP so all NP fields have the same loc but different fields --> range/row are unique within a field
hybrids <- read.table('outData/HIPS_2022_V2.5.tsv', header = TRUE, sep = '\t') %>%
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
    facet_wrap(vars(loc)) + 
    scale_x_continuous(breaks = 0:40) +
    scale_y_continuous(breaks = 0:40) +
    scale_fill_viridis(option = 'turbo', direction = -1) +
    theme_minimal() + 
    theme(axis.text = element_text(angle = 45))
  print(plot)
}

response_vars <- c('earHt', 'flagLeafHt', 'plantHt', 'combineMoisture', 'combineTestWt', 
                   'earLen', 'earFillLen', 'earWidth', 'shelledCobWidth', 'shelledCobWt', 'kernelsPerEar',
                   'moistureCorrectedStarch', 'moistureCorrectedProtein', 'moistureCorrectedOil', 'moistureCorrectedFiber', 'moistureCorrectedAsh', 
                   'yieldPerAc', 'daysToAnthesis', 'daysToSilk', 
                   'kernelsPerRow', 'kernelRows', 'moistureCorrectedKernelMass', 'moistureCorrectedHundredKernelWt', 'pctMoistureNIR')
for(i in response_vars[c(12, 22, 23)])
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
hybrids <- mutate(hybrids, across(c(nLvl, genotype, irrigation, loc), as.factor))

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

hybrids.vp <- hybrids 

# Okay, now let's write a function to get the spatial BLUES for each response on a plot level
# Will fit model by individual location, nitrogen treatment combination
getSpatialCorrections <- function(data, response)
{
  # Declare empty df and levels of locs
  df.sp <- tibble(loc = NULL, plot = NULL, '{response}':= NULL, nLvl = NULL)
  locs <-  c('Missouri Valley', 'Lincoln', 'Scottsbluff', 'North Platte1', 'North Platte2', 'North Platte3')
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
  hybrids <- full_join(hybrids, getSpatialCorrections(hybrids, i), by = join_by(loc, plot, nLvl), suffix = c('', '.blup'), keep = FALSE)
}

hybrids <- hybrids %>%
  rowwise() %>%
  mutate(across(everything(), ~case_when(is.null(.) ~ NA, .default = .)))

partitionVariance2 <- function(df, response) 
{
  df <- filter(df, !is.na(response))
  lm_formula <- as.formula(paste(response, "~ (1|loc/nLvl) + (1|genotype) + (1|loc:genotype) + (1|nLvl:genotype)"))
  model <- lmer(lm_formula, data = df)
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
spatiallyCorrectedResponseVars <- paste0(response_vars, '.blup')
#spatiallyCorrectedResponseVars <- paste0(spatiallyCorrectedResponseVars, '.blup')

# hybrids.vp <- hybrids %>%
#   rowwise() %>%
#   mutate(earHt.blup = earHt.blup.blup, 
#          moistureCorrectedOil.blup = case_when(loc=='North Platte1' ~ moistureCorrectedOil, .default = moistureCorrectedOil.blup),
#          earFillLen.blup = case_when(loc=='North Platte1' ~ earFillLen, .default = earFillLen.blup),
#          moistureCorrectedFiber.blup = case_when(loc=='North Platte1' ~ moistureCorrectedFiber, .default = moistureCorrectedFiber.blup),
#          moistureCorrectedHundredKernelWt.blup = case_when(loc=='North Platte1' ~ moistureCorrectedHundredKernelWt, 
#                                                            .default = moistureCorrectedHundredKernelWt.blup))
for(i in spatiallyCorrectedResponseVars)
{
  vc_all <- bind_rows(vc_all, partitionVariance2(hybrids, i))
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
  sp.correction.plot <- ggplot(hybrids, (aes(.data[[response_vars[i]]], .data[[spatiallyCorrectedResponseVars[i]]], color = nLvl))) +
    geom_point() +
    geom_abline(slope = 1) +
    facet_wrap(vars(loc)) 
  print(sp.correction.plot)
}

# # Let's drop some outliers: moved to CombineHIPS22Data.R
# combineNotes_drop <- c("Left row ran over", "Right row gone", "Run over", "Clog", "Animal damage", "Animal damage, not enough grain for accurate moisture", "One row", 
#                        "Same as last", "11/9/2022, 11:45:04 AM, Tare Warning, Test Weight: 5.75", "Plot lost to belt issue")
# combineVars <- c('combineYield', 'combineMoisture', 'combineTestWt', 'yieldPerAc')
# combineNotes_dropMoisture <- c( "Water stress, not enough grain for accurate moisture", "Not enough grain for accurate moisture")
# 
# hybrids <- hybrids %>%
#   mutate(across(all_of(combineVars), ~case_when(combineNotes %in% combineNotes_drop ~ NA, .default = .))) %>%
#   mutate(combineMoisture = case_when(combineNotes %in% combineNotes_dropMoisture | combineMoisture==0 ~ NA, .default = combineMoisture),
#          earHt = case_when(loc=='Lincoln' & plot %in% c(5244, 5283, 5141) ~ NA, .default = earHt),
#          combineYield = case_when(loc=='North Platte1' & plot==250 ~ NA, .default = combineYield),
#          yieldPerAc = case_when(loc=='North Platte1' & plot==250 ~ NA, .default = yieldPerAc),
#          combineTestWt = na_if(combineTestWt, 0),
#          earLen = case_when(loc=='North Platte1' & plot==426 ~ NA, .default = earLen),
#          earFillLen = case_when(loc=='North Platte1' & plot==426 ~ NA,
#                                 loc=='North Platte2' & plot==879 ~ mean(15.5, 16, 12), 
#                                 .default = earFillLen),
#          earWidth = case_when(loc=='North Plattte1' & plot==426 ~ NA, .default = earWidth),
#          shelledCobWidth = case_when(loc=='North Platte1' & plot==426 ~ NA, .default = shelledCobWidth))
# np1 <- hybrids %>% 
#   filter(loc=='North Platte' & field == 'North Platte1' & !is.na(row) & !is.na(range) & ! is.na(combineYield))
# # np.spLm <- lme(combineYield ~ 1, correlation = corGaus(~ range + row |field), data = np, na.action = na.omit)
# # np.spLm <- lme(combineYield ~ 1, random = ~ 1 | field / (range + row), correlation = corGaus(form = ~ range + row), data = np, na.action = na.omit)
# # np.spLm <- lme(combineYield ~ 1, correlation = corGaus(form = ~ range + row | field), data = np, na.action = na.omit)
# # np.spLm <- gls(combineYield ~ (1 | field / range) + (1 | field / row), data = np, na.action = na.omit,
# #                correlation = corGaus(form = ~ range + row | field),
# #                method = "ML",
# #                control = lmeControl(msMaxIter = 50))
# # Use a variogram & kriging to understand the spatial variance-covariance structure in each field - essentially BLUPS, but for spatial data
# np1.gstat <- gstat(id = 'yieldSpatialCovar', formula = combineYield ~ 1, locations = ~ row + range, data = np1, model = vgm(psill = 1, 'Gau', range =1000))
# np1.spPred <- predict(np1.gstat, np1.)
# # partitionVariance <- function(df, phenotype)
# # {
# #   lm <- lmer(substitute(phenotype) ~ (1|loc) + (1|row) + (1|range) + (1|rep) + (1|nLvl) + (1|irrigation) + (1|genotype), data = as.matrix(df))
# #   sigmas <- as.data.frame(lm, row.names = TRUE, order = 'cov.last', comp = 'Variance')
# #   return(sigmas)
# # }
# # 
# # combineYield <- partitionVariance(hybrids, combineYield)
# # Fit spatial model; think through nesting and interactions
# # Write function
# # Model: spatial nesting + nLvl + genotype + irrigation
# 
# # yield.lm <- lmer(combineYield ~ (1|loc / field / range:row) + (1| loc / field / row) + (1|nLvl) + (1|irrigation) + (1|genotype) + 
# #                    (1|genotype:nLvl) + (1|genotype:irrigation) + (1|nLvl:irrigation) + (1|nLvl:irrigation:genotype), data = hybrids) 
# # summary(yield.lm)
# # anova(yield.lm)
# # Anova(yield.lm, type = 'III')
# # yield.sigmas <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# # yield.totalVar <- sum(yield.sigmas$vcov)
# # yield.sigmas <- yield.sigmas %>%
# #   as_tibble() %>%
# #   rowwise() %>%
# #   mutate(pctVar = vcov/yield.totalVar*100)
# 
# yield.lm <- lmer(combineYield ~ (1|loc / nLvl) + (1|irrigation) + (1|genotype) + 
#                    (1|genotype:nLvl) + (1|genotype:irrigation) + (1|nLvl:irrigation) + (1|nLvl:irrigation:genotype), data = hybrids) 
# summary(yield.lm)
# anova(yield.lm)
# Anova(yield.lm, type = 'III')
# yield.sigmas2 <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# yield.totalVar <- sum(yield.sigmas2$vcov)
# yield.sigmas2 <- yield.sigmas2 %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(pctVar = vcov/yield.totalVar*100)
# 
# yield.lm <- lmer(combineYield ~ (1|loc / nLvl) + (1|genotype) + 
#                    (1|genotype:nLvl), data = hybrids) 
# summary(yield.lm)
# anova(yield.lm)
# Anova(yield.lm, type = 'III')
# yield.sigmas3 <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# yield.totalVar <- sum(yield.sigmas3$vcov)
# yield.sigmas3 <- yield.sigmas3 %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(pctVar = vcov/yield.totalVar*100)
# 
# yield.lm <- lmer(combineYield ~ (1|loc / field / nLvl) + (1|irrigation) + (1|genotype) + 
#                    (1|genotype:nLvl) + (1|genotype:irrigation) + (1|nLvl:irrigation) + (1|nLvl:irrigation:genotype), data = hybrids) 
# summary(yield.lm)
# anova(yield.lm)
# Anova(yield.lm, type = 'III')
# yield.sigmas4 <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# yield.totalVar <- sum(yield.sigmas4$vcov)
# yield.sigmas4 <- yield.sigmas4 %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(pctVar = vcov/yield.totalVar*100)
# 
# yield.lm <- lmer(combineYield ~ (1|loc / field / nLvl) + (1|irrigation) + (1|genotype) + 
#                    (1|genotype:nLvl) + (1|genotype:irrigation) + (1|nLvl:irrigation), data = hybrids) 
# summary(yield.lm)
# anova(yield.lm)
# Anova(yield.lm, type = 'III')
# yield.sigmas5 <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# yield.totalVar <- sum(yield.sigmas5$vcov)
# yield.sigmas5 <- yield.sigmas5 %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(pctVar = vcov/yield.totalVar*100)
# 
# yield.lm <- lmer(combineYield ~ (1|loc /  nLvl) + (1|irrigation) + (1|genotype) + 
#                    (1|genotype:nLvl) + (1|genotype:irrigation) + (1|nLvl:irrigation), data = hybrids) 
# summary(yield.lm)
# anova(yield.lm)
# Anova(yield.lm, type = 'III')
# yield.sigmas6 <- as.data.frame(VarCorr(yield.lm), row.names = TRUE, order = 'cov.last', comp = 'Variance')
# yield.totalVar <- sum(yield.sigmas6$vcov)
# yield.sigmas6 <- yield.sigmas6 %>%
#   as_tibble() %>%
#   rowwise() %>%
#   mutate(pctVar = vcov/yield.totalVar*100)