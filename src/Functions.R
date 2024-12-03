library(tidyverse)
library(SpATS)
# library(spFW)
library(viridis)
SEED <- 101762103
# Returns a data frame filtered to rows of data that have values for trait 
# that are less than quantile 1 - 1.5*iqr or greater than quantile 3 + 1.5*iqr
# data is a data frame
# trait is a string with the name of the column of the trait of interest
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

# Now let's make this into a function to make maps for every response variable:
mapResponse <- function(data, trait)
{
  plot <- ggplot(data, aes(range, row, fill = .data[[trait]], color = 'white')) + 
    geom_raster() +
    # facet_wrap(vars(location, sublocation)) + 
    scale_x_continuous(breaks = 0:40) +
    scale_y_continuous(breaks = 0:40) +
    scale_fill_viridis(option = 'turbo', direction = -1) +
    theme_minimal() + 
    theme(axis.text = element_text(angle = 45))
  print(plot)
}

# Returns a pivoted data frame with a column for each observation of each phenotype of a genotype within a treatment 
# and location
# It also creates a scatter plot and prints the correlation value
# data is a dataframe
# treatmentVar is a string name of the column for the treatment
# genotype is a string name of the genotype column
# phenotypes is a string vector of the phenotypes to plot 
# facet is a string name of the column to create plot facets by - I usually use location for this
plotRepCorr <- function(data, treatmentVar = 'nitrogenTreatment', genotype = 'genotype', phenotypes, facet = 'location')
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
    print(cor(df.wide[[rep1]], df.wide[[rep2]], use = 'complete.obs', method = 'spearman'))
    
    p <- ggplot(df.wide, aes(.data[[rep1]], .data[[rep2]], color = .data[[treatmentVar]])) + 
      geom_point() + 
      facet_wrap(vars(all_of(.data[[facet]]))) +
      labs(subtitle = paste0('r=', cor(df.wide[[rep1]], df.wide[[rep2]], use='complete.obs', method = 'spearman')))
    print(p)
    # ggsave(paste0('analysis/repCorrelationByLocationInbreds', i, '.png'))
  }
  return(df.wide)
}

# Correlation plot for 2 variables
# data is a data frame
# x is the first variable
# y is the second variable
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

# ***Specific to HIPS 2022 data** Modify as needed. 
# Returns a dataframe with a column where the values are the fitted values after spatial correction using SpATS
# Fits plot identifier as the genotype so we get values for every plot. 
# data is a dataframe
# response is a string name of the phenotype column to spatially adjust
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
      intercept <- model$coeff['Intercept']
      sp <- as_tibble(model$coeff, rownames = 'plotNumber') %>%
        mutate(location = currlocation,
               nitrogenTreatment = currTrt, 
               plotNumber = as.numeric(plotNumber)) %>%
        filter(!is.na(plotNumber)) %>%
        rowwise() %>%
        mutate(value = value + intercept) %>%
        rename('{response}':= value)
      # Bind to df
      df.sp <- bind_rows(df.sp, sp) %>%
        mutate(plotNumber = as.numeric(plotNumber))
  }
  print(length(df.sp$plotNumber))
  # Return df
  return(df.sp)
  }
}

# ***Specific to HIPS data** Modify as needed. 
# Returns a dataframe with a column where the values are the plot-wise BLUPs after spatial correction using SpATS + model intercept
# Fits plot identifier as the genotype so we get values for every plot. Assumes this is a column titled 'plotNumber'.
# data is a dataframe
# environment is the column identifying individual treatments as a string
# response is a string name of the phenotype column to spatially adjust
getSpatialCorrectionsEnvironment <- function(data, response, environment)
{
  # Declare empty df and levels of locations
  df.sp <- tibble(environment = NULL,  plotNumber = NULL, '{response}':= NULL)
  environments <- unique(data[[environment]])
  # Loop over environments
  for(currEnvironment in environments)
  {
    environment.df <- filter(data, .data[[environment]]==currEnvironment & !is.na(row) & !is.na(range) & !is.na(.data[[response]]))
    if(length(environment.df$plotNumber)==0)
    {
      print(paste0('No data for ', response, ' at ', currEnvironment))
      next
    }
    
    rangeKnots <- floor(max(environment.df$range, na.rm = TRUE)/2) + 1
    rowKnots <- floor(max(environment.df$row, na.rm = TRUE)/2) + 1
    print(currEnvironment)
    model <- SpATS(response, 
                   genotype = 'plotNumber', 
                   genotype.as.random = TRUE, 
                   spatial = ~ SAP(range, row, nseg = c(rangeKnots, rowKnots)), 
                   data = environment.df)
    # Plot model
    plot.SpATS(model, main = paste0(response, ':', currEnvironment))
    # Extract BLUPS
    intercept <- model$coeff['Intercept']
    sp <- as_tibble(model$coeff, rownames = 'plotNumber') %>%
      mutate(environment = currEnvironment, 
             plotNumber = as.numeric(plotNumber)) %>%
      filter(!is.na(plotNumber)) %>%
      rowwise() %>%
      mutate(value = value + intercept) %>%
      rename('{response}':= value)
    # Bind to df
    df.sp <- bind_rows(df.sp, sp) %>%
      mutate(plotNumber = as.numeric(plotNumber))
    }
  print(length(df.sp$plotNumber))
  # Return df
  return(df.sp)
}
# Function to run variance partitioning
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with variance components
# df is the data frame
# response is the name of the response variable column
# label is a string to label the response on a plot
partitionVariance2 <- function(df, response, label) 
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
    mutate(pctVar = vcov/totalVar*100, 
           label = label) %>%
    select(responseVar, grp, vcov, pctVar, label)
  return(vc)
}

# Function to run variance partitioning
# Allows passing in right-hand side of model call as a string as modelStatement
# Returns data frame with variance components
# df is the data frame
# response is the name of the response variable column
# label is a string to label the response on a plot
partitionVariance3 <- function(df, response, label, modelStatement) 
{
  df <- filter(df, !is.na(response))
  lm_formula <- as.formula(paste(response, modelStatement))
  model <- lmer(lm_formula, data = df, na.action = na.omit)
  vc <- as.data.frame(VarCorr(model), row.names = TRUE, order = 'cov.last', comp = 'Variance') %>%
    as_tibble() %>%
    mutate(responseVar = response)
  totalVar <- sum(vc$vcov)
  vc <- vc %>%
    rowwise() %>%
    mutate(pctVar = vcov/totalVar*100, 
           label = label) %>%
    select(responseVar, grp, vcov, pctVar, label)
  return(vc)
}

# Least-squares estimate of FW linear plasticity
# From BQTP textbook pg 190
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with the linear plasticity
# data is a data frame
# trait is the response phenotype as a string
# envs is the column identifying the environments as a string
estimatePlasticity <- function(data, trait, envs)
{
  mu <- mean(data[[trait]], na.rm = TRUE)
  df <- data %>%
    group_by(.data[[envs]]) %>%
    mutate(traitMean = mean(.data[[trait]], na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(genotype, traitMean) %>%
    group_by(genotype)
  denominator <- sum((unique(df$traitMean) - mu)^2, na.rm = TRUE)
  df <- df %>%
    summarise('{trait}':= sum(.data[[trait]] * (traitMean - mu), na.rm = TRUE)/denominator)
  return(df)
}

# Least-squares estimate of FW linear plasticity across nitrogen treatments within a location
# From BQTP textbook pg 190
# ***Specific to HIPS data** Modify as needed. 
# Returns data frame with the linear plasticity
# data is a data frame
# response is the response phenotype as a string
# location is the column identifying the locations
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
    # fw <- FW(y = location.df[[response]], VAR = location.df$genotype, ENV = location.df$nitrogenTreatment, saveAt = paste0('analysis/gibbs-samples-', response, '-', currlocation),
    #          nIter = 51000, burnIn = 1000, thin = 10, seed = 3425656, saveVAR = c(1:2), saveENV = c(1:2))
    # pl <- fw$b %>%
    #   as_tibble(rownames = 'genotype') %>%
    #   mutate(location = currlocation, 
    #          '{response.out}':= Init1) %>%
    #   select(!Init1)
    pl <- estimatePlasticity3(location.df, response, 'nitrogenTreatment') %>%
      mutate(location = currlocation)
    response.df <- bind_rows(response.df, pl)
  }
  return(response.df)
}

# Least-squares estimate of FW linear plasticity
# From BQTP textbook pg 190
# Returns data frame with the linear plasticity and predicted performance in best and worst environments
# data is a data frame
# trait is the response phenotype as a string
# environment is the column identifying the environments as a string
# genotype is the column identifying the environments as a string
estimatePlasticity2 <- function(data, trait, environment, genotype)
{
  mu <- mean(data[[trait]], na.rm = TRUE)
  df <- data %>%
    group_by(.data[[environment]]) %>%
    mutate(t_j = mean(.data[[trait]], na.rm = TRUE) - mu,
           n = sum(!is.na(.data[[trait]]))) %>%
    filter(n > 0) %>%
    ungroup() %>%
    group_by(.data[[genotype]]) %>%
    mutate(g_i = mean(.data[[trait]], na.rm = TRUE) - mu) %>%
    ungroup() %>%
    group_by(.data[[environment]], .data[[genotype]]) %>%
    summarise(y_ij = mean(.data[[trait]], na.rm = TRUE),
              t_j = max(t_j, na.rm = TRUE),
              g_i = max(g_i, na.rm = TRUE))
  
  denominator <- sum(unique(df$t_j)^2)
  maxTj <- max(df$t_j)
  minTj <- min(df$t_j)
  
  df <- df %>%
    group_by(.data[[genotype]]) %>%
    summarise('{trait}.b' := sum(y_ij*t_j, na.rm = TRUE)/denominator,
              '{trait}.FWB' := mu + max(g_i, na.rm = TRUE) + (sum(y_ij*t_j, na.rm = TRUE)/denominator)*maxTj,
              '{trait}.FWW' := mu + max(g_i, na.rm = TRUE) + (sum(y_ij*t_j, na.rm = TRUE)/denominator)*minTj,
              '{trait}.mu' := mu + max(g_i, na.rm = TRUE))
  return(df)
}
#spFW
estimatePlasticity3 <- function(data, trait, environment, genotype, seed = SEED)
{
  df.compute <- data %>%
    filter(!is.na(.data[[trait]]))
  if(length(df.compute[[trait]]) < 1)
  {
    print(paste0('No data for ', trait, ' in this data.'))
    return(tibble('{genotype}' = NA, '{trait}.mu':= NA, '{trait}.FWB' := NA, '{trait}.FWW' := NA, '{trait}.b' := NA))
  }
  y <- df.compute[[trait]]
  geno <- df.compute[[genotype]]
  env <- df.compute[[environment]]
  
  model <- HFWM_est(Y = y,
                    VAR = geno,
                    ENV = env, 
                    seed = SEED)
  b <- as_tibble(model$b, rownames = 'genotype') %>%
    rowwise() %>%
    mutate(b = value + 1) %>%
    select(!value)
  g <- as_tibble(model$g, rownames = 'genotype') %>%
    rename(g = value)
  
  mu <- model$mu
  
  df <- full_join(b, g, join_by(genotype), keep = FALSE, suffix = c('', '')) %>%
    rowwise() %>%
    mutate('{trait}.mu' := mu + g,
           '{trait}.FWB' := mu + g + b*max(model$h), 
           '{trait}.FWW' := mu + g + b*min(model$h)) %>%
    rename('{trait}.b' := b) %>%
    select(!g)
  return(df)
}

getNitrogenPlasticityByLocationYear <- function(data, trait, nitrogenTreatment, genotype, locationYear)
{
  dfCompute <- data
  locationYears <- unique(dfCompute$locationYear)
  
  # Initialize tibble to save computed data to
  dfOut <- tibble(genotype = NULL, locationYear = NULL, '{trait}.b' := NULL, '{trait}.FWB' := NULL, '{trait}.FWW' := NULL, '{trait}.mu' := NULL)
  for(currLocationYear in locationYears)
  {
    dfLocationYear <- filter(dfCompute, locationYear==currLocationYear & !is.na(.data[[trait]]))
    
    if(length(unique(dfLocationYear[[nitrogenTreatment]])) < 2)
    {
      next
    }
    
    pl <- estimatePlasticity3(dfLocationYear, trait, nitrogenTreatment, genotype) %>% 
      mutate(locationYear = currLocationYear)
    dfOut <- bind_rows(dfOut, pl) %>%
      filter(!is.na(.data[[genotype]]))
  }
  return(dfOut)
}

getNitrogenPlasticityByLocationYearBlock <- function(data, trait, nitrogenTreatment, genotype)
{
  dfCompute <- data %>%
    rowwise() %>%
    mutate(locationYear = str_c(year, semanticLocation, sep = ':'))
  locationYears <- unique(dfCompute$locationYear)
  
  # Initialize tibble to save computed data to
  dfOut <- tibble(genotype = NULL, locationYear = NULL, '{trait}.b' := NULL, '{trait}.FWB' := NULL, '{trait}.FWW' := NULL, '{trait}.mu' := NULL, blockSet = NULL)
  for(currLocationYear in locationYears)
  {
    dfLocationYear <- filter(dfCompute, locationYear==currLocationYear)
    
    if(length(unique(dfLocationYear[[nitrogenTreatment]])) < 2)
    {
      next
    }
    # Randomly choose a block from each nitrogen level to use in the first set; use the other block in the second set
    blocks1 <- c(sample(1:2, 1), sample(3:4, 1), sample(5:6, 1))
    blocks2 <- setdiff(1:6, blocks1)
    df1 <- filter(dfLocationYear, block %in% blocks1)
    df2 <- filter(dfLocationYear, block %in% blocks2)
    
    pl1 <- estimatePlasticity3(df1, trait, nitrogenTreatment, genotype) %>%
      mutate(locationYear = currLocationYear,
             blockSet = 1)
    pl2 <- estimatePlasticity3(df2, trait, nitrogenTreatment, genotype) %>%
      mutate(locationYear = currLocationYear,
             blockSet = 2)
    
    dfOut <- bind_rows(dfOut, pl1, pl2)
  }
  return(dfOut)
}
# First, a function to calculate GDDs for a single day in fahrenheit
# Returns GDD value for the given day
# minTemp is the daily minimum temperature in Fahrenheit
# maxTemp is the daily maximum temperature in Fahrenheit
getGDDs <- function(minTemp, maxTemp)
{
  cropMinTemp <- 50
  cropMaxTemp <- 86
  min <- minTemp
  max <- maxTemp
  
  # Reassign min and max if they are outside the bounds of the crop's min and max temps for growth
  if(min <= cropMinTemp)
  {
    min <- cropMinTemp
  }
  
  if(max <= cropMinTemp)
  {
    max <- cropMinTemp
  }
  
  if(max >= cropMaxTemp)
  {
    max <- cropMaxTemp
  }
  
  if(min >= cropMaxTemp)
  {
    min <- cropMaxTemp
  }
  GDD <- (min + max)/2 - cropMinTemp
  return(GDD)
}


# Function to calculate GDDs accumulated between 2 dates at a given location
# ***Specific to HIPS data** Modify as needed. 
# start is the start date as a POSIX date
# end is the end date as a POSIX date
# weather is the data frame with a GDD column of daily GDDs
# location is the location to get the cumulative GDDs at
getCumulativeGDDs <- function(start, end, weather, location)
{
  if(is.na(start) | is.na(end))
  {
    return(NA)
  }
  weather.df <- filter(weather, location==location & date %in% 
                         seq(ymd(min(start, end, na.rm = TRUE)), ymd(max(start, end, na.rm = TRUE)), 'days'))
  cumulativeGDDs <- sum(weather.df$GDD)
  return(cumulativeGDDs)
}

# Function to check for duplicates, given a vector of grouping variables in the dataframe
getDuplicates <- function(data, ...)
{
  duplicates <- data %>%
    group_by(...) %>%
    mutate(n = n()) %>%
    filter(n > 1)
  
  return(duplicates)
}

# Function to change parsed genotypes based on a key with the first column as the original genotypes and the second column with the genotype they should be 
fixGenos <- function(data, givenKey)
{
  key <- givenKey
  df_ok <- filter(data, !(genotype %in% key$orig))
  print(unique(df_ok$genotype))
  df_fix <- filter(data, genotype %in% key$orig)
  print(unique(df_fix$genotype))
  df_fix <- full_join(df_fix, key, join_by(genotype == orig), keep = FALSE, relationship = 'many-to-one')
  df_fix %>% select(starts_with('correct')) %>% print()
  df_fix <- df_fix %>%
    rowwise() %>%
    mutate(genotype = correct) %>%
    select(!starts_with('correct'))
  df <- bind_rows(df_ok, df_fix)
  return (df)
}

# Create key
hips1.5genos_fix <- c("HOEGEMEYER 7089 AMX", "SYNGENTA NK0760-311", "PIONEER P0589 AMX", "SYNGENTA NK0760-31", "SYNGENTA NK0760-3", "HOEGEMEYER 7089 A", 
                      "HOEGEMEYER 7089 AM", "PIONEER P0589 AM", "HOEGEMEYER 7089", "SYNGENTA NK0760-", "HOEGEMEYER 8065R", "PIONEER 1311 AMX",
                      "COMMERCIAL HYBRID 5", "COMMERCIAL HYBRID 4", "COMMERCIAL HYBRID 3", "COMMERCIAL HYBRID 2", "COMMERCIAL HYBRID 1", "", "MO17 FILLER", 
                      '4N506 X 3IIH!6', 'PHP02 X PHJ894')
hips1.5genos_correct <- c("HOEGEMEYER 7089 AMXT", 'SYNGENTA NK0760-3111', "PIONEER P0589 AMXT", rep('SYNGENTA NK0760-3111', 2), rep("HOEGEMEYER 7089 AMXT", 2), 
                          "PIONEER P0589 AMXT", "HOEGEMEYER 7089 AMXT", 'SYNGENTA NK0760-3111', "HOEGEMEYER 8065RR", "PIONEER 1311 AMXT", 
                          'SYNGENTA NK0760-3111', 'HOEGEMEYER 7089 AMXT', 'HOEGEMEYER 8065RR', 'PIONEER P0589 AMXT', 'PIONEER 1311 AMXT', NA, 'MO17', 
                          '4N506 X 3IIH6', 'PHP02 X PHJ89')
hips1.5_genoFixKey <- tibble(orig = hips1.5genos_fix, correct = hips1.5genos_correct)

# NP uses this formula: accounts for moisture and converts to standard bushels (56 lbs at 15.5% moisture)
# This is a pretty standard formula; equivalent to those given by UW-Madison and other Extension sources 
# Does not account for differences in test weight
buPerAc15.5 <- function(harvestWeightLbs, harvestMoisture, plotLen)
{
  if (is.na(harvestWeightLbs) | is.na(harvestMoisture) | is.na(plotLen))
  {
    return (NA)
  }
  else
  {
    dryMatter <- harvestWeightLbs*(100 - harvestMoisture)/100
    stdMoistureBu <- dryMatter/47.32 # At 15.5% moisture, there are 47.32 lbs dry matter in a bushel with a test weight of 56 lbs
    portionAc <- 43560/(5*plotLen) # sq ft/ac divided by square feet per harvested plot area: 2 rows with 30" spacing = 5' x plot length in ft
    buPerAc <- stdMoistureBu*portionAc
    return(buPerAc)
  }
}

# First, a function to calculate GDDs for a single day in fahrenheit
getGDDs <- function(minTemp, maxTemp)
{
  cropMinTemp <- 50
  cropMaxTemp <- 86
  min <- minTemp
  max <- maxTemp
  
  # Reassign min and max if they are outside the bounds of the crop's min and max temps for growth
  if(min <= cropMinTemp)
  {
    min <- cropMinTemp
  }
  
  if(max <= cropMinTemp)
  {
    max <- cropMinTemp
  }
  
  if(max >= cropMaxTemp)
  {
    max <- cropMaxTemp
  }
  
  if(min >= cropMaxTemp)
  {
    min <- cropMaxTemp
  }
  GDD <- (min + max)/2 - cropMinTemp
  return(GDD)
}

# Function to calculate GDDs between 2 dates
getCumulativeGDDs <- function(start, end, weather, loc)
{
  if(is.na(start) | is.na(end))
  {
    return(NA)
  }
  start <- as.POSIXct(start, format = '%F')
  end <- as.POSIXct(end, format = '%F')
  # dates <- seq(min(start, end), max(start, end), 'days')
  loc <- as.character(loc)
  weather.df <- filter(weather, location==loc)
  weather.df <- filter(weather.df, (start <= date) & (end >= date))
  cumulativeGDDs <- sum(weather.df$GDD)
  return(cumulativeGDDs)
}  

# Function to calculate R2 between two variables
getR2 <- function(data, x, y)
{
  r2 <- cor(data[[x]], data[[y]], use = 'complete.obs')^2
  return(r2)
}

# Function to calculate R between two variables
getR <- function(data, x, y)
{
  r <- cor(data[[x]], data[[y]], use = 'complete.obs')
  return(r)
}

# Modifies dataframe genotypePairs with columns genotype1 and genotype2 at a minimum that identify all pairwise comparisons between genotypes
# Remove dashes from genotype names before using so we can split the comparisons in the tukey step
getSignificantCrossovers <- function(data, pheno, environments)
{
  phenotype <- paste0(pheno, '.sp')
  phenotypeMean <- paste0(pheno, 'Mean')
  phenotypeRank <- paste0(pheno, 'Rank')
  phenotypeAdjustedP <- paste0(pheno, 'AdjP')
  phenotypeSigDiff <- paste0(pheno, 'SigDiff')
  phenotypeRankChange <- paste0(pheno, 'RC')
  phenotypeScore <- paste0(pheno, 'Score')
  phenotypeComparedEnvs <- paste0(pheno, 'ComparedEnvs')
  
  for(env in environments)
  {
    envSuffix <- paste0('.E', env)
    
    environmentData <- data %>%
      filter(environmentCode==env) %>%
      select(genotype, all_of(phenotype))
    environmentData <- environmentData[complete.cases(environmentData), ]
    
    if(length(environmentData[[phenotype]]) < 1|length(unique(environmentData$genotype)) < 1){next}
    
    anova <- aov(as.formula(paste(phenotype, ' ~ genotype')), data = environmentData)
    
    tukey <- TukeyHSD(anova)$genotype %>%
      as_tibble(rownames = 'genotypes') %>%
      rowwise() %>%
      mutate(genotype1 = str_split_i(genotypes, '-', 1),
             genotype2 = str_split_i(genotypes, '-', 2)) %>%
      rename('{phenotypeAdjustedP}' := `p adj`) %>%
      mutate('{phenotypeSigDiff}' := .data[[phenotypeAdjustedP]] < 0.05) %>%
      select(c(genotypes, genotype1, genotype2,  all_of(c(phenotypeAdjustedP, phenotypeSigDiff))))
    
    environmentDataSummary <- data %>%
      filter(environmentCode==env) %>%
      group_by(genotype) %>%
      summarise('{phenotypeMean}' := mean(.data[[phenotype]], na.rm = TRUE)) %>%
      mutate('{phenotypeRank}' := dense_rank(desc(.data[[phenotypeMean]]))) %>%
      select(c(all_of(phenotypeRank), genotype))
    
    envG1Suffix <- paste0(envSuffix, '.G1')
    envG2Suffix <- paste0(envSuffix, '.G2')
    
    genotypePairs <- left_join(genotypePairs, tukey, join_by(genotype1==genotype1, genotype2==genotype2), keep = FALSE, suffix = c('', '')) %>%
      bind_rows(left_join(genotypePairs, tukey, join_by(genotype1==genotype2, genotype2==genotype1), keep = FALSE, suffix = c('', ''))) %>%
      distinct(genotype1, genotype2, .keep_all = TRUE) %>%
      rename('{phenotypeAdjustedP}{envSuffix}' := .data[[phenotypeAdjustedP]],
             '{phenotypeSigDiff}{envSuffix}' := .data[[phenotypeSigDiff]]) %>%
      full_join(environmentDataSummary, join_by(genotype1==genotype), keep = FALSE, suffix = c('', ''), relationship = 'many-to-one') %>%
      rename('{phenotypeRank}{envG1Suffix}' := .data[[phenotypeRank]]) %>%
      full_join(environmentDataSummary, join_by(genotype2==genotype), keep = FALSE, suffix = c('', ''), relationship = 'many-to-one') %>%
      rename('{phenotypeRank}{envG2Suffix}' := .data[[phenotypeRank]])
  }
  
  cols <- colnames(genotypePairs)
  for(i in 1:totalEnvironments)
  {
    envI <- environments[i]
    if(is.na(envI)){next} 
    envISuffix <- paste0('.E', envI)
    envIG1Rank <- paste0(phenotypeRank, envISuffix, '.G1')
    envIG2Rank <- paste0(phenotypeRank, envISuffix, '.G2')
    envISigDiff <- paste0(phenotypeSigDiff, envISuffix)
    
    if(length(setdiff(c(envISigDiff, envIG1Rank, envIG2Rank), cols)) > 0){next}
    
    for(j in (i + 1):totalEnvironments)
    {
      envJ <- environments[j]
      if(is.na(envJ)){next} 
      envJSuffix <- paste0('.E', envJ)
      envJG1Rank <- paste0(phenotypeRank, envJSuffix, '.G1')
      envJG2Rank <- paste0(phenotypeRank, envJSuffix, '.G2')
      envJSigDiff <- paste0(phenotypeSigDiff, envJSuffix)
      
      if(length(setdiff(c(envJSigDiff, envJG1Rank, envJG2Rank), cols)) > 0){next}
      
      envPairSuffix <- paste0('.E', envI, '-', envJ)
      envPairRankChange <- paste0(phenotypeRankChange, envPairSuffix)
      envPairScore <- paste0(phenotypeScore, envPairSuffix)
      
      genotypePairs <- genotypePairs %>%
        rowwise() %>%
        mutate('{envPairRankChange}' := ((.data[[envIG1Rank]] - .data[[envIG2Rank]])/(.data[[envJG1Rank]] - .data[[envJG2Rank]])) < 0) %>%
        mutate('{envPairScore}' := case_when(!.data[[envPairRankChange]] ~ 0, 
                                             .data[[envPairRankChange]] ~ .data[[envISigDiff]] + .data[[envJSigDiff]]))
    }
  }
  
  genotypePairs <- genotypePairs %>%
    rowwise() %>%
    mutate('{phenotypeScore}' := rowSums(across(contains(phenotypeScore))), 
           '{pheno}ComparedEnvs' := rowSums(!is.na(across(contains(phenotypeAdjustedP))))) %>%
    mutate('{phenotypeScore}Normalized' := .data[[phenotypeScore]]/((.data[[phenotypeComparedEnvs]]*(.data[[phenotypeComparedEnvs]] - 1))))
  return(genotypePairs)
}

plotInteractionImportanceGrid <- function(significantInteractionsData = sigCrossovers, 
                                          performanceData = hybrids, trait, traitLabel, 
                                          legendTitle = str_wrap('Normalized Interaction Importance Score', 25),
                                          legendPosition = 'right', legendTextAngle = 0, 
                                          legendTextHJust = 1, xAxisLabelAngle = 0)
{
  phenotype <- trait
  phenotypeSpatial <- paste0(phenotype, '.sp')
  phenotypeLabel <- traitLabel
  phenotypeScoreNormalized <- paste0(phenotype, 'ScoreNormalized')
  
  df1 <- significantInteractionsData %>%
    select(c(genotype1, genotype2, all_of(phenotypeScoreNormalized)))
  df2 <- df1 %>%
    rowwise() %>%
    mutate(genotype1A = genotype2, 
           genotype2 = genotype1) %>%
    select(c(genotype1A, genotype2, all_of(phenotypeScoreNormalized))) %>%
    rename(genotype1 = genotype1A)
  
  df <-bind_rows(df1, df2) %>% 
    filter(!is.na(.data[[phenotypeScoreNormalized]]))
  
  blups <- lmer(as.formula(paste0(phenotypeSpatial, ' ~ environment + (1|genotype)')), data = performanceData) 
  blups <- ranef(blups)
  blups <- as_tibble(blups$genotype, rownames = 'genotype') %>%
    rename(blup = `(Intercept)`) %>%
    mutate(rank = dense_rank(blup)) %>%
    select(genotype, rank)
  
  df <- left_join(df, blups, join_by(genotype1==genotype), keep = FALSE, suffix = c('', ''), relationship = 'many-to-one') %>%
    rename(rankG1 = rank) %>%
    left_join(blups, join_by(genotype2==genotype), keep = FALSE, suffix = c('', '')) %>%
    rename(rankG2 = rank) %>%
    # filter(!is.na(.data[[phenotypeScoreNormalized]]))
    select(c(genotype1, genotype2, rankG1, rankG2, all_of(phenotypeScoreNormalized)))
  
  heatmap <- ggplot(df, aes(rankG1, rankG2, fill = .data[[phenotypeScoreNormalized]])) + 
    geom_tile() + 
    scale_x_continuous(limits = c(0, 120)#, 
                      # breaks = seq.int(0, 120, by = 2)
                      ) +
    scale_y_continuous(limits = c(0, 120)#, 
                       # breaks = seq.int(0, 120, by = 2)
                       ) +
    # guides(fill = guide_colourbar(barheight = 0.5)) +
    scale_fill_viridis(direction = -1, 
                       limits = c(0, 0.5)
                       ) +
    labs(x = 'Hybrid Rank', y = 'Hybrid Rank', fill = legendTitle, title = phenotypeLabel) + 
    theme_minimal() +
    theme(text = element_text(color = 'black', size = 9),
          axis.text.x = element_text(color = 'black', size = 9, angle = xAxisLabelAngle),
          axis.text = element_text(color = 'black', size = 9),
          legend.text = element_text(color = 'black', size = 9, angle = legendTextAngle, hjust = legendTextHJust, vjust = 1),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(), 
          legend.position = legendPosition,
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 9))
  return(heatmap)
}

plotNPlasticityCor <- function(nitrogenResponsePlasticityData = nResponse.pl, trait, traitLabel, 
                           legendPosition = 'right')
{
  plasticity <- paste0(trait, '.sp.b')
  dfWide <- nResponse.pl %>%
    pivot_wider(id_cols = genotype, 
                names_from = locationYear,
                values_from = .data[[plasticity]]) #%>% 
    # select(where(~!all(is.na(.))))
  
  corData <- cor(dfWide[, 2:length(colnames(dfWide))], use = 'complete.obs', method = 'spearman') %>%
    as.table() %>%
    as.data.frame()
  names(corData) <- c('locationYear1', 'locationYear2', 'nPlasticityCor')
  print(min(corData$nPlasticityCor, na.rm = TRUE))
  print(paste0(trait, ':', min(corData$nPlasticityCor, na.rm = TRUE), '-', max(corData$nPlasticityCor[corData$nPlasticityCor!=1], na.rm = TRUE)))
  
  plot <- ggplot(corData, aes(locationYear1, locationYear2, fill = nPlasticityCor)) +
    geom_tile(color = 'white') +
    scale_fill_viridis_c(direction = -1, limits = c(-0.25, 1)) + 
    scale_x_discrete(breaks = unique(corData$locationYear1),
                     labels = label_wrap(10)) +
    scale_y_discrete(breaks = unique(corData$locationYear1),
                     labels = label_wrap(10)) +
    guides(fill = guide_colourbar(barwidth = 1,
                                  barheight = 8)) +
    labs(x = '', y = '', fill = str_wrap('Nitrogen Plasticity Correlation', 1), title = traitLabel) + 
    theme_minimal() +
    theme(text = element_text(color = 'black', size = 9),
          axis.text.x = element_text(color = 'black', size = 9, angle = 90, vjust = 0.5),
          axis.text = element_text(color = 'black', size = 9, hjust = 1, margin = margin(0, 0, 0, 0)),
          axis.line = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(), 
          plot.background = element_blank(), 
          legend.position = legendPosition,
          legend.title.align = 0,
          plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
          legend.background = element_blank())
  return(plot)
}

plotNitrogenPlasticityBlockCor <- function(nitrogenBlockPlasticity, phenotype, phenotypeLabel)
{
  phenotype.pl <- paste0(phenotype, '.sp.b')
  nResponseBlockWide <- nitrogenBlockPlasticity %>%
    pivot_wider(id_cols = c(genotype, locationYear), names_from = blockSet, values_from = .data[[phenotype.pl]], names_prefix = 'b')
  
  nResponseBlockRho <- cor(nResponseBlockWide$b1, nResponseBlockWide$b2, use = 'complete.obs', method = 'spearman')
  
  nResponseCorr <- ggplot(nResponseBlockWide, aes(b1, b2)) +
    geom_point(color = viridis_pal()(4)[1]) + 
    labs(x = 'Nitrogen Plasticity - Block 1', y = 'Nitrogen Plasticity - Block 2', title = phenotypeLabel,
         subtitle = paste0('Spearman Rank Correlation: ', round(nResponseBlockRho, 4))) + 
    scale_x_continuous(limits = c(-1.15, 1.75)) +
    scale_y_continuous(limits = c(-1.15, 1.75)) +
    theme_minimal() +
    theme(axis.text.x = element_text(color = 'black', size = 9),
          axis.text.y = element_text(color = 'black', size = 9),
          plot.title = element_text(color = 'black', size = 9, hjust = 0.5),
          text = element_text(color = 'black', size = 9),
          panel.grid = element_blank())
  print(nResponseCorr)
}

# All arguments except data should be strings
estimatePercentEnvironmentMeanPlasticity <- function(data, genotype, environmentMean, percentEnvironmentMeanPhenotype)
{
  lm_formula <- as.formula(paste0(percentEnvironmentMeanPhenotype, ' ~ ', genotype,  ' + ', genotype, '*', environmentMean))
  model <- lm(lm_formula, data = data, na.action = na.omit)
  mu <- model$coefficients['(Intercept)']
  environmentMeanEffect <- model$coefficients[environmentMean]
  df.pl <- model$coefficients %>%
    as_tibble(rownames = 'genotype') %>%
    rowwise() %>%
    mutate(valueID = case_when(str_detect(genotype, ':environmentMean') ~ 'slope', .default = 'intercept'), 
           genotype = str_remove(genotype, 'genotype') %>%
             str_remove(':environmentMean')) %>%
    pivot_wider(id_cols = genotype, 
                names_from = valueID, 
                values_from = value) %>%
    filter(!(genotype %in% c('(Intercept)', environmentMean))) %>%
    mutate(expectedValue = intercept + mu, 
           b = slope + environmentMeanEffect + 1)
  return(df.pl)
}
