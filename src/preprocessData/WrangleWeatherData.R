library(tidyverse)
library(readxl)
library(readr)
library(grDevices)
library(lubridate)
library(weathermetrics)
library(daymetr)

# Time to wrangle the weather data to a daily level so we can calculate GDDs
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
## No data for Crawfordsville from HIPS 2022 field so use the data from the nearby G2F station as suggested by Lisa
## This has already been imputed using NASA; doi:10.25739/3d3g-pe51
weather.cf <- read.table('data/weather/de.cyverse.org_anon-files__iplant_home_shared_commons_repo_curated_GenomesToFields_G2F_data_2022_b._2022_weather_data_g2f_2022_weather_cleaned.csv', header = TRUE, sep = ',')
# Filter to crawfordsville
weather.cf <- weather.cf %>% 
  filter(Field.Location=='IAH1') %>%
  rowwise() %>%
  mutate(Date_key = as.POSIXct(Date_key), 
         date = as.POSIXct(paste0(Year, '-', Month, '-', Day), format = '%F')) %>%
  group_by(date) %>%
  summarise(maxTemp = max(Temperature..C., na.rm = TRUE) %>%
              celsius.to.fahrenheit(),
            minTemp = min(Temperature..C., na.rm = TRUE) %>%
              celsius.to.fahrenheit(),
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp), 
            loc = 'Crawfordsville')
season.cf <- seq(as.POSIXct('2022-05-11', format = '%F'), as.POSIXct('2022-10-07', format = '%F'), 'days')
days.cf <- unique(weather.cf$date)
impute.cf <- setdiff(season.cf, days.cf) # Empty so we can proceed

weather.ames <- read_excel('data/weather/HIPS_Ames_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.ames <- weather.ames %>%
  filter(is.na(`flagged by LC`)) %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(maxTemp = max(`2022 HIPS Ames: H: Temperature`, na.rm = TRUE),
            minTemp = min(`2022 HIPS Ames: H: Temperature`, na.rm = TRUE),
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp), 
            loc = 'Ames')
season.ames <- seq(as.POSIXct('2022-05-23', format = '%F'), as.POSIXct('2022-10-16', format = '%F'), 'days') %>%
  as.character()
days.ames <- unique(weather.ames$date) %>%
  as.character()
impute.ames <- setdiff(season.ames, days.ames) # Not empty, so we need to get data from daymet for these days
ames.lat <- mean(42.015354, 42.012376)
ames.lon <- mean(-93.732519, -93.737301)
ames.daymet <- download_daymet(site = 'Ames', lat = ames.lat, lon = ames.lon, start = 2022, end = 2022)
ames.daymet <- ames.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'Ames') %>%
  filter(date %in% impute.ames) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.ames <- bind_rows(weather.ames, ames.daymet)

weather.mv <- read_excel('data/weather/HIPS_MO_Valley_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.mv <- weather.mv %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(`2022 MO Valley: H: Temperature`, na.rm = TRUE),
            maxTemp = max(`2022 MO Valley: H: Temperature`, na.rm = TRUE),
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'Missouri Valley')
season.mv <- seq(as.POSIXct('2022-04-29', format = '%F'), as.POSIXct('2022-10-11', format = '%F'), 'days') %>%
  as.character()
days.mv <- unique(weather.mv$date) %>%
  as.character()
impute.mv <- setdiff(season.mv, days.mv)
mv.lat <- 41.671747
mv.lon <- -95.943982
mv.daymet <- download_daymet(site = 'Missouri Valley', lat = mv.lat, lon =  mv.lon, start = 2022, end = 2022)
mv.daymet <- mv.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'Missouri Valley') %>%
  filter(date %in% impute.mv) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.mv <- bind_rows(weather.mv, mv.daymet)

weather.lnk <- read_excel('data/weather/HIPS_Lincoln_2022.xlsx', sheet = 'Sheet1', skip = 2)
weather.lnk <- weather.lnk %>%  rowwise() %>%
  mutate(date = as.character(`...2`) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(TMP, na.rm = TRUE),
            maxTemp = max(TMP, na.rm = TRUE),
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'Lincoln')
season.lnk <- seq(as.POSIXct('2022-05-22', format = '%F'), as.POSIXct('2022-10-10', format = '%F'), 'days') %>%
  as.character()
days.lnk <- weather.lnk$date %>%
  as.character()
impute.lnk <- setdiff(season.lnk, days.lnk)
lnk.lat <- mean(40.852386, 40.852014, 40.852417, 40.851978)  
lnk.lon <- mean(-96.613961, -96.616817, -96.616811, -96.613969)
lnk.daymet <- download_daymet(site = 'Lincoln', lat = lnk.lat, lon = lnk.lon, start = 2022, end = 2022)
lnk.daymet <- lnk.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'Lincoln') %>%
  filter(date %in% impute.lnk) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.lnk <- bind_rows(weather.lnk, lnk.daymet)

weather.np1 <- read_excel('data/weather/HIPS_North_Platte_Full_Irrigation_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.np1 <- weather.np1 %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(`2022 North Platte - Full Irrigation: H: Temperature`, na.rm = TRUE),
            maxTemp = max(`2022 North Platte - Full Irrigation: H: Temperature`, na.rm = TRUE), 
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'North Platte1')
season.np1 <- seq(as.POSIXct('2022-05-17', format = '%F'), as.POSIXct('2022-11-01', format = '%F'), 'days') %>%
  as.character()
days.np1 <- weather.np1$date %>%
  as.character()
impute.np1 <- setdiff(season.np1, days.np1)
np.lat <- c(41.084583)
np.lon <- (-100.780911)
np.daymet <- download_daymet(site = 'North Platte1', lat = np.lat, lon = np.lon, start = 2022, end = 2022)
np1.daymet <- np.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'North Platte1') %>%
  filter(date %in% impute.np1) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.np1 <- bind_rows(weather.np1, np1.daymet)

weather.np2 <- read_excel('data/weather/HIPS_North_Platte_Reduced_Irrigation_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.np2 <- weather.np2 %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(`2022 North Platte - Reduced Irr: H: Temperature`, na.rm = TRUE),
            maxTemp = max(`2022 North Platte - Reduced Irr: H: Temperature`, na.rm = TRUE), 
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'North Platte2')
season.np2 <- seq(as.POSIXct('2022-05-17', format = '%F'), as.POSIXct('2022-10-28', format = '%F'), 'days') %>%
  as.character()
days.np2 <- weather.np2$date %>%
  as.character()
impute.np2 <- setdiff(season.np2, days.np2)
np2.daymet <- np.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'North Platte2') %>%
  filter(date %in% impute.np2) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.np2 <- bind_rows(weather.np2, np2.daymet)

weather.np3 <- read_excel('data/weather/HIPS_North_Platte_No_Irrigation_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.np3 <- weather.np3 %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(`2022 North Platte - Non-Irrigated: H: Temperature`, na.rm = TRUE),
            maxTemp = max(`2022 North Platte - Non-Irrigated: H: Temperature`, na.rm = TRUE), 
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'North Platte3')
season.np3 <- seq(as.POSIXct('2022-05-18', format = '%F'), as.POSIXct('2022-10-24', format = '%F'), 'days') %>%
  as.character()
days.np3 <- weather.np3$date %>%
  as.character()
impute.np3 <- setdiff(season.np3, days.np3)
np3.daymet <- np.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'North Platte3') %>%
  filter(date %in% impute.np3) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.np3 <- bind_rows(weather.np3, np3.daymet)

weather.sb <- read_excel('data/weather/HIPS_Scottsbluff_2022.xlsx', sheet = 'Weather Data', skip = 1)
weather.sb <- weather.sb %>%
  filter(`2022 HIPS Scottsbluff: H: Temperature` < 120) %>%
  rowwise() %>%
  mutate(date = as.character(Timestamp) %>%
           str_split_i(' ', 1) %>%
           as.POSIXct(format = '%F')) %>%
  group_by(date) %>%
  summarise(minTemp = min(`2022 HIPS Scottsbluff: H: Temperature`, na.rm = TRUE),
            maxTemp = max(`2022 HIPS Scottsbluff: H: Temperature`, na.rm = TRUE), 
            GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
            loc = 'Scottsbluff')
season.sb <- seq(as.POSIXct('2022-05-19', format = '%F'), as.POSIXct('2022-11-11', format = '%F'), 'days') %>%
  as.character() %>%
  str_split_i(' ', 1)
days.sb <- weather.sb$date %>%
  as.character()
impute.sb <- setdiff(season.sb, days.sb)
sb.lat <- mean(41.933593, 41.950014)
sb.lon <- mean(-103.700056, -103.700046)
sb.daymet <- download_daymet(lat = sb.lat, lon = sb.lon, start = 2022, end = 2022)
sb.daymet <- sb.daymet$data %>%
  rowwise() %>%
  mutate(date = as.POSIXct(str_c(year, yday, sep = '-'), format = '%Y-%j') %>%
           format(format = '%F') %>%
           as.character(), 
         minTemp = tmin..deg.c. %>%
           celsius.to.fahrenheit(),
         maxTemp = tmax..deg.c. %>%
           celsius.to.fahrenheit(),
         GDD = getGDDs(minTemp = minTemp, maxTemp = maxTemp),
         loc = 'Scottsbluff') %>%
  filter(date %in% impute.sb) %>%
  mutate(date = as.POSIXct(date, format = '%F'))
weather.sb <- bind_rows(weather.sb, sb.daymet)

weather.daily <- bind_rows(weather.cf, weather.ames, weather.mv, weather.lnk, weather.np1, weather.np2, weather.np3, weather.sb) %>%
  filter(!is.na(date)) %>%
  select(c(date, minTemp, maxTemp, GDD, loc))

# Function to calculate GDDs between 2 dates
getCumulativeGDDs <- function(start, end, weather, location)
{
  if(is.na(start) | is.na(end))
  {
    return(NA)
  }
  start <- as.POSIXct(start, format = '%F')
  end <- as.POSIXct(end, format = '%F')
  # dates <- seq(min(start, end), max(start, end), 'days')
  location <- as.character(location)
  weather.df <- filter(weather, loc==location)
  weather.df <- filter(weather.df, (start <= date) & (end >= date))
  cumulativeGDDs <- sum(weather.df$GDD)
  return(cumulativeGDDs)
}  
