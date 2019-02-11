##  We need to create a big spreadsheet of data that we will use later 
##  for the inequality index. This is for every imd year so this may
##  take a wee while and requires us to pay attention to the sourtce files
##  This is for Scotland

source('UI paper 1 source.R')

### This is the neater formatting for the Scottish data
##  Again the format is the same as the English
##  For ease we will create a variable called 'zone' to always do the merger


##  Essential step: load in commonly used data ----
##  So this is lookups for area size (for summary stats); employment access
##  and distance to centre
##  All these variable already have 'zone' as a var

area.tab <- 
  './Saved generated data/Area size for lsoa and datazones lkp.csv' %>%
  read.csv

accessdz.01 <- 
  'Saved generated data/emp access dz01 lkp.csv' %>% read.csv %>%
  mutate(type = '')
accessdz.11 <- 
  'Saved generated data/emp access dz11 lkp.csv' %>% read.csv

dist <-
  'Saved generated data/Distance from nearest centre for zones and TTWA lkp.csv' %>%
  read.csv

pm25 <- 'Saved generated data/Annual avg air pollution dz and lsoa.csv' %>%
  read.csv
pm25 %>% head

##  1) 2004 scot data -----

##  Load in primarily imd data
geo <- 
  google.drive.spatial %>% paste0('/Scottish IMDs/2004/simd 04 geo access.csv') %>% 
  read.csv 
geo <- geo %>%
  mutate(zone = Data.Zone)

house <- 
  google.drive.spatial %>% paste0('/Scottish IMDs/2004/simd 04 housing.csv') %>% 
  read.csv
house <- house %>%
  mutate(zone = Data.Zone)

pop <- 
  google.drive.spatial %>% paste0('/Scottish IMDs/2004/simd 04.csv') %>% read.csv
pop <- pop %>%
  mutate(zone = Data.Zone)

##  mergers of geodata
imd <- pop %>% 
  left_join(geo, by = 'zone') %>% 
  left_join(house, by = 'zone') %>% 
  left_join(accessdz.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz01'), 
            by = 'zone') 


##  Some SIMD varianbles have issues; income deprivation ns are missing
summary(imd); 
missing <- imd %>% subset(is.na(Number.of.Current.Income.Deprived))
summary(missing) # all missing cases have the highest SIMD income rank indciatng
# missingness is due to 0 values
imd$Number.of.Current.Income.Deprived[is.na(imd$Number.of.Current.Income.Deprived)] <- 0
summary(imd)

##  putting in the proportion of income deprived and non income deprived
imd$inc.n <- imd$Number.of.Current.Income.Deprived ## this is for clarity
imd$noninc.n <- imd$Total.Population..2001.Census. - imd$Number.of.Current.Income.Deprived

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- data.frame(year = 2004,
                       country = 'Scotland',
                       zonecd = imd$Data.Zone,
                       la = imd$Local.Authority.Name,
                       ttwa = imd$ttwa01,
                       inc.n = imd$inc.n,
                       noninc.n = imd$noninc.n,
                       pop = imd$inc.n + imd$noninc.n,
                       dist_main = imd$dist_cen,
                       dist_nearest = imd$dist_nearest,
                       crime = NA,# No crime data for 2004
                       live.in = imd$Housing.domain.rank,
                       live.out = imd$pol.score %>% {-1 * .} %>% rank,
                       geo = imd$Geographic.Access.and.Telecommunications.domain.rank,
                       area = imd$area,
                       pm25 = imd$year2004 %>% {-1 * .} %>% rank,
                       work = imd$access)

##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('./Results/Formatted Scotland data 2004.csv')
rm(neat.tab)

##  2) Scottish 2006 data formatting ----
dz.access <- './Results/Scot access to emp 2001.csv' %>% read.csv

##  Step two: Batch merge with the imd
geo <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 geo access.csv' %>% read.csv
house <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 housing.csv' %>% read.csv
crime <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 crime.csv' %>% read.csv
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06.csv' %>% read.csv
live.out <- google.drive.spatial %>% paste0('/Scottish air pollution/scot air pollution from Gavin longform.csv' %>%
  read.csv %>% subset(time == '2006')##this is the indicator of air quality from gaving

pm25 <- './Results/Defra grid lookups/census zonal pm25 levels.csv' %>% read.csv
pm25 <- pm25 %>% subset(type == 'dz01')

##  mergers of geodata
imd <- pop %>% merge(geo) %>% merge(house, by = 'Data.Zone') %>% 
  merge(crime, by = 'Data.Zone') %>%
  merge(dz.access, by.x = 'Data.Zone', by.y = 'lsoa') %>%
  merge(live.out, by.x = 'Data.Zone', by.y = 'datazone') %>%
  merge(area.tab %>% subset(type == 'dz01'), 
        by.x = 'Data.Zone', by.y = 'zone', all.x = T) %>%
  merge(pm25, by.x = 'Data.Zone', by.y = 'zone')

##  Some blank spaces on income dep and crime indicate zero; although only inc matters
summary(imd); 
miss.inc <- is.na(imd$Number.of.Current.Income.Deprived.People.2006)
imd$Number.of.Current.Income.Deprived.People.2006[miss.inc] <- 0


##  putting in the proportion of income deprived and non income deprived
imd$inc.n <- imd$Number.of.Current.Income.Deprived.People.2006 ## this is for clarity
imd$noninc.n <- imd$Total.Population..SAPE.2004. - imd$Number.of.Current.Income.Deprived.People.2006

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- data.frame(year = 2006,
                       country = 'Scotland',
                       zonecd = imd$Data.Zone,
                       la = imd$Local.Authority.Name,
                       ttwa = imd$ttwa01,
                       inc.n = imd$inc.n,
                       noninc.n = imd$noninc.n,
                       pop = imd$inc.n + imd$noninc.n,
                       dist_main = imd$dist_cen,
                       dist_nearest = imd$dist_nearest,
                       crime = imd$SIMD.Crime.2006.rank.x,
                       live.in = imd$Housing.domain.rank.2004.and.2006.x,
                       live.out = imd$pol.score %>% {-1 * .} %>% rank,
                       geo = imd$Geographic.Access.domain.2006.rank,
                       area = imd$area,
                       pm25 = imd$year2006 %>% {-1 * .} %>% rank,
                       work = imd$access)

##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('./Results/Formatted Scotland data 2006.csv')
rm(neat.tab)

##  Step 3: Scot data 2009----
##  ~~~~~ ##
dz.access <- './Results/Scot access to emp 2001.csv' %>% read.csv

##  Step two: Batch merge with the imd
geo <- google.drive.spatial %>% paste0('/Scottish IMDs/2009/simd 09 geo access.csv' %>% read.csv
house <- google.drive.spatial %>% paste0('/Scottish IMDs/2009/simd 09 housing.csv' %>% read.csv
crime <- google.drive.spatial %>% paste0('/Scottish IMDs/2009/simd 09 crime.csv' %>% read.csv
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2009/simd 09.csv' %>% read.csv
live.out <- google.drive.spatial %>% paste0('/Scottish air pollution/scot air pollution from Gavin longform.csv' %>%
  read.csv %>% subset(time == '2009')##this is the indicator of air quality from gaving

pm25 <- './Results/Defra grid lookups/census zonal pm25 levels.csv' %>% read.csv
pm25 <- pm25 %>% subset(type == 'dz01')

##  mergers of geodata
imd <- pop %>% merge(geo, by = 'Data.Zone') %>% 
  merge(house, by = 'Data.Zone') %>% merge(crime, by = 'Data.Zone') %>%
  merge(dz.access, by.x = 'Data.Zone', by.y = 'lsoa') %>%
  merge(live.out, by.x = 'Data.Zone', by.y = 'datazone') %>%
  merge(area.tab %>% subset(type == 'dz01'), 
        by.x = 'Data.Zone', by.y = 'zone', all.x = T) %>%
  merge(pm25, by.x = 'Data.Zone', by.y = 'zone')

##  Some blank spaces on income dep and crime indicate zero; although only inc matters
summary(imd); 
miss.inc <- is.na(imd$Number.of.Income.Deprived.People.2009.V2..Revised.19.07.10.)
imd$Number.of.Income.Deprived.People.2009.V2..Revised.19.07.10.[miss.inc] <- 0


##  putting in the proportion of income deprived and non income deprived
imd$inc.n <- imd$Number.of.Income.Deprived.People.2009.V2..Revised.19.07.10. ## this is for clarity
imd$noninc.n <- imd$Total.Population..SAPE.2007. - imd$inc.n

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- data.frame(year = 2009,
                       country = 'Scotland',
                       zonecd = imd$Data.Zone,
                       la = imd$Local.Authority.Name,
                       ttwa = imd$ttwa01,
                       inc.n = imd$inc.n,
                       noninc.n = imd$noninc.n,
                       pop = imd$inc.n + imd$noninc.n,
                       dist_main = imd$dist_cen,
                       dist_nearest = imd$dist_nearest,
                       crime = imd$SIMD.Crime.2009.rank.x,
                       live.in = imd$Housing.domain.rank.2004..2006...2009,
                       live.out = imd$pol.score %>% {-1 * .} %>% rank,
                       geo = imd$Geographic.Access.domain.2009.rank.x,
                       area = imd$area,
                       pm25 = imd$year2009 %>% {-1 * .} %>% rank,
                       work = imd$access)

##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('./Results/Formatted Scotland data 2009.csv')
rm(neat.tab)

##  Step 4: Scot data 2012 -----
dz.access <- './Results/Scot access to emp 2001.csv' %>% read.csv

geo <- google.drive.spatial %>% paste0('/Scottish IMDs/2012/simd 12 geo access.csv' %>% read.csv
house <- google.drive.spatial %>% paste0('/Scottish IMDs/2012/simd 12 housing.csv' %>% read.csv
crime <- google.drive.spatial %>% paste0('/Scottish IMDs/2012/simd 12 crime.csv' %>% read.csv
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2012/simd 12.csv' %>% read.csv

pm25 <- './Results/Defra grid lookups/census zonal pm25 levels.csv' %>% read.csv
pm25 <- pm25 %>% subset(type == 'dz01')

##  mergers of geodata
imd <- pop %>% merge(geo, by = 'Data.Zone') %>% 
  merge(house, by = 'Data.Zone') %>% merge(crime, by = 'Data.Zone') %>%
  merge(dz.access, by.x = 'Data.Zone', by.y = 'lsoa') %>%
  merge(area.tab %>% subset(type == 'dz01'), 
        by.x = 'Data.Zone', by.y = 'zone', all.x = T) %>%
  merge(pm25, by.x = 'Data.Zone', by.y = 'zone')


##  Some blank spaces on income dep and crime indicate zero; although only inc matters
miss.inc <- is.na(imd$Number.of.Income.Deprived.People.2012)
imd$Number.of.Income.Deprived.People.2012[miss.inc] <- 0


##  putting in the proportion of income deprived and non income deprived
imd$inc.n <- imd$Number.of.Income.Deprived.People.2012 ## this is for clarity
imd$noninc.n <- imd$Total.Population..SAPE.2010. - imd$inc.n

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- data.frame(year = 2012,
                       country = 'Scotland',
                       zonecd = imd$Data.Zone,
                       la = imd$Local.Authority.Name,
                       ttwa = imd$ttwa01,
                       inc.n = imd$inc.n,
                       noninc.n = imd$noninc.n,
                       pop = imd$inc.n + imd$noninc.n,
                       dist_main = imd$dist_cen,
                       dist_nearest = imd$dist_nearest,
                       crime = imd$SIMD.Crime.2012.rank.x,
                       live.in = imd$Housing.domain.rank.2004..2006..2009...2012.x,
                       live.out = NA,
                       geo = imd$Geographic.Access.domain.2012.rank.x,
                       area = imd$area,
                       pm25 = imd$year2012 %>% {-1 * .} %>% rank,
                       work = imd$access)

##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('./Results/Formatted Scotland data 2012.csv')
rm(neat.tab)

##  5) Scot data 2016 ----
dz.access <- './Results/Scot access to emp 2011.csv' %>% read.csv #okay so we did not calculate access for all... why?


##  Step two: The 2016 imd is all in one place but we need to merge with ranks
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2016/simd 16 all.csv' %>% read.csv
ranks <- google.drive.spatial %>% paste0('/Scottish IMDs/2016/simd 16 ranks.csv' %>% read.csv

pm25 <- './Results/Defra grid lookups/census zonal pm25 levels.csv' %>% read.csv
pm25 <- pm25 %>% subset(type == 'dz11')

##  mergers of geodata
imd <- pop %>% merge(ranks) %>% 
  merge(dz.access, by.x = 'Data_Zone', by.y = 'lsoa') %>%
  merge(area.tab %>% subset(type == 'dz11'), 
        by.x = 'Data_Zone', by.y = 'zone', all.x = T) %>%
  merge(pm25, by.x = 'Data_Zone', by.y = 'zone')

##  Some blank spaces on income dep and crime indicate zero; although only inc matters
summary(imd); 
miss.inc <- is.na(imd$Income_count)
imd$Income_count[miss.inc] <- 0


##  putting in the proportion of income deprived and non income deprived
imd$inc.n <- imd$Income_count ## this is for clarity
imd$noninc.n <- imd$Total_population- imd$inc.n
names(imd)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- data.frame(year = 2016,
                       country = 'Scotland',
                       zonecd = imd$Data_Zone,
                       la = imd$Council_area,
                       ttwa = imd$ttwa01,
                       inc.n = imd$inc.n,
                       noninc.n = imd$noninc.n,
                       pop = imd$inc.n + imd$noninc.n,
                       dist_main = imd$dist_cen,
                       dist_nearest = imd$dist_nearest,
                       crime = imd$Crime_domain_2016_rank,
                       live.in = imd$Housing_domain_2016_rank,
                       live.out = NA,
                       geo = imd$Access_domain_2016_rank,
                       area = imd$area,
                       pm25 = imd$year2016 %>% {-1 * .} %>% rank,
                       work = imd$access)

##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('./Results/Formatted Scotland data 2016.csv')
rm(neat.tab)

##  End -----
