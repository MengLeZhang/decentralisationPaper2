### Formatting English data
##  This is to create the big spreadsheet that we used to format the English
##  data.

source('UI paper 1 source.R')

##  Essential step: load in commonly used data ----
##  For ease we will create a variable called 'zone' to always do the merger
##  So this is lookups for area size (for summary stats); employment access
##  and distance to centre
##  All these variable already have 'zone' as a var

area.tab <-
  'Saved generated data/Area size for lsoa and datazones lkp.csv' %>%
  read.csv

accesslsoa.01 <-
  'Saved generated data/emp access lsoa01 lkp.csv' %>% read.csv %>%
  mutate(type = '')
accesslsoa.11 <-
  'Saved generated data/emp access lsoa11 lkp.csv' %>% read.csv

dist <-
  'Saved generated data/Distance from nearest centre for zones and TTWA lkp.csv' %>%
  read.csv

pm25 <-
  'Saved generated data/Annual avg air pollution dz and lsoa.csv' %>%
  read.csv

allTenure <-
  'Saved generated data/ census HH tenure lookup.csv' %>% read.csv


##  1) 2004 English data -----
##  IMD
geo <- 
  google.drive.spatial %>% paste0('/English IMDs/2004/eimd04 geo.csv') %>% read.csv
geo <- geo %>%
  rename(zone = SOA.CODE)

crime <- 
  google.drive.spatial %>% paste0('/English IMDs/2004/SOA levelid2004 crime.csv') %>% read.csv
crime <- crime %>% 
  rename(zone = SOA)

live <- 
  google.drive.spatial %>% paste0('/English IMDs/2004/SOA levelid2004 living env sub.csv') %>% read.csv
live <- live %>%
  rename(zone = SOA.CODE)

income <- 
  google.drive.spatial %>% paste0('/English IMDs/2004/SOA levelid2004 income.csv') %>% read.csv
income <- income %>%
  rename(zone = SOA)
                                
pop <- 
  google.drive.spatial %>% paste0('/English IMDs/2004/soalevel2001 pop.csv') %>% read.csv
pop <- pop %>% 
  rename(zone = Lower.SOA.code)

##  mergers of all data
imd <- pop %>% 
  left_join(geo, 
            by = 'zone') %>%
  left_join(crime, 
            by = 'zone') %>%
  left_join(live, 
            by = 'zone') %>%
  left_join(income, 
            by = 'zone') %>%
  left_join(accesslsoa.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'lsoa01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'), #First instance of using 2001 census data
            by = 'zone')


imd %>% summary # fine


##  Now to compute the neater formatted output table
neat.tab <- 
  imd %>% 
  mutate(year = 2004,
         country = 'England',
         la = LA.name,
         ttwa = ttwa11nm,
         pop = Total.population,
         inc.n = INCOME.SCORE * Total.population,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = RANK.OF.CRIME.AND.DISORDER.SCORE..where.1.is.most.deprived.,
         live.in = Indoors.Sub.Domain %>% {-1 * .} %>% rank,
         geo = Geographical.Barriers.Sub.Domain.Score %>% {-1 * .} %>% rank,
         area = area,
         pm25 = year2004 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)



##~~~~~~~ ###
##  Save
summary(neat.tab) #checks
neat.tab %>% write.csv('Saved generated data/Formatted English data 2004.csv')
rm(neat.tab)

##  2) 2007 English data ----

##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  Load in LSOA distance file and then subset to LSOA01
geo <- google.drive.spatial %>% paste0('/English IMDs/2007/geo 07.csv') %>% read.csv
geo <- geo %>% rename(zone = LSOA.CODE)

crime <- google.drive.spatial %>% paste0('/English IMDs/2007/crime 07.csv') %>% read.csv
crime <-crime %>% rename(zone = LSOA)

live <- google.drive.spatial %>% paste0('/English IMDs/2007/living env 07.csv') %>% read.csv
live <- live %>% rename(zone = LSOA.CODE)

income <- google.drive.spatial %>% paste0('/English IMDs/2007/income 07.csv') %>% read.csv
income <- income %>% rename(zone =LSOA)

pop <- google.drive.spatial %>% paste0('/English IMDs/2007/pop denom 07.csv') %>% read.csv
pop <- pop %>% rename(zone = lsoacode)

##  Let's merge it all; imd with spatial lookup
imd <- pop %>% 
  left_join(geo, 
            by = 'zone') %>%
  left_join(crime, 
            by = 'zone') %>%
  left_join(live, 
            by = 'zone') %>%
  left_join(income, 
            by = 'zone') %>%
  left_join(accesslsoa.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'lsoa01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


imd %>% summary # fine

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2007,
         country = 'England',
         la = LA.CODE.x,
         ttwa = ttwa11nm,
         pop = Total.population..mid.2005..excluding.prisoners.,
         inc.n = INCOME.SCORE * pop,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = RANK.OF.CRIME.AND.DISORDER.SCORE..where.1.is.most.deprived.,
         live.in = Indoors.Sub.Domain %>% {-1 * .} %>% rank,
         geo = Geographical.Barriers.Sub.Domain.Score %>% {-1 * .} %>% rank,
         area = area,
         pm25 = year2007 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)



##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('Saved generated data/Formatted English data 2007.csv')
rm(neat.tab)

##  3) English 2010 data -----
##  ~~~~~~~~~~~~~~ ##
##  Load in LSOA distance file and then subset to LSOA01
imd <- google.drive.spatial %>% paste0('/English IMDs/2010/EIMD 2010.csv') %>% read.csv
imd <- imd %>% rename(zone = LSOA.CODE)

pop <- google.drive.spatial %>% paste0('/English IMDs/2010/EIMD 2010 pop.csv') %>% read.csv
pop <- pop %>% rename(zone = LSOA.CODE)

##  Let's merge it all; imd with spatial lookup
imd <- pop %>% 
  left_join(imd, 
            by = 'zone') %>%
  left_join(accesslsoa.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'lsoa01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'lsoa01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 

neat.tab <- 
  imd %>% 
  mutate(year = 2010,
         country = 'England',
         la = PRE.2009.LA.NAME,
         ttwa = ttwa11nm,
         pop = Total.population..mid.2008..excluding.prisoners.,
         inc.n = INCOME.SCORE * pop,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = CRIME.AND.DISORDER.SCORE %>% {-1 * .} %>% rank,
         live.in = Indoors.Sub.domain.Rank..where.1.is.most.deprived.,
         geo = Geographical.Barriers.Sub.domain.Rank..where.1.is.most.deprived.,
         area = area,
         pm25 = year2010 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)


##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('Saved generated data/Formatted English data 2010.csv')
rm(neat.tab)

##  4) English 2015 data ----
##  Load in LSOA distance file and then subset to LSOA11
imd <- google.drive.spatial %>% 
  paste0('/English IMDs/2015/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv') %>% read.csv
imd <- imd %>% rename(zone = LSOA.code..2011.)

pop <- google.drive.spatial %>% 
  paste0('/English IMDs/2015/File_6_ID_2015_Population_denominators.csv') %>% read.csv
pop <- pop %>% rename(zone = LSOA.code..2011.)
pop %>% head
pop %>% summary

##  Let's merge it all; imd with spatial lookup
imd <- pop %>% 
  left_join(imd, 
            by = 'zone') %>%
  left_join(accesslsoa.11, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'lsoa11'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'lsoa11'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'lsoa11'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2011'), ## Note here we use 2011 census data
            by = 'zone')



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 

neat.tab <- 
  imd %>% 
  mutate(year = 2015,
         country = 'England',
         la = Local.Authority.District.name..2013..y,
         ttwa = ttwa11nm,
         pop = Total.population..mid.2012..excluding.prisoners..x %>% 
           gsub(',', '', x = .) %>% #decimals
           as.numeric,
         inc.n = Income.Score..rate. * pop,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = Crime.Rank..where.1.is.most.deprived.,
         live.in = Indoors.Sub.domain.Rank..where.1.is.most.deprived.,
         geo = Geographical.Barriers.Sub.domain.Rank..where.1.is.most.deprived.,
         area = area,
         pm25 = year2015 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)

neat.tab 
##
summary(neat.tab) #checks
summary(neat.tab %>% filter(pop  %>% is.na)) #checks # no more missing
neat.tab %>% filter(pop  %>% is.na) #some imd population data is just missing
neat.tab %>% write.csv('Saved generated data/Formatted English data 2015.csv')
rm(neat.tab)

##  5) English 2019 data ----
##  Load in LSOA distance file and then subset to LSOA11
imd_pop <- 
  google.drive.spatial %>%
  file.path(
    'English IMDs/2019/File_6_-_IoD2019_Population_Denominators.csv'
  ) %>% 
  read.csv

imd_scores <- 
  google.drive.spatial %>%
  file.path(
    'English IMDs/2019/File_5_-_IoD2019_Scores.csv'
  ) %>% 
  read.csv


##  Merge, remove and rename zones
imd <- 
  imd_pop %>% left_join(imd_scores)
rm(imd_pop, imd_scores)

imd <-
  imd %>%
  rename(zone = LSOA.code..2011.)

##  Let's merge it all; imd with spatial lookup
imd <- 
  imd %>%
  left_join(accesslsoa.11, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'lsoa11'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'lsoa11'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'lsoa11'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2011'),
            by = 'zone')



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 

neat.tab <- 
  imd %>% 
  mutate(year = 2019,
         country = 'England',
         la = Local.Authority.District.name..2019.,
         ttwa = ttwa11nm,
         pop = Total.population..mid.2015..excluding.prisoners.,
         inc.n = Income.Score..rate. * pop,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = -1 * Crime.Score,
         live.in = -1 * Living.Environment.Score,
         geo = -1 * Geographical.Barriers.Sub.domain.Score,
         area = area,
         pm25 = year2018 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)

neat.tab %>% head
neat.tab %>% write.csv('Saved generated data/Formatted English data 2019.csv')
rm(neat.tab)


##  End ----