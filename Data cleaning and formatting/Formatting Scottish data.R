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

allTenure <- 'Saved generated data/ census HH tenure lookup.csv' %>% read.csv

##  1) 2004 scot data -----

##  Load in primarily imd data
##  Note: Some SIMD varianbles have issues; income deprivation ns are missing
# all missing cases have the highest SIMD income rank indciatng
##  or very high therefore  missingness is due to 0 values
##  Therefore we repalce missing with 0

pop <- 
  google.drive.spatial %>% paste0('/Scottish IMDs/2004/simd 04.csv') %>% read.csv
pop <- pop %>%
  replace_na(
    list(Number.of.Current.Income.Deprived = 0)
  ) %>%
  mutate(zone = Data.Zone)

## all in pop
##  mergers of geodata
imd <- pop %>% 
  left_join(accessdz.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2004,
         country = 'Scotland',
         la = Local.Authority.Name,
         ttwa = ttwa11nm,
         pop = Total.Population..2001.Census.,
         
         inc.n = Number.of.Current.Income.Deprived,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,

         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = NA_integer_, #we are specifcying type just for neatness later
         live.in = Housing.domain.rank,
         geo = Geographic.Access.and.Telecommunications.domain.rank,
         area = area,
         pm25 = year2004 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)


##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('Saved generated data/Formatted Scotland data 2004.csv')
rm(neat.tab)

##  2) Scottish 2006 data formatting ----

#geo <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 geo access.csv') %>% read.csv
#house <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 housing.csv' %>% read.csv
#crime <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06 crime.csv' %>% read.csv
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2006/simd 06.csv') %>% read.csv


##  Add a zone id and replace NA of number ofi ncome deprived to 0
pop <- 
  pop %>% 
  replace_na(
    list(Number.of.Current.Income.Deprived.People.2006 = 0)
  ) %>%
  mutate(zone = Data.Zone)

##  mergers of geodata
imd <- pop %>% 
  left_join(accessdz.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2006,
         country = 'Scotland',
         la = Local.Authority.Name,
         ttwa = ttwa11nm,
         pop = Total.Population..SAPE.2004.,
         inc.n = Number.of.Current.Income.Deprived.People.2006,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
        
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = SIMD.Crime.2006.rank,
         live.in = Housing.domain.rank.2004.and.2006,
         geo = Geographic.Access.domain.2006.rank,
         area = area,
         pm25 = year2006 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)


##  ~~~#
summary(neat.tab) #checks
neat.tab %>% write.csv('Saved generated data/Formatted Scotland data 2006.csv')
rm(neat.tab)

##  Step 3: Scot data 2009----
##  ~~~~~ ##
##  Load SIMD data then replace Na to 0 and create zone variable
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2009/simd 09.csv') %>% read.csv

pop <- pop %>% 
  replace_na(
    list(Number.of.Income.Deprived.People.2009.V2..Revised.19.07.10. = 0)
  ) %>%
  mutate(zone = Data.Zone)

##  mergers of geodata
imd <- pop %>% 
  left_join(accessdz.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2009,
         country = 'Scotland',
         la = Local.Authority.Name,
         ttwa = ttwa11nm,
         pop = Total.Population..SAPE.2007.,
         inc.n = Number.of.Income.Deprived.People.2009.V2..Revised.19.07.10.,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = SIMD.Crime.2009.rank,
         live.in = Housing.domain.rank.2004..2006...2009,
         geo = Geographic.Access.domain.2009.rank,
         area = area,
         pm25 = year2009 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)



##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('Saved generated data/Formatted Scotland data 2009.csv')
rm(neat.tab)

##  Step 4: Scot data 2012 -----
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2012/simd 12.csv') %>% read.csv

pop <- pop %>% 
  replace_na(
    list(Number.of.Income.Deprived.People.2012 = 0)
    ) %>%
  mutate(zone = Data.Zone)

##  mergers of geodata
imd <- pop %>% 
  left_join(accessdz.01, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz01'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz01'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz01'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2001'),
            by = 'zone')


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2012,
         country = 'Scotland',
         la = Local.Authority.Name,
         ttwa = ttwa11nm,
         pop = Total.Population..SAPE.2010.,
         inc.n = Number.of.Income.Deprived.People.2012,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = SIMD.Crime.2012.rank,
         live.in = Housing.domain.rank.2004..2006..2009...2012,
         geo = Geographic.Access.domain.2012.rank,
         area = area,
         pm25 = year2012 %>% {-1 * .} %>% rank,
         work = accessB) %>%
  dplyr::select(zone, year:work, area)



##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('Saved generated data/Formatted Scotland data 2012.csv')
rm(neat.tab)

##  5) Scot data 2016 ----
pop <- google.drive.spatial %>% paste0('/Scottish IMDs/2016/simd 16 all.csv') %>% read.csv

pop <- pop %>% 
  replace_na(
    list(Income_count = 0)
    )%>%
  mutate(zone = Data_Zone)

ranks <- google.drive.spatial %>%  
  paste0('/Scottish IMDs/2016/simd 16 ranks.csv') %>% read.csv
ranks <- ranks %>%
  mutate(zone = Data_Zone)


##  mergers of geodata
imd <- pop %>% 
  left_join(ranks) %>%
  left_join(accessdz.11, 
            by = 'zone') %>%
  left_join(area.tab %>% filter(type == 'dz11'),
            by = 'zone') %>%
  left_join(dist %>% filter(zone_type == 'dz11'), 
            by = 'zone') %>%
  left_join(pm25 %>% filter(type == 'dz11'), 
            by = 'zone') %>%
  left_join(allTenure %>% filter(census == '2011'),
            by = 'zone')


nrow(pop) == nrow(imd) # so still 1 to 1 join
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##  neat table 
neat.tab <- 
  imd %>% 
  mutate(year = 2016,
         country = 'Scotland',
         la = Council_area,
         ttwa = ttwa11nm,
         pop = Total_population,
         inc.n = Income_count,
         noninc.n = pop - inc.n,
         
         social.HH = socialHH,
         nonsocial.HH = nonsocialHH,
         
         
         dist_main = main_dist,
         dist_nearest = nearest_dist,
         crime = Crime_domain_2016_rank,
         live.in = Housing_domain_2016_rank,
         geo = Access_domain_2016_rank,
         area = area,
         pm25 = year2016 %>% {-1 * .} %>% rank,
         work = accessB)%>%
  dplyr::select(zone, year:work, area)



##  ~~~#
summary(neat.tab) #checks
head(neat.tab)
neat.tab %>% write.csv('Saved generated data/Formatted Scotland data 2016.csv')
rm(neat.tab)

##  End -----

