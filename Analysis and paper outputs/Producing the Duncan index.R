##  Producing the Duncan index 
##  We need to basically load in the data from tables to produce the duncan index
##  for every ttwa using amenities values that 1) Change over time and 2) are fixed over time

##  Step 1) Now we need to get all the data that we need from various files
eng04 <- read.csv('Saved generated data/Formatted English data 2004.csv') 
eng15 <- read.csv('Saved generated data/Formatted English data 2015.csv')
eng19 <- read.csv('Saved generated data/Formatted English data 2019.csv')
sco04 <- read.csv('Saved generated data/Formatted Scotland data 2004.csv')
sco16 <- read.csv('Saved generated data/Formatted Scotland data 2016.csv')
sco20 <- read.csv('Saved generated data/Formatted Scotland data 2020.csv')


## combine tables
eng.tab <- 
  rbind(eng04, eng15, eng19) %>%
  mutate(country ='England')

sco.tab <-
  rbind(sco04, sco16, sco20) %>%
  mutate(country = 'Scotland')

combined.tab <- 
  rbind(eng.tab, sco.tab)


##  Creating the stats fixed at 2004 quantities; we do not fill in anything if imd == 2015 or 2016
##  Noted issue -- we cannot fix values @ 2004 quantities for later dates!!!

combined.tab <-
  combined.tab %>%
  group_by(zone) %>%
  mutate(
    crime_fixed = ifelse(country == 'England' & year != 2015, 
                         crime[year == '2004'], 
                         ifelse(country == 'Scotland' & year != 2016,
                                      crime[year == '2006'], 
                         NA)
                         ),# Since scotland missing crime at 2004
    geo_fixed = ifelse(year %in% c(2015, 2016), NA, geo[year == '2004']),
    pm25_fixed = ifelse(year %in% c(2015, 2016), NA, pm25[year == '2004'])
  ) %>%
  ungroup


##  Save a copy of the combined table: this is used for other linkage operations
combined.tab %>% 
  write.csv('Saved generated data/Master formated data of variables for England and Scotland (UI paper1).csv',
            row.names = F)

##  Step 2) Caculate the duncan index for every combination of year and ttwa / la
##  TTWA
ttwa.tab <- 
  combined.tab %>%
  group_by(year, ttwa, country) %>% ## we want to calculate stats by year, ttwa and country
  summarise(total.pop = sum(pop), #name of summary statistic and how its calculated; i.e total.pop is sum of pop
            total.area = sum(area),#,
            prop.inc = sum(inc.n) / sum(pop),
            dist_main = dindex(x = noninc.n, y = inc.n, sort.var = dist_main), #name of summary statistic and how its calculated
            dist_nearest = dindex(x = noninc.n, y = inc.n, sort.var = dist_nearest),
            di = sum( abs((inc.n / sum(inc.n)) - (noninc.n / sum(noninc.n))) ) / 2,
            concen = sum( abs((inc.n / sum(inc.n)) - (area / sum(area))) ) / 2,
            live.in = dindex(x = noninc.n, y = inc.n, sort.var = live.in),
            crime = dindex(x = noninc.n, y = inc.n, sort.var = crime),
            geo = dindex(x = noninc.n, y = inc.n, sort.var = geo),
            work = dindex(x = noninc.n, y = inc.n, sort.var = work),
            pm25 = dindex(x = noninc.n, y = inc.n, sort.var = pm25),
            suburb = dindex(x = noninc.n, y = inc.n, sort.var = -1 * pop / area),
  ##  Fixed Dindex            
            crime_fixed = dindex(x = noninc.n, y = inc.n, sort.var = crime_fixed),
            geo_fixed = dindex(x = noninc.n, y = inc.n, sort.var = geo_fixed),
            pm25_fixed = dindex(x = noninc.n, y = inc.n, sort.var = pm25_fixed)
            
  )

ttwa.tab %>% write.csv('Results/Duncan index by TTWA.csv')
cor(ttwa.tab[, 4:19], use= "pairwise.complete.obs", method = 'spearman')
## Large correlation between ranks for crime etc

##  2) Results using segregation by housing tenure ----
ttwa.tab_HH <- 
  combined.tab %>%
  group_by(year, ttwa, country) %>% ## we want to calculate stats by year, ttwa and country
  summarise(total.pop = sum(pop), #name of summary statistic and how its calculated; i.e total.pop is sum of pop
            total.area = sum(area),#,
            prop.inc = sum(social.HH) / sum(pop),
            dist_main = dindex(x = nonsocial.HH, y = social.HH, sort.var = dist_main), #name of summary statistic and how its calculated
            dist_nearest = dindex(x = nonsocial.HH, y = social.HH, sort.var = dist_nearest),
            di = sum( abs((social.HH / sum(social.HH)) - (nonsocial.HH / sum(nonsocial.HH))) ) / 2,
            concen = sum( abs((social.HH / sum(social.HH)) - (area / sum(area))) ) / 2,
            live.in = dindex(x = nonsocial.HH, y = social.HH, sort.var = live.in),
            crime = dindex(x = nonsocial.HH, y = social.HH, sort.var = crime),
            geo = dindex(x = nonsocial.HH, y = social.HH, sort.var = geo),
            work = dindex(x = nonsocial.HH, y = social.HH, sort.var = work),
            pm25 = dindex(x = nonsocial.HH, y = social.HH, sort.var = pm25),
            suburb = dindex(x = nonsocial.HH, y = social.HH, sort.var = -1 * pop / area),
            ##  Fixed Dindex            
            crime_fixed = dindex(x = nonsocial.HH, y = social.HH, sort.var = crime_fixed),
            geo_fixed = dindex(x = nonsocial.HH, y = social.HH, sort.var = geo_fixed),
            pm25_fixed = dindex(x = nonsocial.HH, y = social.HH, sort.var = pm25_fixed)
            
  )


ttwa.tab_HH %>% write.csv('Results/Duncan index by TTWA Households.csv')
cor(ttwa.tab_HH[, 4:19], use= "pairwise.complete.obs", method = 'spearman')


##  End