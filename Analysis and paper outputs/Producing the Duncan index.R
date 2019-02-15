##  Producing the Duncan index 
##  We need to basically load in the data from tables to produce the duncan index
##  for every ttwa and/or la 

##  Step 1) Now we need to get all the data that we need from various files
eng04 <- read.csv('Saved generated data/Formatted English data 2004.csv') 
eng07 <- read.csv('Saved generated data/Formatted English data 2007.csv')
eng10 <- read.csv('Saved generated data/Formatted English data 2010.csv')
eng15 <- read.csv('Saved generated data/Formatted English data 2015.csv')
sco04 <- read.csv('Saved generated data/Formatted Scotland data 2004.csv')
sco06 <- read.csv('Saved generated data/Formatted Scotland data 2006.csv')
sco09 <- read.csv('Saved generated data/Formatted Scotland data 2009.csv')
sco12 <- read.csv('Saved generated data/Formatted Scotland data 2012.csv')
sco16 <- read.csv('Saved generated data/Formatted Scotland data 2016.csv')

combined.tab <- rbind(eng04, eng07, eng10, eng15,
                      sco04, sco06, sco09, sco12, sco16)

##  Step 2) Caculate the duncan index for every combination of year and ttwa / la
##  TTWA
combined.tab %>% str

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
            suburb = dindex(x = noninc.n, y = inc.n, sort.var = -1 * pop / area)
  )


ttwa.tab %>% write.csv('Results/Duncan index by TTWA.csv')
cor(ttwa.tab[, 4:12], use= "pairwise.complete.obs", method = 'spearman')

##  LA
la.tab <- combined.tab %>%
  group_by(year, la, country) %>% ## we want to calculate stats by year, ttwa and country
  summarise(total.pop = sum(pop), #name of summary statistic and how its calculated; i.e total.pop is sum of pop 
            total.area = sum(area),
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
            suburb = dindex(x = noninc.n, y = inc.n, sort.var =  -1 * pop / area)
  )

la.tab
la.tab %>% write.csv('./Results/Duncan index by LA.csv')
cor(la.tab[, 4:12], use= "pairwise.complete.obs", method = 'spearman')
##  End 