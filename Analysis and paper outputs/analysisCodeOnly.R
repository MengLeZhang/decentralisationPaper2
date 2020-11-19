##  Analysis for Paper (code only)

##  Pre-amble
source('00-utils.R')
library(MatchIt, quietly = T)
library(Matching, quietly = T)
library(stargazer, quietly = T)

## Graphing libraries
pkgs <- c('tidyverse', 'ggplot2', 'gridExtra')
lapply(pkgs, require, character.only = T)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

##  1. Isolating the sample

ttwa.tab <- 
  'Results/Duncan index by TTWA.csv' %>% read.csv(stringsAsFactors = F)

crossborder.ttwa <- c('Hawick', 'Kelso & Jedburgh', 'Berwick', 'Carlisle') #cross national ttwas to be omitted
isles.ttwa <- c(
  'Eilean Siar',
  'Mull & Islay',
  'Campbeltown',
  'Penzance & Isles of Scilly',
  'Orkney Islands',
  'Shetland Islands',
  'Skye & Lochalsh',
  'Lochaber',
  'Irvine & Arran',
  'Isle of Wight')
#islands


ttwa.tab <- 
  ttwa.tab %>% 
  filter(!(ttwa %in% crossborder.ttwa)) %>% #not a cross border ttwa
  filter(!(ttwa %in% isles.ttwa)) %>% #nor isle
  ##  We need to take these out first since crossborder ttwa are duplicated
  group_by(ttwa) %>%
  mutate(
    pop04 = total.pop[year == '2004']) %>% #variable for pop in 2004
  mutate(dist_nearest04 = dist_nearest[year == '2004']) %>%
  mutate(di04 = di[year == '2004']) %>%
  ungroup %>%
  filter(pop04 > 70000) %>% ## over 70k in 2004
  filter(!is.na(ttwa)) ##finally no missign ttwa as this code is just due to our previous routine

##  Recode IMD years to reflect just years of interest
ttwa.tab <-
  ttwa.tab %>%
  mutate(year = year %>% as.character) %>%
  mutate(
    year = 
      ifelse(year %in% c('2019', '2020'), '2019/20', year) 
  ) %>%
  filter( year %in% c('2004', '2019/20'))

##  2. Matching using genmatch -----

## Get the 2004 data to do the matching
unmatch.tab <-
  ttwa.tab %>% 
  filter(year == '2004')


##  The matching formula
match.form <- 
  I(country == 'Scotland') ~ 
  log(total.pop) + 
  log(total.area) + 
  cumNoninc_at_20 +
  cumNoninc_at_40 +
  cumNoninc_at_60 +
  cumNoninc_at_80 + 
  di04

set.seed(123) ## genetic matchhing is a random process so we must set seed
match.res <- 
  matchit(match.form,
          ratio = 2,
          distance = 'mahalanobis',
          ## match on 201
          data =   
            unmatch.tab %>%
            dplyr::select(ttwa:total.area, cumNoninc_at_20:cumNoninc_at_80, di04),
          method = 'genetic')

?matchit
## double check the matc
match.stats <-
  match.res %>% summary

plot(match.res)
plot(
  match.res %>% summary(standardize = T)
  )


##  3. Plotting and summarising common support ----

## 3.1: Go get the matched data into a seperate dataset with a col saying sample (All / Matched)
temp.match <- MatchIt::match.data(match.res)
temp.match <- temp.match %>% dplyr::select(ttwa, weights)
match_df_for_merge <- 
  unmatch.tab %>% 
  right_join(temp.match, by = 'ttwa') %>%
  mutate(
    sample = 'Matched'
  )

unmatch_df_for_merge <-
  unmatch.tab %>% 
  mutate(
    sample = 'All',
    weights = 1
  )
## Join the two datasets for plotting and checking common support
common_support_df <-
  bind_rows(match_df_for_merge, unmatch_df_for_merge)
  
##  3.2 The plots for the matching variables
ggplot(common_support_df) +
  geom_histogram(
    aes(x = log(di), y = ..density.., weight = weights, fill = country)
  ) +
  facet_grid(cols = vars(sample))
