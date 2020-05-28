##  Worksheet for social housing etc

library(tidyverse)
library(MatchIt, quietly = T)
library(Matching, quietly = T)
library(stargazer, quietly = T)
library(lme4)

pkgs <- c('tidyverse', 'ggplot2', 'gridExtra')
lapply(pkgs, require, character.only = T)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

## Style of writing size
title_size <- element_text(size = 10) 


##  Get int results
ttwa.tab <- 'Results/Duncan index by TTWA.csv' %>% read.csv(stringsAsFactors = F)
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
  filter(year <= 2016) %>% # filter to 2016 or earlier
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

## Household data

ttwa.tab_HH <- 
  'Results/Duncan index by TTWA Households.csv' %>% read.csv(stringsAsFactors = F)


compare.tab <-
  ttwa.tab %>%
  left_join(ttwa.tab_HH,
            by = c('ttwa', 'country', 'year'),
            suffix = c('.inc', '.HH')
  ) %>%
  filter(
    country == 'England'
  ) %>%
  filter(
    year %in% c(2004, 2015) 
  )

## Simple model for change in RCI

lm(dist_nearest.HH ~ as.factor(year), compare.tab) %>% summary
lm(dist_main.HH ~ as.factor(year), compare.tab) %>% summary

## Private renting households
ttwa.tab_privHH <-
  'Results/Duncan index by TTWA private Households.csv' %>% 
  read.csv(stringsAsFactors = F)


compare.tab <-
  ttwa.tab %>%
  left_join(ttwa.tab_privHH,
            by = c('ttwa', 'country', 'year'),
            suffix = c('.inc', '.HH')
  ) %>%
  filter(
    country == 'England'
  ) %>%
  filter(
    year %in% c(2004, 2015) 
  )

## Simple model for change in RCI

lm(dist_nearest.HH ~ as.factor(year), compare.tab) %>% summary
lm(dist_main.HH ~ as.factor(year), compare.tab) %>% summary
