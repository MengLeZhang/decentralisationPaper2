##  Paper stats
##  This is a worksheet that basically generate the stats that we used in the 
##  paper from the saved results that we have
##  Start: 3/1/2018

library(tidyverse)

##  Results summary
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')

##  create quantile variable?
inequal.tab <- 
  inequal.tab %>%
  mutate(third = ntile(total.pop, 3),
         decile = ntile(total.pop, 10))

inequal.tab$third %>% table #okay so euql ns good 
inequal.tab$decile %>% table


inequal.tab %>% 
  filter(type == 'inc') %>% 
  summary

inequal.tab %>% 
  filter(type == 'inc') %>%
  group_by(third) %>%
  summarise_all(mean, na.rm = T) %>% 
  print.data.frame ## to print all

inequal.tab %>% 
  filter(type == 'inc') %>%
  group_by(decile) %>%
  summarise_all(mean, na.rm = T) %>% 
  print.data.frame ## to print all

inequal.tab %>% 
  filter(type == 'jsa') %>% 
  summary

##  England only?
wales.df <- 
  inequal.tab %>% 
  filter(is.na(rci01))
##22 ttwa
(inequal.tab %>% nrow) /2
171 - 22

inequal.tab %>% 
  filter(type == 'jsa') %>% 
  summary

inequal.tab %>% 
  filter(type == 'jsa' & !is.na(geo04)) %>% 
  summary

##  RCI difference
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')

inequal.tab %>% head
inequal.tab <- inequal.tab %>% 
  mutate(rci01_diff_method = abs(rci01 - rci01_main),
         rci11_diff_method = abs(rci11 - rci11_main)) 

inequal.tab$rci01_diff_method %>% quantile(na.rm = T, 0.9)

inequal.tab %>% 
  filter(type == 'inc') %>% 
  dplyr::select(total.pop, 
                rci01_diff_method, 
                rci11_diff_method) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')

##  Correlations matrix
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')


inequal.tab %>% 
  filter(type == 'inc') %>% 
  dplyr::select(total.pop, 
                rcidiff,
                workdiff_exp,
                geodiff) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')



inequal.tab %>% 
  filter(type == 'jsa') %>% 
  dplyr::select(total.pop, 
                rcidiff,
                workdiff_exp,
                geodiff,
                work01_exp,
                work11_exp) %>%
  cor(use = 'pairwise.complete.obs',
      method = 'spearman')
##  Cor between

##  test for cor
cor.test(formula =  ~ rcidiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 
cor.test(formula =  ~ workdiff_sq + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 
cor.test(formula =  ~ geodiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'jsa'),
         method = 'spearman') 


cor.test(formula =  ~ rcidiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 
cor.test(formula =  ~ workdiff_sq + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 
cor.test(formula =  ~ geodiff + total.pop, 
         data = inequal.tab,
         subset = (type == 'inc'),
         method = 'spearman') 

### Compare RAE in 2001 using work11_exp and work01_exp ----

master.tab <- 'Working analysis files/Master data tables of variables for LSOA01.csv' %>% 
  read.csv
master.tab %>% summary

##  omit crossborder scottish ttwa
crossborder.ttwa <- c('Hawick', 'Kelso & Jedburgh', 'Berwick', 'Carlisle')
master.tab <- master.tab %>% filter(!(TTWA11NM %in% crossborder.ttwa))

# 2) Now to calculate the various inequality indices ----------------------
# remember we have to weight our results
##  We can calculate it using different statistics actually

sens.tab_inc <- 
  master.tab %>% 
  ##  Need to created weighted pop data
  mutate(jsa01_w = jsa01 * weight,
         jsa11_w = jsa11 * weight,
         nojsa01_w = adult.pop01 * weight - jsa01_w,
         nojsa11_w = adult.pop11 * weight - jsa11_w,
         inc_n04_w =inc_n04 * weight,
         inc_n15_w = inc_n15 * weight,
         noinc_n04_w = pop04 * weight - inc_n04_w,
         noinc_n15_w = pop15 * weight - inc_n15_w) %>%
  group_by(TTWA11NM) %>%
  summarise(total.pop = (adult.pop11 * weight) %>% sum,
            n.bua = nearest_bua %>% unique %>% length,
            
            geo04 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = geo04),
            geo10 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = geo10),
            geo_diff = geo10 - geo04,
            
            work01_exp = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = - access01_exp),
            work11_exp = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = - access11_exp),
            workdiff_exp = work11_exp - work01_exp
  ) %>%
  arrange(- total.pop)

sens.tab_inc %>% summary



sens.tab_jsa <- 
  master.tab %>% 
  ##  Need to created weighted pop data
  mutate(jsa01_w = jsa01 * weight,
         jsa11_w = jsa11 * weight,
         nojsa01_w = adult.pop01 * weight - jsa01_w,
         nojsa11_w = adult.pop11 * weight - jsa11_w,
         inc_n04_w =inc_n04 * weight,
         inc_n15_w = inc_n15 * weight,
         noinc_n04_w = pop04 * weight - inc_n04_w,
         noinc_n15_w = pop15 * weight - inc_n15_w) %>%
  group_by(TTWA11NM) %>%
  summarise(total.pop = (adult.pop11 * weight) %>% sum,
            n.bua = nearest_bua %>% unique %>% length,
            
            work01_exp = dindex(x = nojsa01_w, y = jsa01_w, sort.var = - access01_exp),
            work11_exp = dindex(x = nojsa01_w, y = jsa01_w, sort.var = - access11_exp),
            workdiff_exp = work11_exp - work01_exp
  ) %>%
  arrange(- total.pop)

sens.tab_jsa %>% summary


### Supplementary material -----
##  Check difference
##  can we find a wierd exception where the total rci is just simply very different to the weighted sum
##  test this
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')
inequal.tab %>%head
inequal.tab %>% filter(TTWA11NM == 'Aberystwyth')

split.rci <- 
  master.tab %>% 
  ##  Need to created weighted pop data
  mutate(jsa01_w = jsa01 * weight,
         jsa11_w = jsa11 * weight,
         nojsa01_w = adult.pop01 * weight - jsa01_w,
         nojsa11_w = adult.pop11 * weight - jsa11_w,
         inc_n04_w =inc_n04 * weight,
         inc_n15_w = inc_n15 * weight,
         noinc_n04_w = pop04 * weight - inc_n04_w,
         noinc_n15_w = pop15 * weight - inc_n15_w) %>%
  group_by(TTWA11NM, nearest_bua) %>%
  summarise(total.pop = (adult.pop11 * weight) %>% sum,
            rci01 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = nearest_dist),
            rci01_main = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = main_dist),
            type = 'inc'
  ) 

split.rci <- split.rci %>%
  group_by(TTWA11NM) %>%
  summarise(weight_rci = weighted.mean(rci01, total.pop)) %>%
  left_join(inequal.tab %>% 
              filter(type =='inc'))

x.tab <- split.rci %>%
  mutate(diff = rci01 - weight_rci) %>%
  dplyr::select(TTWA11NM, rci01, weight_rci, diff, rci01_main)
head(split.rci)
summary(x.tab)  
x.tab %>% filter(abs(diff) > 0.05)
inequal.tab %>% filter(type == 'inc') %>% head
##  Checking RCI for 2001 with the 2011 work access stat----


