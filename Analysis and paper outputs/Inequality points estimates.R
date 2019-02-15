# The information needed to calculate the inequality index ought to all be in
# a file called Master data table.... 
##  This has all the data needed to calculate whatever inequality index we need
# Now we simply just output the data

source('RCI functions.R')

# 1) Read in the master file and ttwa then merge-------------------------------
master.tab <- 'Working analysis files/Master data tables of variables for LSOA01.csv' %>% 
  read.csv
master.tab %>% summary

##  omit crossborder scottish ttwa
crossborder.ttwa <- c('Hawick', 'Kelso & Jedburgh', 'Berwick', 'Carlisle')
master.tab <- master.tab %>% filter(!(TTWA11NM %in% crossborder.ttwa))

# 2) Now to calculate the various inequality indices ----------------------
# remember we have to weight our results
##  We can calculate it using different statistics actually

inequal.tab_jsa <- 
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
  summarise(total.pop = (all.pop11 * weight) %>% sum,
            n.bua = nearest_bua %>% unique %>% length,
            rci01 = dindex(x = nojsa01_w, y = jsa01_w, sort.var = nearest_dist),
            rci11 = dindex(x = nojsa11_w, y = jsa11_w, sort.var = nearest_dist),
            rcidiff = rci11 - rci01,
            
            rci01_main = dindex(x = nojsa01_w, y = jsa01_w, sort.var = main_dist),
            rci11_main = dindex(x = nojsa11_w, y = jsa11_w, sort.var = main_dist),
            rcidiff_main = rci11_main - rci01_main,
            
            work01 = dindex(x = nojsa01_w, y = jsa01_w, sort.var = - access01),
            work01_exp = dindex(x = nojsa01_w, y = jsa01_w, sort.var = - access01_exp),
            work11 = dindex(x = nojsa11_w, y = jsa11_w, sort.var = - access11),
            work11_exp = dindex(x = nojsa11_w, y = jsa11_w, sort.var = - access11_exp),
            workdiff = work11 - work01,
            workdiff_exp = work11_exp - work01_exp,
            geo04 = dindex(x = nojsa01_w, y = jsa01_w, sort.var = geo04),
            geo10 = dindex(x = nojsa11_w, y = jsa11_w, sort.var = geo10),
            geodiff = geo10 - geo04,
            type = 'jsa'
            ) %>%
  arrange(- total.pop)

##  by income
inequal.tab_inc <- 
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
  summarise(total.pop = (all.pop11 * weight) %>% sum,
            n.bua = nearest_bua %>% unique %>% length,
            rci01 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = nearest_dist),
            rci11 = dindex(x = noinc_n15_w, y = inc_n15_w, sort.var = nearest_dist),
            rcidiff = rci11 - rci01,
            
            rci01_main = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = main_dist),
            rci11_main = dindex(x = noinc_n15_w, y = inc_n15_w, sort.var = main_dist),
            rcidiff_main = rci11_main - rci01_main,
            
            work01 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = - access01),
            work01_exp = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = - access01_exp),
            work11 = dindex(x = noinc_n15_w, y = inc_n15_w, sort.var = - access11),
            work11_exp = dindex(x = noinc_n15_w, y = inc_n15_w, sort.var = - access11_exp),
            workdiff = work11 - work01,
            workdiff_exp = work11_exp - work01_exp,
            geo04 = dindex(x = noinc_n04_w, y = inc_n04_w, sort.var = geo04),
            geo10 = dindex(x = noinc_n15_w, y = inc_n15_w, sort.var = geo10),
            geodiff = geo10 - geo04,
            type = 'inc'
  ) %>%
  arrange(- total.pop)

##
inequal.tab <- rbind(inequal.tab_jsa, inequal.tab_inc)

##  Save the results
inequal.tab %>% 
  write.csv('Working analysis files/Duncan index results table.csv', row.names = F)
