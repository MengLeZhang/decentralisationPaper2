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
  filter(pop04 > 70e3) %>% ## over 70k in 2004
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


##  2. Getting the correlation
cor.df <- 
  ttwa.tab %>% filter(year == 2004) 

##  Making the correlation table 
cor.vars <- c('dist_nearest', 'di','live.in', 'crime', 'geo', 'pm25', 'work')
cor.table <- cor(cor.df %>% dplyr::select(cor.vars), 
                 method = 'spearman', 
                 use = 'pairwise.complete.obs') # only modest correlation

##  Formatting it for stargazer
cor.table[cor.table %>% upper.tri(diag = T)] <- NA
ind.nms <- c('RCI', 'Diss. Index', 'Housing', 'Crime', 'Geo. access', 'Air pollution', 'Work access')
rownames(cor.table) <- ind.nms
colnames(cor.table) <- ind.nms

stargazer(cor.table, 
          type = 'text', #anything else stargazer will output like code
          title = 'Spearman\'s rank correlation matrix between indicies (2004)',
          out = '../Results/ Index correlation table.html',
          notes = 'Note: Scottish crime data excluded from calculation')

##  correlation @ 2004
cor2.table <- cor(cor.df %>% 
                   dplyr::select('dist_nearest', 'di', total.area, total.pop), 
                 method = 'spearman', 
                 use = 'pairwise.complete.obs') # only modest correlation

cor2.table #only very weak correlation; bigger areas less centralised
## however uneveness has a strong correlation with total pop


##  3. Matching using genmatch -----

## Get the 2004 data to do the matching
unmatch.tab <-
  ttwa.tab %>% 
  filter(year == '2004')


##  The matching formula
match.form <- 
  I(country == 'Scotland') ~ 
#  log(total.pop) + 
  log(total.area) + 
  dist_nearest04 + 
  di04

set.seed(123) ## genetic matchhing is a random process so we must set seed
match.res <- 
  matchit(match.form,
          ratio = 1,
          distance = 'mahalanobis',
          ## match on 201
          data =   
            unmatch.tab %>%
            dplyr::select(ttwa:total.area, 
                          dist_nearest04,
                          di04),
          method = 'nearest')

?matchit
## double check the matc
match.stats <-
  match.res %>% summary

plot(match.res)
# plot(
#   match.res %>% summary(standardize = T)
#   )


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
##  Literally exist to save lines of code
##  To parse variable names from a string to ggplot we need to use get()
## see https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot

boxplot_this <-
  function(y #String for the variable name 
           ){
    
  ggplot(common_support_df, 
         aes(y = get(y), 
             fill = country,
             weight = weights
             )
    ) +
    geom_boxplot(
      ) +
    facet_grid(cols = vars(sample))

      }
boxplot_this(y = 'total.area')
boxplot_this(y = 'total.pop')
boxplot_this(y = 'di')
boxplot_this(y = 'dist_nearest')
boxplot_this(y = 'concen') ## very different concentration

## Redone the area in log population

ggplot(common_support_df, 
       aes(y = log(total.pop), 
           fill = country,
           weight = weights
       )
) +
  geom_boxplot(
  ) +
  facet_grid(cols = vars(sample))

## 4. create into wideform
ttwa.tab2 <-
  ttwa.tab %>%
  right_join(
    match_df_for_merge %>%
      dplyr::select(ttwa, weights, sample)
  )

ttwa.tab_all <-
  ttwa.tab %>% 
  mutate(sample = 'All') %>%
  bind_rows(
    ttwa.tab2
  )

overtime_summary <-
  ttwa.tab_all %>%
  group_by(ttwa, country, sample) %>%
  summarise(
    di04 = di[year == '2004'],
    di20 = di[year == '2019/20'],
    change_di = di[year == '2004'] - di[year == '2019/20'],
    ## distance nearest
    rci04 = dist_nearest[year == '2004'],
    rci20 = dist_nearest[year == '2019/20'],
    change_rci = rci20 - rci04
    
  )

di_OverTime

di_OverTime <-
  ggplot(overtime_summary,
       aes(y = di20, 
           colour = country,
           shape = country,
           x = di04
       )
  ) +
  geom_point(
    size = 2
  ) +
  geom_abline(
    slope = 1
  ) +
  facet_grid(cols = vars(sample))

rci_OverTime <-
  ggplot(overtime_summary, 
       aes(y = rci20, 
           colour = country,
           shape = country,
           x = rci04
       )
  ) +
  geom_point(
    size = 2
  ) +
  geom_abline(
    slope = 1
  ) +
  facet_grid(cols = vars(sample))

mylegend <- 
  g_legend(di_OverTime + theme(legend.direction = 'horizontal'))


gfx <- grid.arrange(di_OverTime + ylab('DI 2019/20') + xlab('DI 2004')+ theme(legend.position="none"), 
                    rci_OverTime + ylab('RCI 2019/20') + xlab('RCI 2004')+ theme(legend.position="none")
)

grid.arrange(mylegend, gfx, nrow=2,heights=c(1, 12),
             top = 'Segregation measures 2004 - 2019/20')


##  3. Run the analysis
## it's very hard to matched by both area AND population

##  The  models 
form1 <- ~ country + I(year == '2019/20')*country
form2 <- ~ country + scale(total.pop) + scale(total.area) + dist_nearest04 + di04 ## We omit the I(year - 2004) variable since last 
##  years 2015 or 2016 is associated with country
##Note:  form2 is form1 with more variables

# Segregation indicies regression ------------------------------------------------
##  
dist.reg <- lm(form1 %>% update(dist_nearest ~ .), unmatch_df_for_merge)
dist.reg2 <- lm(form2 %>% update(dist_nearest ~ .), match_df_for_merge, weights = weights)

di.reg <- lm(form1 %>% update(di ~ .), unmatch_df_for_merge)
di.reg2 <- lm(form2 %>% update(di ~ .), match_df_for_merge, weights = weights)


## Save table
stargazer(dist.reg,
          di.reg, 
          type = 'text',
          title = 'Model results for segregation (OLS)',
          dep.var.caption = 'Segregation index',
          dep.var.labels.include = F,
          covariate.labels = c('Scotland', 'Year (2019/20 = 1)', 'Year:Scotland'),
          column.labels = c('RCI', 'DI'),
          omit = c('scale', 'dist_nearest04', 'di04'),
          no.space = T,
          out = '../Results/ Segregation index model results unmatched.html',
          keep.stat = c('n')
)

stargazer(dist.reg2,
          di.reg2, 
          type = 'text',
          title = 'Model results for segregation (matched OLS)',
          dep.var.caption = 'Segregation index',
          dep.var.labels.include = F,
          covariate.labels = c('Scotland', 'Constant'),
          column.labels = c('RCI', 'DI'),
          omit = c('scale', 'dist_nearest04', 'di04'),
          no.space = T,
          out = '../Results/ Segregation index model results matched.html',
          keep.stat = c('n')
)


