##  Neater plots for ununmatched results
##  This is an updated way to plot our results
pkgs <- c('tidyverse', 'ggplot2', 'gridExtra')
lapply(pkgs, require, character.only = T)


names(unmatch.tab)
unmatched.tab <- selected.ttwa
##  Load in results tables
unmatched.tab <- './Results/Scottish and English TTWA results unmatched (paper).csv' %>%
  read.csv(stringsAsFactors = F)

##  Error bar functions
hi.se <- function(x, t.val = 1.96 * sqrt(2) / 2){
  return(mean(x) + t.val * sd(x) / sqrt(length(x)))
}
lo.se <- function(x, t.val = 1.96 * sqrt(2) / 2){
  return(mean(x) - t.val * sd(x) / sqrt(length(x)))
}

##  Function to extract common legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}




## Segregation plots
rci.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = dist_nearest, colour = country)) + 
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean', size = 1) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Centralisation (RCI)') 
rci.gg
names(unmatched.tab)

sub.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = suburb, colour = country)) + 
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean', size = 1) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Surburbanisation')
sub.gg

di.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = di, colour = country)) + 
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean', size = 1) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Uneveness (DI)')

## One big plot
mylegend<-g_legend(rci.gg + theme(legend.direction = 'horizontal'))

seg.grid <- grid.arrange(rci.gg + xlab('') + theme(legend.position="none"), 
                         sub.gg + xlab('') + theme(legend.position="none"), 
                         di.gg + xlab('')+ theme(legend.position="none"), 
                         ncol = 2,
                         #                    top = 'Changes in Segregation',
                         left = 'Index',
                         bottom = 'Year'
)

grid.arrange(mylegend, seg.grid, nrow=2,heights=c(1, 12),
             top = 'Change in segregation (Error bars for annual comparison)')



###  Plotting the trends -----
##  Load data (if not already loaded)

house.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = live.in, colour = country)) + 
  #  geom_point(position = 'jitter', alpha = 0.1) +
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               #               fun.args = (t.val = t.val = 1.96 * sqrt(2) / 2),
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Housing quality') 


crime.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = crime, colour = country)) + 
  #  geom_point(position = 'jitter', alpha = 0.1) +
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               #               fun.args = (t.val = 1.96 * sqrt(2) / 2),
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Crime')

##
geo.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = geo, colour = country)) + 
  #  geom_point(position = 'jitter', alpha = 0.1) +
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               #               fun.args = (t.val = 1.96 * sqrt(2) / 2),
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean') +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Geographical access')

air.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = pm25, colour = country)) + 
  #  geom_point(position = 'jitter', alpha = 0.1) +
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               #               fun.args = (t.val = 1.96 * sqrt(2) / 2),
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean', size = 1) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Exposure to particulates') 

work.gg <- 
  ggplot(data = unmatched.tab, mapping =aes(x = year, y = work, colour = country)) + 
  #  geom_point(position = 'jitter', alpha = 0.1) +
  stat_summary(geom = 'errorbar', 
               fun.ymax = hi.se, 
               fun.ymin = lo.se,
               #               fun.args = (t.val = t.val = 1.96 * sqrt(2) / 2),
               alpha = 0.5) +
  stat_summary(geom = 'line', fun.y = 'mean', size = 1) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  ylab('Access to employment') +
  xlab('')# 

##  Arrange ggplots

mylegend<-g_legend(house.gg + theme(legend.direction = 'horizontal'))


gfx <- grid.arrange(house.gg + xlab('') + theme(legend.position="none"), 
                    crime.gg + xlab('') + theme(legend.position="none"), 
                    geo.gg + xlab('')+ theme(legend.position="none"), 
                    air.gg + xlab('') + theme(legend.position="none"), 
                    work.gg + xlab('') + theme(legend.position="none"), 
                    #             top = 'Changes in inequality and centralisation',
                    left = 'Index',
                    bottom = 'Year'
)

grid.arrange(mylegend, gfx, nrow=2,heights=c(1, 12),
             top = 'Change in inequality (Error bars for annual comparison)')


