# This is the graphics used for the revised Urban studies paper
##  First we will recreate the graphs we first used


  
# Preamble: Loading files and common datasets -----------------------------
source('RCI functions.R')
library(tidyverse)
library(gridExtra)
library(tmap)
library(tmaptools)

##  Common graph elements
##  Size of font
font1 <- element_text(size = 14)

##  Creating common datasets 
##  Load in data
inequal.tab <-
  read.csv('Working analysis files/Duncan index results table.csv')

inequal.tab %>% filter(TTWA11NM %>% grepl('Warring', x = .))

##  For maps : need CRS
ukgrid = "+init=epsg:27700" ## always remember the CRS

##  Creating a long form version; it's already in pop size order 
topten <- inequal.tab$TTWA11NM[1:10]
topten_long <- 
  inequal.tab %>%
  filter(TTWA11NM %in% topten) %>% 
  gather(key = stat, value = value, -TTWA11NM, - type) %>%
  mutate(type_stat = paste(type, stat, sep = '_')) # Stat plus type of claimant


##  Non-default colours for 2 colour palette
contrast <- c("#E69F00", "#56B4E9")

##  Graphs
##  Example graph for RCI -----
gg.tab <- 'Extra data/Example RCI table.csv' %>% read.csv
green <- rgb(0.4, 1, 0.4, alpha=0.25)

duncan <- 
  ggplot(gg.tab, aes(x = c.propB, y = c.propA)) + 
  geom_line(lwd = 2) + 
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', lwd = 2) +
  geom_ribbon(aes(ymin = c.propB, ymax = c.propA, fill = green, alpha = 0.5)) +
  scale_fill_manual(values=c(green)) + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5)) + 
#  ggtitle('Relative centralisation index') +
  theme(text = font1) + 
  ylab('Cumulative prop. of poor ranked by distance to centre (low to high)') +
  xlab('Cumulative prop. of non-poor ranked by distance to centre (low to high)')


print(duncan)

##  Sheffield example ---- 
##  This one is special since we have to load in raw data do it 
##  For this figure we need points data and Sheffield LSOA data

##  Load in the master data and 
master.tab <- 'Working analysis files/Master data tables of variables for LSOA01.csv' %>% 
  read.csv

warrington.tab <- master.tab %>% 
  filter(TTWA11NM %>% grepl('Warrington', x = .)) 

warrington.tab$nearest_bua %>% table #so we have cesterfield and barnsley but not an issue


### load in the lsoa.sf 
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales') %>%
  st_transform(crs = st_crs(ukgrid))

## joining -- we could omit different BUAs but that's not necessarily right
warrington.sf <- lsoa01.sf %>% 
  rename(LSOA01CD = lsoa01cd) %>%
  right_join(warrington.tab, by = 'LSOA01CD') %>%
  mutate(prop.inc = inc_n04 / pop04)
head(warrington.sf)
#%>% 
#  filter(nearest_bua %>% grepl('warrington', x = .) |
#           nearest_bua %>% grepl('Rotherham', x = .)) # exclude chesterfield and barnsley etc

##  warrington outline
warrington.sf_outline <- warrington.sf %>%
  group_by(TTWA11NM) %>%
  summarise(test = length(TTWA11NM))

## load in points data
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')

##  Find which TTWA they belong to
warr_centre.sf <- centres.imputed %>%
  filter(name %in% warrington.tab$nearest_bua) %>%
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = st_crs(ukgrid))

##  osmdata for warrington for context
warr.osm <- warrington.sf %>% read_osm(ext = 1)

## Now to create the actual plot
##  Can be done by facetting as well but less customisation
# tm_shape(warr.osm) +
#   tm_raster(alpha = 0.4) +
#   tm_shape(warrington.sf) +
#   tm_fill(col = c('prop.inc', 'main_dist', 'nearest_dist'), 
#           alpha = c(0.4, 0.5, 0.5),
#           style = 'cont', 
#           palette = list('Purples', 'Oranges', 'Oranges'),
#           breaks = list(
#             c(0, 0.2, 0.4, 0.6),
#             c(0, 2000, 4000, 10000),
#             c(0, 2000, 4000, 10000)
#             ),
#           labels = list(
#             c('0', '20', '40', '>60'),
#             c('0', '2000', '4000', '>10000'),
#             c('0', '2000', '4000', '>10000')
#             ),
#           legend.is.portrait = F,
#           title = c('Distance to centre')) +
#   tm_shape(warrington.sf_outline) +
#   tm_borders() +
#   tm_shape(warr_centre.sf) +
#   tm_text(text = c(NULL, 'imputed_osmm', NULL) )



prop.tmap <- 
  tm_shape(warr.osm) +
  tm_raster(alpha = 1) +
  tm_shape(warrington.sf) +
  tm_fill(col = 'prop.inc', 
          alpha = 0.8,
          style = 'cont', 
          breaks = c(0, 0.2, 0.4, 0.6),
          labels = c('0', '20', '40', '>60'),
          title = 'Percent in poverty (2001)',
          legend.is.portrait = F,
          palette = 'Purples') +
  tm_shape(warrington.sf_outline) +
  tm_borders() +
  tm_shape(warr_centre.sf) +
  tm_dots(size = 0.1) +
  tm_layout(title = '(a) Poverty (2001)',
            legend.show = F)


## individual dist graps
dist_main.tmap <-
  tm_shape(warr.osm) +
  tm_raster(alpha = 0.4) +
  tm_shape(warrington.sf) +
  tm_fill(col = 'main_dist', 
          alpha = 0.5,
          style = 'cont', 
          breaks = c(0, 2000, 4000, 10000),
          labels = c('0', '2000', '4000', '>10000'),
          legend.is.portrait = F,
          title = 'Distance to centre') +
  tm_shape(warrington.sf_outline) +
  tm_borders() +
  tm_shape(warr_centre.sf) +
  tm_text(text = 'imputed_osmm') +
  tm_layout(title = '(b) Distance (Main)',
            legend.show = F)



dist_nearest.tmap <-
  tm_shape(warr.osm) +
  tm_raster(alpha = 0.4) +
  tm_shape(warrington.sf) +
  tm_fill(col = 'nearest_dist', 
          alpha = 0.5,
          style = 'cont', 
          breaks = c(0, 2000, 4000, 10000),
          labels = c('0', '2000', '4000', '>10000'),
          legend.is.portrait = F,
          title = 'Distance to centre') +
  tm_shape(warrington.sf_outline) +
  tm_borders() +
  tm_shape(warr_centre.sf) +
  tm_dots(size = 0.1) +
  tm_layout(title = '(c) Distance (Near)',
            legend.show = F)

tmap_arrange(prop.tmap,
             dist_main.tmap, dist_nearest.tmap)

## Statistics for paper
warr_centre.sf
inequal.tab %>% filter(TTWA11NM %>% grepl('Warrington', x = .))



##  Change by population -------------
##  Basically neeed a new variable saying what type of stat it is
facet.gg <- rbind(inequal.tab %>% 
                    mutate(stat_diff = rcidiff,
                           index = '(a) RCI'),
                  inequal.tab %>% 
                    mutate(stat_diff = workdiff_exp,
                           index = '(b) RAE'),
                  inequal.tab %>% 
                    mutate(stat_diff = geodiff,
                           index = '(c) RPA'))


fig_popdiff <- 
  ggplot(data = facet.gg, 
         aes(x = log(total.pop), 
             y = stat_diff, 
             colour = type)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(alpha = 0.4) + 
  geom_smooth(alpha = 0.8, se = F) + 
  ylab(NULL) + 
  xlab('Population (log)')+
  facet_grid(. ~ index) +
  scale_colour_discrete(labels = c('Low Income', 'Jobseekers'),
                        name = 'Claimant type') +
  theme(legend.position = 'bottom',
        title = NULL,
        text = font1
        )

### Change for top ten TTWAs ----
facet.gg_topten <- 
  topten_long %>%
  filter(
    stat %in% c(
      'rci01', 'rci11',
      'work01_exp', 'work11_exp',
      'geo04', 'geo10'
    ) & 
      type == 'inc') %>%
  mutate(year = ifelse(stat %in% c('rci01', 'work01_exp', 'geo04'),
                       '2001', 
                       '2011'),
         index =stat %>% 
           plyr::revalue(c(
             rci01 = '(a) RCI',
             rci11 = '(a) RCI',
             work01_exp = '(b) RAE',
             work11_exp = '(b) RAE',
             geo04 = '(c) RPA',
             geo10 = '(c) RPA'
           )
           )
  )



fig_topten <- 
  ggplot(data = facet.gg_topten, 
          aes(x = TTWA11NM, y = value, fill = year)) + 
  geom_hline(yintercept=0) +
  geom_bar(stat='identity', position=position_dodge()) +
  ylab(NULL) + 
  xlab(NULL) +
  facet_grid(index ~ .) +
  scale_fill_manual(values = contrast, 
                    labels = c('2001', '2011'),
                    name = 'Year') +
  theme(legend.position = 'bottom',
        title = NULL,
        text = font1,
        axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        legend.direction = 'horizontal'
  )


### Older plots (don't run) ----
# #  RCI -----
# rci1 <- 
#   ggplot(data = inequal.tab, aes(x = rci01, y = rci11, group = type)) + 
#   geom_point(alpha = 0.8, aes(colour = type)) +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   geom_hline(yintercept = 0) + 
#   geom_vline(xintercept = 0) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('2011') + xlab('2001') +
#   ggtitle('(b) RCI in 2001 and 2011/12') +
#   scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 
# 
# 
# rci2 <- 
#   ggplot(data = inequal.tab, aes(x = log(total.pop), y = rcidiff, colour = type)) + 
#   geom_point(alpha = 0.8) + 
#   geom_smooth(alpha = 0.2) + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('Change in index') + xlab('Population (log)')+
#   ggtitle('(c) Index change by population (loess curve)') +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   guides(colour = F)
# 
# rci3 <- 
#   ggplot(data = 
#            topten_long %>%
#            filter(stat %in% c('rci01', 'rci11') & type == 'inc'), 
#          aes(x = TTWA11NM, y = value, fill = type_stat)) + 
#   geom_bar(stat='identity', position=position_dodge()) +
#   scale_fill_manual(values = contrast, 
#                     labels = c('2001', '2011')) +
#   #  scale_fill_discrete(labels = c('2001', '2011')) +
#   theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = 'top',
#         legend.background = element_rect(fill="transparent"),
#         legend.title = element_blank(),
#         legend.direction = 'horizontal') +
#   ylab('Relative centralisation') + 
#   xlab('')+
#   ggtitle('(a) RCI for most populated TTWAs (Poverty)') 
# 
# grid.arrange(rci3, rci1, rci2, 
#              layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
#                                    c(2, 2, 2, 2, 2, 3, 3, 3, 3))
# )
# 
# ##  Access to work -----
# work1 <- 
#   ggplot(data = inequal.tab, aes(x = work01_exp, y = work11_exp, group = type)) + 
#   geom_point(alpha = 0.8, aes(colour = type)) +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   geom_hline(yintercept = 0) + 
#   geom_vline(xintercept = 0) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('2011') + xlab('2001') +
#   ggtitle('(b) Access to employment in 2001 and 2011/12') +
#   scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 
# 
# 
# work2 <- 
#   ggplot(data = inequal.tab, aes(x = log(total.pop), y = workdiff_exp, colour = type)) + 
#   geom_point(alpha = 0.8) + 
#   geom_smooth(alpha = 0.2) + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('Change in index') + xlab('Population (log)')+
#   ggtitle('(c) Index change by population (loess curve)') +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   guides(colour = F)
# 
# work3 <- 
#   ggplot(data = 
#            topten_long %>%
#            filter(stat %in% c('work01_exp', 'work11_exp') & type == 'inc'), 
#          aes(x = TTWA11NM, y = value, fill = type_stat)) + 
#   geom_bar(stat='identity', position=position_dodge()) +
#   scale_fill_manual(values = contrast, 
#                     labels = c('2001', '2011')) +
#   #  scale_fill_discrete(labels = c('2001', '2011')) +
#   theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = 'top',
#         legend.background = element_rect(fill="transparent"),
#         legend.title = element_blank(),
#         legend.direction = 'horizontal') +
#   ylab('Relative access to employment index') + 
#   xlab('')+
#   ggtitle('(a) Access to employment for most populated TTWAs (Poverty)') 
# 
# grid.arrange(work3, work1, work2, 
#              layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
#                                    c(2, 2, 2, 2, 2, 3, 3, 3, 3))
# )
# 
# 
# 
# 
# ##  Geo ----
# geo1 <- 
#   ggplot(data = inequal.tab, aes(x = geo04, y = geo10, group = type)) + 
#   geom_point(alpha = 0.8, aes(colour = type)) +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('2011') + xlab('2001') +
#   ggtitle('(b) Proximity to amenities index in 2001 and 2011/12') +
#   scale_color_discrete(name = 'Type', labels = c('Income', 'JSA')) 
# 
# 
# geo2 <- 
#   ggplot(data = inequal.tab, aes(x = log(total.pop), y = geodiff, colour = type)) + 
#   geom_point(alpha = 0.8) + geom_smooth(alpha = 0.2) + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ylab('Change in index') + xlab('Population (log)')+
#   ggtitle('(c) Index change by population (loess curve)') +
#   geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
#   guides(colour = F)
# 
# geo3 <- 
#   ggplot(data = 
#            topten_long %>%
#            filter(stat %in% c('geo04', 'geo10') & type == 'inc'), 
#          aes(x = TTWA11NM, y = value, fill = type_stat)) + 
#   geom_bar(stat='identity', position=position_dodge()) +
#   scale_fill_manual(values = contrast, 
#                     labels = c('2001', '2011')) +
#   #  scale_fill_discrete(labels = c('2001', '2011')) +
#   theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 8),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = 'top',
#         legend.background = element_rect(fill="transparent"),
#         legend.title = element_blank(),
#         legend.direction = 'horizontal') +
#   ylab('Amenities index') + 
#   xlab('')+
#   ggtitle('(a) Proximity to amenities index for most populated TTWAs (Poverty)') 
# 
# grid.arrange(geo3, geo1, geo2, 
#              layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 1, 1),
#                                    c(2, 2, 2, 2, 2, 3, 3, 3, 3))
# )
