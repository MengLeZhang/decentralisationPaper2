##  Defra linkage: Now we need to use the grid to zoen information to just link
##  up all the pm2.5 info.
##  Start: 13/7/2018
source('UI paper 1 source.R')

##  Step one: Load in the various defra grid pollution files ----
defra.path <- 
  c(
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252004g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252006gh.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252007g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252009g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252010g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252012g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252015g.csv'),
    google.drive.spatial %>% paste0('/Defra air pollution/mappm252016g.csv')
  )


defra.list <- lapply(defra.path, read.csv, na.string = 'MISSING')
defra.list %>% lapply(nrow)

##  Combine the defra data
defra.all <- defra.list[[1]][, c(1,4)]

for (i in 2:length(defra.list)){
  defra.all <- defra.all %>% merge(defra.list[[i]][, c(1,4)])
}

defra.all %>% tail
summary(defra.all)

##  Great

##  Step two: Read in the grids data ----
grid2all <- 
  './Saved generated data/Defra grids to dz and lsoa lkp.csv' %>%
  read.csv

pm.wide <- 
  grid2all %>% merge(defra.all) # the big merger
pm.wide <- 
  data.table(pm.wide)
head(pm.wide)

zones.wide <- 
  pm.wide %>%
  group_by(zone, type) %>%
  summarise(
    year2004 = mean(pm252004g, na.rm = T),
    year2006 = mean(pm252006gh, na.rm = T),
    year2007 = mean(pm252007g, na.rm = T),
    year2009 = mean(pm252009g, na.rm = T),
    year2010 = mean(pm252010g, na.rm = T),
    year2012 = mean(pm252012g, na.rm = T),
    year2015 = mean(pm252015g, na.rm = T),
    year2016 = mean(pm252016g, na.rm = T)
  )

# replace(1:5, 2:3, c('a', 'b')) ## replace is useful and in base R
zones.wide

##  Step three: Now jsut save it ----
zones.wide %>% 
  write.csv('Saved generated data/Annual avg air pollution dz and lsoa.csv',
            row.names = F)
