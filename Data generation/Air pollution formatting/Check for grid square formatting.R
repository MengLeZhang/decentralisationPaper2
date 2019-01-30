## This script is purely to check that the grid squares assigned in the 
##  Defra grid sq to datazone and lsoa work actually did it's job

##  Step one: Load in data ----
source('UI paper 1 source.R')
ukgrid = "+init=epsg:27700" ## always remember the CRS for northing easting

##  Intial files
grids <- 
  google.drive.spatial %>% paste0('/Defra air pollution/mappm252004g.csv') %>%
  read.csv
str(grids)

grids.sf <-
  st_as_sf(x = grids,
           coords = c('x', 'y'),
           crs = ukgrid)
##  Our lookup
grid2all <- './Saved generated data/Defra grids to dz and lsoa lkp.csv' %>%
  read.csv
grid2all %>% duplicated %>% table #No duplication

##  Step two: Define function to plot grid and zone object
tmap_mode('view')

grids.sf

plot_zonegrid <- function(zone.id, zone.sf, zone.col){
  st_crs(grids.sf) <- st_crs(zone.sf)
  zone.nm <- zone.sf[[zone.col]][zone.id]
  print(zone.nm)
  out <- zone.sf[zone.id, ]
  
  out.grid <- grid2all %>% subset(zone %in% zone.nm)
  dots.sf <- grids.sf %>% subset(ukgridcode %in% out.grid$ukgridcode)

  print(tm_shape(out) + tm_borders() + 
          tm_shape(dots.sf) + tm_dots())
}

##  Load in the LSOA 2001 files (sample)
lsoa2001.sf <-
  read_sf(dsn = google.drive.spatial %>% paste0('/LSOA 2001'),
          layer = 'Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales')

plot_zonegrid(4, lsoa2001.sf, 'lsoa01cd')

## Load and the datazone 2011 file
##  30/1/2019 Doesnt work but don't know why
# dz2011.sf <-
#   read_sf(dsn = google.drive.spatial %>% paste0('/Scottish datazones/2011'),
#                     layer = 'SG_SIMD_2016')
# dz2011.sf %>% head
# plot_zonegrid(1070, dz2011.sf, 'DataZone')

##  End