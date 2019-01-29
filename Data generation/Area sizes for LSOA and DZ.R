##  Simple routine to extract the areas
##  download LSOA data and get the areas.
library(sf)

##  Right load in each file and then get the data
lsoa01 <-
  st_read(dsn = google.drive.spatial %>% paste('/LSOA 2001', sep = ''),
          layer = 'Lower_Layer_Super_Output_Areas_December_2001_Full_Extent_Boundaries_in_England_and_Wales')
lsoa01.tab <- data.frame(zone = lsoa01$lsoa01cd,
                         type = 'lsoa01',
                         area = st_area(lsoa01))

lsoa11 <-
  st_read(dsn = google.drive.spatial %>% paste('/LSOA 2011', sep = ''),
          layer = 'Lower_Layer_Super_Output_Areas_December_2011_Full_Extent__Boundaries_in_England_and_Wales')
lsoa11.tab <- data.frame(zone = lsoa11$lsoa11cd,
                         type = 'lsoa11',
                         area = st_area(lsoa11))

dz01 <-
  st_read(dsn = google.drive.spatial %>% paste('/Scottish datazones/2001', sep = ''),
          layer = 'SG_DataZone_Bdry_2001')
dz01.tab <- data.frame(zone = dz01$DZ_CODE,
                       type = 'dz01',
                       area = st_area(dz01))

dz11 <-
  st_read(dsn = google.drive.spatial %>% paste('/Scottish datazones/2011', sep = ''),
          layer = 'SG_SIMD_2016')
dz11.tab <- data.frame(zone = dz11$DataZone,
                       type = 'dz11',
                       area = st_area(dz11))

##  Now to save
area.tab <- rbind(lsoa01.tab, lsoa11.tab,
                  dz01.tab, dz11.tab)
area.tab %>%
  write.csv('./Saved generated data/Area size for lsoa and datazones lkp.csv')
rm(list = ls())
