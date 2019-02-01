##  Generating a lookup of datazones and LSOAs to ttwa
##  Do this for 1) LSOA01 and 11
##  2) datazone 2001 and 2011

##  So in the RCI paper we had a really complicated way to do this-- now we
##  just use the population weighted centroids


##  Step 1: Load in TTWA 2011 files ----
ttwa.sf <- 
  google.drive.spatial %>%
  paste0('/TTWA 2011') %>%
  st_read(layer = 'Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') %>%
  st_transform(crs = ukgrid)

# tmap_mode('plot')
# ttwa.sf %>% qtm

##  Step 2: Load in the various zone types and format them with common vars -----
##  var to distinguish
##  Using pop weighted centroids

lsoa01.sf <-
  google.drive.spatial %>%
  paste0('/LSOA 2001') %>%
  st_read(layer =
  'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = ukgrid) %>%
  mutate(zone = lsoa01cd,
         zone_type = 'lsoa01') %>%
  select.sf(zone, zone_type)

lsoa11.sf<-
  google.drive.spatial %>%
  paste0('/LSOA 2011') %>%
  st_read(layer =
            'Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids') %>%
  st_transform(crs = ukgrid) %>%
  mutate(zone = lsoa11cd,
         zone_type = 'lsoa11') %>%
  select.sf(zone, zone_type)

dz01.sf <-
  google.drive.spatial %>%
  paste0('/Scottish datazones/2001') %>%
  st_read(layer =
            'SG_DataZone_Cent_2001') %>%
  st_transform(crs = ukgrid) %>%
  mutate(zone = DZ_CODE,
         zone_type = 'dz01') %>%
  select.sf(zone, zone_type)

dz11.sf <-
  google.drive.spatial %>%
  paste0('/Scottish datazones/2011') %>%
  st_read(layer =
            'SG_DataZone_Cent_2011') %>%
  st_transform(crs = ukgrid) %>%
  mutate(zone = DataZone,
         zone_type = 'dz11') %>%
  select.sf(zone, zone_type)


##  Step 3) Merge the zones file and find which TTWA each centroid lies within ----
zones.sf <- rbind(lsoa01.sf,
                  lsoa11.sf,
                  dz01.sf,
                  dz11.sf)

zone2ttwa.sf <- zones.sf %>% st_join(ttwa.sf) ## st_within returns indicies this joins the data
##  directly

##  Step 4: Save the zones and their ttwa along with coordinates ----
zone2ttwa.pwc <- zone2ttwa.sf %>% st_coordinates# pop weighted centroids
zone2ttwa.sf <- zone2ttwa.sf %>% cbind(zones2ttwa.pwc)

st_geometry(zone2ttwa.sf) <- NULL

zone2ttwa.lkp <- 
  zone2ttwa.sf %>%
  dplyr::select(zone, zone_type, ttwa11cd, ttwa11nm, X, Y)


##  Step 5: Save output location
zone2ttwa.lkp %>% 
  write.csv('Saved generated data/lsoas and datazones to ttwa lkp.csv',
            row.names = F)
