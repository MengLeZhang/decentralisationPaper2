##  This code is almost identical to that of a previous paper but with scotland as well
##  The scottish data is mostly copied and pasted from the SLS repo -- it uses OA
##  England its BUA nad subdivision and for scotland its localities only
##  Deriving statistics and population weighted centroids for BUASD
##  Work using pop 2011 for both countries
##  This is all a matter of calculating population weighted centroids

##  Pre: Load in all the RCI functions we need
source('UI paper 1 source.R')
ukgrid = "+init=epsg:27700" ## always remember the CRS


##  England and Wales BUA ------
### Eng 1) Load in the data for BUA and LSOA pop weighted centroids ----
bua.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA 2011', sep = ''),
  layer = 'Builtup_Areas_December_2011_Boundaries_V2') %>%
  st_transform(crs = st_crs(ukgrid))

bua.sf <- bua.sf %>% 
  filter(has_sd == 'N' & urban_bua == 'Yes') %>%
  mutate(name = bua11nm, 
         type = 'bua') %>%
  dplyr::select(name, type, geometry) #keep those without subdivision and is urban
###
buasd.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/BUA subdivision 2011', sep = ''),
  layer = 'Builtup_Area_Sub_Divisions_December_2011_Boundaries') %>%
  st_transform(crs = st_crs(ukgrid))

buasd.sf <- buasd.sf %>% 
  mutate(name = buasd11nm, 
         type = 'buasd') %>%
  dplyr::select(name, type, geometry) #keep those without subdivision and is urban


#################
##  Eng 2) Load in lsoa11 data with centroids and merge with pop data -----------
lsoa11.sf <- google.drive.spatial %>% 
  paste('/LSOA 2011', sep ='') %>%
  st_read('Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

pop.2011 <- google.drive.spatial %>% 
  paste('/LSOA 2011/LSOA pop census 2011.csv', sep ='') %>%
  read.csv

pop.2011 <- pop.2011 %>% 
  mutate(total = Age..All.usual.residents..measures..Value,
         lsoa11cd = geography.code)

lsoa11.sf <- lsoa11.sf %>% merge(pop.2011)
lsoa11.sf <- lsoa11.sf %>% cbind(lsoa11.sf %>% st_coordinates)

##  Eng 3) Do a spatial join of the lsoa file to bua and buasd then caluclate stats -------
##  First for the bua
lsoa11tobua.sf <- lsoa11.sf %>% st_join(bua.sf)
st_geometry(lsoa11tobua.sf) <- NULL

bua.weighted <-   lsoa11tobua.sf%>% 
  group_by(name) %>%
  summarise(pweight_x = X %>% weighted.mean(total),
            pweight_y = Y %>% weighted.mean(total),
            pop11 = sum(total),
            number_lsoa = total %>% length,
            type = 'bua')

lsoa11tobuasd.sf <- lsoa11.sf %>% st_join(buasd.sf)
st_geometry(lsoa11tobuasd.sf) <- NULL
buasd.weighted <-   lsoa11tobuasd.sf%>% 
  group_by(name) %>%
  summarise(pweight_x = X %>% weighted.mean(total),
            pweight_y = Y %>% weighted.mean(total),
            pop11 = sum(total),
            number_lsoa = total %>% length,
            type = 'buasd')

all.weighted_bua <- rbind(bua.weighted, buasd.weighted)

##  Now to save
all.weighted_bua %>%
  write.csv('Saved generated data/bua and buasd11 pop weighted centroids and other data.csv', row.names = F)

##End



##  Scotland localities (mostly copied and pasted) ------
### Scot 1) Load in settlements and localities ----
####  localities
loc.sf <- st_read(dsn = google.drive %>% 
                    paste('/Spatial lookup files/UK/Scottish settlements', sep = ''), 
                  layer = 'Localities2012') %>%
  st_transform(crs = st_crs(ukgrid))




#################
##  Scot 2) Load in lsoa11 data with centroids and merge with pop data -----------
##  Scottish OA files; they are assigned to their pop weighted centroids
oa.sf <- st_read(dsn = google.drive %>%
                   paste('/Spatial lookup files/UK/OA 2011/Scotland', sep = ''),
                 layer = 'OutputArea2011_PWC') %>%
  st_transform(crs = st_crs(ukgrid))

##  polygons with pop counts
oa.area <- st_read(dsn = google.drive %>%
                     paste('/Spatial lookup files/UK/OA 2011/Scotland', sep = ''),
                   layer = 'OutputArea2011_MHW') %>%
  st_transform(crs = st_crs(ukgrid))

oa.area <- oa.area %>% 
  mutate(oa.pop = Popcount) %>%
  dplyr::select(code, oa.pop)

combined.df <- oa.sf %>% st_join(oa.area) %>% st_join(loc.sf)
st_geometry(combined.df) <- NULL

##  Scot 3) Do a spatial join of the lsoa file to bua and buasd then caluclate stats -------
combined.df <- oa.sf %>% st_join(oa.area) %>% st_join(loc.sf)
st_geometry(combined.df) <- NULL
combined.df %>% head

loc.weighted <- combined.df %>% 
  group_by(L12NAME) %>% 
  mutate(easting = easting %>% as.numeric, 
         northing = northing %>% as.numeric) %>%
  summarise(pweight_x = weighted.mean(easting, oa.pop), 
            pweight_y = weighted.mean(northing, oa.pop),
            pop11 = sum(oa.pop),
            number_oa = oa.pop %>% length,
            type = 'locality') %>%
  rename(name = L12NAME)

##  Now to save
loc.weighted %>%
  write.csv('Saved generated data/locality pop weighted centroids and other data.csv', row.names = F)
##  End