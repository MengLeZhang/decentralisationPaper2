##  Deriving statistics and population weighted centroids for BUASD
##  Work using pop 2011

##  Pre: Load in all the RCI functions we need
source.file <- 'RCI functions.R' #path to source
source(source.file)
ukgrid = "+init=epsg:27700" ## always remember the CRS

### 1) Load in the data for BUA and LSOA pop weighted centroids
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
##  2) Load in lsoa11 data with centroids and merge with pop data -----------
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

##  3) Do a spatial join of the lsoa file to bua and buasd then caluclate stats -------
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


all.weighted <- rbind(bua.weighted, buasd.weighted)

##  Now to save
all.weighted %>%
  write.csv('Working analysis files/bua and buasd11 pop weighted centroids and other data.csv', row.names = F)
