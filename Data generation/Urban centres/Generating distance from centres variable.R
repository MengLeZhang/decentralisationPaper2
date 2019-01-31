##  Creating distances from urban centres based on LSOA11
##  Start: 30/11

source.file <- 'RCI functions.R' #path to source
source(source.file)
ukgrid = "+init=epsg:27700" ## always remember the CRS
library(RANN)


# 1) Load in pop weighted centroids ---------------------------------------
lsoa01.sf <- st_read(
  dsn = google.drive.spatial %>%
    paste('/LSOA 2001', sep = ''),
  layer = 'Lower_Layer_Super_Output_Areas_December_2001_Population_Weighted_Centroids') %>%
  st_transform(crs = st_crs(ukgrid))

lsoa01.coords <- data.frame(lsoa01 = lsoa01.sf$lsoa01cd,
                          lsoa01.sf %>% st_coordinates)
##  merge with TTWA2011 info
lsoa01tottwa11.lkp <-
  read.csv('Working analysis files/lsoa01 to ttwa11 lkp.csv')

##  Not perfect lsoa01 to ttwa11
lsoa01.coords <- lsoa01.coords %>% left_join(lsoa01tottwa11.lkp)


# 2) Load in the centres data ---------------------------------------------
centres.imputed <-
  read.csv('Working analysis files/Imputed centres based on osmdata.csv')

##  Find which TTWA they belong to
centre.sf <- centres.imputed %>% 
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = st_crs(ukgrid))
ttwa11.sf <- st_read(dsn = google.drive.spatial %>% 
  paste('/TTWA 2011', sep = ''),
  layer = 'Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') %>%
  st_transform(crs = st_crs(ukgrid))

##  Find which centres belong inside with TTWA
centre.sf <- centre.sf %>% st_join(ttwa11.sf)
centre.sf %>% summary
centre.sf$ttwa11cd %>% is.na %>% table
##  Add to the centre.imputed data
centres.imputed <- centres.imputed %>%
  mutate(ttwa11cd = centre.sf$ttwa11cd,
         ttwa11nm = centre.sf$ttwa11nm)

##  Any over 10k and 30 k respectively
centres10k.imputed <- centres.imputed %>% filter(pop11 > 10000)
centres30k.imputed <- centres.imputed %>% filter(pop11 > 30000)


# 3) Time to find the nearest centre over 30k------------------------------
nn.list <- nn2(query = lsoa01.coords %>% dplyr::select(X, Y) %>% as.matrix,
    data = centres30k.imputed %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
    k = 1)

nearest30k.lkp <- data.frame(lsoa01cd = lsoa01.coords$lsoa01,
                             nearest_dist30k = nn.list$nn.dists %>% c,
                             nearest_bua30k = centres30k.imputed$name[nn.list$nn.idx])
nearest30k.lkp %>% 
  write.csv('Working analysis files/Distance from nearest centre for LSOA01 (30k).csv',
            row.names = F)


# 4) Finding the nearest significant centre -------------------------------
##  Critera is to include an centre over 10k but also half the the largest centre
##  for the TTWA. Also deffo include over 60k
##  Note: For reasons our ttwa lkp and distances files is combined here

ttwa.nms <- 
  lsoa01.coords$TTWA11NM %>% unique
lsoa01.coords$TTWA11NM %>% table


nearest_paper.list <- list(NULL) 

##  Big for loop to determine nearest centre -----
for (i in 1:length(ttwa.nms)){
  temp.ttwa <- ttwa.nms[i]
  print(temp.ttwa)
  
  ##  Now select the centre in the ttwa and find the largest -- this creates a 
  ##  variable called cutoff which we will use to calculat distance
  large.cent <- centres10k.imputed %>% 
    filter(ttwa11nm == temp.ttwa) %>%
    arrange(- pop11) #important to arrnge by pop size
  
  temp.cut <- 10000 # set default cut off
  
  ## set cut off if there is a centre to half largest bua
  if(large.cent %>% nrow >= 1){
    temp.cut <- large.cent$pop11[1] / 2
  }
  
  ##  Include cities over 60k
  if(temp.cut > 60000){temp.cut <- 60000}
  
  ##  Now to take the nearest distance
  temp.lsoa <- lsoa01.coords %>% 
    filter(TTWA11NM == temp.ttwa)
  
  
  temp.centre <- centres10k.imputed %>% filter(pop11 >= temp.cut)
  temp.main <- large.cent[1, ]
  
  ##  Now to run the nearest neightbour search separately ----
  ##  For the main distance -- we need an if else statement in case zeroes
  if(large.cent %>% nrow >= 1){
  nn_main <- nn2(query = temp.lsoa %>% dplyr::select(X, Y) %>% as.matrix,
                    data = temp.main %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
                    k = 1)
  temp.lsoa <- temp.lsoa %>% 
    mutate(main_dist = nn_main$nn.dists %>% c,
           main_bua = temp.main$name[nn_main$nn.idx])
  }else(
  temp.lsoa <- temp.lsoa %>% 
      mutate(main_dist = NA,
             main_bua = NA)
    
  )
  
  ##  nearest dist
  nn_nearest <- nn2(query = temp.lsoa %>% dplyr::select(X, Y) %>% as.matrix,
                   data = temp.centre %>% dplyr::select(imputed_easting, imputed_northing) %>% as.matrix,
                   k = 1)

  temp.lsoa <- temp.lsoa %>% 
    mutate(nearest_dist = nn_nearest$nn.dists %>% c,
           nearest_bua = temp.centre$name[nn_nearest$nn.idx])
  ##  Save to list
  nearest_paper.list[[i]] <- temp.lsoa
}

##  Saving the nearest distance data from section 4 ----
nearest_paper.lkp <-
  do.call(rbind, nearest_paper.list)


nearest_paper.lkp %>% 
  dplyr::select(-X, -Y) %>%
  write.csv('Working analysis files/Distance from nearest centre for LSOA01 and TTWA lkp (paper).csv',
            row.names = F)
