##  Creating distance from the centre routine for each type of zone in:
##  1) Datazones (2001) and 2011
##  2) LSOA01 and LSOA11 
##  This requires use to know which datazone and which LSOAs are where

library(RANN)

# 1) Loading in PWC and ttwa lkp -----
# A look up file for datazones and lsoas to ttwa11 is already created in lookup
##  table ...R So just load it in
zone2ttwa.lkp <-
  read.csv('Saved generated data/lsoas and datazones to ttwa lkp.csv')

##  coords are X and Y

# 2) Load in the centres data ---------------------------------------------
centres.imputed <-
  read.csv('Saved generated data/Imputed centres based on osmdata for England and Scotland.csv')

##  Find which TTWA they belong to-- so convert to sf and load in ttwa11
centre.sf <- centres.imputed %>% 
  st_as_sf(coords = c('imputed_easting', 'imputed_northing'),
           crs = st_crs(ukgrid))

ttwa11.sf <- st_read(dsn = google.drive.spatial %>% 
  paste('/TTWA 2011', sep = ''),
  layer = 'Travel_to_Work_Areas_December_2011_Full_Extent_Boundaries_in_United_Kingdom') %>%
  st_transform(crs = st_crs(ukgrid))

##  Find which centres belong inside with TTWA
centre.sf <- 
  centre.sf %>% st_join(ttwa11.sf)
centre.sf %>% summary
centre.sf %>% filter(ttwa11cd %>% is.na)# 4 centres without ttwa
## very rural though so fine

##  Add new vars to the non-sf file
centres.imputed <- 
  centres.imputed %>%
  mutate(ttwa11cd = centre.sf$ttwa11cd,
         ttwa11nm = centre.sf$ttwa11nm)
centre.sf

# 3) Finding the nearest significant centre -------------------------------
##  Critera is to include an centre over 10k but also half the the largest centre
##  for the TTWA. Also deffo include over 60k
##  Note: For reasons our ttwa lkp and distances files is combined here

zone2ttwa.lkp$ttwa11nm %>% is.na %>% sum ## okay np
ttwa.nms <- 
  zone2ttwa.lkp$ttwa11nm %>%
  na.omit %>%
  unique

nearest_paper.list <- list(NULL) 

##  Big for loop to determine nearest centre -----
for (i in 1:length(ttwa.nms)){
  temp.ttwa <- ttwa.nms[i]
  print(temp.ttwa)

  ##  Now select the centre in the ttwa and find the largest -- this creates a 
  ##  variable called cutoff which we will use to calculat distance
  ##  So filter to over 10k first and isolate to ttwa
  large.cent <- centres.imputed %>% 
    filter(pop11 > 10000) %>%
    filter(ttwa11nm == temp.ttwa) %>%
    arrange(- pop11) #important to arrnge by pop size
  
  ## Set cut off if there is a centre to half largest bua
  
  temp.cut <- 10000 # set default cut off for ttwa pop
  
  
  if(large.cent %>% nrow >= 1){
    temp.cut <- large.cent$pop11[1] / 2
  }
  
  ##  Include cities over 60k
  if(temp.cut > 60000){temp.cut <- 60000}
  
  ##  Now to take the nearest distance
  temp.lsoa <- zone2ttwa.lkp %>% 
    filter(ttwa11nm == temp.ttwa)
  
  
  temp.centre <- centres.imputed %>% filter(pop11 >= temp.cut) ## filter to ALL ttwas over pop cut off
  temp.main <- large.cent[1, ] # selecting largest bua/loc within ttwa by pop centre

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

##  Saving the nearest distance data from section 3 ----
nearest_paper.lkp <-
  do.call(rbind, nearest_paper.list)


nearest_paper.lkp %>% 
  dplyr::select(-X, -Y) %>%
  write.csv('Saved generated data/Distance from nearest centre for zones and TTWA lkp.csv',
            row.names = F)
