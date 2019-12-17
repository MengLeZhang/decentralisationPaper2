##  Compiling the stats of social rental households relative to other tenure groups

##  Scotland tenure by datazone -----
##  2001
scotTenure01 <- 
  google.drive.spatial %>%
  file.path('Scot census data/UV64 (datazone 2001 census tenure).csv') %>%
  read.csv(skip = 3, na.strings = '-') 

scotTenure01 <-
  scotTenure01 %>%
  mutate(
    zone = X,
    census = 2001,
    allHH = All.households,
    socialHH = Social.rented,
    nonsocialHH = allHH - socialHH,
    privateHH = 
      sum(Private.rented..Furnished., Private.rented..Unfurnished., na.rm = T)
  ) %>%
  replace_na(
    list(
    allHH = 0,
    socialHH = 0,
    nonsocialHH = 0,
    privateHH = 0
    )  
  ) %>%
  dplyr::select(zone:privateHH)


##  2011
scotTenure11 <- 
  google.drive.spatial %>%
  file.path('Scot census data/QS405SC (datazone 2011 census tenure).csv') %>%
  read.csv(skip = 3, na.strings = '-') 

scotTenure11 <-
  scotTenure11 %>%
  mutate(
    zone = X,
    census = 2011,
    allHH = All.households,
    socialHH = Social.rented,
    nonsocialHH = allHH - socialHH,
    privateHH = Private.rented
  ) %>%
  replace_na(
    list(
      allHH = 0,
      socialHH = 0,
      nonsocialHH = 0,
      privateHH = 0
    )  
  ) %>%
  dplyr::select(zone:privateHH)

##  England
## 2001
engTenure01 <- 
  google.drive.spatial %>%
  file.path('Census tenure/tenure households 2001 lsoa.csv') %>%
  read.csv(na.strings = '-') 

engTenure01 <-
  engTenure01 %>%
  mutate(
    zone = super.output.areas...lower.layer %>% substr(1, 9),
    census = 2001,
    allHH = All.categories..Tenure,
    socialHH = Social.rented..Total,
    nonsocialHH = allHH - socialHH,
    privateHH = Private.rented..Total
  ) %>%
  replace_na(
    list(
      allHH = 0,
      socialHH = 0,
      nonsocialHH = 0,
      privateHH = 0
    )  
  ) %>%
  dplyr::select(zone:privateHH)


##  2011
engTenure11 <- 
  google.drive.spatial %>%
  file.path('Census tenure/tenure households 2011 lsoa.csv') %>%
  read.csv(na.strings = '-') 

engTenure11 <-
  engTenure11 %>%
  mutate(
    zone = X2011.super.output.area...lower.layer %>% substr(1, 9),
    census = 2011,
    allHH = All.categories..Tenure,
    socialHH = Social.rented..Total,
    nonsocialHH = allHH - socialHH,
    privateHH = Private.rented..Total
  ) %>%
  replace_na(
    list(
      allHH = 0,
      socialHH = 0,
      nonsocialHH = 0,
      privateHH = 0
    )  
  ) %>%
  dplyr::select(zone:privateHH)



##  Step 2: Rbind and save ----
allTenure <- 
  rbind(engTenure01, engTenure11,
        scotTenure01, scotTenure11
  )

# allTenure %>% summary # check for no NA in HH

allTenure %>% write.csv('Saved generated data/ census HH tenure lookup.csv')

