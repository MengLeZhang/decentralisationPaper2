Information on air pollution comes from Defra modelled grid square data for pm2.5. https://uk-air.defra.gov.uk/data/pcm-data. Using pm2.5 gives us almost identical results for Scotland as using four other indicators (?=0.96). The use of pm2.5 was based on the promenience of this particular pollutant to human morbidity (http://webarchive.nationalarchives.gov.uk/20140505111634/http://www.comeap.org.uk/documents/statements/39-page/linking/46-mortality-burden-of-particulate-air-pollution). 

We used grid square centre points within LSOA polygons, for LSOAs without a grid square centre we used the nearest grid square based on distance. This is the same method used in the English IMD (https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/464485/English_Indices_of_Deprivation_2015_-_Technical-Report.pdf). 

The statistic for each LSOA is the averaged grid square annual pm2.5 pollution.

Steps:

1) The data is in grid sqs first so we must convert to dz and lsoa
2) Check that the grid square routine does its job (optional)
3) General the annual means
