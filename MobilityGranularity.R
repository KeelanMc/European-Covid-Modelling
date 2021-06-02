####################################################################################################
###
### File:    MobilityGranularity.R
### Purpose: Working out which countries have the highest granularity on the Covid data
### Authors: Keelan McMahon, 
### Date:    18/2/21
###
####################################################################################################




mobility = read.csv( "Original_data/Provided_data/Global_Mobility_Report.csv")
countriestoplot = str_remove(countryfits,"Rt_medians.csv")
Euromob = mobility[mobility$country_region %in% countriestoplot,]


Euromob = Euromob[Euromob$sub_region_2!= "",]


t = table(Euromob$date)
maxdate = names(t)[which.max(t)]


Euromob = Euromob[Euromob$date==maxdate,]
regions = count(Euromob$country_region)

countryfits = list.files("Output_data\\Rt_medians")
countriestoplot = str_remove(countryfits,"Rt_medians.csv")

populations = read.csv( "Original_data/Additional_data/Populationdata.csv",skip=3, header=TRUE)


populations = populations[populations$Country.Name %in% countriestoplot,c("Country.Name","X2019")]
regions = merge(regions,populations,by.x = "x",by.y = "Country.Name")
regions$average_region_density = regions$X2019/regions$freq

colnames(regions)=c("Country","Subregions with mobility data","Population","Regions/Population")
mobilitysubregion = regions

Euromob = mobility[mobility$country_region %in% countriestoplot,]
Euromob = Euromob[Euromob$sub_region_1!= ""&Euromob$sub_region_2== "",]
t = table(Euromob$date)
maxdate = names(t)[which.max(t)]

Euromob = Euromob[Euromob$date==maxdate,]
regions = count(Euromob$country_region)

populations = read.csv( "Original_data/Additional_data/Populationdata.csv",skip=3, header=TRUE)


populations = populations[populations$Country.Name %in% countriestoplot,c("Country.Name","X2019")]

regions = merge(regions,populations,by.x = "x",by.y = "Country.Name")
regions$average_region_density = regions$X2019/regions$freq


colnames(regions)=c("Country","Regions Mobility data","Population","Regions/Population")
mobilityregions =regions

coviddata = read.csv( "Original_data/Provided_data/dailynotificationeu.csv")
t = table(coviddata$date)
maxdate = names(t)[which.max(t)]

coviddata = coviddata[coviddata$date==maxdate,]

regions = count(coviddata$country)
regions =  merge(regions,populations,by.x = "x",by.y = "Country.Name")
regions$average_region_density = regions$X2019/regions$freq
colnames(regions)=c("Country","Regions with Daily Covid data","Population","Regions/Population")
dailycoviddatagranularity = regions

coviddata = read.csv( "Original_data/Provided_data/weeklynotificationeu.csv")

t = table(coviddata$year_week)
maxdate = names(t)[which.max(t)]

coviddata = coviddata[coviddata$year_week==maxdate,]

regions = count(coviddata$ï..country)
regions =  merge(regions,populations,by.x = "x",by.y = "Country.Name")
regions$average_region_density = regions$X2019/regions$freq
colnames(regions)=c("Country","Regions with Weekly Covid data","Population","Regions/Population")

weeklycoviddatagranualrity = regions

write.csv(mobilityregions,"Regions_with_mobility_data.csv")
write.csv(mobilitysubregion,"Subregions_with_mobility_data.csv")
write.csv(dailycoviddatagranularity,"Regions_with_daily_covid_data.csv")
write.csv(weeklycoviddatagranualrity,"Regions_with_weekly_covid_data.csv")
