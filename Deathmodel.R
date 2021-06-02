####################################################################################################
###
### File:    Deathmodel.R
### Purpose: Modeling covid deaths based on positivity rate
### Authors: Keelan McMahon
### Date:    18/2/21
###
####################################################################################################

library('pracma')
allcountries = read.csv("Original_data\\Provided_data\\owid-covid-data.csv")
allcountries$date = as.Date(allcountries$date,format = "%d/%m/%Y")

countrydf = data.frame(Country=character(),modelcoef = double())
k=1
for (country  in countriestoplot){
  countrycases= allcountries[allcountries$location==country,c('date','new_cases','new_cases_smoothed','total_cases','positive_rate','new_deaths_smoothed','new_tests_smoothed','population','new_deaths')]
  countrycases = countrycases[which(countrycases$total_cases==countrycases$new_cases):nrow(countrycases),]
  
  countrycases['populationtested'] = countrycases$population/countrycases$new_tests_smoothed
  
  countrycases['smooth_pos_rate'] = movavg(countrycases$positive_rate,n=7,type='s')
  countrycases = countrycases[is.na(countrycases$smooth_pos_rate)==FALSE,]
  if(nrow(countrycases)==0) next

  exponential.model <- lm(log(countrycases$new_deaths_smoothed - min(countrycases$new_deaths_smoothed)+1)~  0+lag(smooth_pos_rate,n=7),data = countrycases ) #model accounting for negatives.
  countrydf[k,1] = country
  countrydf[k,2]= log(exponential.model$coefficients[1])
  k=k+1
}

write.csv(countrydf,file = "deathmodelcoef.csv",row.names = FALSE)
clusters = read.csv("clusters.csv")

countrydf = merge(countrydf,clusters,by.x   = 'Country',by.y = 'country')
clustercomp = ddply(countrydf, .(cluster), summarise, mean = mean(modelcoef), Lower_95_Ci = mean(modelcoef)-sd(modelcoef),Upper_95_Ci = mean(modelcoef)+1.96*sd(modelcoef))

write.csv(clustercomp,file = "clustercomp.csv",row.names = FALSE)
# ggplot() +
#   geom_point(data=countrycases, mapping=aes(x=date, y=new_cases_smoothed*0.32,col="new cases smoothed(Scaled)"))+
#   geom_point(data=countrycases, mapping=aes(x=date, y=new_deaths_smoothed,col="new deaths smoothed"))+
#   ggtitle(paste(country,"Cases vs deaths"))+
#   ylab("Cases (Scaled by 0.32)")+
#   xlab("Deaths")
# 
# ggplot() +
#   geom_point(data=countrycases, mapping=aes(x=date, y=new_deaths_smoothed,col="new deaths"))+
#   geom_point(data=countrycases, mapping=aes(x=date, y=exp(28*smooth_pos_rate),col="exp(28*positivity rate)"))+
#   ggtitle(paste(country,"Cases vs deaths"))+
#   ylab("exp(28*positivity rate)")+
#   xlab("Deaths")
