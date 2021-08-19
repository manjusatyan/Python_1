library(dplyr)
setwd("C:/Users/MANJU/Desktop/GIM/BDA TERM 2/PPA (Predictive & Prescriptive Analytics) ,3")
data <- read.csv("pollution.csv")
#View(data)

#Removing stn_code,sampling_data,Agency column, location_monitoring_station - not useful for our analysis
data<- data[-c(1,2,5,11)]
#View(data)


summary(data)

sum(is.na(data))

r_data=nrow(data)
apply(data,2,function(a) round((sum(is.na(a))/r_data)*100,2))
apply(data,2,function(a) round((sum(is.na(a))),2))
#Removing outliers


Q <- quantile(data$no2, probs=c(.25, .75), na.rm =TRUE)
iqr_1 <- IQR(data$no2,na.rm =TRUE)
iqr_2<- IQR(data$so2,na.rm =TRUE)
iqr_3 <- IQR(data$rspm,na.rm =TRUE)
iqr_3<- IQR(data$spm,na.rm =TRUE)

Q[1]<-as.numeric(Q[1])

Q[2]<-as.numeric(Q[2])
data_no2_outlier<- subset(data, data$no2 > (14) & data$no2 < (32.2))
View(data_no2_outlier)


data_<- subset(data, data$so2>(Q[1] - 1.5*iqr) & data$so2 <(Q[2]+1.5*iqr))
#435742

data<- subset(data, data$rspm >(Q[1] - 1.5*iqr) &  data$rspm < (Q[2]+1.5*iqr))


data<- subset(data, data$spm >(Q[1] - 1.5*iqr) & data$spm < (Q[2]+1.5*iqr))

View(data)
summary(data)
data<- data[-c(8)]


#Derivation for Individual Pollutant Index and AQI
data$no2[data$no2>40 & data$no2<=80] <-  data$no2*(5/4)



nic <- function(no2) 
  {
  ni=0
  if(no2<=40)
    ni= no2*50/40
  else if(no2>40 & no2<=80)
   ni= 50+(no2-40)*(50/40)
  else if(no2>80 & no2<=180)
    ni= 100+(no2-80)*(100/100)
  else if(no2>180 & no2<=280)
    ni= 200+(no2-180)*(100/100)
  else if(no2>280 & no2<=400)
    ni= 300+(no2-280)*(100/120)
  
  return (ni)
}

data$ni=data$no2.apply(nic)


