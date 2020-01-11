library(stringr)
library(tidyr)
library(data.table)
library(rjson)
library(dplyr)
library(lubridate)
library(tidyverse)

# Dataset1
#Solution for Q1
load("C:/Users/cxie/Desktop/ExamData/birthdata.Rdata")
View(birthdata)
birthdata <- separate(birthdata, coordonnees,into=c("Latitude","Longitude"),sep=",")
as.numeric(gsub("\\)|c\\(","",birthdata[,c(Latitude,Longitude)]))
head(birthdata)
tibble_birth <- as_tibble(birthdata)
tibble_birth %>% 
  rename(
    naissances = births,
    annee = year,
    depcom = cityCode
  )

#Solution for Q2
load("C:/Users/cxie/Desktop/ExamData/deceases.Rdata")
View(deceases)
deceases <- separate(deceases, coordonnees_geo, into=c("Latitude","Longitude"),sep=",")
deceases[, grepl('^geo',colnames(deceases))] <- NULL
deceases3 <- merge(deceases,birthdata, by.x = ville, by.y =libcom)

load("C:/Users/cxie/Desktop/ExamData/deceases2.Rdata")
head(deceases2)
deceases2 <- rename(deceases2,condeinsee=cityCode, ville=city )

#Solution for Q3
load("C:/Users/cxie/Desktop/ExamData/deceasesFinal.Rdata")
View(deceasesFinal)
load("C:/Users/cxie/Desktop/ExamData/birthFinal.Rdata")
View(birthFinal)
final_merge<-merge(x = deceasesFinal, y = birthFinal, by = c("cityCode","year"), all = TRUE)

#Solution for Q4
load("C:/Users/cxie/Desktop/ExamData/life.Rdata")
View(life)
ifelse(life$Latitude == life$latitude & life$Longitude == life$latitude,TRUE,FALSE)

#Solution for Q5
boxplot_city <- function(x) {
  selected_city = life[city=x, ]
  boxplot(selected_city$birth)
}

boxplot_city(x = Lille)
boxplot_city(x = Roubaix)
boxplot_city(x = Tourcoing)

#Solution for Q6
life <- life[, Net_result := life$birth-life$deceases, by=city]
plot(life$birth,
     type="l",
     col="red",
     ylim=c(5000,20000), 
     main="Births and deceases over time",
     xlab="Year",
     ylab="Number of occurences",
     lwd=2)
lines(life$deceases, type= "l",lty=3, col="black",lwd=2)
lines(life$Net_result, type="l", lty = 5, col="orange",lwd=2)
legend("top", legend=c("Births","Deceases","Net Result"),ncol=3,bty="n",
       col=c("red","black","green"),lty=c(1,3,5),lwd=2)

#Solution for Q7
births <- aggregate(life$birth_num, by=list(City=life$city), FUN=sum)
par(oma=c(0,4,0,0))
barplot(top_n(births$birth_num,10), names.arg=births$City, xlim = c(0, max(births$birth_num)), col="lightgrey",horiz = TRUE)


# Dataset2
#Solution for Q1
weather <- read.csv("https://opendata.lillemetropole.fr/explore/dataset/prevision-meteo-lille-arome/download/?format=csv&timezone=Eroupe/Berlin&use_labels_for_header=true", 
                      sep = ";", na.strings = "NA", strip.white = TRUE, stringsAsFactors = FALSE)
#Solution for Q2
load(file = "C:/Users/cxie/Desktop/ExamData/weather.Rdata")
View(weather)
weather$Forecast.timestamp <- as.Date(weather$Forecast.timestamp)
weather$Forecast.base <- as.Date(weather$Forecast.base)

#Solution for Q3
weather$timediff <- difftime(weather$Forecast.timestamp,weather$Forecast.base, units = c("hours"))

#Solution for Q4
weather <- separate(weather, Position,into = c("Posit1","Posit2"), sep = ',')
weather$new_factor <- ifelse(weather$Posit1 <= 50.625, "S", ifelse (weather$Posit1 >= 50.7, "N", "M"))
  
#Solution for Q5
tibble_weather <- as_tibble(weather)
tibble_weather %>% 
  rename(
    X2.metre.temperature = temperature,
    Relative.humidity = humidity
  )

#Solution for Q6
tibble_weather2 <- as_tibble(weather)
tibble_weather2 %>% 
  filter(tibble_weather, timediff > 24) %>%
  group_by(new_factor) %>%
  order_by(timediff)  %>%
  summarise(avg = mean(temperature))
