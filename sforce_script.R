setwd("C:/Users/Kalapala/Desktop/sforce") #sets working directory
incidents <- read.csv("incidents.csv", header = TRUE) #reads csv file into a vector called "incidents"
dim(incidents) #finds dimensions (number of rows and columns) of incidents
head(incidents) #displays first 6 rows of incidents
tail(incidents) #displays last 6 rows of incidents

install.packages("Hmisc")
library("Hmisc")
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)

missing_data <- table(is.na.data.frame(incidents)) #displays "TRUE" for missing values in the dataframe
missing_data

number_of_crimes <- sort(table(incidents$PdDistrict)) #extracts all entries of column "PdDistrict" and also displays the levels(individual entries that have multiple occurrences)
number_of_crimes
dotchart2(number_of_crimes, dotsize = 2, xlab = "Number of crimes committed", xlim = c(10000, 370000), ylab = "SF City Police Districts", main = "Crime Distribution by Police District")

types_of_crime <- unique(incidents$Category) #displays the different categories of crime
types_of_crime

numCrimes_eachCategory <- sort(table(incidents$Category)) #displays number of crimes committed in each categorydim
numCrimes_eachCategory

months_mostCrimes <- (table(substr(incidents$Date, 0, 2)))
months_mostCrimes
dotchart2(months_mostCrimes, dotsize = 2, xlab = "Number of crimes committed", ylab = "Months", main = "Crime Distribution by Month")

spring_crimes <- sum(table(substr(incidents$Date, 0, 2))[c(3, 4, 5)])
spring_crimes

summer_crimes <- sum(table(substr(incidents$Date, 0, 2))[c(6, 7, 8)])
summer_crimes

fall_crimes <- sum(table(substr(incidents$Date, 0, 2))[c(9, 10, 11)])
fall_crimes

winter_crimes <- sum(table(substr(incidents$Date, 0, 2))[c(12, 1, 2)])
winter_crimes

crimes_by_day <- sort(table(incidents$DayOfWeek))
crimes_by_day <- crimes_by_day[c(1, 2, 4, 6, 3, 7, 5)]
crimes_by_day
dotchart2(crimes_by_day, dotsize = 2, xlab = "Number of crimes committed", ylab = "Days of the Week", main = "Crime Distribution by Day")

hours <- table(substr(incidents$Time, 0, 2))
plot(hours, xlim = c(1, 24), col="red", type="l", xlab="Time (in hours)", ylab="Number of crimes committed", main="Crime Distribution by the Hour")

years <- table(substr(incidents$Date, 7, 10))
plot(years, xlim = c(2003, 2017), col = "green", type = "l", xlab = "Years", ylab = "Number of crimes committed", main= "Crime Distribution by Year")

resol <- table(incidents$Resolution)
resol
no_resolution <- (1257737/2039943)*100
arrests_made <- ((484388+154729)/2039943)*100
remaining <- 6.02
library("plotrix")
slices <- c(remaining, no_resolution, arrests_made)
pie_labels <- c("                            Remaining - 6.02%", "Unresolved - 62.65%                              ",  "                          Arrests Made - 31.33%")
pie3D(slices, labels=pie_labels, explode = 0.1, main = "Pie Chart of Crime Resolution", col = heat.colors(length(pie_labels)))

install.packages("ggmap")
library(ggmap)
myLocation <- geocode("San Francisco")
myMap <- get_map(location = myLocation, source = "google", maptype = "roadmap", zoom = 12)

arrests <- incidents %>% filter(grepl("ARREST", Resolution))
ggmap(myMap) + geom_point(data = arrests, aes(x = X, y = Y), color = "darkred", size = 0.5, alpha = 0.01) + labs(title = "Arrests made in SF City during 2003-2017")

top_arrests <- incidents %>% group_by(Category) %>% summarize(count = n()) %>% arrange(desc(count))
plot(top_arrests, las=2, type = "l", xlab = NULL, ylab = "Number of crimes committed", ylim=c(0, 430000))
