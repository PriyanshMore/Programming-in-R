printname <- ("Priyansh Prakash More")
prefixname <- paste("Final Project", printname, sep = ":")
prefixname

#INSTALLING PACKAGES
#install.packages(c("FSA", "FSAdata", "magrittr", "dplyr", "tidyr", "plyr", 
#                  "tidyverse", "psych", "janitor", "plotrix", "ggplot2", "lubridate", "cleaner"))

#LOADING LIBRARY
lapply(c("FSA", "FSAdata", "magrittr", "dplyr", "tidyr", "plyr", "tidyverse", 
         "psych", "janitor", "plotrix", "ggplot2", "lubridate", "cleaner"), 
       require, character.only = TRUE)

#LOADING DATASET 
options(max.print = 100000)
Priyansh_CovidData <- read.csv("Priyansh_CovidData.csv")
#Priyansh_CovidData

#CLEANING THE DATASET
Priyansh_CovidData$Date <- clean_Date(Priyansh_CovidData$Date, guess_each = TRUE)

counts <- cbind(length(Priyansh_CovidData$Countries), list(Priyansh_CovidData$Countries))
counts

unique(Priyansh_CovidData$Countries)

#DESCRIPTIVE STATISTICS
str(Priyansh_CovidData)
summary(Priyansh_CovidData)
describe(Priyansh_CovidData)

#CLEANING DATA AND RENAME COLUMNS NAMES
Priyansh_CovidData$weekly <- floor_date(Priyansh_CovidData$Date, unit = "week")
mean(Priyansh_CovidData$weekly)

Week_data <- aggregate(Priyansh_CovidData$Cases, by = list(Priyansh_CovidData$Countries, 
                                                           Priyansh_CovidData$weekly), FUN = mean)
colnames(Week_data)[colnames(Week_data) == 'Group.1'] = 'Countries'
colnames(Week_data)[colnames(Week_data) == 'Group.2'] = 'Weekly Date'
colnames(Week_data)[colnames(Week_data) == 'x'] = 'Weekly Cases in Millions'
Week_data

Priyansh_CovidData$monthly <- floor_date(Priyansh_CovidData$Date, unit = "month")
Month_data <- aggregate(Priyansh_CovidData$Cases, by = list(Priyansh_CovidData$Countries, 
                                                           Priyansh_CovidData$monthly), FUN = mean)
colnames(Month_data)[colnames(Month_data) == 'Group.1'] = 'Countries'
colnames(Month_data)[colnames(Month_data) == 'Group.2'] = 'Monthly Date'
colnames(Month_data)[colnames(Month_data) == 'x'] = 'Monthly Cases in Millions'
Month_data

#CREATING SUBSET DATASET
India <- filter(Priyansh_CovidData, Countries == "India")
#India

Afghanistan <- filter(Priyansh_CovidData, Countries == "Afghanistan")
#Afghanistan

Pakistan <- filter(Priyansh_CovidData, Countries == "Pakistan")
#Pakistan

SaudiArabia <- filter(Priyansh_CovidData, Countries == "Saudi Arabia")
#SaudiArabia

UAE <- filter(Priyansh_CovidData, Countries == "UAE")
#UAE

#FINDING AND REMOVING OUTLIERS
mean = mean(India$Cases)
sd = sd(India$Cases)

min = mean - (3*sd)
max = mean + (3*sd)

# Find Outlier
IndiaOutlier <- India$Cases[which(India$Cases < min | India$Cases > max)]
IndiaOutlier
# Remove Outlier
IndiaWithoutOutlier <- India$Cases[which(India$Cases > min & India$Cases < max)]
IndiaWithoutOutlier

#EXPLORATORY DATA ANALYSIS

#SCATTERPLOT
par(mfrow = c(1,2))
plot(India$Cases, xlim = c(0,400), ylim = c(0,300), main = "Fig 1 : With Outlier", 
     ylab = c("Covid Cases in India (in millions)"), xlab = c("Freqency of  Covid Cases in India"), 
     cex.axis = 0.8, cex.lab = 0.8, cex.main = 0.8,  col.main = "red",
     col.axis = "red")
plot(IndiaWithoutOutlier, xlim = c(0,400), ylim = c(0,300), main = "Fig 2 : Without Outlier", 
     ylab = c("Covid Cases in India (in millions)"), xlab = c("Freqency of  Covid Cases in India"),
     cex.axis = 0.8, cex.lab = 0.8,  cex.main = 0.8, col.main = "red",
     col.axis = "red")

#HISTOGRAM
def_par <- par(mar=c(6,4.5,3,3))
hist(UAE$Cases, xlim = c(0,500), ylim = c(0,100), main = "Histogram : UAE Covid Cases in 2021", 
     xlab = c("Covid Cases in UAE (in millions)"), ylab = c("Freqency of  Covid Cases in UAE"),
     cex.axis = 0.8, cex.lab = 0.9,  cex.main = 0.8, col = "cadetblue", col.main = "cadetblue",
     col.axis = "red")

#SCATTERPLOT
pchs <- c("+", "x")
cols <- c("red", "gray")
plot(UAE$Cases, UAE$Deaths, main = "COVID Cases vs Deaths in UAE 2021", 
     xlim = c(0,500), ylim = c(0,2.5), xlab = "UAE Cases (in millions)",
     ylab = "UAE Deaths (in millions)", col.main = "blue", col.axis = "blue",
     col = cols, pch = pchs)

#PIECHART

tmp <- aggregate(cbind(Deaths, Cases) ~ Countries, data = Priyansh_CovidData, FUN = sum, na.rm = TRUE)
tmp

a <- mutate(tmp, cpercent = c(round(prop.table(tmp$Cases)*100)), dpercent = c(round(prop.table(tmp$Deaths)*100)))
a

par(mfcol = c(1,2))
pie(a$cpercent, labels = paste(c(a$cpercent) ,"%",sep=""), 
    main = "Pie 1 : Piechart of Cases as per Countries", cex.main = 0.7, data = a$cpercent, fill = 1:5, 
    col = c("grey", "orange", "green", "red", "blue"), radius = 1)
legend("bottomleft", a$Countries, cex = 0.7, bty = 'n', 
       fill = c("grey", "orange", "green", "red", "blue"))
pie(a$dpercent, labels = paste(c(a$dpercent) ,"%",sep=""), 
    main = "Pie 2 : Piechart of Death as per Countries", cex.main = 0.7,data = a$dpercent, fill = 1:5, 
    col = c("grey", "orange", "green", "red", "blue"), radius = 1)

#PARETO CHART
tmp <- aggregate(cbind(Deaths, Cases) ~ Countries, data = Priyansh_CovidData, FUN = sum, na.rm = TRUE)
tmp

DeathPercent <- mutate(tmp, percent = c(round(prop.table(tmp$Deaths)*100)))
DeathPercent

Percentage <- DeathPercent[order(DeathPercent[,2], decreasing = TRUE),]
Percentage<- mutate(Percentage, Cum_cases=cumsum(Percentage$Cases), Cum_deaths=cumsum(Percentage$Deaths), cum_percent = cumsum(Percentage$percent))
Percentage

def_par <- par(mar=c(6,4.5,3,3))
pc = barplot(Percentage$Deaths,
             width = 1, space = 0.15, axes = F,
             ylim = c(0, 3.05 * max(Percentage$Deaths, na.rm = T)), 
             cex.names = 0.8, ylab = "Cummulative Death Count",
             names.arg = Percentage$Countries,
             main = 'Pareto Chart', col.main = "Cadetblue", las = 2)
lines(pc, Percentage$Cum_deaths, type = 'b', cex = 0.3, pch = 19, col='cyan4')
box(col = 'grey62')
axis(side = 2, at = c(0, round(Percentage$Cum_deaths)), las = 1, col.axis = 'grey40', col = 'grey62', cex.axis = 0.8)
axis(side = 4, at = c(0, Percentage$Cum_deaths), labels = paste(c(0, round(Percentage$cum_percent)) ,"%",sep=""), 
     las = 1, col.axis = 'cyan4', col = 'cyan4', cex.axis = 0.8)


#BOXPLOT

boxplot(India$Deaths, UAE$Deaths, UAE$Deaths, SaudiArabia$Deaths, Pakistan$Deaths, 
        main = "Boxplot : Country wise Deaths", ylim = c(0,3.5), ylab = "Death (per million)",
        col.main = "brown", col.axis = "Brown",
        xlab = "Countries", names = c("India", "Afghanistan", "UAE", "Saudi Arabia", "Pakistan"),
        las = 2, cex.axis = 0.7)
