############################################# Course Project 2 #####################################################################################
####################################### EPA National emissions Analysis ########################################################################

## Downloading Data----
if(!dir.exists("./data"))dir.create("./data")
fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileurl,destfile = "./data/data.zip") 
download_time <- Sys.time()                           # First Downloaded "2020-05-30 17:49:07 CST"

## Loading Data----
unzip("./data/data.zip",exdir = "./data")
data_emissions <- readRDS("./data/summarySCC_PM25.rds")
data_SCC <- readRDS("./data/Source_Classification_Code.rds")

str(data_emissions)
head(data_emissions)

str(data_SCC)
head(data_SCC)

## Question 1 ----
# Subsetting data by year
subdata_emissions_1999 <- subset(data_emissions, year == 1999)
subdata_emissions_2002 <- subset(data_emissions, year == 2002)
subdata_emissions_2005 <- subset(data_emissions, year == 2005)
subdata_emissions_2008 <- subset(data_emissions, year == 2008)

# Plotting emissions by year
boxplot(subdata_emissions_1999$Emissions,
        subdata_emissions_2002$Emissions,
        subdata_emissions_2005$Emissions,
        subdata_emissions_2008$Emissions)

## Evaluating outliers
outlier <- subset(subdata_emissions_2002, Emissions == max(subdata_emissions_2002$Emissions))
source_outlier <- outlier$SCC
View(subset(data_SCC, SCC == source_outlier))  ## The outlier was caused by a huge wild fire, so i wont remove it

## rescaling plot
boxplot(log(subdata_emissions_1999$Emissions),
        log(subdata_emissions_2002$Emissions),
        log(subdata_emissions_2005$Emissions),
        log(subdata_emissions_2008$Emissions))

# Computing total emissions for 1999 and 2008
total_1999 <- sum(subdata_emissions_1999$Emissions)
total_2008 <- sum(subdata_emissions_2008$Emissions)
total_1999 > total_2008                              # Total Emissions are greater in 1999 than in 2008

# Plotting total emissions by year
total_2002 <- sum(subdata_emissions_2002$Emissions)
total_2005 <- sum(subdata_emissions_2005$Emissions)
total_emissions <-  data.frame(year = c(1999,2002,2005,2008),
                               total_emissions = c(total_1999,total_2002,total_2005,total_2008))

plot(total_emissions, type = "b", main = "USA PM2.5 Total Emissions 1999 - 2008")

# Question 2
Baltimore <- "24510"
Baltimore_emissions_1999 <- subset(subdata_emissions_1999, fips = Baltimore)
Baltimore_emissions_2002 <- subset(subdata_emissions_2002, fips = Baltimore)
Baltimore_emissions_2005 <- subset(subdata_emissions_2005, fips = Baltimore)
Baltimore_emissions_2008 <- subset(subdata_emissions_2008, fips = Baltimore)
Baltimore_totalemissions <- data.frame(year = c(1999,2002,2005,2008),
                                       emissions = c(sum(Baltimore_emissions_1999$Emissions),
                                                     sum(Baltimore_emissions_2002$Emissions),
                                                     sum(Baltimore_emissions_2005$Emissions),
                                                     sum(Baltimore_emissions_2008$Emissions)))


plot(Baltimore_totalemissions, type = "b")        

# Question 3
library(ggplot2)

Baltimore_emissions <- subset(data_emissions, fiips = Baltimore)
