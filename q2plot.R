
#   Project : Exploratory Data Analysis | Course Project 2
#   Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008?
#   Author: M. Brown

############################################################################################################


# define working directory, create if it does not exist and set working directory
mainDir <- "C:/Users/Mike/Documents/R/Exploratory_Data_Analysis"
if (!file.exists(mainDir)){
  dir.create(file.path(mainDir))
}

setwd("C:/Users/Mike/Documents/R/Exploratory_Data_Analysis")

# get PM25_emissions_data
data_file_path = paste(getwd() , "/PM25_emissions_data.zip" , sep = '')

download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip' , data_file_path)
unzip(data_file_path)

# create NEI and SCC objects
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset for only FIPS code = 24510 (Baltimore, MD)
NEI.24510 <- subset(NEI , NEI[,1] == 24510)

#aggregate total emissions by year
NEI_agg <- aggregate( NEI.24510[,4] , by = list(NEI.24510[,6]) , FUN = sum , na.rm = TRUE )

#forat plot
par(mar=c(4, 5, 4, 0.5))
q2plot <-barplot(NEI_agg[,2] , main = 'Total PM2.5 Emissions in Baltimore, MD Decrease \nfrom 1999 to 2008' , ylab = 'PM2.5 emitted [tons]' , names = NEI_agg[,1] , col = 'red')

#Display plot 
print (q2plot)

#create .PNG file of plot in working directory 
dev.copy(device=png,"q2plot.png" , width = 600, height = 480)
dev.off()
