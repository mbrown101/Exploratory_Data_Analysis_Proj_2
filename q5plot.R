
#   Project : Exploratory Data Analysis | Course Project 2
#   Question 5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#   Author: M. Brown

########################################################################################################


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

# Subset for only FIPS code = 24510 (Baltimore City, MD)
NEI.24510 <- subset(NEI , NEI[,1] == 24510)

# Subset SCC for Data.Category = 'Onroad' and create new data frame
SCC.car.df <- SCC[SCC[,2] %in% c('Onroad') , ]

# merge the SCC on-road sources entries with NEI 
data.merge.car <- merge(NEI.24510 , SCC.car.df , by.x = 'SCC' , by.y = 'SCC')

#aggregate total emissions by year
car.agg <- aggregate( data.merge.car[,4] , by = list(data.merge.car[,6] , data.merge.car[,5]) , FUN = sum , na.rm = TRUE )

#name columns after aggregation 
colnames(car.agg) <- c('year' , 'type' , 'Emissions')

#change year data to character so GGplot doesn't scale the integers on the x-axis
car.agg[,1] <- as.character(car.agg[,1])

#format plot
library(ggplot2)
q5plot <- ggplot(data = car.agg , aes(x = year , y = Emissions , fill = type)) + 
  geom_bar(stat = 'identity' ,  color = 'black') +  
  ggtitle("PM2.5 Motor vehicle Emissions Drop in Baltimore City for years 1999 - 2008") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  scale_y_continuous(name="Emissions [tons]")


#Display plot 
print (q5plot)

#create .PNG file of plot in working directory 
dev.copy(device=png,"q5plot.png" , width = 600, height = 480)
dev.off()
