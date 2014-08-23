
#   Project : Exploratory Data Analysis | Course Project 2
#   Question 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
#               sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes 
#               over time in motor vehicle emissions?
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

# Subset for only FIPS code = 24510 (Baltimore City, MD) or fips == 06037 (Los Angeles County, California) 
NEI.fips <- subset(NEI , NEI[,1] == '24510' | NEI[,1] == '06037')

# Subset SCC for Data.Category = 'Onroad' and create new data frame
SCC.car.df <- SCC[SCC[,2] %in% c('Onroad') , ]

# merge the SCC on-road sources entries with NEI 
data.merge.car <- merge(NEI.fips , SCC.car.df , by.x = 'SCC' , by.y = 'SCC')

#aggregate total emissions by year
car.agg <- aggregate( data.merge.car[,4] , by = list(data.merge.car[,6] , data.merge.car[,5] , data.merge.car[,2]) , FUN = sum , na.rm = TRUE )

#name columns after aggregation 
colnames(car.agg) <- c('year' , 'type' , 'Location' , 'Emissions')

#change year data to character so GGplot doesn't scale the integers on the x-axis
car.agg[,1] <- as.character(car.agg[,1])

# change FIPS code to Proper Names
car.agg[,3] <- gsub('06037' , 'Los Angeles County' , car.agg[,3] )  # FIPS to LA County
car.agg[,3] <- gsub('24510' , 'Baltimore City, MD' , car.agg[,3] )  # FIPS to Baltimore City, MD


#format plot
library(ggplot2)
qplot <- ggplot(data = car.agg , aes(x = year , y = Emissions , fill = Location ) ) + 
  geom_bar(stat = 'identity' ,  color = 'black' , position=position_dodge()) +  
  ggtitle("PM2.5 Motor Vehicle Emissions Baltimore City and Los Angeles County \nfor years 1999 - 2008") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  scale_y_continuous(name="Emissions [tons]")


#Display plot 
print (qplot)

#create .PNG file of plot in working directory 
dev.copy(device=png,"q6plot.png" , width = 600, height = 480)
dev.off()

#Second plot showing change

#subset for differences by region from 1999 to 2008
car.agg.2yr <- subset(car.agg , car.agg[,1] == '1999' | car.agg[,1] == '2008' )
car.agg.2yr.la.diff <- diff(car.agg.2yr[car.agg.2yr[,3] ==  'Los Angeles County',4])
car.agg.2yr.balt.diff <- diff(car.agg.2yr[car.agg.2yr[,3] ==  'Baltimore City, MD',4])
diff.data <- as.data.frame(rbind(c('Los Angeles County' , car.agg.2yr.la.diff) , c('Baltimore City, MD' , car.agg.2yr.balt.diff )))
diff.data[,2] <- as.character(diff.data[,2])
diff.data[,2] <- as.numeric(diff.data[,2])
colnames(diff.data) <- c('Location' , 'Emissions')


qplot <- ggplot(data = diff.data , aes(x = Location , y = Emissions  ) ) + 
  geom_bar(stat = 'identity' ,  color = 'black' , position=position_dodge()) +  
  ggtitle("CHange in PM2.5 Motor Vehicle Emissions for Baltimore City and Los Angeles County \nfrom 1999 & 2008") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  scale_y_continuous(name="Change in Emissions from 1999 to 2008 [tons]")


#Display plot 
print (qplot)

#create .PNG file of plot in working directory 
dev.copy(device=png,"q6bplot.png" , width = 600, height = 480)
dev.off()
