


#   Project : Exploratory Data Analysis | Course Project 2
#   Question 4: Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
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

# search on SCC$short.Name for anything with 'Coal' and create new data frame
SCC.coal.df <- SCC[grepl('Coal' , SCC[,3]),]

# merge the SCC coal entries with NEI 
data.merge.coal <- merge(NEI , SCC.coal.df , by.x = 'SCC' , by.y = 'SCC')

#aggregate total emissions by year
coal.agg <- aggregate( data.merge.coal[,4] , by = list(data.merge.coal[,6] , data.merge.coal[,5]) , FUN = sum , na.rm = TRUE )

#name columns after aggregation 
colnames(coal.agg) <- c('year' , 'type' , 'Emissions')

#change year data to character so GGplot doesn't scale the integers on the x-axis
coal.agg[,1] <- as.character(coal.agg[,1])

#format plot
library(ggplot2)
q4plot <- ggplot(data = coal.agg , aes(x = year , y = Emissions , fill = type)) + 
  geom_bar(stat = 'identity' ,  color = 'black') +  
  ggtitle("PM2.5 Coal Emissions Decrease from 1999 - 2008") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + 
  scale_y_continuous(name="Emissions [tons]")


#Display plot 
print (q4plot)

#create .PNG file of plot in working directory 
dev.copy(device=png,"q4plot.png" , width = 600, height = 480)
dev.off()

